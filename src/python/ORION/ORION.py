import re
import numpy as np

__all__ = ['read_TEC', 'write_TEC']


# One variable name of a VARIABLES list, after any blanks, tabs and commas that
# separate it from the previous one: in double quotes, in single quotes, or
# bare. A quoted name may hold blanks and commas; a bare name ends at the first
# blank, tab or comma. Two quoted names with nothing between them ("a""b") are
# two names.
_VARIABLE_NAME = re.compile(r"""[\s,]*(?:"([^"]*)"|'([^']*)'|([^\s,"'][^\s,]*))""")


def _split_variable_names(variables_str):
    """Split the text of a VARIABLES list into names, as the Fortran reader does.

    Each name is taken on its own, so quoted and bare names may be mixed:
    ``"x", 'y' z`` gives ``['x', 'y', 'z']``. An empty quoted name is skipped.
    A quote that is never closed raises ValueError.
    """
    names = []
    pos = 0
    while True:
        match = _VARIABLE_NAME.match(variables_str, pos)
        if not match:
            break
        pos = match.end()
        name = next(group for group in match.groups() if group is not None)
        if name:
            names.append(name)
    if variables_str[pos:].strip(' \t\r\n,'):
        raise ValueError('unterminated quoted variable name: ' + variables_str[pos:].strip())
    return names


def read_variables(file_path):
    with open(file_path, 'r') as file:
        head_text = file.read()

    # Extracting variables. The list runs from "VARIABLES =" to the first ZONE
    # record or the first line of numbers, over one or more lines; quoted text
    # after it, such as a zone title, is not a variable name. Names may be in
    # quotes ("rho(1)", as write_TEC writes them) or not (rho(1), as the
    # Fortran writer of ORION 1.7.0 and later writes them), as in the Fortran
    # reader: see _split_variable_names.
    variables_match = re.search(r'VARIABLES\s*=[ \t]*', head_text, re.IGNORECASE)
    if variables_match:
        list_end = re.compile(r'^[ \t]*(?:ZONE\b|[-+]?\.?\d)', re.MULTILINE | re.IGNORECASE)
        end_match = list_end.search(head_text, variables_match.end())
        end = end_match.start() if end_match else len(head_text)
        variables = _split_variable_names(head_text[variables_match.end():end])
        num_variables = len(variables)
    else:
        num_variables = 0
        variables = []

    return {
        'number': num_variables,
        'name': variables,
    }


def read_dimensions(file_path):
    """I, J and K of each zone, read from their keywords on the line of the zone that holds I=.

    Other numbers on that line, such as those of a zone title (T = "Block 1", T = B1-of-2), are not
    dimensions, and text in quotes is left out. Blanks around = are allowed (I = 4). A zone line without
    J= or K= gives fewer than three values, as before. On a ZONE record, an I, J or K whose value is not
    a whole number, such as the I=*** that a Fortran writer leaves when the number does not fit its
    field, raises a ValueError that names the file, the zone and the keyword: the size of that zone is
    not in the file.
    """
    dim = []

    with open(file_path, 'r') as file:
        lines = file.readlines()

    for line in lines:
        unquoted = re.sub(r'"[^"]*"', '""', line)
        if re.match(r'\s*ZONE\b', unquoted, re.IGNORECASE):
            for key in ('I', 'J', 'K'):
                value = re.search(r'\b' + key + r'\s*=\s*([^\s,]*)', unquoted)
                if value and not re.fullmatch(r'\d+', value.group(1)):
                    title = re.search(r'\bT\s*=\s*("[^"]*"|[^,]*)', line)
                    raise ValueError('{}: zone {}{}: {} = {!r} is not a whole number'.format(
                        file_path, len(dim) + 1, ' (T = {})'.format(title.group(1).strip()) if title else '',
                        key, value.group(1)))
        found = [re.search(r'\b' + key + r'\s*=\s*(\d+)', unquoted) for key in ('I', 'J', 'K')]
        if found[0]:
            dim.append([int(match.group(1)) for match in found if match])

    return dim


def read_geometry(lines, Nx, Ny, Nz, jumpline):
    """x, y and z of the (Nx+1) x (Ny+1) x (Nz+1) nodes of a zone, from the lines of the file (one value per line, i
    fastest, the x of all the nodes and then the y and the z). jumpline is the 1-based number of the line of the first
    value."""
    shape = (Nx + 1, Ny + 1, Nz + 1)
    n = shape[0] * shape[1] * shape[2]
    first = jumpline - 1
    return tuple(np.array(lines[first + c * n:first + (c + 1) * n], dtype=np.float64).reshape(shape, order='F')
                 for c in range(3))


def read_field(lines, N, Nx, Ny, Nz, jumpline):
    """N variables at the cells of a zone of Nx x Ny x Nz cells (one layer of cells where a direction has one plane of
    nodes), from the lines of the file, in the form of read_geometry. jumpline is the 1-based number of the line of the
    first value."""
    shape = (max(Nx, 1), max(Ny, 1), max(Nz, 1))
    n = shape[0] * shape[1] * shape[2]
    first = jumpline - 1
    return [np.array(lines[first + v * n:first + (v + 1) * n], dtype=np.float64).reshape(shape, order='F')
            for v in range(N)]


def zone_data_starts(file_path):
    """Return the 1-based line number at which each zone's numeric data begins.

    A zone is identified by its dimension line (the one carrying ``I=``); its
    data starts at the first subsequent line that parses as a number.  Scanning
    for the start of every zone (rather than assuming a fixed number of header
    lines between zones) makes the reader robust to multi-zone files where each
    zone repeats a ``ZONE T=...`` / ``I=...`` / ``DATAPACKING=...`` header block.
    """
    with open(file_path, 'r') as file:
        lines = file.readlines()

    starts = []
    n = len(lines)
    idx = 0
    while idx < n:
        if re.search(r'\bI\s*=', lines[idx]):
            j = idx + 1
            while j < n:
                try:
                    float(lines[j].strip())
                    break
                except ValueError:
                    j += 1
            starts.append(j + 1)   # 1-based, matching read_geometry's convention
            idx = j
        else:
            idx += 1
    return starts


def _location_cells(header, count):
    """The indices (1-based) of the variables that the VARLOCATION of a zone header lists as CELLCENTERED, among the first
    count; none when the header has no VARLOCATION. A variable that it does not list is at the nodes."""
    match = re.search(r'VARLOCATION\s*=\s*\((.*?)\)', header)
    cells = set()
    if not match:
        return cells
    for ranges, place in re.findall(r'\[([^\]]*)\]\s*=\s*(NODAL|CELLCENTERED)', match.group(1)):
        if place != 'CELLCENTERED':
            continue
        for part in ranges.split(','):
            first, _, last = part.strip().partition('-')
            if first.strip().isdigit():
                cells.update(range(int(first), int(last or first) + 1))
    return {i for i in cells if i <= count}


def _check_form(file_path, lines, nvariables, dimensions, data_starts):
    """Refuse the zones that read_TEC does not read, with the reason. It reads only the form of the files that ORION
    writes: BLOCK packing, x, y and z at the nodes and the other variables at the cells, one value per line. The check
    is on the header of each zone and on the number of its values, so that a file of another form (POINT packing, one or
    two coordinates, variables at the nodes, several values per line) is refused, not read as the wrong values."""
    zone_lines = [i for i, line in enumerate(lines) if re.match(r'\s*ZONE\b', line, re.IGNORECASE)]
    if len(zone_lines) != len(data_starts) or len(dimensions) != len(data_starts):
        raise ValueError('{}: {} ZONE records and {} lines with I= : the zones cannot be told'.format(
            file_path, len(zone_lines), len(data_starts)))
    for b, (z, start) in enumerate(zip(zone_lines, data_starts)):
        first = start - 1
        header = re.sub(r'"[^"]*"', '""', ' '.join(lines[z:first])).upper()
        where = '{}: zone {}'.format(file_path, b + 1)
        if re.search(r'\b(?:DATAPACKING|F)\s*=\s*POINT\b', header):
            raise ValueError('{} is in POINT packing: read_TEC reads BLOCK packing only'.format(where))
        if len(dimensions[b]) != 3:
            raise ValueError('{} has {} of I, J and K: read_TEC reads zones of three coordinates, with the three'.format(
                where, len(dimensions[b])))
        if nvariables > 3:
            cells = _location_cells(header, nvariables)
            if cells & {1, 2, 3}:
                raise ValueError('{} has a coordinate at the cells: a file of one or two coordinates, whose third variable is '
                                 'a solution variable at the cells? read_TEC reads x, y and z at the nodes'.format(where))
            if cells != set(range(4, nvariables + 1)):
                raise ValueError('{} has variables at the nodes: read_TEC reads x, y and z at the nodes and the other '
                                 'variables at the cells, as VARLOCATION says '
                                 '(VARLOCATION=([1-3]=NODAL,[4-{}]=CELLCENTERED))'.format(where, nvariables))
        # the values of the zone: up to the next ZONE record or the first data set auxiliary data record
        end = zone_lines[b + 1] if b + 1 < len(zone_lines) else len(lines)
        for i in range(first, end):
            if lines[i].lstrip().upper().startswith('DATASETAUXDATA'):
                end = i
                break
        found = sum(1 for line in lines[first:end] if line.strip())
        ni, nj, nk = dimensions[b]
        nodes = ni * nj * nk
        cells = max(ni - 1, 1) * max(nj - 1, 1) * max(nk - 1, 1)
        expected = 3 * nodes + max(nvariables - 3, 0) * cells
        if found != expected:
            raise ValueError('{} has {} lines of values, not the {} of 3 coordinates of {} nodes and {} variables of {} '
                             'cells: read_TEC reads one value per line, and this form only'.format(
                                 where, found, expected, nodes, max(nvariables - 3, 0), cells))


def read_TEC(file_path):
    """Read a Tecplot ASCII file of the form that ORION writes: zones in BLOCK packing, with x, y and z at the nodes and
    the other variables at the cells, one value per line.

    Returns xb, yb, zb, vb, names: lists with one entry per zone (the arrays of the nodes, of shape (I, J, K), and for each
    zone the list of its variables at the cells, of shape (max(I-1,1), max(J-1,1), max(K-1,1))), and the names of the
    VARIABLES list. Raises ValueError, with the reason, for a file of another form (POINT packing, one or two
    coordinates, variables at the nodes, several values per line), and OSError when it cannot be read.
    """
    variables = read_variables(file_path)
    dimensions = read_dimensions(file_path)
    data_starts = zone_data_starts(file_path)
    with open(file_path, 'r') as file:
        lines = file.readlines()
    _check_form(file_path, lines, variables['number'], dimensions, data_starts)

    xb = []; yb = []; zb = []; vb = []

    # Read mesh
    Nb = len(dimensions)
    Nvar = variables['number']-3
    for b in range(Nb):
        Nx = dimensions[b][0]-1
        Ny = dimensions[b][1]-1
        Nz = dimensions[b][2]-1

        # Geometry (X, Y, Z) begins at this zone's first data line; the field
        # variables follow immediately after in BLOCK packing.
        geom_jump = data_starts[b]
        xn, yn, zn = read_geometry(lines, Nx, Ny, Nz, geom_jump)
        xb.append(xn); yb.append(yn); zb.append(zn)

        field_jump = geom_jump + 3*(Nx+1)*(Ny+1)*(Nz+1)
        vb.append(read_field(lines, Nvar, Nx, Ny, Nz, field_jump))

    return xb, yb, zb, vb, variables['name']


def write_TEC(file_path, xb, yb, zb, vb, var_names=None):
    """Write a Tecplot ASCII file in the form that read_TEC reads: one zone per block, in BLOCK packing, with x, y and z at
    the nodes and the variables at the cells, one value per line.

    xb, yb, zb: lists with one array per block, of shape (I, J, K), the coordinates of its nodes. vb: for each block, the
    list of its variables, arrays of shape (max(I-1,1), max(J-1,1), max(K-1,1)), the values at its cells (one layer of
    cells where a direction has one plane of nodes). var_names: the names of the VARIABLES list, the coordinates first
    (default x, y, z, v1, v2, ...). The zones are named Block1, Block2, ... Raises ValueError for arrays whose shapes do
    not fit, and for a number of names that is not that of the variables, before it writes anything.
    """
    Nb = len(xb)
    if not (len(yb) == len(zb) == len(vb) == Nb) or Nb == 0:
        raise ValueError('write_TEC: xb, yb, zb and vb must be lists of the same length, one entry per block (at least one)')
    xb = [np.asarray(a, dtype=np.float64) for a in xb]
    yb = [np.asarray(a, dtype=np.float64) for a in yb]
    zb = [np.asarray(a, dtype=np.float64) for a in zb]
    vb = [[np.asarray(a, dtype=np.float64) for a in block] for block in vb]
    Nv = len(vb[0])
    for b in range(Nb):
        if xb[b].ndim != 3 or xb[b].shape != yb[b].shape or xb[b].shape != zb[b].shape:
            raise ValueError('write_TEC: block {}: x, y and z must be arrays of the same shape (I, J, K)'.format(b + 1))
        cells = tuple(max(n - 1, 1) for n in xb[b].shape)
        if len(vb[b]) != Nv:
            raise ValueError('write_TEC: block {} has {} variables, block 1 has {}'.format(b + 1, len(vb[b]), Nv))
        for v, a in enumerate(vb[b]):
            if a.shape != cells:
                raise ValueError('write_TEC: block {}, variable {}: shape {}, not {}, the cells of its {} nodes'.format(
                    b + 1, v + 1, a.shape, cells, xb[b].shape))
    if var_names is None:
        var_names = ['x', 'y', 'z'] + ['v{}'.format(v + 1) for v in range(Nv)]
    elif len(var_names) != 3 + Nv:
        raise ValueError('write_TEC: {} names for 3 coordinates and {} variables'.format(len(var_names), Nv))

    if Nv == 0:
        location = '[1-3]=NODAL'
    elif Nv == 1:
        location = '[1-3]=NODAL,[4]=CELLCENTERED'
    else:
        location = '[1-3]=NODAL,[4-{}]=CELLCENTERED'.format(3 + Nv)
    with open(file_path, 'w') as file:
        file.write('VARIABLES = ' + ' '.join('"{}"'.format(name) for name in var_names) + '\n')
        for b in range(Nb):
            Nx, Ny, Nz = xb[b].shape
            file.write('ZONE T="Block{}", I={}, J={}, K={}, DATAPACKING=BLOCK, VARLOCATION=({})\n'.format(
                b + 1, Nx, Ny, Nz, location))
            for a in [xb[b], yb[b], zb[b]] + vb[b]:
                # i fastest, as the Fortran writer: the values of the array in Fortran order, in the shortest form that
                # gives them back
                file.write(''.join('{!r}\n'.format(value) for value in a.flatten(order='F').tolist()))
