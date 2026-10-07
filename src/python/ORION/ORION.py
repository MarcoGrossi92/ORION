import re
import numpy as np

def count_lines_before_float(file_path):
    line_count = 0

    with open(file_path, 'r') as file:
        for line in file:
            line_count += 1

            # Check if the line contains only a floating-point number
            try:
                float_value = float(line.strip())
                # If we successfully convert the line to a float, we've found a line with a sole floating number
                break
            except ValueError:
                # The line does not contain a floating-point number, continue to the next line

                # Additionally, if the line starts with a keyword (e.g., "VARIABLES", "Zone T"),
                # we consider it a non-data line and continue searching for the floating-point number
                if line.strip().startswith(("VARIABLES", "Zone T", "SOLUTIONTIME", "DATAPACKING", "VARLOCATION")):
                    continue

    return line_count


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
    
    dim = []

    with open(file_path, 'r') as file:
        lines = file.readlines()

    for line in lines:
        if 'I=' in line:
            pattern = r'\b\d+\b'
            numbers = re.findall(pattern, line)
            numbers = [int(num) for num in numbers]
            dim.append(numbers[0:3])

    return dim


def read_geometry(file_path,Nx,Ny,Nz,jumpline):
    MESH = open(file_path,"r")
    MESH = MESH.readlines()

    Ni = Nx+1; Nj = Ny+1; Nk = Nz+1

    line = jumpline-2
    xn = np.zeros((Ni, Nj, Nk))
    yn = np.zeros((Ni, Nj, Nk))
    zn = np.zeros((Ni, Nj, Nk))

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                xn[i,j,k] = float(MESH[line])

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                yn[i,j,k] = float(MESH[line])

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                zn[i,j,k] = float(MESH[line])

    # xc = np.zeros((Nx, Ny, Nz))
    # yc = np.zeros((Nx, Ny, Nz))
    # zc = np.zeros((Nx, Ny, Nz))
    # for k in range(Nz):
    #     for j in range(Ny):
    #         for i in range(Nx):
    #             xc[i,j,k] = 0.125*(xn[i,j,k]+xn[i+1,j,k]+xn[i,j+1,k]+xn[i,j,k+1]+xn[i+1,j+1,k]+xn[i+1,j,k+1]+xn[i,j+1,k+1]+xn[i+1,j+1,k+1])
    #             yc[i,j,k] = 0.125*(yn[i,j,k]+yn[i+1,j,k]+yn[i,j+1,k]+yn[i,j,k+1]+yn[i+1,j+1,k]+yn[i+1,j,k+1]+yn[i,j+1,k+1]+yn[i+1,j+1,k+1])
    #             zc[i,j,k] = 0.125*(zn[i,j,k]+zn[i+1,j,k]+zn[i,j+1,k]+zn[i,j,k+1]+zn[i+1,j+1,k]+zn[i+1,j,k+1]+zn[i,j+1,k+1]+zn[i+1,j+1,k+1])

    return xn, yn, zn


def read_field(file_path,N,Nx,Ny,Nz,jumpline):
    MESH = open(file_path,"r")
    MESH = MESH.readlines()

    var = [np.zeros((max(Nx,1), max(Ny,1), max(Nz,1))) for _ in range(N)]
    line = jumpline-2

    for v in range(N):
        for k in range(max(Nz,1)):
            for j in range(max(Ny,1)):
                for i in range(max(Nx,1)):
                    line += 1
                    var[v][i,j,k] = float(MESH[line])

    return var


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


def read_TEC(file_path):

    variables = read_variables(file_path)
    dimensions = read_dimensions(file_path)
    data_starts = zone_data_starts(file_path)

    # Displaying the result
    # print("Number of Variables:", variables['number'])
    # print("Variables:", variables['name'])
    # print("Block Dimensions:", dimensions)
    # print()

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
        [xn,yn,zn] = read_geometry(file_path,Nx,Ny,Nz,geom_jump)
        xb.append(xn); yb.append(yn); zb.append(zn)

        field_jump = geom_jump + 3*(Nx+1)*(Ny+1)*(Nz+1)
        var = read_field(file_path,Nvar,Nx,Ny,Nz,field_jump)
        vb.append(var)

    return xb, yb, zb, vb, variables['name']


def write_TEC(file_path, xb, yb, zb, vb, var_names=None):

    with open(file_path, 'w') as file:
        Nv = len(vb[0])
        if var_names is None:
            var_names = ['"x"', '"y"', '"z"'] + [f'"v{v+1}"' for v in range(Nv)]
        else:
            var_names = [f'"{name}"' for name in var_names]
        file.write("VARIABLES = " + " ".join(var_names) + "\n")

        Nb = len(vb)
        for b in range(Nb):
            Nx, Ny, Nz = xb[b].shape
            if Nv == 1:
                file.write(f'ZONE T="Block{b+1}", I={Nx}, J={Ny}, K={Nz}, DATAPACKING=BLOCK, VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED)\n')
            else:
                file.write(f'ZONE T="Block{b+1}", I={Nx}, J={Ny}, K={Nz}, DATAPACKING=BLOCK, VARLOCATION=([1-3]=NODAL,[4-{3+Nv}]=CELLCENTERED)\n')
            for k in range(Nz):
                for j in range(Ny):
                    for i in range(Nx):
                        val = xb[b][i, j, k]
                        file.write(f"{val}\n")
            for k in range(Nz):
                for j in range(Ny):
                    for i in range(Nx):
                        val = yb[b][i, j, k]
                        file.write(f"{val}\n")
            for k in range(Nz):
                for j in range(Ny):
                    for i in range(Nx):
                        val = zb[b][i, j, k]
                        file.write(f"{val}\n")
            for v in range(Nv):
                for k in range(Nz-1):
                    for j in range(Ny-1):
                        for i in range(Nx-1):
                            val = vb[b][v][i, j, k]
                            file.write(f"{val}\n")
