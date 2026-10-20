"""read_TEC reads back the Tecplot ASCII files that ORION writes.

ctest runs this script (ORION.python_read_tec) in the test runtime directory,
after ORION.tecplot_write has written tecfile.tec there. It reads ten forms
of header, the names being read as the Fortran reader reads them:

1. tecfile.tec, written by the Fortran writer: names and zone titles without
   quotes (VARIABLES = x y z variable1, ZONE T = blocco-A);
2. a file written by write_TEC: names and zone titles in quotes
   (VARIABLES = "x" ..., ZONE T="Block1");
3. a file in the form of the Fortran writer before 1.7.0: names in quotes,
   over two lines and two of them adjacent ("a""b"), zone titles without
   quotes;
4. names in quotes and zone titles in quotes after a blank
   (ZONE T = "Block1");
5. an empty list, "VARIABLES =" alone on its line: the zone header that
   follows is not a list of names;
6. names in double quotes, in single quotes and bare in the same list, with
   commas and tabs as separators and a quoted name that holds a blank;
7. a quote that is never closed: read_TEC raises ValueError;
8. a continuation line of the list that begins with a name starting with "zone"
   (zone_velocity): it is a name, not a zone record;
9. zone titles that hold numbers: in quotes after a blank (T = "Block 1"),
   without quotes after a hyphen (T = B1-of-2, as written by a block
   splitter), in quotes with a keyword (T = "K=1 plane 1"), and dimensions
   written with blanks (I = 4, J = 3, K = 3): the dimensions of a zone are
   its I, J and K, not the first numbers of the line nor text in quotes;
10. a dimension that is not a number on a ZONE record, I=*** in the first
    zone and J=*** in the second, as a Fortran writer leaves them when the
    number does not fit its field: read_TEC raises a ValueError that names
    the file, the zone and the keyword;
11. files of a form that read_TEC does not read are refused with the reason, not read as other values: variables at the
    nodes (with VARLOCATION and without it), POINT packing, a coordinate at the cells (a file of two coordinates), a zone
    without J and K, several values on a line, a zone with a value missing; and the data set auxiliary data records
    after the last zone are not values;
12. write_TEC writes every value it is given, whatever the shape of the zone (a block, a plane, a line and a point of
    nodes, a zone without variables), and refuses with a ValueError the shapes and the names that do not fit, writing
    nothing.

Exit status 0 when every check passes, 1 otherwise.
"""
import os
import sys

# The package of this source tree, ahead of any installed ORION
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))

import numpy as np                     # noqa: E402
from ORION import read_TEC, write_TEC  # noqa: E402

failures = []
checks = 0


def check(condition, message):
    global checks
    checks += 1
    if not condition:
        failures.append(message)


def read(path):
    """read_TEC(path), or None after recording the exception it raised."""
    try:
        return read_TEC(path)
    except Exception as error:
        check(False, '{}: read_TEC raised {}: {}'.format(path, type(error).__name__, error))
        return None


def compare(path, data, names, xb, yb, zb, vb):
    """Check names, coordinates and cell variables read from path, bit for bit."""
    if data is None:
        return
    x, y, z, var, read_names = data
    check(read_names == names, '{}: names {} instead of {}'.format(path, read_names, names))
    check(len(x) == len(xb), '{}: {} blocks instead of {}'.format(path, len(x), len(xb)))
    for b in range(min(len(x), len(xb))):
        check(np.array_equal(x[b], xb[b]) and np.array_equal(y[b], yb[b]) and np.array_equal(z[b], zb[b]),
              '{}: block {}: coordinates differ'.format(path, b + 1))
        check(len(var[b]) == len(vb[b]),
              '{}: block {}: {} variables instead of {}'.format(path, b + 1, len(var[b]), len(vb[b])))
        for v in range(min(len(var[b]), len(vb[b]))):
            check(np.array_equal(var[b][v], vb[b][v]),
                  '{}: block {}: variable {} differs'.format(path, b + 1, names[3 + v]))


def nodes(ni, nj, nk):
    return np.meshgrid(np.arange(ni + 1), np.arange(nj + 1), np.arange(nk + 1), indexing='ij')


def cells(ni, nj, nk):
    return np.meshgrid(np.arange(1, ni + 1), np.arange(1, nj + 1), np.arange(1, nk + 1), indexing='ij')


# 1. Fortran writer (src/fortran/test/tecplot_write.f90): two blocks of 10x10x10 and 20x10x10 cells,
#    nodes at x = i (i + 10 on the second block), y = j, z = k; one cell variable, i*j*k.
xb, yb, zb, vb = [], [], [], []
for ni, x0 in ((10, 0), (20, 10)):
    i, j, k = nodes(ni, 10, 10)
    xb.append((x0 + i).astype(float))
    yb.append(j.astype(float))
    zb.append(k.astype(float))
    i, j, k = cells(ni, 10, 10)
    vb.append([(i * j * k).astype(float)])
compare('tecfile.tec', read('tecfile.tec'), ['x', 'y', 'z', 'variable1'], xb, yb, zb, vb)

# Two small blocks for the files written here; values that need all their digits.
xb, yb, zb, vb = [], [], [], []
for b, (ni, nj, nk) in enumerate(((3, 2, 2), (2, 3, 1))):
    i, j, k = nodes(ni, nj, nk)
    xb.append(i / 3.0 + b)
    yb.append(j * 0.1)
    zb.append(k * 1.0e-3)
    i, j, k = cells(ni, nj, nk)
    vb.append([(i + 10 * j + 100 * k) / 7.0, -(i * j * k) / 3.0])
names = ['x', 'y', 'z', 'a', 'b']

# 2. write_TEC: names and zone titles in quotes.
path = 'python_read_tec_quoted.tec'
write_TEC(path, xb, yb, zb, vb, names)
compare(path, read(path), names, xb, yb, zb, vb)

def write_ascii(path, header, title, with_variables=True, dims='I={}, J={}, K={}'):
    """Write xb, yb, zb (and vb) after the given header, one value per line, zone titles from title."""
    with open(path, 'w') as f:
        f.write(header)
        for b in range(len(xb)):
            location = ', VARLOCATION=([1-3]=NODAL,[4-5]=CELLCENTERED)' if with_variables else ''
            f.write(' ZONE  T = {}, {}, DATAPACKING=BLOCK{}, SOLUTIONTIME=0.5\n'.format(
                title.format(b + 1), dims.format(*xb[b].shape), location))
            for a in [xb[b], yb[b], zb[b]] + (vb[b] if with_variables else []):
                f.write(''.join('{!r}\n'.format(float(value)) for value in a.flatten(order='F')))


# 3. Names in quotes over two lines, "a""b" adjacent; zone titles without quotes.
path = 'python_read_tec_two_lines.tec'
write_ascii(path, ' VARIABLES ="x" "y" "z"\n"a""b"\n', 'Block{}')
compare(path, read(path), names, xb, yb, zb, vb)

# 4. Names in quotes; zone titles in quotes after a blank (T = "Block1"), which are not variable names.
path = 'python_read_tec_zone_titles.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', '"Block{}"')
compare(path, read(path), names, xb, yb, zb, vb)

# 5. An empty list: no names, the coordinates are read and there is no variable.
path = 'python_read_tec_empty_list.tec'
write_ascii(path, ' VARIABLES =\n', 'Block{}', with_variables=False)
compare(path, read(path), [], xb, yb, zb, [[] for _ in xb])

# 6. Quoted (both kinds) and bare names in one list, separated by blanks, commas and a tab; a quoted
#    name may hold a blank. The names are those of the Fortran reader, and the keyword is read in any case.
path = 'python_read_tec_mixed.tec'
write_ascii(path, ' variables = "x", y\t\'z\' "a b" b\n', 'Block{}')
compare(path, read(path), ['x', 'y', 'z', 'a b', 'b'], xb, yb, zb, vb)

# 7. A quote that is never closed is an error, as it is for the Fortran reader.
path = 'python_read_tec_unclosed.tec'
write_ascii(path, ' VARIABLES = x y z "a b\n', 'Block{}')
try:
    read_TEC(path)
    check(False, '{}: read_TEC read a list with an unclosed quote'.format(path))
except ValueError:
    check(True, '')
except Exception as error:
    check(False, '{}: read_TEC raised {} instead of ValueError'.format(path, type(error).__name__))

# 8. A name that begins with "zone" on a continuation line is a name; the ZONE record ends the list.
path = 'python_read_tec_zone_name.tec'
write_ascii(path, ' VARIABLES = x y z\nzone_velocity Zone2\n', 'Block{}')
compare(path, read(path), ['x', 'y', 'z', 'zone_velocity', 'Zone2'], xb, yb, zb, vb)

# 9. Zone titles that hold numbers, and dimensions written with blanks.
path = 'python_read_tec_title_numbers.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', '"Block {}"')
compare(path, read(path), names, xb, yb, zb, vb)
path = 'python_read_tec_title_hyphen.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', 'B{}-of-2')
compare(path, read(path), names, xb, yb, zb, vb)
path = 'python_read_tec_title_keyword.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', '"K=1 plane {}"')
compare(path, read(path), names, xb, yb, zb, vb)
path = 'python_read_tec_dims_blanks.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', 'Block{}', dims='I = {}, J = {}, K = {}')
compare(path, read(path), names, xb, yb, zb, vb)


def expect_error(path, *parts):
    """read_TEC(path) must raise a ValueError whose message holds every one of parts."""
    try:
        read_TEC(path)
    except ValueError as error:
        for part in parts:
            check(part in str(error), '{}: the error "{}" does not name {}'.format(path, error, part))
        return
    except Exception as error:
        check(False, '{}: read_TEC raised {} instead of ValueError: {}'.format(path, type(error).__name__, error))
        return
    check(False, '{}: read_TEC raised no error for a file that it does not read'.format(path))


# 10. A dimension that is not a number: I=*** in zone 1, J=*** in zone 2 (whose J is 4).
path = 'python_read_tec_overflow_i.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', '"block: {}"', dims='I=***,J={1},K={2}')
expect_error(path, path, 'zone 1 (T = "block: 1")', "I = '***'")
path = 'python_read_tec_overflow_j.tec'
write_ascii(path, ' VARIABLES = "x" "y" "z" "a" "b"\n', 'Block{}')
with open(path) as f:
    text = f.read()
check(text.count('J=4') == 1, '{}: the second zone header was not written as expected'.format(path))
with open(path, 'w') as f:
    f.write(text.replace('J=4', 'J=***'))
expect_error(path, path, 'zone 2 (T = Block2)', "J = '***'")



# 11. Files of another form are refused, with the reason that names what is wrong.
def write_form(path, variables, zone, count, per_line=1, tail=''):
    """A file of the VARIABLES list and the ZONE record given, with count values 0.5 (per_line on each line)."""
    with open(path, 'w') as f:
        f.write(' VARIABLES = {}\n ZONE T = z, {}\n'.format(variables, zone))
        values = ['0.5'] * count
        for i in range(0, count, per_line):
            f.write(' '.join(values[i:i + per_line]) + '\n')
        f.write(tail)


cc = ', VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED)'
good = 'I=3, J=2, K=2, DATAPACKING=BLOCK' + cc          # 12 nodes, 2 cells: 3 * 12 + 2 values

path = 'python_read_tec_form_ok.tec'
write_form(path, 'x y z u', good, 3 * 12 + 2)
data = read(path)
check(data is not None and data[3][0][0].shape == (2, 1, 1), '{}: the form of the files that ORION writes'.format(path))
path = 'python_read_tec_form_aux.tec'
write_form(path, 'x y z u', good, 3 * 12 + 2, tail=' DATASETAUXDATA CYCLE = "5"\n DATASETAUXDATA a = "1.5"\n')
data = read(path)
check(data is not None and data[3][0][0].shape == (2, 1, 1), '{}: auxiliary data records are not values'.format(path))

path = 'python_read_tec_form_nodes.tec'
write_form(path, 'x y z u', 'I=3, J=2, K=2, DATAPACKING=BLOCK, VARLOCATION=([1-4]=NODAL)', 4 * 12)
expect_error(path, path, 'zone 1', 'variables at the nodes')
path = 'python_read_tec_form_no_location.tec'
write_form(path, 'x y z u', 'I=3, J=2, K=2, DATAPACKING=BLOCK', 4 * 12)
expect_error(path, path, 'zone 1', 'variables at the nodes')
path = 'python_read_tec_form_point.tec'
write_form(path, 'x y z u', 'I=3, J=2, K=2, F=POINT', 4 * 12)
expect_error(path, path, 'zone 1', 'POINT packing')
path = 'python_read_tec_form_point_title.tec'            # POINT in a title is not the packing
write_form(path, 'x y z u', 'I=3, J=2, K=2, DATAPACKING=BLOCK' + cc, 3 * 12 + 2)
with open(path) as f:
    text = f.read()
with open(path, 'w') as f:
    f.write(text.replace('T = z', 'T = "POINT F=POINT"'))
data = read(path)
check(data is not None, '{}: a title that holds POINT'.format(path))
path = 'python_read_tec_form_xy.tec'
write_form(path, 'x y u v', 'I=3, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3-4]=CELLCENTERED)', 2 * 6 + 2 * 2)
expect_error(path, path, 'zone 1', 'coordinate at the cells')
path = 'python_read_tec_form_no_jk.tec'
write_form(path, 'x y z u', 'I=4, DATAPACKING=BLOCK' + cc, 3 * 4 + 3)
expect_error(path, path, 'zone 1', 'I, J and K')
path = 'python_read_tec_form_several.tec'
write_form(path, 'x y z u', good, 3 * 12 + 2, per_line=4)
expect_error(path, path, 'zone 1', 'one value per line')
path = 'python_read_tec_form_short.tec'
write_form(path, 'x y z u', good, 3 * 12 + 1)
expect_error(path, path, 'zone 1', '37 lines of values', 'not the 38')


# 12. write_TEC and read_TEC give each other back, for every shape of zone.
rng = np.random.default_rng(7)
for shape in ((4, 3, 3), (4, 3, 1), (4, 1, 1), (1, 1, 1)):
    path = 'python_write_tec_{}x{}x{}.tec'.format(*shape)
    i, j, k = np.meshgrid(*[np.arange(n, dtype=float) for n in shape], indexing='ij')
    cell_shape = tuple(max(n - 1, 1) for n in shape)
    u, w = rng.standard_normal(cell_shape), rng.standard_normal(cell_shape)
    write_TEC(path, [i / 3.0], [j * 0.1], [k * 1.0e-3], [[u, w]], ['x', 'y', 'z', 'u', 'w'])
    compare(path, read(path), ['x', 'y', 'z', 'u', 'w'], [i / 3.0], [j * 0.1], [k * 1.0e-3], [[u, w]])
path = 'python_write_tec_no_variables.tec'
write_TEC(path, [i], [j], [k], [[]])
compare(path, read(path), ['x', 'y', 'z'], [i], [j], [k], [[]])
path = 'python_write_tec_default_names.tec'
write_TEC(path, [i], [j], [k], [[rng.standard_normal(cell_shape)]])
check(read(path)[4] == ['x', 'y', 'z', 'v1'], '{}: the default names'.format(path))

nodes_a, nodes_b = np.zeros((3, 2, 2)), np.zeros((2, 2, 2))     # 2 x 1 x 1 cells, and 1 x 1 x 1
cells_a = np.zeros((2, 1, 1))
for what, args in (('a variable of the wrong shape', ([nodes_a], [nodes_a], [nodes_a], [[nodes_b]])),
                   ('coordinates of different shapes', ([nodes_a], [nodes_b], [nodes_a], [[]])),
                   ('lists of different lengths', ([nodes_a, nodes_a], [nodes_a], [nodes_a], [[]])),
                   ('blocks with different numbers of variables',
                    ([nodes_a, nodes_a], [nodes_a, nodes_a], [nodes_a, nodes_a], [[cells_a], []])),
                   ('names that are not those of the variables',
                    ([nodes_a], [nodes_a], [nodes_a], [[cells_a]], ['x', 'y', 'z']))):
    path = 'python_write_tec_refused.tec'
    if os.path.exists(path):
        os.remove(path)
    try:
        write_TEC(path, *args)
        check(False, 'write_TEC wrote {}'.format(what))
    except ValueError:
        check(not os.path.exists(path), 'write_TEC refused {} but left a file'.format(what))
    except Exception as error:
        check(False, 'write_TEC raised {} instead of ValueError for {}'.format(type(error).__name__, what))

if failures:
    print('read_TEC: {} of {} checks failed:'.format(len(failures), checks))
    for message in failures:
        print('  ' + message)
    sys.exit(1)
print('read_TEC: all {} checks passed'.format(checks))
