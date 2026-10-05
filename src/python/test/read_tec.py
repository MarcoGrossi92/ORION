"""read_TEC reads back the Tecplot ASCII files that ORION writes.

ctest runs this script (ORION.python_read_tec) in the test runtime directory,
after ORION.tecplot_write has written tecfile.tec there. It reads five forms
of header:

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
   follows is not a list of names.

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

def write_ascii(path, header, title, with_variables=True):
    """Write xb, yb, zb (and vb) after the given header, one value per line, zone titles from title."""
    with open(path, 'w') as f:
        f.write(header)
        for b in range(len(xb)):
            location = ', VARLOCATION=([1-3]=NODAL,[4-5]=CELLCENTERED)' if with_variables else ''
            f.write(' ZONE  T = {}, I={}, J={}, K={}, DATAPACKING=BLOCK{}, SOLUTIONTIME=0.5\n'.format(
                title.format(b + 1), *xb[b].shape, location))
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

if failures:
    print('read_TEC: {} of {} checks failed:'.format(len(failures), checks))
    for message in failures:
        print('  ' + message)
    sys.exit(1)
print('read_TEC: all {} checks passed'.format(checks))
