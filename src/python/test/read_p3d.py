"""read_P3D reads PLOT3D grid files of every common layout.

ctest runs this script (ORION.python_read_p3d) in the test runtime directory.
It writes each layout byte by byte, the cases of the Fortran test
p3d_read_formats with a writer of its own, reads it back with read_P3D and
compares every coordinate; files that are not PLOT3D grids of a single layout
must raise ValueError, with the reason.

Exit status 0 when every check passes, 1 otherwise.
"""
import os
import sys

# The package of this source tree, ahead of any installed ORION
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))

import numpy as np            # noqa: E402
from ORION import read_P3D    # noqa: E402

DIMS = [(5, 2, 2), (3, 3, 2), (4, 1, 3)]      # nodes of the blocks
CASE = 'case_py.p3d'


def layout(**kw):
    """A test layout: the defaults, changed by kw (names as in p3d_read_formats)."""
    lay = dict(enc='fortran',       # 'ascii', 'fortran' (unformatted records) or 'stream' (C binary)
               multi=True,          # multi-grid (3 blocks) or single grid (block 1)
               ndir=3,              # 3D or 2D
               iblank=False,
               endian='<',          # binary byte order
               isize=4,             # binary: bytes of an integer
               rsize=8,             # binary: bytes of a real
               mk=4,                # Fortran: bytes of a record marker
               planes=False,        # Fortran: one record per k plane
               hdr=0,               # Fortran header records: 0 N | dims, 1 N and dims in one, 2 N | dims of each block
               per_line=6,          # ASCII: values per line, 0 the whole file on one line
               dims_one_line=False,  # ASCII: the dimensions of all blocks on one line
               fancy=False,         # ASCII: repeat counts r*c, D exponents, commas, CRLF line ends
               drop_last=False,     # ASCII: the last value is missing
               truncate=0)          # binary: bytes missing at the end
    lay.update(kw)
    return lay


def blocks(lay):
    """Dimensions (I, J, K) of the blocks of a layout (K = 1 for 2D)."""
    dims = DIMS if lay['multi'] else DIMS[:1]
    return [(d[0], d[1], d[2] if lay['ndir'] == 3 else 1) for d in dims]


def coord(b, d, shape):
    """Coordinate d (1-based) of the nodes of block b (1-based), indexed [i, j, k];
    z is constant on a k plane, so that the ASCII writer has runs of equal values."""
    i, j, k = np.meshgrid(np.arange(shape[0]), np.arange(shape[1]), np.arange(shape[2]), indexing='ij')
    if d == 3:
        return 1000.0 * b + 300.0 + 10.0 * k
    return 1000.0 * b + 100.0 * d + 10.0 * k + 1.0 * j + 0.25 * i


def header(lay):
    """Number of blocks (multi-grid) and dimensions, as integers."""
    ints = [len(blocks(lay))] if lay['multi'] else []
    for d in blocks(lay):
        ints += list(d[:lay['ndir']])
    return ints


def write_ascii(path, lay):
    sep = ', ' if lay['fancy'] else ' '
    eol = '\r\n' if lay['fancy'] else '\n'
    nl = sep if lay['per_line'] == 0 else eol
    out = []
    if lay['multi']:
        out.append(str(len(blocks(lay))) + nl)
    for b, d in enumerate(blocks(lay)):
        out.append(sep.join(str(n) for n in d[:lay['ndir']]))
        out.append(sep if lay['dims_one_line'] and b < len(blocks(lay)) - 1 else nl)

    def put(values, integer=False):
        words = []
        q = 0
        while q < len(values):
            r = 1
            if lay['fancy']:
                while q + r < len(values) and values[q + r] == values[q]:
                    r += 1
            w = str(int(values[q])) if integer else '%.17E' % values[q]
            if lay['fancy'] and not integer:
                w = w.replace('E', 'D')
            words.append(('%d*' % r if r > 1 else '') + w)
            q += r
        if lay['per_line'] > 0:
            lines = [sep.join(words[n:n + lay['per_line']]) for n in range(0, len(words), lay['per_line'])]
            out.append(eol.join(lines) + nl)
        else:
            out.append(sep.join(words) + nl)

    nb = len(blocks(lay))
    for b, shape in enumerate(blocks(lay), 1):
        n = shape[0] * shape[1] * shape[2]
        for d in range(1, lay['ndir'] + 1):
            v = coord(b, d, shape).ravel(order='F')
            if lay['drop_last'] and b == nb and d == lay['ndir'] and not lay['iblank']:
                v = v[:-1]
            put(v)
        if lay['iblank']:
            v = np.ones(n)
            if lay['drop_last'] and b == nb:
                v = v[:-1]
            put(v, integer=True)
    with open(path, 'wb') as f:
        f.write((''.join(out) + eol).encode('ascii'))


def write_binary(path, lay):
    e = lay['endian']
    it = e + ('i4' if lay['isize'] == 4 else 'i8')
    rt = e + ('f8' if lay['rsize'] == 8 else 'f4')
    mt = e + ('i4' if lay['mk'] == 4 else 'i8')
    out = []

    def record(payload):
        if lay['enc'] == 'fortran':
            m = np.array([len(payload)], mt).tobytes()
            out.append(m + payload + m)
        else:
            out.append(payload)

    ints = header(lay)
    if lay['multi'] and lay['hdr'] != 1:
        record(np.array(ints[:1], it).tobytes())
        ints = ints[1:]
    if lay['hdr'] == 2:
        for b in range(len(blocks(lay))):
            record(np.array(ints[b * lay['ndir']:(b + 1) * lay['ndir']], it).tobytes())
    else:
        record(np.array(ints, it).tobytes())
    for b, shape in enumerate(blocks(lay), 1):
        comps = [coord(b, d, shape) for d in range(1, lay['ndir'] + 1)]
        if lay['planes']:
            for k in range(shape[2]):
                payload = b''.join(np.asarray(c[:, :, k].ravel(order='F'), rt).tobytes() for c in comps)
                if lay['iblank']:
                    payload += np.ones(shape[0] * shape[1], it).tobytes()
                record(payload)
        else:
            payload = b''.join(np.asarray(c.ravel(order='F'), rt).tobytes() for c in comps)
            if lay['iblank']:
                payload += np.ones(shape[0] * shape[1] * shape[2], it).tobytes()
            record(payload)
    data = b''.join(out)
    with open(path, 'wb') as f:
        f.write(data[:len(data) - lay['truncate']])


def write_case(path, lay):
    if lay['enc'] == 'ascii':
        write_ascii(path, lay)
    else:
        write_binary(path, lay)


def same_blocks(xb, yb, zb, lay):
    """The blocks read are those of the layout."""
    if len(xb) != len(blocks(lay)) or (zb is None) != (lay['ndir'] == 2):
        return False
    for b, shape in enumerate(blocks(lay), 1):
        comps = [xb[b - 1], yb[b - 1]] + ([zb[b - 1]] if zb is not None else [])
        for d, c in enumerate(comps, 1):
            if c.shape != shape or c.dtype != np.float64 or not np.array_equal(c, coord(b, d, shape)):
                return False
    return True


# the cases of p3d_read_formats
CASES = [
    ('ASCII', [
        ('6 values per line, a line per component', layout(enc='ascii')),
        ('one value per line', layout(enc='ascii', per_line=1)),
        ('the whole file on one line', layout(enc='ascii', per_line=0)),
        ('repeat counts, D exponents, commas, CRLF', layout(enc='ascii', fancy=True, iblank=True)),
        ('iblank', layout(enc='ascii', iblank=True)),
        ('2D, dimensions on one line', layout(enc='ascii', ndir=2, dims_one_line=True)),
        ('3D, dimensions on one line', layout(enc='ascii', dims_one_line=True)),
        ('single grid 3D', layout(enc='ascii', multi=False)),
        ('single grid 2D with iblank', layout(enc='ascii', multi=False, ndir=2, iblank=True)),
    ]),
    ('Fortran unformatted', [
        ('8-byte reals', layout()),
        ('8-byte reals, big endian', layout(endian='>')),
        ('4-byte reals, iblank', layout(rsize=4, iblank=True)),
        ('4-byte reals, iblank, big endian', layout(rsize=4, iblank=True, endian='>')),
        ('8-byte integers', layout(isize=8)),
        ('8-byte record markers', layout(mk=8)),
        ('planes', layout(planes=True)),
        ('planes, iblank, big endian', layout(planes=True, iblank=True, endian='>')),
        ('N and dimensions in one record', layout(hdr=1)),
        ('dimensions of each block in a record', layout(hdr=2)),
        ('2D', layout(ndir=2)),
        ('single grid 3D', layout(multi=False)),
        ('single grid 2D, 4-byte reals', layout(multi=False, ndir=2, rsize=4)),
    ]),
    ('C binary (stream)', [
        ('8-byte reals', layout(enc='stream')),
        ('8-byte reals, big endian', layout(enc='stream', endian='>')),
        ('4-byte reals, iblank', layout(enc='stream', rsize=4, iblank=True)),
        ('2D', layout(enc='stream', ndir=2)),
        ('single grid 3D, big endian', layout(enc='stream', multi=False, endian='>')),
    ]),
]

REFUSED = [
    ('ASCII, a value missing', layout(enc='ascii', drop_last=True)),
    ('Fortran, the last bytes missing', layout(truncate=8)),
    ('C binary, the last bytes missing', layout(enc='stream', truncate=8)),
    ('not numbers', b'mesh 1 2 3\n'),
    ('a slash', b'1 1 1 1 0 0 0 /\n'),
    ('null values', b'1 1 1 1 3*\n'),
    # multi-grid 1x1x1 at (0,0,0), or single grid 1x1x1 at (1,0,0) with iblank 0
    ('more than one layout', b'1 1 1 1 0 0 0\n'),
    ('empty', b''),
]


def main():
    failures = []
    for group, cases in CASES:
        print(group)
        for name, lay in cases:
            write_case(CASE, lay)
            try:
                xb, yb, zb = read_P3D(CASE)
                ok = same_blocks(xb, yb, zb, lay)
                why = '' if ok else 'wrong blocks'
            except Exception as e:      # any failure is reported, not raised
                ok, why = False, '%s: %s' % (type(e).__name__, e)
            print('  %s  %s%s' % ('ok  ' if ok else 'FAIL', name, '' if ok else ': ' + why))
            if not ok:
                failures.append(name)

    print('Refused')
    for name, case in REFUSED:
        if isinstance(case, bytes):
            with open(CASE, 'wb') as f:
                f.write(case)
        else:
            write_case(CASE, case)
        try:
            read_P3D(CASE)
            ok, why = False, 'read without error'
        except ValueError as e:
            ok, why = bool(str(e)), str(e)
        except Exception as e:
            ok, why = False, '%s: %s' % (type(e).__name__, e)
        print('  %s  %s (%s)' % ('ok  ' if ok else 'FAIL', name, why))
        if not ok:
            failures.append(name)
    try:
        read_P3D('case_py_missing.p3d')
        ok = False
    except OSError:
        ok = True
    print('  %s  missing file (OSError)' % ('ok  ' if ok else 'FAIL'))
    if not ok:
        failures.append('missing file')

    if failures:
        print('%d case(s) failed' % len(failures))
        return 1
    print('read_p3d: OK')
    return 0


if __name__ == '__main__':
    sys.exit(main())
