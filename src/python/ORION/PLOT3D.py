"""PLOT3D grid files: read_P3D reads every common layout, detected from the file.

The rules are those of p3d_read_multiblock in the Fortran library
(src/fortran/lib/Lib_PLOT3D.f90), so that both accept and refuse the same files:

- encoding: ASCII (free format: any number of values per line, Fortran repeat
  counts r*c, D exponents, commas, CRLF line ends), Fortran unformatted (4- or
  8-byte record markers; records over 2 GiB split in subrecords) or C binary
  (stream, no markers);
- binary: either byte order, 4- or 8-byte integers, 4- or 8-byte reals;
- multi-grid (the file begins with the number of blocks) or single grid;
- 3D (x, y, z) or 2D (x, y);
- with or without iblank (read past, as in the Fortran reader);
- whole or planes (one Fortran record per k plane).

Every layout is checked against the count of the values (ASCII) or of the bytes
(binary) of the file, and a Fortran file against its records: the file is read
only when exactly one layout matches, never guessed.
"""
import bisect
import os

import numpy as np

__all__ = ['read_P3D']

_CHUNK = 1 << 26              # bytes of an ASCII file converted at a time
_MAXDIM = 2**31 - 1           # largest dimension: the Fortran reader keeps them in default integers
_MAXINT = 2**63 - 1           # largest integer of an ASCII header (8-byte integers in the Fortran reader)
_NUMBER = b'0123456789+-.eEdD*'
_SEPARATORS = b' \t\r\n,'
_TO_PYTHON = str.maketrans('dD,', 'eE ')   # D exponents and commas, as float() reads them


class _Refused(Exception):
    """The file is not a PLOT3D grid of a single layout; the message tells why."""


class _Layout(object):
    """Layout of a PLOT3D grid file."""

    def __init__(self, multi, ndir, iblank, nhead, endian='<', isize=4, rsize=8):
        self.multi = multi      # the file begins with the number of blocks (multi-grid)
        self.ndir = ndir        # coordinates per node: 3 (x, y, z) or 2 (x, y)
        self.iblank = iblank    # an iblank array follows the coordinates of each block
        self.nhead = nhead      # header length: values (ASCII) or bytes (binary)
        self.endian = endian    # binary: '<' little or '>' big endian
        self.isize = isize      # binary: bytes of an integer (4 or 8)
        self.rsize = rsize      # binary: bytes of a real (4 or 8)
        self.planes = False     # binary: one Fortran record per k plane (PLOT3D "planes")

    def name(self, binary):
        s = ('multi-grid ' if self.multi else 'single grid ') + '%dD' % self.ndir
        if self.iblank:
            s += ' with iblank'
        if binary:
            s += ', %d-byte reals, %d-byte integers, %s endian' % (
                self.rsize, self.isize, 'big' if self.endian == '>' else 'little')
        return s


class _Payload(object):
    """Data bytes of a binary file, without the record markers of a Fortran file,
    as segments of the file. A C binary (stream) file is one segment."""

    def __init__(self, records, endian, pos, lens, recend, broken=0):
        self.records = records      # Fortran unformatted (records) or C binary (stream)
        self.endian = endian        # byte order of the record markers
        self.pos = pos              # file offset of the first byte of each segment
        self.lens = lens            # bytes of each segment
        self.starts = [0]           # payload offset of each segment
        for n in lens[:-1]:
            self.starts.append(self.starts[-1] + n)
        self.size = sum(lens)       # payload bytes
        self.recend = recend        # payload offset of the end of each record
        self.recends = set(recend)
        self.broken = broken        # file offset where a chain of Fortran records breaks (0: none)


def read_P3D(file_path):
    """Read a PLOT3D grid file, whatever its layout (see the module docstring).

    Returns xb, yb, zb: lists with one array of shape (I, J, K) per block,
    indexed [i, j, k] as in read_TEC. A 2D file gives arrays of shape (I, J, 1)
    and zb None. Raises ValueError, with the reason, when no PLOT3D layout
    matches the file or more than one does; OSError when it cannot be read.
    """
    size = os.path.getsize(file_path)
    try:
        if size == 0:
            raise _Refused('the file is empty')
        with open(file_path, 'rb') as f:
            if _is_text(f.read(4096)):
                blocks = _read_ascii(f, size)
            else:
                blocks = _read_binary(f, size)
    except _Refused as e:
        raise ValueError('%s: %s' % (file_path, e))
    xb = [b[0] for b in blocks]
    yb = [b[1] for b in blocks]
    zb = [b[2] for b in blocks] if len(blocks[0]) == 3 else None
    return xb, yb, zb


def _is_text(head):
    """The first bytes of the file are printable ASCII, tabs and line ends (an ASCII
    file); a binary file holds other bytes in its first integers and record markers."""
    return all(c in (9, 10, 13) or 32 <= c <= 126 for c in bytearray(head))


# ---------------------------------------------------------------------------- ASCII

def _read_ascii(f, size):
    """ASCII file: all its values, then the one layout they fit, then the blocks
    (views of the values, i fastest)."""
    values, lead = _ascii_values(f, size)
    layout, dims = _ascii_layout(values.size, lead)
    blocks = []
    off = layout.nhead
    for d in dims:
        shape = (d[0], d[1], d[2] if layout.ndir == 3 else 1)
        n = shape[0] * shape[1] * shape[2]
        comps = []
        for _ in range(layout.ndir):
            comps.append(values[off:off + n].reshape(shape, order='F'))
            off += n
        if layout.iblank:
            off += n
        blocks.append(comps)
    return blocks


def _ascii_values(f, size):
    """All the values of an ASCII file as float64 (r*c gives r values), and the
    integers the file begins with, up to the longest header possible (1 + 3*N
    values when the first is N). Only the characters of numbers and separators
    are accepted, as in the Fortran reader (whose list-directed READ a '/' would
    end early, and a null value would leave a coordinate unset)."""
    allowed = _NUMBER + _SEPARATORS
    parts = []
    lead = []
    head = {'open': True, 'cap': 3}
    carry = b''
    base = 0                    # file offset of the first byte of data
    f.seek(0)
    while True:
        chunk = f.read(_CHUNK)
        data = carry + chunk
        if chunk:
            # values never span two chunks: cut after the last separator
            cut = max(data.rfind(_SEPARATORS[i:i + 1]) for i in range(len(_SEPARATORS)))
            if cut < 0:
                carry = data
                continue
            carry = data[cut + 1:]
            data = data[:cut + 1]
        bad = data.translate(None, allowed)
        if bad:
            at = base + data.index(bad[:1]) + 1
            raise _Refused('the character "%s" at byte %d cannot be part of a PLOT3D ASCII file'
                           % (bad[:1].decode('latin-1'), at))
        # str tokens convert twice as fast as bytes tokens
        tokens = data.decode('ascii').translate(_TO_PYTHON).split()
        counts = _repeats(tokens) if b'*' in data else None
        if head['open']:
            _leading_integers(tokens, counts, lead, head, size)
        if tokens:
            try:
                v = np.array(tokens, dtype=np.float64)
            except ValueError:
                for t in tokens:
                    try:
                        float(t)
                    except ValueError:
                        raise _Refused('"%s" is not a number' % t)
                raise
            if counts is not None:
                v = np.repeat(v, counts)
            parts.append(v)
        base += len(data)
        if not chunk:
            break
    values = np.concatenate(parts) if parts else np.empty(0)
    return values, lead


def _repeats(tokens):
    """Repeat counts of the tokens; a token r*c is replaced by c in place."""
    counts = np.ones(len(tokens), dtype=np.int64)
    for i, t in enumerate(tokens):
        if '*' in t:
            r, _, v = t.partition('*')
            n = _parse_int(r)
            if n is None or n < 1 or not v:
                raise _Refused('the repeat "%s" is not of the form r*c (null values are not accepted)' % t)
            counts[i] = n
            tokens[i] = v
    return counts


def _leading_integers(tokens, counts, lead, head, size):
    """Append to lead the integers the tokens begin with, up to head['cap']."""
    for i, t in enumerate(tokens):
        v = _parse_int(t)
        if v is None:
            head['open'] = False
            return
        for _ in range(1 if counts is None else int(counts[i])):
            if len(lead) >= head['cap']:
                head['open'] = False
                return
            lead.append(v)
            if len(lead) == 1 and v >= 1:
                head['cap'] = max(3, 1 + 3 * min(v, size))


def _parse_int(t):
    """The integer t holds (optional sign, digits only), None otherwise."""
    s = t[1:] if t[:1] in ('+', '-') else t
    if not s.isdigit():
        return None
    v = int(s)
    if v > _MAXINT:
        return None
    return -v if t[:1] == '-' else v


def _ascii_layout(ntok, lead):
    """The layouts of an ASCII file, tried against its count of values: multi-grid
    or single grid, 3D or 2D, with or without iblank. Exactly one must match."""
    matches = []
    for multi in (True, False):
        for nd in (3, 2):
            for ib in (0, 1):
                if multi:
                    if not lead:
                        continue
                    nb, off = lead[0], 1
                else:
                    nb, off = 1, 0
                if nb < 1 or nb > ntok or off + nd * nb > len(lead):
                    continue
                total = off + nd * nb
                dims = []
                for b in range(nb):
                    d = lead[off + b * nd:off + (b + 1) * nd]
                    n = _npoints(d, ntok)
                    if n is None or n * (nd + ib) > ntok - total:
                        break
                    total += n * (nd + ib)
                    dims.append(tuple(d))
                else:
                    if total == ntok:
                        matches.append((_Layout(multi, nd, ib == 1, off + nd * nb), dims))
    if not matches:
        raise _Refused('no PLOT3D layout (multi-grid or single grid, 3D or 2D, with or without iblank) '
                       'matches the %d values of the file' % ntok)
    if len(matches) > 1:
        raise _Refused('the %d values of the file match more than one PLOT3D layout: %s'
                       % (ntok, '; '.join(m[0].name(False) for m in matches)))
    return matches[0]


def _npoints(d, lim):
    """Nodes of a block of dimensions d; None when a dimension is below 1 or beyond
    _MAXDIM, or when the nodes exceed lim."""
    if any(x < 1 or x > _MAXDIM for x in d):
        return None
    n = 1
    for x in d:
        n *= x
        if n > lim:
            return None
    return n


# --------------------------------------------------------------------------- binary

def _read_binary(f, size):
    """Binary file: its payload (Fortran records or C stream), its layout, its
    coordinates."""
    pl = _find_payload(f, size)
    layout, dims = _binary_layout(f, pl)
    rt = np.dtype(layout.endian + ('f8' if layout.rsize == 8 else 'f4'))
    perpt = layout.ndir * layout.rsize + (layout.isize if layout.iblank else 0)
    off = layout.nhead
    blocks = []
    for d in dims:
        shape = (d[0], d[1], d[2] if layout.ndir == 3 else 1)
        npl = shape[0] * shape[1]
        if layout.planes:
            comps = [np.empty(shape) for _ in range(layout.ndir)]
            for k in range(shape[2]):
                for c in range(layout.ndir):
                    plane = _payload_array(f, pl, off + c * npl * layout.rsize, npl, rt)
                    comps[c][:, :, k] = plane.reshape(shape[:2], order='F')
                off += npl * perpt
        else:
            n = npl * shape[2]
            comps = [_payload_array(f, pl, off + c * n * layout.rsize, n, rt).reshape(shape, order='F')
                     for c in range(layout.ndir)]
            off += n * perpt
        blocks.append(comps)
    return blocks


def _find_payload(f, size):
    """A Fortran unformatted file is a chain of records, each between two markers
    holding its byte length (a negative leading marker continues the record in a
    further subrecord: gfortran and ifx split records over 2 GiB so). The chain
    must end exactly at the end of the file; markers of 4 and 8 bytes, in either
    byte order, are tried. Otherwise the file is C binary (stream): one segment."""
    broken = 0
    for mk in (4, 8):
        for endian in ('<', '>'):
            pl, at = _fortran_records(f, size, mk, endian)
            if pl is not None:
                return pl
            broken = max(broken, at)
    return _Payload(False, '<', [0], [size], [size], broken)


def _fortran_records(f, size, mk, endian):
    """(payload, 0) when the file is a chain of Fortran records with markers of mk
    bytes in the given byte order; (None, offset where a chain of two records or
    more breaks, else 0) otherwise."""
    mt = np.dtype(endian + ('i4' if mk == 4 else 'i8'))
    pos, lens, recend = [], [], []
    total = 0
    p = 0
    lead = 0
    ok = False
    while p < size:
        ok = False
        if size - p < 2 * mk:
            break
        f.seek(p)
        lead = int(np.frombuffer(f.read(mk), mt)[0])
        n = abs(lead)
        if n > size - p - 2 * mk:
            break
        f.seek(p + mk + n)
        if abs(int(np.frombuffer(f.read(mk), mt)[0])) != n:
            break
        ok = True
        pos.append(p + mk)
        lens.append(n)
        total += n
        if lead >= 0:
            recend.append(total)
        p += 2 * mk + n
    if ok and recend and lead >= 0:
        return _Payload(True, endian, pos, lens, recend), 0
    return None, (p + 1 if len(pos) >= 2 else 0)


def _binary_layout(f, pl):
    """The layouts of a binary file, tried against its payload bytes: byte order
    (that of the record markers for a Fortran file), 4- or 8-byte integers,
    multi-grid or single grid, 3D or 2D, 8- or 4-byte reals, with or without
    iblank. Exactly one must match. A 3D Fortran file of one record per k plane
    is then read as planes."""
    matches = []
    for endian in ((pl.endian,) if pl.records else ('<', '>')):
        for isize in (4, 8):
            for multi in (True, False):
                for nd in (3, 2):
                    head = _binary_header(f, pl, endian, isize, multi, nd)
                    if head is None:
                        continue
                    dims, hb, ptot = head
                    for rsize in (8, 4):
                        for ib in (0, 1):
                            if hb + ptot * (nd * rsize + ib * isize) == pl.size:
                                matches.append((_Layout(multi, nd, ib == 1, hb, endian, isize, rsize), dims))
    if not matches:
        if pl.records:
            raise _Refused('Fortran unformatted file, but no PLOT3D layout matches its records')
        if pl.broken:
            raise _Refused('no PLOT3D layout matches the file: its Fortran records break at byte %d '
                           '(a truncated file?)' % pl.broken)
        raise _Refused('no PLOT3D layout matches the %d bytes of the file (read as C binary: '
                       'no Fortran records found)' % pl.size)
    if len(matches) > 1:
        raise _Refused('the file matches more than one PLOT3D layout: %s'
                       % '; '.join(m[0].name(True) for m in matches))
    layout, dims = matches[0]
    if pl.records and layout.ndir == 3:
        layout.planes = _is_planes(pl, layout, dims)
    return layout, dims


def _binary_header(f, pl, endian, isize, multi, nd):
    """Header of a binary file read with integers of isize bytes in the given byte
    order: (dims, header bytes, nodes), or None as soon as it cannot be one: a
    dimension below 1, more nodes than the payload can hold, a header of a Fortran
    file that does not end at the end of a record."""
    it = np.dtype(endian + ('i4' if isize == 4 else 'i8'))
    if multi:
        if pl.size < isize:
            return None
        nb = int(np.frombuffer(_payload_bytes(f, pl, 0, isize), it)[0])
        off = isize
    else:
        nb, off = 1, 0
    # every block takes nd integers and at least one node of nd 4-byte reals
    if nb < 1 or nb > (pl.size - off) // (nd * isize + nd * 4):
        return None
    hb = off + nd * isize * nb
    if pl.records and hb not in pl.recends:
        return None
    lim = (pl.size - hb) // (nd * 4)
    dims = []
    ptot = 0
    for b0 in range(0, nb, 4096):
        nc = min(4096, nb - b0)
        v = np.frombuffer(_payload_bytes(f, pl, off + b0 * nd * isize, nd * nc * isize), it)
        for row in v.reshape(nc, nd).tolist():
            n = _npoints(row, lim - ptot)
            if n is None:
                return None
            ptot += n
            dims.append(tuple(row))
    return dims, hb, ptot


def _is_planes(pl, layout, dims):
    """A 3D Fortran file holds the coordinates of a block in one record (whole) or
    in one record per k plane, x, y, z (and iblank) of the plane (planes). Planes
    when the records after the header are those of the planes and not those of
    the blocks; any other split of the records is read as whole."""
    perpt = layout.ndir * layout.rsize + (layout.isize if layout.iblank else 0)
    h0 = pl.recend.index(layout.nhead) + 1
    lens = [e - s for s, e in zip([0] + pl.recend[:-1], pl.recend)][h0:]
    whole = [d[0] * d[1] * d[2] * perpt for d in dims]
    planes = [d[0] * d[1] * perpt for d in dims for _ in range(d[2])]
    return lens == planes and lens != whole


def _payload_bytes(f, pl, lo, n):
    """n payload bytes from payload offset lo, across the segments."""
    if lo < 0 or lo + n > pl.size:
        raise _Refused('read beyond the end of the file')
    s = bisect.bisect_right(pl.starts, lo) - 1
    pieces = []
    got = 0
    while got < n:
        inseg = lo + got - pl.starts[s]
        take = min(n - got, pl.lens[s] - inseg)
        if take > 0:
            f.seek(pl.pos[s] + inseg)
            piece = f.read(take)
            if len(piece) != take:
                raise _Refused('read error at byte %d' % (pl.pos[s] + inseg + len(piece)))
            pieces.append(piece)
            got += take
        s += 1
    return b''.join(pieces)


def _payload_array(f, pl, lo, n, dtype):
    """n values of dtype from payload offset lo, as a writable float64 array."""
    nbytes = n * dtype.itemsize
    s = bisect.bisect_right(pl.starts, lo) - 1
    if lo + nbytes <= pl.starts[s] + pl.lens[s]:
        # within one segment (always, but for records over 2 GiB): no intermediate copy
        f.seek(pl.pos[s] + lo - pl.starts[s])
        a = np.fromfile(f, dtype=dtype, count=n)
        if a.size != n:
            raise _Refused('read error: the file ends early')
        return a.astype(np.float64, copy=False)
    return np.frombuffer(_payload_bytes(f, pl, lo, nbytes), dtype).astype(np.float64)
