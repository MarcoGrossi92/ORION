# Fortran API

The Fortran API provides a high-performance interface for reading and writing scientific data in multiple formats. This guide covers the core concepts and usage patterns.

## Core Concepts

### The orion_data Type

All I/O operations use the `orion_data` derived type, which encapsulates all simulation data:

```fortran
use Lib_ORION_data

type(orion_data) :: IOfield
```

**Key Components:**

- `varnames(:)` - Array of variable name strings (allocatable)
- `solutiontime` - Solution time (real)
- `block(:)` - Array of computational blocks (allocatable)
- `tec` - Tecplot format options
- `vtk` - VTK format options
- `p3d` - PLOT3D format options
- `strandid` - STRANDID of the last zone read from a Tecplot ASCII file (integer, -1 when absent)

### The obj_block Type

Each block contains mesh and solution data:

```fortran
type :: obj_block
  character(len=4096) :: name       ! Block name, as long as a path (PATH_MAX of Linux)
  integer :: Ni, Nj, Nk             ! Cells in each direction (I, J, K)
  real(R8P), allocatable :: mesh(:,:,:,:)  ! Coordinates, mesh(1:ndir,0:Ni,0:Nj,0:Nk)
  real(R8P), allocatable :: vars(:,:,:,:)  ! Variables, vars(1:nvar,1:max(Ni,1),1:max(Nj,1),1:max(Nk,1)) at the cells,
                                           ! vars(1:nvar,0:Ni,0:Nj,0:Nk) at the nodes
end type
```

**Block structure:**
- `mesh(1,i,j,k)` - X coordinates
- `mesh(2,i,j,k)` - Y coordinates
- `mesh(3,i,j,k)` - Z coordinates
- `vars(n,i,j,k)` - nth solution variable

### Format Options

Each format has specific options:

**Tecplot (`Type_tec_Format`):**
```fortran
character(6) :: extension  ! File extension (default: '.tec')
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
logical :: node            ! Node or cell data location (default: .false.)
logical :: bc              ! Save boundary conditions (default: .false.)
logical :: double          ! Binary files (.plt/.szplt): .true. = 64-bit values, .false. = 32-bit (default: .true.)
```

`double` sets the precision of the values stored in a binary Tecplot file (TecIO `VIsDouble`); the data are
always handed to TecIO as double precision. A program that writes binary files and needs a given precision sets
the option before the write; otherwise its files follow this default. ASCII files are not affected.

**VTK (`Type_vtk_Format`):**
```fortran
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
logical :: node            ! Node or cell data location (default: .false.)
```

**PLOT3D (`Type_p3d_Format`):**
```fortran
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
```

### Error Handling

All I/O functions return an integer error code:

```fortran
integer :: E_IO

E_IO = tec_read_structured_multiblock(filename='data.tec', orion=IOfield)

if (E_IO == 0) then
  print *, 'Success!'
else
  print *, 'Error occurred, code:', E_IO
endif
```

**Error Codes:**

- `0`: Success
- Non-zero: Error (specific codes vary by function)

## Format Modules

ORION provides three format-specific modules:

```fortran
use Lib_Tecplot ! For Tecplot ASCII and binary formats:
```
```fortran
use Lib_VTK ! For VTK ASCII and binary formats
```
```fortran
use Lib_PLOT3D ! For NASA PLOT3D grid files
```

## Reading Data

**Tecplot Files**

ASCII Format

```fortran
program read_tecplot_ascii
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO, iblock, nblocks, nvars
  
  ! Read file
  E_IO = tec_read_structured_multiblock(filename='solution.tec', orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read file'
    nblocks = size(IOfield%block)
    print *, 'Number of blocks:', nblocks
    print *, 'Variables:', IOfield%varnames
    
    ! Access block 1 information
    print *, 'Block 1 name:', trim(IOfield%block(1)%name)
    print *, 'Block 1 dimensions:', IOfield%block(1)%Ni, IOfield%block(1)%Nj, IOfield%block(1)%Nk
    
    ! Access mesh coordinates
    print *, 'X range:', minval(IOfield%block(1)%mesh(1,:,:,:)), &
                        maxval(IOfield%block(1)%mesh(1,:,:,:))
  endif
end program read_tecplot_ascii
```

Binary Format (with TecIO)

For `.szplt` files, use the same function: ORION takes the format from the extension, the text after the last dot of
the last component of the name (`cases.data/f.szplt` and `a.dat.szplt` are `.szplt` files). A name whose extension is
another one, such as `grid.data` (extension `data`), `mesh.tecplot` or `field.tec.bak`, is refused with the return value
1, even when it holds `.dat` or `.tec` elsewhere; rename such a file with the extension `.dat` or `.tec`. The ASCII writer
writes a file under any name, so a file it writes under such a name is not read back.

```fortran
E_IO = tec_read_structured_multiblock(filename='solution.szplt', orion=IOfield)
```

A `.plt` file, which `tec_write_structured_multiblock` writes, is not read: the function returns 1, with a message,
for a file name whose extension is none of `.dat`, `.tec` and `.szplt`. A `.szplt` file that TecIO opens and
then cannot read, such as a damaged one, gives the nonzero status of the TecIO call that fails, with a message: the
read ends there, and the blocks hold only what was read before it. ORION reads ordered zones: a `.szplt` file with a
finite-element zone (`FELINESEG` to `FEPOLYHEDRON`, Data Format Guide, TECZNE142) is refused with the return value 1 and a
message that names the zone and its type, before any zone is read.

!!! note "TecIO Requirement"
    Binary format support requires building with `--use-tecio` flag. Without TecIO, a call that reads or writes a binary
    file stops the program with error termination (`ERROR STOP`): the message `You can not read in binary format without
    compiling against TecIO` (or `write`) and a nonzero exit status.

A read into an `orion_data` that already holds blocks, from an earlier read, replaces them: the ASCII, the `.szplt` and
the points readers (`tec_read_points_multivars`) alike.

### VTK Files

```fortran
program read_vtk_structured
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Read the multi-block field out/field.vtm in the raw format; the .vtm lists its block files relative to its
  ! directory, so vtspath is that directory
  IOfield%vtk%format = 'raw'
  E_IO = vtk_read_structured_multiblock(vtspath='out/', vtmpath='out/field', orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read VTK file'
    print *, 'Number of blocks:', size(IOfield%block)
    print *, 'Block 1 cells:', IOfield%block(1)%Ni * &
                                IOfield%block(1)%Nj * &
                                IOfield%block(1)%Nk
  endif
end program read_vtk_structured
```

### Dimensions of what a reader returns

- **Tecplot ASCII, structured zones.** A zone header gives `I`, `J` and `K`; a header without `J` is one node line
  (`J = 1`, an I-ordered zone, as in the examples of the Tecplot Data Format Guide), and one without `K` is one node
  plane (`K = 1`, Tecplot's default). The number of coordinates is that of the file, the same for every zone, by the
  rule of the `.szplt` reader: three when a zone has `K > 1`, or when the header declares a third coordinate: the
  first three variables are named `x`, `y` and `z` (in any case) and `z` is nodal in every zone (not listed as
  `CELLCENTERED` in `VARLOCATION`), so that its zones of single node planes or lines are part of a three-dimensional
  field (a slice); otherwise two when a zone has `J > 1`, or when the first two variables are named `x` and `y` (in
  any case) and `y` is nodal in every zone, a two-dimensional file, also of lines only (the lines of a two-dimensional
  field); otherwise one, a file of lines (I-ordered zones). `mesh` holds the coordinates (`mesh(1:3,...)` with three,
  also for a zone with one node plane, `Nk = 0`; `mesh(1:2,...)` with two) and the solution variables start after
  them. The rule goes by name: a two-dimensional file whose third variable is nodal and named `z` (a mixture fraction
  `Z`, for example) is read as a slice, so rename that variable or write it cell-centred to read the file as
  two-dimensional. In the same way a one-dimensional field at the nodes whose first variable is named `y` (a mass
  fraction `Y`, for example) is read with two coordinates, and one whose first two variables are `y` and `z` with
  three: rename them or write the field at the cells to read it with one coordinate. Any other file of lines does not
  tell a one-dimensional field from lines of a two-dimensional one: it is read with one coordinate, with a warning
  when its second variable is named `y` (at the cells). A zone with one node plane or line in a direction holds one
  layer of cells there: a cell-centred variable has `max(I-1,1)*max(J-1,1)*max(K-1,1)` values (Tecplot 360 Data Format
  Guide, sections 2-1 and 4-4). `varnames` holds the coordinate names followed by the variable names, so
  `size(varnames) = size(mesh,1) + size(vars,1)`. The solution variables are at the nodes (`orion%tec%node` set) when
  no variable is listed as `CELLCENTERED` in `VARLOCATION`, at the cells otherwise; a file without solution variables
  sets `orion%tec%node`, as no variable of it is `CELLCENTERED`, while the `.szplt` reader leaves the flag as it is
  for such a file. ORION holds the coordinates at the nodes and one location for all solution variables of a file: a
  file with a coordinate at the cells, or with solution variables at the nodes and at the cells, in a zone or between
  zones, is refused (return value 1, with a message on the standard error).
- **Tecplot binary (`.szplt`, TecIO builds).** The number of coordinates is that of the file, the same for every zone,
  as the variables of a Tecplot file are: three when a zone has more than one node plane in K (`KMax > 1`), or when
  the file declares a third coordinate by the rule of the ASCII reader: the first three variables are named `x`, `y`
  and `z` (in any case) and `z` is nodal in every zone, so that its zones of single node planes or lines are part of a
  three-dimensional field (a slice); otherwise two when a zone has more than one node line in J (`JMax > 1`), or when
  the first two variables are named `x` and `y` and `y` is nodal in every zone, also in a file of lines only;
  otherwise one, a file of lines. `mesh` holds the coordinates (`mesh(1:3,...)` with three, also for a zone with one
  node plane, `Nk = 0`) and the solution variables start after them. As in the ASCII reader, a two-dimensional file
  whose third variable is nodal and named `z` is read as a slice, and a one-dimensional field at the nodes whose first
  variables are named `y` (and `z`) with two (three) coordinates. A zone with one node plane or line in a direction
  holds one layer of cells there. `varnames` is filled with the names stored in the file, with the same convention as
  the ASCII reader (coordinates first). A zone with a coordinate at the cells, or with solution variables at the nodes
  and at the cells, is refused (return value 1, with a message on the standard error). Each block takes the location
  of its zone, `vars(:,0:...)` at the nodes and `vars(:,1:...)` at the cells, so the zones of a file may differ:
  `lbound(vars,2)` tells the location of each block. The reader sets `orion%tec%node` when the solution variables of
  every zone are at the nodes, clears it when those of every zone are at the cells, and leaves it as it is when the
  zones differ or the file has no solution variables. The ASCII reader refuses a file whose zones differ, and
  `tec_write_structured_multiblock` writes every block of a field at the location of `orion%tec%node`. A passive
  variable, which holds no values in its zone, reads as zero, the value Tecplot gives it, and does not decide the
  location of its block; a zone whose solution variables are all passive (a zone of coordinates only) takes the
  location of the file, that of `orion%tec%node`. A file with fewer variables than the coordinates of its zones (a
  zone with more than one node line in J and the variable `x` only) is refused (return value 1, with a message); the
  ASCII reader refuses it too, without a message.
- **Values at the cells of `.szplt` zones with one node plane or line.** The `.szplt` reader reads the values at
  the cells of a zone with `IMax`, `JMax` or `KMax` = 1 (a surface, a line or a point, such as the faces of a volume
  block) one layer of cells in K at a time, or one row of cells in I at a time when `KMax` = 1: the TecIO reader
  gives some of them right only so.
- **PLOT3D grids.** The first dimensions record decides the dimension of the whole file: two integers
  (`Ni Nj`) give a two-dimensional grid (two coordinates, `Nk = 0`), three integers (`Ni Nj Nk`) a
  three-dimensional one. Each block has its own dimensions record; a node count below 1 is refused.
- **VTK multi-block (`vtk_read_structured_multiblock`).** A block is read as two-dimensional when it has two node
  planes in k and every node has `z = 0`, the form in which `vtk_write_structured_multiblock` writes a
  two-dimensional mesh (two node planes with the same `x` and `y`, one layer of cells): `mesh` holds the two
  coordinates of one node plane (`mesh(1:2,...,0:0)`). A block that also has two node lines in j with `y = 0` at
  every node is read as one-dimensional, the form in which the writer writes a mesh of one coordinate: `mesh`
  holds `x` of one node line (`mesh(1:1,...,0:0,0:0)`) and the variables one layer of cells in j and k, or, at the
  nodes, the node line (`vars(:,0:Ni,0:0,0:0)`). Any other block is read with three coordinates, also when
  its `z` add up to 0, as for a slab around `z = 0`, and when it has one node plane in k with `z = 0` at every node,
  as for the face of a three-dimensional block on the plane `z = 0`; such a block has no layer of cells in k, so the
  cell values of a two-dimensional field that another program writes as one node plane are not read (`vars(:,1:Ni,1:Nj,1:0)`,
  return value 0). `Ni`, `Nj` and `Nk` are the cells of the block file: a two-dimensional block has `Nk = 1`, a
  one-dimensional one `Nj = Nk = 1`, where the Tecplot readers return 0 for the directions of one node plane; the
  bounds of `mesh` and `vars` are those of the Tecplot readers. The reader opens `vtspath//f` for each block file `f`
  that the `.vtm` lists, and the `.vtm` lists them relative to its own directory: `vtspath` is the directory of the
  `.vtm` file (`'out/'` for `vtmpath = 'out/field'`), or `''` when the `.vtm` is in the current directory or lists
  absolute paths. The reader returns 1 when it refuses its arguments or the layout of a file, or when the `.vtm` file
  lists no block file, and the nonzero code of the read that fails (-1, or an `IOSTAT` value) when the `.vtm` file
  cannot be opened or ends before the end of its block list, and when a block file cannot be opened or its points or
  the values of a variable cannot be read; what it has read before the error is left in `orion`. It closes each block
  file once read, and the file of a block it stops on, so the number of blocks of a field is not bounded by the number
  of files a process can keep open. A read into an object that already holds blocks, from an earlier read or from one
  that failed, replaces
  them once the block list of the `.vtm` file is read; a `.vtm` file that cannot be opened, or whose block list cannot
  be read, leaves them as they were. The variables of a field are at one location: a block file with arrays at the
  nodes and at the cells (`PointData` and `CellData`) is not read (a nonzero value after the mesh of that block). The
  reader takes the files in the layout in
  which `vtk_write_structured_multiblock` writes them: a `.vtm` file whose `Block` element lists one `DataSet` per line, and
  block files of one `StructuredGrid` piece whose `Points` array (named `Points`, `Float64`) comes before its `PointData` or
  `CellData` arrays (`Float64`, `NumberOfComponents="1"`), in the format of `orion%vtk%format`: `ascii`, `binary` (base64
  with a 4-byte length, not compressed) or `raw` (raw appended data). Start tags must be on one line: a block file with a
  tag split over lines is refused (return value 1) before any block is read, with a message on the standard error that
  gives the file, the line and the tag. A block file that lists its data arrays before its `Points`, as VTK and ParaView
  write them, is refused (return value 1) before any block is read.
- **Solution time.** `solutiontime` is 0 in a new `orion_data`. The Tecplot ASCII reader stores the
  `SOLUTIONTIME` of the file, or `-huge(1._R8P)`, the most negative real, when its zone headers give none: a time
  that no solution has, so that a program can tell a file without a time (with signed times, the -10 that the
  reader gave before is the time of a steady solution at iteration 10). The `.szplt` reader stores the solution
  time of the last zone (every zone of a binary file has one), also in a read with `dims_only`. The PLOT3D reader
  leaves `solutiontime` as it was (0 for a new object), since a PLOT3D grid has no time. The VTK multi-block reader
  `vtk_read_structured_multiblock` does not set
  `solutiontime` either: its optional `time` argument returns the `TIME` field data that
  `vtk_write_structured_multiblock` writes when it is given `time` (every format keeps every bit, sign included;
  see Digits of the text formats), or 0 when the `.vts` files hold none. Field data are not variables: the reader
  skips them when it lists the variable names.
- **Cycle and named scalars.** Besides `time`, `vtk_write_structured_multiblock` takes the optional `cycle`
  (integer, written as the field data `CYCLE`, a step number) and the named real scalars
  `fldnames(:)` and `fldvalues(:)` (each written as field data with its name), and writes them in every `.vts`
  file. `vtk_read_structured_multiblock` takes the same arguments and returns them, with `fldfound(:)` telling
  which names the files hold: an absent `CYCLE` or name reads as 0 (and `.false.`). It looks for them, and for
  `TIME`, in the `FieldData` element of every block file that has one before its first data array, where
  `vtk_write_structured_multiblock` writes it (a `FieldData` element after the arrays is not read): when the
  files differ, the values are those of the last block file that holds them. Every format keeps every bit of a
  real. The reader takes the `FieldData` element with its arrays on lines of their own, as ORION writes it, or on
  one line, as XML allows (XML 1.0 (Fifth Edition), W3C Recommendation 26 November 2008, section 3.1, production
  [43]: the content of an element holds its elements with no line break needed between them).
  `fldnames` and `fldvalues` (and
  `fldfound`) go together and have the same size, and no name is blank; otherwise both functions return 1 and write
  or read nothing. The reader compares the names without regard to case (`'a'` finds `A`).
  A program calls the writer without these arguments to get the files it got before. `tec_write_structured_multiblock`
  and `tec_read_structured_multiblock` take the same optional arguments, with the same rules, and keep them as the
  data set auxiliary data of the Tecplot file (Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.6, p. 135,
  and `TECAUXSTR142`, pp. 29-30): the item `CYCLE` and one item per name, the reals with 17 significant digits; in
  an ASCII file each item is a record `DATASETAUXDATA name = "value"` after the zones. A name must be one that the
  auxiliary data can hold: not blank, without blanks, equal signs or double quotes, given once, and other than `CYCLE`
  and `Time`, the items of the writer (in any case); otherwise the writer returns 1 and writes nothing. The readers
  match the names in any case, as the VTK reader does. The ASCII reader takes the records between the zones and after
  them: a record between the `VARIABLES` header and the first zone is read as variable names, and the file is refused
  (return value 1). The `.szplt` reader takes the data set auxiliary data of the file, also with `dims_only`.
- **Digits of the text formats.** The Tecplot ASCII files and the ascii format of the VTK files hold every 64-bit
  real (coordinates, variables, `SOLUTIONTIME`, field data) with 17 significant digits (Fortran format `E25.17E3`),
  the fewest that give back every 64-bit real, bit for bit, when the file is read. A 64-bit real has a 53-bit
  significand: the reals of a range [2^e, 2^(e+1)) are 2^(e-52) apart, more than x/2^53 for every x of the range,
  while the decimal numbers of n significant digits of a range [10^d, 10^(d+1)) are 10^(d-n+1) apart, at most
  x/10^(n-1). When 10^(n-1) > 2^53, that is n >= 17 (2^53 is about 9.0e15), the decimal numbers are closer
  together than the reals everywhere, so the decimal number of n digits nearest to a real is closer to it than to
  any other real and reads back as that real. Sixteen digits are not enough: in [1, 2) the reals are 2^-52 (about
  2.2e-16) apart and the decimal numbers of 16 digits 10^-15 apart, so 1, 1 + 2^-52 and 1 + 2^-51 are all written
  `1.000000000000000` and read back as 1. With 15 digits, as the writers used, `0.1 + 0.2` was written
  `0.300000000000000E+000` and read back as `0.3`; with 17 it is `0.30000000000000004E+000`. Both conversions must
  round to the nearest value, while the input/output rounding mode of a file opened without `ROUND=`, and of an
  internal file, is processor dependent (Fortran 2018, ISO/IEC 1539-1:2018, final draft J3/18-007r1, 12.5.6.16,
  paragraph 1, p. 223, and 12.5.2, paragraph 2, p. 217): the tests `tecplot_write` and `vtk_read_wrapper` check that
  the values read back are those written, bit for bit. The binary formats (Tecplot `.plt` and `.szplt` with 64-bit
  values, VTK `binary` and `raw`) hold the bits themselves.
- **Steady solutions and strands.** `tec_write_structured_multiblock` writes `solutiontime` with its sign, as the
  `SOLUTIONTIME` of every zone header of an ASCII file and as the solution time of every zone of a binary file. A
  negative `solutiontime` marks a steady solution (for instance minus the iteration count of the run): its zones are
  static, with no `STRANDID` in an ASCII file and StrandID 0, a static zone for TecIO, in a binary file. The zones of a
  time-accurate solution (`solutiontime >= 0`) belong to the strand of their block, `STRANDID = b` in the header of
  zone `b` and StrandID `b` in a binary file, so that Tecplot follows each block over the files of a run (Tecplot 360 EX
  2023 R1 Data Format Guide: a `STRANDID` of 1 or more in the ASCII zone record, p. 124, "by convention ... successive
  integer values"; TECZNE142, p. 61, "0 = static zone, not associated with a strand"). The ASCII reader returns
  `SOLUTIONTIME` as written, sign included, and stores the `STRANDID` of the last zone header in `strandid`, -1 when
  the header has none (a static zone); the `.szplt` reader stores the StrandID of the last zone the same way, -1 for a
  static zone (StrandID 0 for TecIO). A program tells a steady solution from the sign of `solutiontime`. ORION 1.5.5
  to 1.7.0 wrote `|t|` and, for a steady solution, `STRANDID = 0` in every ASCII zone header (a strand for Tecplot,
  not a static zone): such a file reads back with a positive `solutiontime` and `strandid` 0. `strandid` is -1 in a
  new `orion_data` and only these two readers set it: the VTK and PLOT3D readers and `tec_read_points_multivars`
  leave it as it was; `copyORION` copies it, with `solutiontime` and every format option. The data set auxiliary data
  `Time` of a binary file holds `|t|`.

### Length of the Tecplot header and lines

- **VARIABLES header.** `tec_write_structured_multiblock` and `tec_write_points_multivars` build the list of
  variable names (the `VARIABLES` line of an ASCII file, the list handed to TecIO for `.plt` and `.szplt`) in
  a buffer of 32768 characters, and the ASCII reader reads up to 32768 characters of header: the `VARIABLES`
  line, the lines before it and its continuation lines, joined. A longer list is cut without a message.
- **Names given to the writer.** `varnames` of `tec_write_structured_multiblock` holds the names of the variables
  written, separated by blanks, tabs or commas (double quotes count as blanks, so a name holds none of them), as many as
  the variables written (`Nvars`, or `size(vars,1)`; none when block 1 has no `vars`, a write of the mesh only); the
  coordinate names come first in the file. A list of another number of names gives the return value 1, with a message,
  and no file is written: TecIO takes the number of names as the number of values of each zone. `Nvars`, when given, is
  from 0 to `size(vars,1)`, 0 when block 1 has no `vars` (the first `Nvars` variables are
  written); another value gives the return value 1, with a message, and no file is written. Every block holds the variables
  written: a block whose `vars` holds fewer of them than block 1 (or than `Nvars`) gives the return value 1, with a message
  that names the block, and no file is written. Every block holds the coordinates of block 1: a block whose `mesh` holds
  another number of coordinates gives the return value 1, with a message that names the block, and no file is written.
- **Forms of the names.** The ASCII reader takes the names from the `VARIABLES` line and its continuation lines,
  up to the first `ZONE` record: the keyword `ZONE` as a whole word, so a name that merely begins with it (such
  as `zone_velocity`) is a name. Each name may be in double quotes, in single quotes or bare, and the forms may
  be mixed in one list; names are separated by blanks, tabs or commas, and a name in quotes may contain them.
  A quote closes the name, so `"a""b"` is the two names `a` and `b`. A quote that is never closed is an error,
  and so is a list with no name. The Python reader `read_TEC` reads the names in the same way, except that it
  returns an empty list for a list with no name.
- **Number of names.** The ASCII reader keeps at most 512 names, coordinates included. A header with more names is
  refused: the reader returns 1 and prints `no variables found in VARIABLES header`, a message that does not name the
  limit; a list of 512 names followed by a comma is refused in the same way. The `.szplt` reader takes the names one
  by one from the file and has no such limit. Every name is kept to 32 characters (`varnames` is `character(len=32)`);
  the ASCII reader warns when it shortens one, the `.szplt` reader shortens it without a message.
- **Zone headers and data lines.** The ASCII readers read each line whatever its length and join the lines of a zone
  header whatever their length, so a zone header holds a block name of any length (`T =`, the title of the zone, is
  the name of the block in the files that ORION writes). Each data line of a structured zone must fit in 1000
  characters. `tec_write_structured_multiblock` writes one value per line; `tec_write_points_multivars` writes one
  point per line, with all its variables on that line. Tecplot keeps the first 128 characters of a zone title (Tecplot
  360 EX Data Format Guide, section 4-3.2), and TecIO writes only those in a `.szplt` file (a `.plt` file gets the
  whole title). The readers return the title of each zone as the name of its block: the `.szplt` reader the title
  stored in the file; the ASCII readers the text between double quotes or, without quotes, a word up to the next
  blank, comma, tab or `)`, the form in which the writers write the name (a name with blanks or commas is therefore
  not read back as written), and a blank name when the zone has no title; `tec_read_points_multivars` takes the title
  from the line of the zone header that holds `I=`. A title longer than the name of a block keeps its first 4096
  characters. The ASCII readers do not take a backslash before a double quote as an escape (Data Format Guide, section
  4-2, documents `\"` as a double quote in a string): a title `T="a\"b"` closes at that quote, and when `I`, `J` and
  `K` come after it the rest of the header counts as quoted text and the zone is refused (invalid zone dimensions).

### PLOT3D Files

PLOT3D typically uses separate grid and solution files. ORION is designed to handle just grids.

```fortran
program read_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Read both grid and solution files
  E_IO = plot3d_read(filename='grid.p3d',orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read PLOT3D files'
    print *, 'Number of blocks:', size(IOfield%block)
  endif
end program read_plot3d
```

A read into an `orion_data` that already holds blocks, from an earlier read, replaces them, as the Tecplot and VTK
readers do.

## Writing Data

**Tecplot Files**

ASCII Format

```fortran
program write_tecplot
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Assume IOfield is populated with data
  ! IOfield%block(:)%mesh contains coordinates
  ! IOfield%block(:)%vars contains solution variables
  
  ! Set format options
  IOfield%tec%format = 'ascii'  ! or 'binary'
  IOfield%tec%extension = '.tec'
  IOfield%tec%node = .false.     ! Node-centered data
  
  ! Write Tecplot ASCII file
  E_IO = tec_write_structured_multiblock( &
           orion=IOfield, &
           filename='output.tec')
  
  if (E_IO /= 0) then
    print *, 'Error writing file'
  endif
end program write_tecplot
```

!!! tip "Variable Names"
    Variable names are stored in the `varnames` array. Each element is a string containing the variable name.

Binary Format

```fortran
! Same function, different format option
IOfield%tec%format = 'binary'
IOfield%tec%extension = '.plt'

E_IO = tec_write_structured_multiblock( &
         orion=IOfield, &
         filename='output.szplt')
```

Without TecIO the binary writer stops the program with error termination and a nonzero exit status, as the reader does (see
the TecIO note under Reading Data). A binary file is a `.plt` or a `.szplt` file, as the extension of its name says: a name
with another extension, or none, gives the return value 1, with a message, and no file is written. Each TecIO call of the
binary writer is checked: the first that fails, such as one for a file that cannot be opened or for a named scalar whose
name TecIO does not take (a letter or `_` first, then letters, digits, `_` and `.`), ends the write, and its nonzero status
is the return value, with a message that names the call and the file; the file is closed, and what TecIO leaves on disk may
be incomplete.

### Format Options

Control Tecplot output with format options:

```fortran
! ASCII output, node-centered, with boundary conditions
IOfield%tec%format = 'ascii'
IOfield%tec%extension = '.tec'
IOfield%tec%node = .true.   ! Node-centered (vs cell-centered)
IOfield%tec%bc = .true.     ! Include boundary condition cells

! Binary output
IOfield%tec%format = 'binary'
IOfield%tec%extension = '.plt'
IOfield%tec%node = .false.
IOfield%tec%bc = .false.
```

**Variables written.** A block is written as an ordered zone of `Ni+1` x `Nj+1` x `Nk+1` nodes, with `Ni`, `Nj` and `Nk`
the cells of its mesh in each direction (`size(mesh,2)-1` and so on; `Nk` = 0 for a two-dimensional mesh, `mesh(1:2,...)`). In
each direction the writer writes `max(N,1)` values of each variable at the cells, or `N+1` at the nodes (`orion%tec%node`), and
one value in k for a two-dimensional block. It takes them from `vars`, direction by direction:

- when `vars` has that many indices there, all of them, whatever their bounds: at the cells `vars(:,1:Ni,...)` or
  `vars(:,0:Ni-1,...)`, at the nodes `vars(:,0:Ni,...)` or `vars(:,1:Ni+1,...)`; the node plane of a two-dimensional block may
  be `vars(:,:,:,0:0)`, as the Tecplot readers return it, or `vars(:,:,:,1:1)`;
- otherwise the indices `1:max(N,1)` at the cells and `0:N` at the nodes (`1:1` in k for a two-dimensional block), which
  `vars` must hold: variables with ghost cells, such as `vars(:,0:Ni+1,0:Nj+1,0:Nk+1)` at the cells, are written without
  them (with `orion%tec%bc = .false.`, the default; writing ghost cells is not supported).

A surface block, with one plane of nodes in a direction (`Ni`, `Nj` or `Nk` = 0, such as a face of a volume block), and a line
of nodes are written as such zones in ASCII and binary files alike: variables at the cells hold one layer of cells where a
direction has one plane of nodes (`vars(:,1:1,:,:)` for an i-face, `max(Ni,1)*max(Nj,1)*max(Nk,1)` values), as an ordered zone
stores its cell-centered values (Tecplot 360 Data Format Guide, section 2-1); variables at the nodes hold one value per node
(`vars(:,0:0,:,:)` for an i-face).

### VTK Files

```fortran
program write_vtk
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  
  type(ORION_data) :: IOfield
  character(len=64) :: varnames
  integer :: E_IO
  
  ! ... the blocks of IOfield, with mesh and vars ...
  ! Write the multi-block field out/field.vtm and its block files out/<name>.vts (the directory out must exist);
  ! varnames, a variable, holds the names of the variables separated by blanks
  IOfield%vtk%format = 'raw'
  varnames = 'density momentum_x momentum_y momentum_z energy'
  E_IO = vtk_write_structured_multiblock(vtspath='out/', vtmpath='out/field', orion=IOfield, varnames=varnames)
end program write_vtk
```

The block files take their sizes from `Ni`, `Nj` and `Nk`, the number of cells in each direction. A surface block, with one
plane of nodes in one direction (`Ni`, `Nj` or `Nk` = 0, `mesh(:,0:0,:,:)` for an i-face), has one layer of cells in that
direction (`vars(:,1:1,:,:)`), as `tec_write_structured_multiblock` writes it, when the index ranges of `vars` hold that
layer; otherwise, as for node-shaped variables `vars(:,0:Ni,0:Nj,0:0)` with `orion%vtk%node` unset, its cell arrays are
written empty. `vtk_read_structured_multiblock` counts the cells of a block from its extent and does not read the cell
values of a surface block back. Variables at the nodes
(`orion%vtk%node`, point data in the block files) hold one value per node, `vars(:,0:Ni,0:Nj,0:Nk)` with `Ni`, `Nj` and `Nk`
the cells, as the Tecplot readers return them; a two-dimensional block holds the values of its node plane,
`vars(:,0:Ni,0:Nj,0:0)`, which the writer writes on both planes of nodes of the file, or of both planes
(`vars(:,0:Ni,0:Nj,0:1)`). Variables at the nodes with other bounds, such as those of the cells, from 1 or from 0, are refused
(return value 1, nothing written). The reader sets `orion%vtk%node` when the
block files hold point data and no cell data, not for a mesh without variables. A mesh of three coordinates is written as it
is, one of two coordinates (`mesh(1:2,...)`) as two node planes in k with `z = 0` and one layer of cells in k, one of one
coordinate (`mesh(1:1,...)`) as two node lines in j and two node planes in k with `y = z = 0` and one layer of cells in j and k,
the values of its node line at the nodes (`vars(:,0:Ni,0:0,0:0)`) on each of the four lines of nodes, or those of the four
lines (`vars(:,0:Ni,0:1,0:1)`); a mesh of another number
of coordinates is refused (return value 1, nothing written).

The `.vtm` file lists the block files as the `DataSet` elements of one block, with the indices 0 to nb-1 in the order of
the blocks, each relative to the directory of the `.vtm` file, as VTK readers take it: the block files
`vtspath//name//'.vts'` of the file `vtmpath//'.vtm'` are listed with the path that leads from the directory of `vtmpath` to
`vtspath` (`vtspath = ''` and `vtmpath = 'out/field'` give `../name.vts`). `vtspath` and `vtmpath` are both relative to the
same directory or both absolute; an absolute `vtspath` that shares no directory with `vtmpath` is listed as it is. An absolute
`vtmpath` with a relative `vtspath`, or the same directory written in two forms (`./out` and `out`), would need the current
directory, which the writer does not take: write both paths in the same form. A field written with `vtspath` and `vtmpath`
is read back with `vtk_read_structured_multiblock(vtspath=<the directory of vtmpath>, vtmpath=vtmpath, ...)`, or with
`vtspath = ''` when the `.vtm` lists absolute paths. The reader reads the lines of the `.vtm` file
whatever their length and takes each listed file, path included and without `.vts`, as the `name` of its block, which holds
4096 characters, the longest path that Linux accepts (`PATH_MAX`); a longer listed file gives the return value 1.

### PLOT3D Files

```fortran
program write_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  
  type(ORION_data) :: IOfield
  integer :: E_IO
  
  ! Write grid and solution files
  E_IO = plot3d_write(filename='output_grid.p3d',orion=IOfield)

end program write_plot3d
```

## Multi-Block Data

ORION natively supports multi-block structured grids:

```fortran
program multiblock_example
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO, iblock, nblocks
  
  ! Read multi-block file
  E_IO = tec_read_structured_multiblock(filename='multiblock.tec', orion=IOfield)
  
  ! Get number of blocks
  nblocks = size(IOfield%block)
  
  ! Iterate over blocks
  do iblock = 1, nblocks
    print *, 'Block', iblock, ':', trim(IOfield%block(iblock)%name)
    print *, '  Dimensions (I,J,K):', IOfield%block(iblock)%Ni, &
                                       IOfield%block(iblock)%Nj, &
                                       IOfield%block(iblock)%Nk
    
    ! Access mesh coordinates
    print *, '  X range:', minval(IOfield%block(iblock)%mesh(1,:,:,:)), &
                            maxval(IOfield%block(iblock)%mesh(1,:,:,:))
    
    ! Access solution variables (if present)
    if (allocated(IOfield%block(iblock)%vars)) then
      print *, '  Number of variables:', size(IOfield%block(iblock)%vars, 1)
    endif
  enddo
end program multiblock_example
```

## Data Manipulation

### Accessing Mesh Coordinates

```fortran
! For block 'iblock'
real(R8P), allocatable :: x(:,:,:), y(:,:,:), z(:,:,:)
integer :: Ni, Nj, Nk

! Get dimensions
Ni = IOfield%block(iblock)%Ni
Nj = IOfield%block(iblock)%Nj
Nk = IOfield%block(iblock)%Nk

! Extract the coordinates of the nodes: arrays of (Ni+1) x (Nj+1) x (Nk+1) values, allocated by the assignment,
! with lower bounds 1 (the node (0,0,0) of the block is x(1,1,1)); z only when the block has three coordinates
x = IOfield%block(iblock)%mesh(1,:,:,:)
y = IOfield%block(iblock)%mesh(2,:,:,:)
if (size(IOfield%block(iblock)%mesh, 1) == 3) z = IOfield%block(iblock)%mesh(3,:,:,:)
```

### Accessing Solution Variables

```fortran
! Variables are stored as vars(var_index, i, j, k), at the cells or at the nodes (see The obj_block Type)
real(R8P), allocatable :: variables(:,:,:,:)
real(R8P), allocatable :: pressure(:,:,:)
integer :: nvars

! Extract all variables for a block, with the bounds of vars
variables = IOfield%block(iblock)%vars
nvars = size(variables, 1)

! Extract a specific variable (e.g., pressure at index 4): an array of the values of the block, allocated by the
! assignment, with lower bounds 1
pressure = IOfield%block(iblock)%vars(4,:,:,:)
```

## Best Practices

1. **Always check error codes**: Don't assume I/O operations succeed
2. **Deallocate memory**: Free `IOfield` components when done
3. **Match variable names**: Ensure `varnames` matches actual data
4. **Use appropriate formats**: Choose format based on downstream tool requirements
5. **Test with small datasets**: Verify logic before scaling up

## Tests and Examples

For full examples, refers to the `src/test/` folder.

---