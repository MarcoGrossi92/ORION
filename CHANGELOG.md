# Changelog

All notable changes to ORION are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Changes on `main` since `v2.0.0`.

### Fixed

- `read_whole_line` (module `strings`) reads whole the last line of a file
  connected for sequential access also when it has no line feed. With gfortran
  such a line, of a length that is a multiple of 512 characters, gave the end
  of the file and was lost, and the Tecplot ASCII reader, which reads the first
  data line of each zone with `read_whole_line`, refused a file whose only data
  line was such a last line.
- The Tecplot ASCII reader reads the data lines of a structured zone whole,
  whatever their length. It kept the first 1000 characters of each line: a
  file with longer lines, which the Tecplot format allows (up to 32,000
  characters), was refused, or, when the last number of a line crossed column
  1000, read with a wrong value and no message (1.2345678901234567E+05 read as
  1.2345678901234567E+00). Files whose lines hold at most 1000 characters,
  such as those of `tec_write_structured_multiblock` (one value per line), are
  read as before.

## [2.0.0] - 2026-10-20

### Added

- `orion_data%strandid` (integer, default -1): the Tecplot ASCII reader stores
  the `STRANDID` of the last zone header, -1 when the header has none. Code
  that does not use the new component behaves as before.
- Optional arguments of `vtk_write_structured_multiblock` and
  `vtk_read_structured_multiblock`: `cycle` and the named real scalars
  `fldnames` and `fldvalues` (with `fldfound` for the reader), written as the
  field data `CYCLE` and `fldnames(i)` of every `.vts` file next to `TIME`, and
  returned by the reader, 0 (and `fldfound` false) when the files hold none.
  Names and values of different sizes, and blank names, are refused (return
  value 1); the reader compares the names without regard to case.
- The same optional arguments for `tec_write_structured_multiblock` and
  `tec_read_structured_multiblock`, kept as the data set auxiliary data of the
  Tecplot file: `CYCLE` and one item per name, the reals with 17 significant
  digits (`DATASETAUXDATA` records after the zones in an ASCII file). Names
  and values of different sizes, and blank names, are refused (return value
  1); the writer refuses also the names that an item cannot hold.
- `read_whole_line` in the module `strings` (`stringmod.f90`): reads the next
  line of a formatted file whole, whatever its length.
- `p3d_read_multiblock` reads PLOT3D grid files of every common layout, detected
  from the file itself (`orion%p3d%format` is no longer read): ASCII, Fortran
  unformatted (4- or 8-byte record markers, records over 2 GiB in subrecords)
  or C binary (stream); either byte order; 4- or 8-byte integers and reals;
  multi-grid or single grid; 3D or 2D; with or without iblank (read past);
  whole or planes. Each layout is checked against the size of the file (the
  count of its values for ASCII) and, for a Fortran file, against its records;
  the file is read only when exactly one layout matches. ASCII files are free
  format: any number of values per line, the dimensions of all blocks on one
  line, Fortran repeat counts `r*c`, `D` exponents, commas, CRLF line ends.
  New optional argument `errmsg` returns the reason of a failure. Coordinates
  read from an ASCII file are bit-identical to those of the previous reader.
  Test: `p3d_read_formats` (38 layouts and refused files).
- Python: `read_P3D(file_path)` reads PLOT3D grid files by the same rules as
  `p3d_read_multiblock`, so both readers accept and refuse the same files, with
  the same messages. It returns `xb, yb, zb`, lists with one `float64` array of
  shape `(I, J, K)` per block, indexed `[i, j, k]` as in `read_TEC` (a 2D file
  gives `(I, J, 1)` arrays and `zb = None`), and raises `ValueError` with the
  reason when the file is refused. NumPy only. Test: `python_read_p3d`, which
  writes the layouts of `p3d_read_formats` with a writer of its own; on 27
  layouts both readers return the same coordinates, bit for bit, from the
  files of either writer.
- The converter reads `.vtm` files: the block files (`.vts`) are looked for from
  the directory of the `.vtm` file, their format (`ascii`, `binary` or `raw`)
  is taken from the files, or given with `--in-format=`, and the variables stay
  where the VTK files hold them (point data at the nodes, cell data at the
  cells). It reads the files that ORION writes, so a VTK, a PLOT3D or a
  Tecplot file converts to any of the other formats (to PLOT3D, the grid).

### Changed

- The name of a block, `orion%block(b)%name`, holds 4096 characters, the
  longest path that Linux accepts, instead of 128: the VTK reader stores in it
  the block file listed in the `.vtm` file, path included. Programs that use
  ORION are compiled again; each block takes 3968 bytes more.
- `vtk_read_structured_multiblock` returns variables at the nodes (point data)
  in `vars(:,0:Ni,0:Nj,0:Nk)` with `Ni`, `Nj` and `Nk` the cells, as the
  Tecplot readers return them, and the node plane of a two-dimensional block
  at k = 0; it returned the numbers of nodes as `Ni`, `Nj` and `Nk`, and the
  node plane at k = 1. A mesh without variables, also one written before this
  change, is read with `Ni`, `Nj` and `Nk` the cells, one less than before in
  each direction, and with `orion%vtk%node` unset (it was set).
  `vtk_write_structured_multiblock` writes variables at the nodes with one
  value per node (it wrote as many as cells) and refuses variables at the
  nodes with other bounds than its nodes, `vars(:,0:Ni,0:Nj,0:Nk)` (return
  value 1).
- `vtk_read_structured_multiblock` reads a block file of one plane of nodes in
  k with `z = 0` at every node, such as the face of a three-dimensional block
  on the plane z = 0, with three coordinates; it read it with two, and gave its
  cell variables a layer of cells filled from past the end of the values read.
  A two-dimensional field that another program writes as one node plane is
  therefore read with three coordinates, its point data at k = 0 and no cell
  value (return value 0). A block is two-dimensional when it has two planes of
  nodes in k and `z = 0` at every node, as `vtk_write_structured_multiblock`
  writes it.
- `vtk_write_structured_multiblock` lists each block file relative to the
  directory of the `.vtm` file, as VTK readers take it (`vtspath = ''` and
  `vtmpath = 'out/field'` list `../name.vts`); the entries depended on what the
  memory held when `vtspath` and `vtmpath` share no prefix. Such a field is
  read back with the directory of the `.vtm` file as `vtspath`.
- The Tecplot readers refuse, with a message and the return value 1, a zone
  with a coordinate at the cells or with solution variables at the nodes and
  at the cells, which ORION cannot hold: the ASCII reader read such a zone
  with its values shifted, the `.szplt` reader past the end of a buffer. The
  ASCII reader still refuses a file whose zones differ in location, now with a
  message; the `.szplt` reader reads it, each block at the location of its zone.
- The Tecplot readers take the coordinates of a file of lines of nodes by
  name, as those of a file of slices: `x` and `y` (`y` nodal in every zone)
  are two coordinates, the lines of a two-dimensional field as
  `tec_write_structured_multiblock` writes them, and `x`, `y` and `z` (`z`
  nodal) three; they were read with one. A one-dimensional field at the nodes
  whose first variable is named `y` (and `z`) is so read with two (three)
  coordinates: rename them or write the field at the cells.
- `tec_write_structured_multiblock` writes the solution time with its sign:
  a negative `solutiontime` marks a steady solution, whose zones are static
  (no `STRANDID` in an ASCII zone header, StrandID 0 in a binary file), and the
  zones of a time-accurate one belong to the strand of their block
  (`STRANDID = b`, StrandID `b`). It wrote `|t|` with `STRANDID = 0`, a strand
  for Tecplot and not a valid ASCII StrandID, and binary files did not mark a
  steady solution. A program tells it from the sign of `solutiontime`; a file
  written before reads back as before. The data set auxiliary data `Time` of a
  binary file keeps `|t|`.
- The Tecplot ASCII reader gives `solutiontime = -huge(1._R8P)` for a file
  without `SOLUTIONTIME`, instead of -10, the time of a steady solution at
  iteration 10.
- The Tecplot ASCII files and the ascii format of the VTK files hold every
  64-bit real with 17 significant digits (`FR8P` of `IR_Precision`,
  `E25.17E3` instead of `E23.15E3`), the fewest that give it back bit for bit
  when the file is read: every text file changes, each real taking two
  characters more, and `str` of a 64-bit real gives 17 digits.
- The PLOT3D ASCII files of `p3d_write_multiblock` hold every coordinate with
  the same 17 significant digits (`FR8P`, one coordinate per line as before)
  instead of the digits of a list-directed `WRITE`, which the compiler
  chooses: 17 with gfortran, 16 with ifx (`1PG24.15E3`, by its documentation;
  not tested), so that a grid written with ifx did not always read back as
  itself. With gfortran the files do not change in value, only in layout of
  the digits (`E` notation for every coordinate). The test `p3d_read` writes
  and reads back reals that 15 digits do not give back, bit for bit.
- The Tecplot reader takes the format from the extension of the file name
  only: a name that holds `.dat` or `.tec` elsewhere, such as `grid.data`,
  `mesh.tecplot` or `field.tec.bak`, which it read as an ASCII file, is
  refused with the return value 1 and a message; rename such files with the
  extension `.dat` or `.tec`. The ASCII writer writes a file under any name.
- `p3d_read_multiblock` error codes: 1 now means that no PLOT3D layout matches
  the file (it was a block count below 1; a node count below 1, formerly 2, is
  included) and 2 that more than one does. A failed open or read still returns
  its `iostat`. Blocks already in `orion` are replaced (an allocated
  `orion%block` stopped the program), and none are left after a failure.
- The Python tests are registered whenever CMake finds a Python 3 with NumPy:
  `python_read_p3d` in every build, `python_read_tec` (which reads a file of
  `tecplot_write`) still only with TecIO.
- The converter reads and writes a `.dat` file as it does a `.tec` file, in
  ASCII, with `tec_read_structured_multiblock` and
  `tec_write_structured_multiblock`: one, two or three coordinates and any
  number of variables, in BLOCK or POINT packing when read, in BLOCK packing
  when written (a `.dat` file was written in POINT packing, one point per
  line). The variables stay where the input holds them, at the nodes (point
  data in a `.vtm` file) or at the cells.
- `--in-format=` and `--out-format=` of the converter apply to the file they
  name whatever the order of the options (they were looked at when read, so
  `--out-file=` had to come first), are checked (`ascii`, `binary` or `raw` for
  a `.vtm` file, `ascii` for a `.p3d` output; another value ends the converter
  with the exit status 1) and are no longer shared by an input and an output
  `.vtm` file. A `.p3d` output in the format `binary`, which stopped the
  program with the exit status 0 and no file, is refused with a message.
  The converter says (`WARNING`) when a conversion to PLOT3D leaves the
  variables out.
- `p3d_write_multiblock` writes the grid of the meshes of the blocks
  (`mesh(1:ndir,:,:,:)`, ndir of 2 or 3, the same in every block), not of `Nk`,
  and refuses, with the return value 1, a message and no file, a mesh of one
  coordinate, blocks with a different number of coordinates, a block without a
  mesh and a format other than `ascii`. The format `binary` returns 1 with a
  message, as the others: it stopped the program with the exit status 0. The
  default of `orion%p3d%format` is `ascii`, the only format the writer writes
  (it was `binary`, which every program had to change before a write).
  A name without an extension gets `.p3d` by the last component of its path, not
  by any dot of the path.
- The converter takes the format of a file from its extension, the text after
  the last dot of its name, not from any `.dat`, `.tec`, `.szplt`, `.p3d` or
  `.vtm` of its path, and an option (`--in-file=` ...) is the whole argument
  from its start. It keeps the names of the variables (those of the Tecplot
  `VARIABLES` list and of the arrays of the VTK files, with the blanks, commas,
  tabs and quotes replaced by underscores) in place of `var1`, `var2`, ....
- `vtk_read_structured_multiblock` sets `orion%varnames`, as the Tecplot
  readers do: `x`, `y` and `z` (as many as the coordinates), then the names of
  the arrays of the block files.
- Boundary condition cells (`orion%tec%bc`, variables at the cells): the arrays
  of a block hold the layer of one cell around it (`mesh` from index -1), in
  each direction in which the block has cells, and the writer writes it as it
  is. It took the size of the block from the size of the arrays and added the
  layer to it, so that it read past the arrays; a mesh without the layer is
  refused now, with the return value 1 and a message. The option has no effect
  on variables at the nodes.
- Python: `from ORION import *` gives `read_TEC`, `write_TEC` and `read_P3D`
  only (`__all__`); `read_geometry` and `read_field`
  take the lines of the file instead of its name (`read_TEC` reads the file
  once, it read it twice for each block); `count_lines_before_float`, which
  nothing used, is gone. `read_TEC` raises `ValueError` for a file of a form it
  does not read (see Fixed). The package declares its dependency, NumPy, which
  `pip install .` installs.

### Removed

- `tec_write_points_multivars` and `tec_read_points_multivars`. They wrote and
  read a line of points (`F=POINT`, one coordinate, the variables at the
  points) with the indices `1:N` in `mesh(1:1,1:N,1:1,1:1)`, and no other
  field. `tec_write_structured_multiblock` and `tec_read_structured_multiblock`
  do the same for every field, a line of points included: the reader takes a
  zone in POINT packing, and returns a line of `N` points as `Ni = N-1`,
  `mesh(1:1,0:Ni,0:0,0:0)` and, at the nodes (`orion%tec%node` set),
  `vars(:,0:Ni,0:0,0:0)`, as it returns any field; set `orion%tec%node` to
  `.true.` to write such a field, in BLOCK packing. The reader of points read a
  file in BLOCK packing without a message and returned scrambled values.

### Fixed

- The writers report a file that cannot be written. `tec_write_structured_multiblock`
  opened an ASCII file without `iostat`, so that a directory that does not exist
  ended the program with a runtime error; it, `p3d_write_multiblock` and the VTK
  writer return a nonzero value (and the first two print a message) for a file
  that cannot be opened, and the first two for a write or a close that fails
  (a full disk), where the last status overwrote the ones before.
  `VTK_INI_XML` and `VTM_INI_XML` kept the status of `open` until the next
  `write`, which wrote to the unit that was not connected (gfortran creates
  `fort.N` for it), and `vtk_write_structured_multiblock` returned the status of
  its last call: a block file that could not be written ended with the return
  value 0. The converter ended with "Done!" and the exit status 0 having
  written nothing, for a `.vtm` file in a directory that does not exist, and
  with a backtrace for a `.tec` file; it ends with its `ERROR write` line and the
  exit status 1 for every format.
- The converter read a `.vtm` file in a directory named like a format
  (`cases.dat/out.vtm`) with the reader of Tecplot files, and wrote the output
  of a path with two such names with both writers.
- Boundary condition cells (`orion%tec%bc`): the upper bound of the cells in i
  was taken from `Nz` (`ci2 = Nz + gc`), and the layer was added in a direction
  with one plane of nodes, so that a two-dimensional block was written as a zone
  of three planes in k, read past its mesh.
- Python `read_TEC` read as cell values the values of a file with its variables
  at the nodes (the first `(I-1)(J-1)(K-1)` of each), and gave a `ValueError` or
  an `IndexError` with no reason for a file of one or two coordinates or in
  POINT packing. It checks the header of each zone (packing, `I`, `J` and `K`,
  `VARLOCATION`) and the number of its values, and raises a `ValueError` that
  names the file, the zone and what is wrong.
- Python `write_TEC` wrote no value of the variables of a zone with one plane
  or one line of nodes (`range(Nz-1)`), a header `[4-3]=CELLCENTERED` for a zone
  without variables, and took any shape of array; it writes every value, the
  header of the Fortran writer, and raises `ValueError`, before it writes, for
  shapes or names that do not fit.
- The converter converts a PLOT3D file to Tecplot (and `.dat`) files: it built
  the names of the variables from the variables of the first block, which a
  grid has not (the size of an unallocated array), and the Tecplot writer
  refused the names. It read a PLOT3D file with the message `ERROR write
  PLOT3D file` and without the reason; it now gives the reason of the reader.
- `p3d_write_multiblock` writes a mesh of two coordinates read from a VTK file
  (`Nk = 1`) as the two-dimensional grid it is: it took it for a
  three-dimensional one and read a third coordinate and a second plane that the
  mesh has not, past its bounds. A format other than `ascii` and `binary` left
  the output unit unopened.
- `--in-format=` of the converter dropped the first character of its value.
- The converter writes the variables of a `.dat` file, which are at the points,
  as point data of the `.vtm` file; it wrote them as cell data. A `.dat` file
  of two or three coordinates was read as one coordinate and two variables,
  and written with its first line of nodes only: the other lines and the other
  coordinates were lost without a message.
- `p3d_read_multiblock` reads a PLOT3D ASCII file with several coordinates per
  line. It issued one `read` per coordinate, and a list-directed `read` takes a
  whole record, so only the first value of each line was kept and the reader
  reached the end of the file inside an early block (`iostat = -1`: ATLAS
  stopped with "mesh.p3d found, but failed to read it"). Only the one value per
  line of `p3d_write_multiblock` was read. A 2-D file with the dimensions of
  all blocks on one line was taken for 3-D and failed with a read error.
- The Python reader `read_TEC` reads the Tecplot ASCII files that the Fortran
  writer of ORION 1.7.0 and later writes with the variable names without quotes
  (`VARIABLES = x y z rho(1)`). It found no names in them, and so returned the
  coordinates without any variable. The names are now read as the Fortran
  reader reads them: each one in double quotes, in single quotes or bare, the
  forms mixed freely, separated by blanks, tabs or commas. A list in quotes is
  read as before; an unclosed quote raises `ValueError`. The names are taken
  from the `VARIABLES` list only, up to the first `ZONE` record or line of
  numbers, so quoted text after the list, such as a zone title written as
  `T = "Block1"`, is no longer taken for a variable name.
- The Tecplot ASCII reader reads two quoted names with nothing between them
  (`"a""b"`, the form of the `varnames` argument of the writers, which ORION
  before 1.7.0 copied into the file) as the two names `a` and `b`; it took them
  for the one name `a"b`. A quote now always closes the name, so a name cannot
  hold the quote character that encloses it.
- The Tecplot ASCII reader, and `read_TEC`, end the `VARIABLES` list at the
  keyword `ZONE` as a whole word. A continuation line that began with a name
  such as `zone_velocity` was taken for a zone record, and the names from it on
  were lost.
- `vtk_read_structured_multiblock` reads back the files that
  `vtk_write_structured_multiblock` writes with a time, and its `time` argument
  returns the `TIME` of the files, 0 when they hold none: the reader took the
  field data for variables (a segmentation fault) and never set `time`. It
  looks for field data only in the block files that hold a `FieldData` element.
- `vtk_read_structured_multiblock` reads as three-dimensional a block whose z
  coordinates add up to 0, such as a slab around z = 0; it read it as
  two-dimensional, its z lost.
- The VTK multi-block writer and reader take a field of any number of blocks:
  they went past fixed arrays of 99 and 16 entries (memory overwritten or a
  segmentation fault).
- The Python reader `read_TEC` takes the dimensions of a zone from its `I=`,
  `J=` and `K=` keywords, outside text in quotes: a number in a zone title
  became a dimension, and a zone written as `I = 4` was not read. A dimension
  that is not a whole number (`I=***`) stops the read with an error.
- `b64_decode` stops when the array it decodes into is full: a longer code was
  decoded past the end of the array.
- `vtk_write_structured_multiblock` writes the cell values of a surface block
  (one plane of nodes in a direction, such as a face of a volume block) as one
  layer of cells there, as `tec_write_structured_multiblock` does, when the
  index ranges of its variables hold that layer; it wrote no cell value.
  `vtk_read_structured_multiblock` does not read these values back.
- `vtk_read_structured_multiblock` returns an error, instead of a crash or a
  runtime error, when the `.vtm` file or a block file cannot be opened or read,
  and closes each block file once read: a field of more blocks than the files a
  process can keep open was not read.
- `vtk_write_structured_multiblock` gives the `DataSet` elements of the `.vtm`
  file the indices 0 to nb-1: with every index 0, the reader of VTK 9.7.1 gave
  no block of a field of two or more blocks.
- `vtk_write_structured_multiblock` writes a mesh of one coordinate as two
  lines of nodes in j and two planes in k, which
  `vtk_read_structured_multiblock` reads back as such; it left the size of such
  a block undefined. A mesh of another number of coordinates than 1, 2 or 3 is
  refused (return value 1).
- `tec_write_structured_multiblock` writes surface blocks and lines of nodes,
  their cell variables with one layer of cells where a direction has one plane
  of nodes and their nodal variables with one value per node: values were
  copied past the end of buffers and of the variables.
- `tec_write_structured_multiblock` takes the variables where they are in each
  direction: the node plane of a two-dimensional field read from a VTK or a
  Tecplot file, `vars(:,:,:,0:0)`, was taken at k = 1, past the end of the
  variables. Variables that it read within their bounds are written as before.
- The VTK multi-block reader and writer take `.vtm` lines and block file paths
  of any length: lines were cut at 128 characters and paths at 256. A listed
  file longer than the name of a block gives the return value 1.
- `vtk_read_structured_multiblock` reads into an `orion_data` that already
  holds blocks; it stopped the program.
- The Tecplot writers and readers take block names and file paths of any
  length: zone headers were cut at 500 characters, lines read in 1000, the
  path of a `.szplt` file handed to TecIO in 256, and block names read from a
  `.szplt` file ended with a null character.
- The converter writes the `.vtm` file at the path given with `--out-file`,
  whatever its length; the file went to the first 32 characters of the path.
- `vtk_read_structured_multiblock` refuses, before reading any block, a field
  whose block files list their data arrays before their `Points` array, as VTK
  and ParaView write them (return value 1). On such files it returned -1 after
  reading the mesh of the first block, its arrays read under names shifted by
  one (an out-of-bounds error in a build with bound checks).
- The variable names of a VTK block file are read up to its appended data, and
  the location of its variables comes from its `PointData` and `CellData`
  elements: four bytes `Name` in raw appended data were taken for one more
  variable, and a variable whose name holds `CellData` set the cells.
- The names of the data arrays of a VTK block file are the `Name` attributes
  of their `DataArray` start tags: the base64 code of the values of an array
  (binary format) that holds the text `Name` was taken for one more variable,
  of zeros, with the return value 0. A tag split over lines, which the reader
  does not read, is refused (return value 1), with a message that gives the
  file, the line and the tag.
- The converter writes the variables of a Tecplot file at the nodes as point
  data of the VTK files; it wrote them as cell data.
- The converter ends with the exit status 1 when it cannot read its input or
  write its output; it ended with the exit status 0, as a success.
- The `.szplt` reader reads the values at the cells of a zone with one node
  plane or line one layer of cells in K at a time (one row in I when `KMax` =
  1): the TecIO reader of the repository gives some of them wrong, without an
  error, in one call (`KMax` = 1 with `IMax` = 2 and `JMax` >= 3; `IMax` = 1
  with `JMax` = 2 and `KMax` >= 3; `JMax` = 1 with `KMax` >= 3).
- The `.szplt` reader gives every zone the number of coordinates of the file:
  each zone took the number of its own shape, and the face j of a volume block
  that of the zone before it, or an undefined one when it was the first zone.
- The `.szplt` reader sets `orion%tec%node` from the locations of the zones
  (it never set it); it leaves the flag as it is for a file without solution
  variables, for which the ASCII reader sets it, and for a file whose zones
  differ in location.
- The `.szplt` reader returns a passive variable as zero, the value Tecplot
  gives it, and takes the location of a block from its other variables, or,
  when they are all passive (a zone of coordinates only), the location of the
  file, `orion%tec%node`: the passive variable held what the memory held, one
  declared at the nodes made the values at the cells read past the end of
  their buffer, and a zone of coordinates only got the bounds of the cells in a
  field at the nodes, which the writer then read below their lower bound.
- The `.szplt` reader refuses (return value 1, with a message) a file with
  fewer variables than the coordinates of its zones, as the ASCII reader
  refuses it (without a message); it returned 0 with a coordinate never set.
- The Tecplot ASCII reader reads the cell values of a zone with one node plane
  or line in I or J (it read none, without an error), gives every zone the
  number of coordinates of the file, and reads zones whose shape differs from
  that of the first one (refused with error 1 and no message).
- The Tecplot ASCII reader reads a zone header without `J` as one node line,
  an I-ordered zone, as in the examples of the Tecplot Data Format Guide; it
  refused it as a zone of invalid dimensions.
- The Tecplot ASCII reader, `tec_read_structured_multiblock`, gives each block
  the title of its zone as its name, as `tec_read_szplt` does; the names were
  undefined.
- Without TecIO, the Tecplot writer and reader stop a program that asks them
  for a binary file with error termination (`ERROR STOP`, a nonzero exit
  status): `STOP` gave the exit status 0, so the run passed for a success (the
  converter, asked for a `.szplt` file, wrote nothing).
- The `.szplt` reader stores the solution time and the StrandID of the last
  zone in `solutiontime` and `strandid` (-1 for a static zone), also with
  `dims_only`; it dropped them, so a solution read from a `.szplt` file came
  without its time.
- `vtk_read_structured_multiblock` reads a `FieldData` element written on
  one line, as XML allows: the time, the cycle and the named scalars of such
  a file were read as 0, and a line longer than 256 characters hid the names
  of the variables after it. The text of a data array on the line of its
  start tag is read in the case of the file: a base64 code there was read in
  upper case.
- `copyORION` copies every member of the source: the time (`solutiontime`),
  the strand (`strandid`) and every format option; it copied four of the
  options only, so the copy held the defaults of the others (`tec%node`,
  `tec%bc`, `tec%extension`, `vtk%node`), a time of 0 and the strand -1.
- The Tecplot ASCII reader and `p3d_read_multiblock` read into an
  `orion_data` that already holds blocks:
  the blocks of the file take their place, as in the `.szplt` and VTK
  readers; they stopped the program (an allocated variable allocated again).
- `tec_read_structured_multiblock` refuses, with a message and the return
  value 1, a file name whose extension is none of those it reads (`.dat`,
  `.tec`, `.szplt`), such as that of a `.plt` file, which the writer writes:
  the value it returned was undefined.
- The `.szplt` reader returns, with a message, the nonzero status of the
  first TecIO call that fails, and ends the read there: it returned the status
  of the call that closes the file, so a file that TecIO opens and then cannot
  read, such as a damaged one, came back with the return value 0.
- `tec_write_structured_multiblock` asked for a binary file whose name has
  neither the extension `.plt` nor `.szplt` returns 1, with its message, and
  writes no file: the value it returned was undefined.
- The binary writer returns, with a message, the nonzero status of the first
  TecIO call that fails, ends the write there and closes the file: it returned
  the status of the call that closes the file, so a write in which TecIO
  refused a call, such as a named scalar whose name TecIO does not take, came
  back with the return value 0.
- `tec_write_structured_multiblock` refuses, with a message and the return
  value 1, a `varnames` whose names are not as many as the variables written,
  before writing anything. TecIO takes the number of names as the number of
  values of each zone: with one name too few it wrote the values of a `.plt`
  file past the end of its buffers (a field of two blocks stopped the program
  on a corrupted heap) and refused those of a `.szplt` file; an ASCII file got
  a `VARIABLES` line that did not match its values. The names may be separated
  by blanks, tabs or commas in every format: a comma made TecIO split the
  whole list at its commas, coordinate names included.
- The `.szplt` reader refuses, with a message and the return value 1, a file
  with a zone that is not ordered, a finite-element zone, and closes it, before
  any zone is read: a `FEPOLYGON` or `FEPOLYHEDRON` zone stopped the program
  with a `STOP` statement, so with the exit status 0, as a success, the file
  left open; any other finite-element zone was read as an ordered zone, its
  nodes, elements and corners taken for I, J and K, with the return value 0.
- The Tecplot reader and the binary writer take the format from the extension
  of the file name, the text after the last dot of its last component: they
  took it from any part of the name, so `cases.data/f.szplt` and
  `a.dat.szplt` went to the ASCII reader, and the writer wrote a `.plt` file
  under the name `a.plt.szplt`.
- `tec_write_structured_multiblock` refuses, with a message and the return
  value 1, an `Nvars` out of the variables that `vars` holds (0 to
  `size(vars,1)`, 0 when block 1 has no `vars`), before writing anything: a
  larger `Nvars` read past the end of `vars` (the file got the values that
  followed it in memory), a negative one wrote past the end of an array of the
  writer in a binary file.
- `tec_write_structured_multiblock` refuses, with a message that names the
  block and the return value 1, a block whose `vars` holds fewer variables
  than those written (those of block 1, or `Nvars`), before writing anything:
  such a block was read past the end of its `vars`, and the file got the
  values that followed it in memory.
- `tec_write_structured_multiblock` refuses, with a message that names the
  block and the return value 1, a block whose `mesh` holds another number of
  coordinates than block 1, before writing anything: the writer took the
  number of coordinates from block 1, so a block with fewer was read past the
  end of its `mesh`, and the file got the values that followed it in memory;
  a block with more lost the others.
- `tec_write_structured_multiblock` with `Nvars = 0` gives the zone header of
  an ASCII file the coordinates alone in `VARLOCATION`: the variables at the
  cells got the range `[3-2]` (from the first variable after the coordinates
  to the last one written, which came before it), a form that the ranges of
  the Data Format Guide do not have.
- The `.szplt` reader gives its message, with the call and the file, also when
  TecIO cannot open the file (`tecFileReaderOpen`), as for the other calls: it
  returned the status with the messages of TecIO only.
- `vtk_read_structured_multiblock` reads the attributes of a block file and of
  the `.vtm` file as XML writes them, in double or in single quotes and with
  white space around the equal sign, and uses none that it has not read. It read
  them in double quotes and right after the equal sign only, and used them also
  when it had read none: an attribute in single quotes stopped a release build
  with a segmentation fault (a `Name`, a `format`, a `type`, the `byte_order`),
  or had values read from another place of the file (an `offset`). A block file
  without an attribute that the reader needs (the `Name`, `format`, `type` or
  raw `offset` of the points or of a variable, `Extent`, `WholeExtent`, the
  `byte_order` of a raw file) is refused with the return value -1, as is a raw
  one whose `byte_order` is neither `LittleEndian` nor `BigEndian` (the whole
  value, whatever the case of its letters; a value that held one of the two,
  such as `NotLittleEndian`, was read as that byte order); one in which such an
  attribute, or `NumberOfComponents`, has an empty value is refused too, with -1
  for the `Name` of an array and for the `format`, `type` and raw `offset` of
  the points, and 1 for the others. Field data whose `Name`, `format`, `type`,
  `NumberOfTuples` or raw `offset` is missing or empty are not read from that
  block file: the time, the cycle and the named scalars are those of the other
  block files, 0 when none of them holds them. A block file without
  `WholeExtent`, read before, is refused as the reader of VTK 9.7.1 refuses it.
  An array without `NumberOfComponents` has one component, as the reader of VTK
  9.7.1 reads it; a variable whose `NumberOfComponents` is not 1 is refused, as
  the field holds one value per variable (its first values were read, with the
  return value 0). A tag ends at its first `>` outside the values in quotes of
  its attributes, which may hold `>`: a start tag split over lines with such a
  value on its first line is refused as a split tag (return value 1, with its
  message; it was refused with -1, without the message), and an empty
  `FieldData` element with such a value is read as empty (the field was read
  without its variables, with the return value 0).

## [1.7.0] - 2026-09-29

### Added

- New CI workflows.
- Tecplot option `orion%tec%double` (logical, default `.true.`, copied by
  `copyORION`). It sets the precision of the values that
  `tec_write_structured_multiblock` stores in a binary Tecplot file (`.plt`,
  `.szplt`): `.true.` stores 64-bit values, `.false.` 32-bit values as before.
- Optional `zone_mask` and `dims_only` arguments to
  `tec_read_structured_multiblock` and `tec_read_szplt`. `dims_only` reads only
  the zone headers -- names, dimensions and variable count -- without
  allocating a mesh; `zone_mask` reads variable data only for the selected
  zones, coordinates always. Together they let an MPI caller size and partition
  a file before committing memory to it, instead of every rank materialising
  the whole domain. Honoured by the `.szplt` path only: the ASCII format has no
  per-zone index, so its reader ignores both and performs a full read.

### Changed

- Remove compiling and setvars options from install.sh. New scripts places in scripts/ folder
- Reduce the tasks performed by version_bumb script. 
- Binary Tecplot files are written with 64-bit values by default. The writer
  handed TecIO double-precision values but declared them single precision, so
  a `.plt` or `.szplt` file read back differed from the data in memory
  (relative deviations of about 5e-8). The files are 1.5 to 2 times larger.
  An ORION older than 9592709 reads 64-bit files wrongly: the double branch of
  `tec_read_szplt` copied an array it had not allocated. A program that reads
  these files needs 9592709 or later; a program that must keep 32-bit files
  sets `orion%tec%double = .false.` before the write.
- `p3d_read_multiblock` returns the `iostat` of a failed read in `err` instead
  of stopping the program (a file that is not PLOT3D stopped it), and refuses
  a block count below 1 (`err = 1`) and a node count below 1 (`err = 2`)
  before it reads any coordinate.
- Tecplot writers remove quotes to variables names.

### Fixed

- Long Tecplot `VARIABLES` headers are no longer cut. The writers built the
  header in 1000 characters and the ASCII reader read the header lines in 1000
  characters, so a list of more than about 100 names such as `"rho(12)"` was
  cut without a message and the file could not be read back. These buffers now
  hold 32768 characters. The `.szplt` reader no longer writes past the end of
  its join of the names. The ASCII reader still keeps at most 512 names and
  returns an error beyond.
- `tec_read_szplt` reads a one-plane slice of a three-dimensional field with
  three coordinates, by the rule of the ASCII reader. It read `z` as the first
  solution variable, and in a cell-centred file it read the cell values past
  their end.
- `tec_read_structured_multiblock` reads a one-plane slice of a
  three-dimensional field with three coordinates. Since the pure 2-D support,
  every zone with `K = 1` was read with two coordinates, so `z` became the
  first solution variable and every variable moved by one. The rule goes by
  name: when the first three variables are named `x`, `y` and `z` (in any
  case) and `z` is nodal, `mesh` holds `x`, `y` and `z`. A two-dimensional
  file whose third variable is nodal and named `z` is therefore read as a
  slice; rename that variable or write it cell-centred.
- `tec_read_szplt` fills `orion%varnames`, with the convention of the ASCII
  reader (coordinates first). It fetched every name from TecIO and dropped it,
  so `varnames` stayed unallocated or kept the names of an earlier read.
- `p3d_read_multiblock` tells a two-dimensional grid from a three-dimensional
  one with every compiler. The probe used `ndir` as both the implied-do
  variable and its bound: gfortran read a 3-D grid with two coordinates and
  `Nk = 0`, and ifx read every grid wrongly.
- `tec_read_structured_multiblock` reads a zone header without `K` as one node
  plane (`K = 1`), as Tecplot does; it refused such a zone as having `K = 0`.
- The `solutiontime` of a new `orion_data` is 0. It had no default, so a
  PLOT3D grid written back as Tecplot carried uninitialised memory in its
  `SOLUTIONTIME`.
- Writing a binary Tecplot file no longer overflows the stack on large blocks.
  `tec_dat` took its data through an explicit-shape dummy, but every caller
  passes a section with a fixed first index of a rank-4 array (`mesh(1,...)`,
  `vars(s,...)`), which is strided. The compiler therefore packed each section
  into a temporary at the call site, and ifort places array temporaries on the
  stack: past roughly 350k points per block the default 8 MB limit was gone and
  the writer died with `forrtl: severe (174): SIGSEGV`. The dummy is now
  assumed-shape, so the section is passed as a descriptor with no copy, and the
  contiguous buffer TecIO requires is built inside `tec_dat` on the heap.
  Verified on a block whose sections need a 10.4 MB temporary: it converts at
  the default 8 MB stack, where it previously died, and the result round-trips
  to within float32.
- The converter no longer truncates command-line arguments. `arg` was a fixed
  99-character buffer, so a longer `--in-file=`/`--out-file=` path lost its
  tail; when the extension went with it, no branch matched, and a conversion
  could read its whole input and report `Done!` having written nothing. The
  argument is now read into a deferred-length string sized per argument, and
  the file names it fills are deferred-length too.
- The converter validates its file arguments instead of failing silently or
  crashing: a missing `--in-file=`/`--out-file=`, or an extension outside
  `.dat .tec .szplt .p3d .vtm`, is now reported as an error. An unrecognised
  input extension previously fell through every read branch and the first
  `size()` query on the unallocated block segfaulted. `infile` and `outfile`
  are also initialised before use; the format tests in the argument loop read
  them while they could still be undefined.
- TecIO is now linked through the target matching the selected flavour
  (`tecio` or `teciompi`) instead of a hard-coded `tecio::tecio`, so MPI builds
  resolve.
- The app and test executables set `LINKER_LANGUAGE Fortran`. Linking C++ TecIO
  otherwise made CMake choose the CXX linker, which does not provide the entry
  point of an Intel Fortran main program.
- Restored `-lm -lstdc++` in `LINKLIBS` for serial TecIO builds; the Fortran
  linker needs them even though the CXX linker does not.

## [1.6.0] - 2026-09-08

### Added

- `split_tokens` helper for parsing unquoted variable names in Tecplot headers.
- Documentation site, now built with [Zensical](https://zensical.org).
- Citation and release metadata: `AUTHORS.md` and this changelog.

### Changed

- Improved variable extraction in the `read_variables` subroutine.
- `tecvarname` is now allocatable, so long Tecplot headers are no longer
  truncated.
- Increased character length limits and improved NODE handling.
- Removed the fixed upper bound on cell count in `Lib_Tecplot`.
- Raised the minimum CMake version to 3.20.
- Corrected the declared license in `setup.py` from MIT to GPL-3.0, matching
  `LICENSE`.

### Fixed

- `read_TEC` now correctly handles multi-block files through the Python
  interface.
- Removed stray diagnostic output from `read_TEC`.

## [1.5.5] - 2026-01-26

Earlier release changes not reported here; see the [commit history](https://github.com/MarcoGrossi92/ORION/commits/main) for details.
