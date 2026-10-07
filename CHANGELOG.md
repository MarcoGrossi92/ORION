# Changelog

All notable changes to ORION are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Changes on `main` since `v1.6.0`.

### Added

- `orion_data%strandid` (integer, default -1): the Tecplot ASCII reader stores
  the `STRANDID` of the last zone header, -1 when the header has none.
  `tec_write_structured_multiblock` marks a steady solution (a negative
  `solutiontime`) by writing `SOLUTIONTIME=|t|` and `STRANDID = 0` in each zone
  header; `solutiontime` is read back as `|t|`, as before, and `strandid == 0`
  now tells the reader that the file holds a steady solution. Nothing else
  changes: the writer, the files and every value read are as before, and code
  that does not use the new component behaves as before. Known limitation:
  binary Tecplot files (`.plt`, `.szplt`) are not covered -- the binary writer
  stores StrandID 0 for every zone and the `.szplt` reader does not store the
  zone time -- and need a separate change.
- Optional `cycle`, `fldnames` and `fldvalues` arguments to
  `vtk_write_structured_multiblock`, written as the field data `CYCLE` (32-bit
  integer) and `fldnames(i)` (64-bit real) of every `.vts` file, next to
  `TIME`; and the same arguments, with `fldfound`, to
  `vtk_read_structured_multiblock`, which returns them: 0 (and `fldfound`
  false) when the files hold none. A program can store with a solution the
  counters it needs to continue from it. Without the new arguments the files
  written and the values read are those of before. Names and values of
  different sizes are refused (return value 1).

### Fixed

- `vtk_read_structured_multiblock` reads back the files that
  `vtk_write_structured_multiblock` writes with a time. The writer stores the
  time as `TIME` field data in every `.vts` file; the reader took that name for
  the first variable name, read `Points` as a cell variable and stopped with a
  segmentation fault, in the ascii, binary and raw formats. Field data are no
  longer taken for variables, and an empty `<FieldData/>` element does not hide
  the variables after it. The optional `time` argument was never set, because
  the reader called the field-data writer on the file it was reading: it now
  returns the `TIME` of the files, or 0 when they hold none.
- `vtk_read_structured_multiblock` reads a three-dimensional block whose z
  coordinates add up to 0, such as a slab or a wedge around z = 0, as
  three-dimensional. It took every block whose z summed to 0 for a
  two-dimensional one, and so kept only the x and y of one node plane: the
  z coordinates were lost. A block is now two-dimensional when every z is 0,
  the form in which `vtk_write_structured_multiblock` writes a two-dimensional
  mesh. Whether a floating-point sum of such z comes out as exactly 0 depended
  on the values and on the order of the additions.
- The Python reader `read_TEC` reads the Tecplot ASCII files that the Fortran
  writer of ORION 1.7.0 and later writes with the variable names without quotes
  (`VARIABLES = x y z rho(1)`). It found no names in them, and so returned the
  coordinates without any variable. The names are now taken from the
  `VARIABLES` list only, up to the first `ZONE` record or line of numbers, so
  quoted text after the list, such as a zone title written as `T = "Block1"`,
  is no longer taken for a variable name. A list in quotes is read as before.
- `vtk_write_structured_multiblock` and `vtk_read_structured_multiblock` write
  and read a multi-block field of any number of blocks. The writer kept the
  file index of each block in an array of 99 and the reader the block names
  listed in the `.vtm` file in an array of 16, and both went past their array
  for a field of more blocks: memory overwritten without a message or a
  segmentation fault, depending on the number of blocks. The reader now also
  closes the `.vtm` file after reading it.
- The Python reader `read_TEC` takes the dimensions of a zone from its `I=`,
  `J=` and `K=` keywords, outside text in quotes. It took the first three
  numbers of the line that holds `I=`, so a number in the zone title
  (`T = "Block 1"`, or `T = B1-of-2` as a block splitter writes it) became a
  dimension and the file was read wrong without an error; a zone written with
  blanks around the equal signs (`I = 4`) was not read at all. A dimension of
  a `ZONE` record that is not a whole number, such as the `I=***` that a
  Fortran writer leaves when the number does not fit its field, now stops the
  read with an error that names the file, the zone and the keyword; the zone
  was read with a size made of other numbers of its line.
- `b64_decode` stops when the array it decodes into is full. A code longer
  than the array was decoded past the end of the array, over the memory that
  follows it: without a message in a release build, with an out-of-bounds
  error in a build with bound checks. The data array of a binary `.vts` file
  that holds more values than `vtk_read_structured_multiblock` counts in its
  piece is such a code. Codes that fit the array decode as before.
- `vtk_write_structured_multiblock` writes the cell values of a surface block,
  a block with one plane of nodes in one direction (`Ni`, `Nj` or `Nk` = 0)
  such as a face of a volume block, as one layer of cells in that direction,
  as `tec_write_structured_multiblock` writes it. It counted `Ni*Nj*Nk` = 0
  cells, so the file had the extent and the points of the surface but no cell
  value. Blocks with every dimension at least 1, and surface blocks whose
  variables do not hold that layer, are written as before.
  `vtk_read_structured_multiblock` counts the cells of a block from its extent
  and does not read these values back.
- `vtk_read_structured_multiblock` reads a block with one plane of nodes in k
  whose nodes all have z = 0, such as the face of a three-dimensional block on
  the plane z = 0, as three-dimensional. It took such a block for a
  two-dimensional one: it dropped z and gave the variables one layer of cells
  in k, which it filled from past the end of the values it had read (none, as
  it counts the cells of a block from its extent), so it returned wrong values
  or crashed. A block is now two-dimensional only when it has two planes of
  nodes in k and every z is 0, the form in which
  `vtk_write_structured_multiblock` writes a two-dimensional mesh.
- `vtk_read_structured_multiblock` returns an error when a block file cannot
  be opened, such as a file listed in the `.vtm` that does not exist or a
  wrong `vtspath`, or when its points cannot be read. It went on with a file
  unit it had not opened and with coordinates it had not read, and stopped
  with a segmentation fault or a runtime error.
- `vtk_read_structured_multiblock` closes each block file once it has read
  it, and the file of a block it stops on: it kept every block file but the
  last open until the program ended, so a field of more blocks than the files
  a process can keep open could not be read. It also returns an error, and
  stops, when the values of a variable cannot be read, as in a block file cut
  short, instead of copying the values it has not read, and when the `.vtm`
  file cannot be opened, ends before the end of its block list or lists no
  block file, instead of stopping the program. The closing of the last file
  no longer replaces the error code of the read.
- `vtk_write_structured_multiblock` and `vtk_read_structured_multiblock`
  refuse variables at the nodes and return 1: the writer when
  `orion%vtk%node` is set and the blocks have variables, without writing
  any file, the reader for a file whose variables are point data. The writer
  wrote `Ni*Nj*Nk` values, the number of cells, as point data, and the reader
  returned the numbers of nodes as `Ni`, `Nj`, `Nk`, unlike the Tecplot
  reader and writer, so a field with variables at the nodes did not come
  back as it was written. Fields with cell variables, and meshes without
  variables, are written and read as before.
- `vtk_write_structured_multiblock` lists the block files in the `.vtm` file
  with a defined path when `vtspath` and `vtmpath` share no prefix, as with
  `vtspath = ''`: `simplified_relative_path` used a variable it had not set,
  so the entries depended on what the memory held, and a build with bound
  checks could stop on it. An absolute `vtspath` that shares no directory
  with `vtmpath` is listed as it is. Paths with a common prefix are listed as
  before.
- `vtk_write_structured_multiblock` gives the `DataSet` elements of the `.vtm`
  file the indices 0 to nb-1, in the order of the blocks. It listed every
  block with `index="0"`, and a reader places each `DataSet` at its index:
  the reader of VTK 9.7.1 (`vtkXMLMultiBlockDataReader`) gave no block of a
  field of two or more blocks. The `.vts` files, and the `.vtm` files of
  fields of one block, are written as before; `vtk_read_structured_multiblock`
  reads the files of a `.vtm` in the order they are listed, with any index,
  as before.

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
