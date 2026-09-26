# Changelog

All notable changes to ORION are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Each tagged release is archived on Zenodo and receives its own DOI. See
[`CITATION.cff`](CITATION.cff) for how to cite a specific version.

## [Unreleased]

Changes on `main` since `v1.6.0`.

### Added

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

### Fixed

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
- Citation and release metadata: `CITATION.cff`, `.zenodo.json`, `AUTHORS.md`
  and this changelog.

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

Last tagged release before Zenodo archiving. Earlier release changes not reported here; see 
the [commit history](https://github.com/MarcoGrossi92/ORION/commits/main) for
details.
