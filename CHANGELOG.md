# Changelog

All notable changes to ORION are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Each tagged release is archived on Zenodo and receives its own DOI. See
[`CITATION.cff`](CITATION.cff) for how to cite a specific version.

## [Unreleased]

Changes on `main` since `v1.6.0`.

### Added

- Optional `zone_mask` and `dims_only` arguments to
  `tec_read_structured_multiblock` and `tec_read_szplt`. `dims_only` reads only
  the zone headers -- names, dimensions and variable count -- without
  allocating a mesh; `zone_mask` reads variable data only for the selected
  zones, coordinates always. Together they let an MPI caller size and partition
  a file before committing memory to it, instead of every rank materialising
  the whole domain. Honoured by the `.szplt` path only: the ASCII format has no
  per-zone index, so its reader ignores both and performs a full read.

### Fixed

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
