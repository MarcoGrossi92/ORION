# Versioning

ORION follows [Semantic Versioning](https://semver.org/) (SemVer) to manage version numbers and releases.

## Semantic Versioning

### Version Format

ORION version numbers use the format:

```
MAJOR.MINOR.PATCH
```

For example: `1.2.3`

### Version Components

Each component has a specific meaning:

| Component | Meaning | Increment When |
|-----------|---------|----------------|
| **MAJOR** | Breaking changes | API is incompatible with previous version |
| **MINOR** | New features | Functionality added in backward-compatible manner |
| **PATCH** | Bug fixes | Backward-compatible bug fixes |

### Examples

```
1.0.0  →  Initial stable release
1.0.1  →  Bug fix (backward compatible)
1.1.0  →  New feature (backward compatible)
2.0.0  →  Breaking change (incompatible API)
```

## What Triggers Version Bumps

### MAJOR Version (X.0.0)

Increment when making **incompatible** API changes:

**Examples:**
- Changing function signatures
  ```fortran
  ! v1.x: Old signature
  integer function read_file(filename)
  
  ! v2.x: New signature (incompatible)
  integer function read_file(filename, options)
  ```

- Removing public functions or modules
  ```fortran
  ! v1.x: Module exists
  use Lib_OldModule  
  
  ! v2.x: Module removed (breaking)
  ! Lib_OldModule no longer available
  ```

- Changing data structure layouts
  ```fortran
  ! v1.x: Old structure
  type :: ORION_data
    integer :: nblocks
  end type
  
  ! v2.x: Structure changed (incompatible)
  type :: ORION_data
    integer :: num_blocks  ! Renamed field
    logical :: is_structured  ! New required field
  end type
  ```

- Changing default behavior in incompatible ways
- Requiring new dependencies

**When to do it:**
- Necessary for major improvements
- Cleaning up deprecated features
- Fundamental architecture changes

### MINOR Version (x.Y.0)

Increment when adding **backward-compatible** functionality:

**Examples:**
- Adding new file format support
  ```fortran
  ! v1.0: Support Tecplot, VTK
  ! v1.1: Added HDF5 support (new feature)
  use Lib_HDF5  ! New module, old code still works
  ```

- Adding new optional parameters
  ```fortran
  ! v1.0: Basic function
  integer function read_file(filename)
  
  ! v1.1: Added optional parameter (backward compatible)
  integer function read_file(filename, verbose)
    character(*) :: filename
    logical, optional :: verbose  ! New, but optional
  ```

- Adding new utility functions
  ```fortran
  ! v1.1: New convenience function
  integer function quick_convert(input, output)
    ! Old functions still work, this is additional
  end function
  ```

- Adding new command-line options
  ```bash
  # v1.0: Basic converter
  ORION --in-file=data.tec --out-file=vtk/data.vtm
  
  # v1.1: A new option, here an imaginary --compress (old commands still work)
  ORION --in-file=data.tec --out-file=vtk/data.vtm --compress
  ```

**When to do it:**
- Adding new capabilities
- Enhancing existing features
- New optional parameters
- New modules or packages

### PATCH Version (x.y.Z)

Increment when making **backward-compatible** bug fixes:

**Examples:**
- Fixing crashes or errors
  ```fortran
  ! v1.0.0: Had a bug causing crash
  if (allocated(array)) deallocate(array)  ! Bug: missing check
  
  ! v1.0.1: Fixed crash
  if (allocated(array)) then
    deallocate(array)
  endif
  ```

- Correcting calculation errors
  ```fortran
  ! v1.0.0: Wrong formula
  pressure = density * velocity  ! Bug: missing factor
  
  ! v1.0.1: Correct formula
  pressure = density * velocity**2 / 2
  ```

- Fixing memory leaks
- Improving error messages
- Documentation corrections
- Performance improvements (without API changes)

**When to do it:**
- Bug fixes
- Security patches
- Documentation updates
- Minor performance tweaks

## How ORION Releases

The version of ORION is written once, in `scripts/version.txt`; `setup.py` reads it, so it is also the version of the
Python package. `CHANGELOG.md` lists the changes of each release, in the format of
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/): the changes that a user can see go under `[Unreleased]`, in
*Added*, *Changed*, *Removed* and *Fixed*, as they are made, and become a section of the new version at the release.

To release:

1. Decide the kind of the release from the changes of `[Unreleased]`, by the rules above: *Removed* (a public function,
   for instance) and incompatible *Changed* entries make it MAJOR, *Added* MINOR, *Fixed* alone PATCH.
2. Bump the version with the script, which changes `scripts/version.txt` only:
   ```bash
   ./scripts/version_bump.sh --major   # or --minor, or --patch
   ```
   It creates no commit and no tag, and pushes nothing; it prints the git commands to run next.
3. Turn `[Unreleased]` of `CHANGELOG.md` into the section of the new version, with its date, and add a new empty
   `[Unreleased]`.
4. Commit, then tag the release `vMAJOR.MINOR.PATCH` (`v1.7.0`, for instance) and push the commit and the tag, as the
   script's output says.

## References

- [Semantic Versioning Specification](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/en/1.1.0/)