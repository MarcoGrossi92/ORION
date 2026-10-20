# Development

Welcome to the ORION development documentation! This section provides resources for contributors and developers working on ORION.

## Quick Navigation

<div class="grid cards" markdown>

-   :material-account-group:{ .lg .middle } __Contributing__

    ---

    Learn how to contribute to ORION

    [:octicons-arrow-right-24: Contribution guide](contributing.md)

-   :material-tag-multiple:{ .lg .middle } __Versioning__

    ---

    Understand ORION's versioning policy

    [:octicons-arrow-right-24: Version policy](versioning.md)

</div>

## Overview

ORION is an open-source project that welcomes contributions from the community. Whether you're fixing bugs, adding features, improving documentation, or helping with testing, your contributions are valuable.

## For Contributors

### Getting Started

New to contributing? Start here:

1. **[Read the Contributing Guide](contributing.md)** - Understand the contribution process
2. **Fork and Clone** - Get your own copy of the repository
3. **Set Up Development Environment** - Build ORION locally
4. **Make Your Changes** - Fix bugs or add features
5. **Submit a Pull Request** - Share your work

### Quick Start for Contributors

```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/YOUR_USERNAME/ORION.git
cd ORION

# Add upstream remote
git remote add upstream https://github.com/MarcoGrossi92/ORION.git

# Create a feature branch
git checkout -b feature/my-new-feature

# Build and test
./install.sh build --compilers=gnu --use-tecio
ctest --test-dir build --output-on-failure

# Make changes, commit, and push
git add .
git commit -m "Add my new feature"
git push origin feature/my-new-feature

# Open a Pull Request on GitHub
```

## Project Structure

Understanding ORION's organization:

```
ORION/
├── src/                    # Source code
│   ├── fortran/            # Fortran implementation
│   │   ├── lib/            # Core libraries (Lib_Tecplot, Lib_VTK, Lib_PLOT3D, Lib_ORION_Data, ...)
│   │   ├── app/            # Applications (the converter)
│   │   └── test/           # Test programs (one executable and one ctest test per .f90 file)
│   └── python/             # Python interface
│       ├── ORION/          # Python package
│       └── test/           # Python tests (scripts that ctest runs)
├── cmake/                  # CMake configuration
├── scripts/                # Utility scripts (setvars.sh, compile.sh, version_bump.sh, version.txt)
├── docs/                   # Documentation source (built by zensical.toml)
├── lib/                    # External libraries (TecIO)
├── .github/workflows/      # Continuous integration (tests) and documentation deployment
├── install.sh              # Build script
├── setup.py                # Python package
├── CHANGELOG.md            # Changes of each release, and the unreleased ones
├── build/                  # Build directory (created by install.sh, not versioned)
└── bin/                    # Compiled executables: bin/app/converter, bin/test/ (not versioned)
```

## Development Workflow

### Standard Workflow

1. **Create Issue** - Describe the bug or feature
2. **Create Branch** - Work in a feature/fix branch
3. **Write Code** - Follow coding standards
4. **Add Tests** - Ensure code works correctly
5. **Update Docs** - Document new features
6. **Submit PR** - Request code review
7. **Address Feedback** - Iterate based on reviews
8. **Merge** - Maintainers merge approved PRs

### Branch Naming

Use descriptive branch names:

- `feature/add-hdf5-support` - New features
- `fix/memory-leak-tecplot` - Bug fixes
- `docs/improve-api-examples` - Documentation
- `refactor/simplify-io-logic` - Code refactoring

### Commit Messages

Start the subject with the part of ORION that changes, then say what changes; the body says why:

```
area: What changes, in one line

Why the change is needed, and what it changes for a user.

Fixes #123
```

**Examples:**
```
Tecplot writer: refuse a block with fewer variables than those written
tec_read_szplt: return the status of the TecIO call that fails
vtk_read_structured_multiblock: field data on one line
Guides: why 17 significant digits are the fewest
```

Record every change that a user can see in `CHANGELOG.md`, under `[Unreleased]` (see [Versioning](versioning.md)).

## Testing

### Running Tests

`install.sh build` compiles the tests with the library. Run them with `ctest`:

```bash
# All tests: the Fortran ones (ORION.<name>) and, when CMake finds Python 3 with NumPy, the Python ones
ctest --test-dir build --output-on-failure

# A test by name
ctest --test-dir build -R tecplot_read
```

The test programs are in `bin/test/`. Their working directory is `build/test-runtime`, where tests that read the files of
another one find them (`ctest` runs them in the right order). The tests of the binary Tecplot formats are built only
with TecIO (`--use-tecio`). GitHub Actions runs the tests, built with `gfortran` and TecIO on Ubuntu, on every push and
every pull request to `main`.

### Writing Tests

Every new feature or bug fix should include tests.

**Fortran test:** put a program in `src/fortran/test/`. CMake compiles every `.f90` file of that folder into an executable
of the same name in `bin/test/` and registers it as the ctest test `ORION.<name>`; the test fails when the program stops
with a nonzero code. The existing tests follow this pattern:

```fortran
program tecplot_new_feature
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer :: nfail

  nfail = 0
  call round_trip()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_new_feature: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_new_feature: all checks passed'

contains

  subroutine check(ok, what)
    logical,          intent(in) :: ok
    character(len=*), intent(in) :: what
    if (ok) then
      write(*,'(A)') '  ok    '//what
    else
      write(*,'(A)') '  FAIL  '//what
      nfail = nfail + 1
    endif
  end subroutine check

  subroutine round_trip()
    type(orion_data) :: w, r
    integer :: err
    ! ... fill w, write it with tec_write_structured_multiblock, read it back into r
    err = 0
    call check(err == 0, 'written and read without error')
    ! ... compare r with w
  end subroutine round_trip

end program tecplot_new_feature
```

**Python test:** a script in `src/python/test/` that ends with a nonzero exit status when a check fails (an unhandled
exception does), registered with `add_test` in `src/fortran/test/CMakeLists.txt` next to `ORION.python_read_p3d`:

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('tecfile.tec')   # written by the Fortran test tecplot_write

assert len(x) == 2, 'two blocks'                    # x, y, z and var have one entry per block
assert varnames == ['x', 'y', 'z', 'variable1']
# the nodes of a block, and the cells of its variable: one less in each direction
assert all(n == c + 1 for n, c in zip(x[0].shape, var[0][0].shape))
```

## Documentation

**Building Documentation**

```bash
# Install dependencies
pip install -r docs/requirements.txt

# Serve locally with live reload
zensical serve

# Build static site into site/, as the documentation workflow does (broken links are errors)
zensical build --strict
```

Check the commands and the code of the pages you change against the code: the pages are not tested, and the converter
options, the file formats and the function names change.

---

**Thank you for contributing to ORION!** Your efforts help make scientific computing more accessible and efficient for everyone. 🚀
