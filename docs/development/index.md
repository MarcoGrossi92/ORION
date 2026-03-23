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
./install.sh build --compiler=gnu --use-tecio
./scripts/test.sh

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
│   │   ├── lib/            # Core libraries
│   │   ├── app/            # Applications (converter)
│   │   └── test/           # Test suite
│   └── python/             # Python interface
│       ├── ORION/          # Python package
│       └── test/           # Python tests
├── cmake/                  # CMake configuration
├── scripts/                # Build and utility scripts
├── docs/                   # Documentation source
├── lib/                    # External libraries (TecIO)
└── bin/                    # Compiled executables
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

Follow conventional commit format:

```
type(scope): Brief description

Detailed explanation if needed.

Fixes #123
```

**Types:** `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

**Examples:**
```
feat(tecplot): Add support for binary SZPLT format
fix(vtk): Resolve crash with unstructured meshes
docs(api): Add examples for multi-block processing
test(plot3d): Add test cases for binary files
```

## Testing

### Running Tests

```bash
# Fortran tests
./scripts/test.sh

# Specific test
./bin/test/test_tecplot_read

# Python tests
cd src/python
pytest test/
```

### Writing Tests

Every new feature or bug fix should include tests:

**Fortran test:**
```fortran
program test_new_feature
  use Lib_ORION_data
  use Lib_NewFormat
  
  type(orion_data) :: test_data
  integer :: error_code
  
  ! Test case 1: Basic functionality
  error_code = new_format_read('test_file.dat', test_data)
  if (error_code /= 0) then
    print *, 'FAIL: Could not read file'
    stop 1
  endif
  
  ! Test case 2: Validate data
  if (size(test_data%block) /= 1) then
    print *, 'FAIL: Expected 1 block'
    stop 1
  endif
  
  print *, 'PASS: All tests passed'
end program test_new_feature
```

**Python test:**
```python
import pytest
from ORION import read_TEC

def test_read_tecplot_basic():
    """Test basic Tecplot file reading."""
    x, y, z, var, varnames = read_TEC('test_data.dat')
    
    assert x.shape[0] == 1  # Single block
    assert len(varnames) == 3  # Three variables
    assert varnames[0] == 'pressure'

def test_read_tecplot_missing_file():
    """Test error handling for missing file."""
    with pytest.raises(FileNotFoundError):
        read_TEC('nonexistent.dat')
```

## Documentation

**Building Documentation**

```bash
# Install dependencies
pip install mkdocs-material mkdocs-git-revision-date-localized-plugin

# Serve locally
mkdocs serve

# Build static site
mkdocs build
```

---

**Thank you for contributing to ORION!** Your efforts help make scientific computing more accessible and efficient for everyone. 🚀
