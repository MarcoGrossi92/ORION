# Contributing to ORION

Thank you for your interest in contributing to ORION! This document provides guidelines and instructions for contributing to the project.

## Ways to Contribute

There are many ways to contribute to ORION:

- 🐛 **Report bugs** - Help us identify and fix issues
- 💡 **Suggest features** - Share ideas for improvements
- 📝 **Improve documentation** - Fix typos, clarify explanations, add examples
- 🔧 **Submit code** - Fix bugs or implement new features
- 🧪 **Add tests** - Improve test coverage
- 🎨 **Share examples** - Contribute usage examples

## Getting Started

### 1. Fork the Repository

Click the "Fork" button on [GitHub](https://github.com/MarcoGrossi92/ORION) to create your own copy.

### 2. Clone Your Fork

```bash
git clone https://github.com/YOUR_USERNAME/ORION.git
cd ORION
```

### 3. Set Up Development Environment

```bash
# Add upstream remote
git remote add upstream https://github.com/MarcoGrossi92/ORION.git

# Install development dependencies
./install.sh build --compiler=gnu --use-tecio

# Run tests to verify setup
./scripts/test.sh
```

### 4. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-bug-fix
```

Use descriptive branch names:
- `feature/add-hdf5-support`
- `fix/tecplot-binary-crash`
- `docs/improve-installation`

## Development Workflow

### Making Changes

1. **Write clear, focused commits:**
   ```bash
   git add file1.f90 file2.f90
   git commit -m "Add support for HDF5 format reading
   
   - Implement HDF5 reader module
   - Add tests for HDF5 functionality
   - Update documentation"
   ```

2. **Follow coding standards:**
   - Fortran: Free-form format, meaningful variable names, comments for complex logic
   - Python: PEP 8 style guide
   - Use 2 spaces for indentation in Fortran, 4 in Python

3. **Add tests for new features:**
   ```fortran
   ! In src/fortran/test/
   program test_new_feature
     use Lib_ORION_data
     use Lib_NewFormat
     
     ! Test code here
     
   end program test_new_feature
   ```

4. **Update documentation:**
   - Add docstrings to new functions
   - Update relevant .md files in `docs/`
   - Add examples if appropriate

### Testing Your Changes

Run the test suite before submitting:

```bash
# Run all tests
./scripts/test.sh

# Run specific test
./bin/test/test_tecplot_read
```

For Python changes:

```bash
cd src/python
python -m pytest test/
```

### Keeping Your Fork Updated

```bash
# Fetch upstream changes
git fetch upstream

# Merge into your branch
git checkout main
git merge upstream/main

# Update your fork
git push origin main
```

## Submitting Changes

### 1. Push Your Branch

```bash
git push origin feature/your-feature-name
```

### 2. Create a Pull Request

1. Go to your fork on GitHub
2. Click "Pull Request"
3. Select your branch
4. Fill out the PR template

### 3. Pull Request Guidelines

A good pull request includes:

- **Clear title:** "Add HDF5 format support" not "Update files"
- **Description:** What changes were made and why
- **References:** Link to related issues (`Fixes #123`)
- **Tests:** Proof that changes work
- **Documentation:** Updates to relevant docs

**PR Template:**

```markdown
## Description
Brief description of changes

## Motivation
Why is this change needed?

## Changes Made
- Added X feature
- Fixed Y bug
- Updated Z documentation

## Testing
How were these changes tested?

## Checklist
- [ ] Tests pass
- [ ] Documentation updated
- [ ] Code follows style guidelines
- [ ] Commit messages are clear
```

### 4. Code Review Process

1. Maintainers will review your PR
2. Address any requested changes
3. Push updates to the same branch
4. Once approved, your PR will be merged!

## Coding Standards

### Fortran Code

**Naming Conventions:**

```fortran
! Modules: Lib_ModuleName
module Lib_MyModule

! Types: Descriptive names
type :: ORION_data
  
! Variables: Descriptive lowercase with underscores
integer :: num_blocks
real :: grid_spacing

! Constants: UPPERCASE
real, parameter :: PI = 3.14159265359
```

**Error Handling:**

```fortran
! Always return error codes
integer function my_function(...)
  
  if (error_condition) then
    my_function = 1  ! Non-zero indicates error
    return
  endif
  
  my_function = 0  ! Success
end function my_function
```

### Python Code

Follow [PEP 8](https://peps.python.org/pep-0008/):

```python
"""Module-level docstring."""

import numpy as np
from typing import Tuple, List


def read_data(filename: str) -> Tuple[np.ndarray, np.ndarray]:
    """
    Read data from file.
    
    Args:
        filename: Path to input file
        
    Returns:
        Tuple of (coordinates, variables)
        
    Raises:
        FileNotFoundError: If file doesn't exist
        ValueError: If file format is invalid
    """
    # Implementation
    pass


# Use type hints
def process_array(data: np.ndarray, scale: float = 1.0) -> np.ndarray:
    """Process array with scaling factor."""
    return data * scale
```

## Reporting Issues

### Bug Reports

When reporting bugs, include:

1. **ORION version:**
   ```bash
   ORION --version
   ```

2. **System information:**
   - OS and version
   - Compiler and version
   - Build configuration

3. **Steps to reproduce:**
   ```bash
   # Exact commands that trigger the bug
   ORION --input-format tecplot --input-file test.dat ...
   ```

4. **Expected vs actual behavior:**
   - What should happen
   - What actually happens
   - Error messages or output

5. **Sample data** (if possible):
   - Minimal example that reproduces the issue

### Feature Requests

For feature requests, describe:

1. **Use case:** Why is this feature needed?
2. **Proposed solution:** How should it work?
3. **Alternatives:** What alternatives exist?
4. **Impact:** Who would benefit?

## Recognition

Significant contributions may earn you:

- Commit access
- Listed as project contributor

We appreciate your contributions! 🎉