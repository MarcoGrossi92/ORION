# ORION - Ordinary Repository for Input-Output Necessities

![License](https://img.shields.io/github/license/MarcoGrossi92/ORION)
![Fortran](https://img.shields.io/badge/Fortran-90%2B-blue)
![Python](https://img.shields.io/badge/Python-3.6%2B-yellow)
![Platform](https://img.shields.io/badge/platform-Linux%20%7C%20macOS-lightgrey)

ORION is a modular I/O toolkit for reading and writing structured, multi-block scientific data across multiple file formats.
It provides Fortran and Python interfaces designed for seamless integration into high-performance and scientific computing workflows.
In addition to its programmatic APIs, ORION includes a standalone utility for converting files between supported formats.

## Features

- **Language APIs**:
  - Native Fortran API for high-performance applications
  - Python API for scripting and post-processing workflows
- **Format Conversion**:
  - Standalone command-line tool to convert files between supported formats

## Table of Contents

- [Requirements](#requirements)
- [Platform Support](#platform-support)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Versioning](#versioning)
- [Acknowledgements](#acknowledgements)

## Requirements

### System Requirements

- **CMake** >= 3.13
- **Fortran compiler**: Intel, GNU, or compatible
- **C++ compiler**: For TecIO support (optional)
- **Python** >= 3.6 (for Python interface)

### Supported File Formats

- **Tecplot**: 
  - Fortran API: Full support. TecIO library for PLT and SZPLT binary formats support.
  - Python API: ASCII support
- **VTK (Visualization Toolkit)**: 
  - Fortran API: Structured and unstructured mesh formats
  - Python API: Not available
- **PLOT3D**: 
  - Fortran API: NASA's structured grid format
  - Python API: Not available

## Platform Support

ORION has been successfully tested and verified to work on the following platforms:

| Operating System | Fortran Compiler | Status |
|------------------|------------------|--------|
| macOS (Apple Silicon) | GNU | ✅ Tested |
| Ubuntu (Linux) | GNU | ✅ Tested |
| Ubuntu (Linux) | Intel | ✅ Tested |
| OpenSUSE (Linux) | GNU | ✅ Tested |
| OpenSUSE (Linux) | Intel | ✅ Tested |

The project is designed to be portable and should work on other Unix-like systems (Linux, BSD, etc.) with compatible Fortran and C++ compilers. Windows users can use WSL2 (Windows Subsystem for Linux) or MSYS2/MinGW environments.

## Installation

The installation process provides:
- A Fortran static library linkable from external projects
- A converter command-line tool
- The Python module (optional)

### Quick Start

Clone the repository and run the installation script:

```bash
git clone https://github.com/MarcoGrossi92/ORION.git
cd ORION
./install.sh build --compiler=gnu --use-tecio
```

### Build Options

The `install.sh` script provides several build options:

```bash
./install.sh [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]
```

**Global Options:**
- `-v, --verbose`: Enable verbose output

**Commands:**

- **build**: Perform a full build
  - `--compiler=<name>`: Set compiler suite (intel, gnu)
  - `--use-tecio`: Enable TecIO support

- **compile**: Compile using CMakePresets once the project has been already built

- **setvars**: Set ORION environment variables to freely call the converter from a shell

### Python Package Installation

Install ORION via pip for Python support:

```bash
pip install ORION
```

This installs the Python interface with support for reading Tecplot ASCII files.

### Testing

Run the test suite after full build:

```bash
# Run Fortran tests
./scripts/test.sh
```

## Usage

### Command-line Converter Usage

The project includes a command-line utility for conversion between Tecplot, VTK, and PLOT3D formats.

```bash
# Convert Tecplot to VTK
ORION --input-format tecplot --input-file data.dat \
      --output-format vtk --output-file output.vtk

# Convert VTK to PLOT3D
ORION --input-format vtk --input-file mesh.vtk \
      --output-format plot3d --output-file grid.g
```

### API Usage

For complete working examples, see the `src/fortran/test/` directory.

#### Tecplot ASCII Format

**Reading (Fortran)**
```fortran
program read_tecplot
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Read Tecplot ASCII file
  E_IO = tec_read_structured_multiblock(filename='data.dat', orion=IOfield)
  if (E_IO == 0) then
    print *, 'Successfully read file with variables:', IOfield%varnames
  endif
end program read_tecplot
```

**Writing (Fortran)**
```fortran
program write_tecplot
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Prepare data structure
  ! IOfield%mesh populated with coordinates
  ! IOfield%var populated with solution data
  
  ! Write Tecplot ASCII file
  E_IO = tec_write_structured_multiblock(orion=IOfield, &
                                        varnames='velocity,pressure', &
                                        filename='output.dat')
end program write_tecplot
```

**Reading (Python)**
```python
from ORION import read_TEC

# Read Tecplot ASCII file
x, y, z, var, varnames = read_TEC('data.dat')
print(f"Variables: {varnames}")
print(f"Mesh shape: {x.shape}")
```

#### VTK Format (Fortran only)

**Reading**
```fortran
program read_vtk
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Read VTK structured/unstructured mesh
  E_IO = vtk_read_file(filename='mesh.vtk', orion=IOfield)
  if (E_IO == 0) then
    print *, 'Successfully read VTK file'
    print *, 'Number of points:', size(IOfield%mesh)
  endif
end program read_vtk
```

**Writing**
```fortran
program write_vtk
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Prepare data in IOfield
  ! Set mesh coordinates and solution variables
  
  ! Write VTK file
  E_IO = vtk_write_structured_multiblock(orion=IOfield, &
                                        varnames='velocity,pressure,temperature', &
                                        filename='output.vtk')
  if (E_IO /= 0) then
    print *, 'Error writing VTK file'
  endif
end program write_vtk
```

#### PLOT3D Format (Fortran only)

**Reading**
```fortran
program read_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Read PLOT3D grid and solution files
  E_IO = plot3d_read(grid_file='grid.g', &
                     solution_file='solution.q', &
                     orion=IOfield)
  if (E_IO == 0) then
    print *, 'Successfully read PLOT3D files'
  endif
end program read_plot3d
```

**Writing**
```fortran
program write_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  type(ORION_data) :: IOfield
  integer :: E_IO

  ! Prepare data in IOfield
  ! Set mesh coordinates in structured grid format
  ! Set solution variables (density, momentum, energy, etc.)
  
  ! Write PLOT3D files
  E_IO = plot3d_write(grid_file='grid.g', &
                      solution_file='solution.q', &
                      orion=IOfield)
end program write_plot3d
```

## Project Structure

```
ORION/
├── bin/                   # Compiled executables
│   ├── app/               # Applications (converter, etc.)
│   └── test/              # Test executables
├── src/                   # Source code
│   ├── fortran/           # Fortran implementation
│   │   ├── app/           # Applications
│   │   ├── lib/           # Fortran libraries
│   │   └── test/          # Fortran tests
│   └── python/            # Python interface
│       ├── ORION/         # Python package
│       └── test/          # Python tests
├── lib/                   # External libraries
│   └── TecIO/             # TecIO library (auto-built)
├── cmake/                 # CMake modules
├── doc/                   # Documentation (Doxygen)
├── scripts/               # Utility scripts
├── CMakeLists.txt         # Main CMake configuration
├── CMakePresets.json      # CMake presets
├── setup.py               # Python package configuration
└── install.sh             # Installation script
```

## Contributing

Contributions are welcome! Please follow these guidelines:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Versioning

This project follows [Semantic Versioning](https://semver.org/) (SemVer) for managing version numbers. Semantic Versioning is a widely adopted versioning scheme that conveys meaning about the underlying code and what has been modified.

### Version Format

The version number is structured as follows:

```
MAJOR.MINOR.PATCH
```

- **MAJOR**: Incremented when there are incompatible API changes
- **MINOR**: Incremented when functionality is added in a backwards-compatible manner
- **PATCH**: Incremented when backwards-compatible bug fixes are made

For example, a version number of `1.2.3` indicates:
- `1`: The first major version, with potential breaking changes since version `0.x.x`
- `2`: The second minor update, adding new features without breaking existing ones
- `3`: The third patch, fixing bugs in a backwards-compatible manner

### Version Management

Version numbers are automatically updated by running the following command after committing code changes:

```bash
./scripts/version_bump.sh --major|--minor|--patch
```

The script will:
1. Update the version in all relevant files
2. Tag the commit with the new version
3. Prepare the release automatically

Note: Pushing is not required. The updated version is tagged in the repository and released automatically.

## Acknowledgements

This project was developed starting from the following open-source projects:

- **Lib_VTK_IO** — VTK basic routines
  https://github.com/victorsndvg/Lib_VTK_IO

- **OFF** — Tecplot basic routines
  https://github.com/szaghi/OFF

## Support

For issues, feature requests, or questions, please open an issue on the [GitHub repository](https://github.com/MarcoGrossi92/ORION/issues).
