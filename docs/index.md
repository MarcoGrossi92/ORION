# Welcome to ORION

**ORION** (Ordinary Repository for Input-Output Necessities) is a modular I/O toolkit for reading and writing structured, multi-block scientific data across multiple file formats. It provides both Fortran and Python interfaces designed for seamless integration into high-performance and scientific computing workflows.

## Why ORION?

Scientific computing often requires handling data in various formats depending on how the input and output fields are processed and visualized. ORION bridges several formats with a unified interface, eliminating the need for manual conversions and format-specific code.

### Key Features

- **Multi-format Support**: Read and write Tecplot, VTK, and PLOT3D files
- **Dual Language APIs**: Native Fortran for performance, Python for convenience
- **Standalone Converter**: Command-line tool for quick format conversions
- **Type Safety**: Structured data types ensure consistency across formats
- **Production Ready**: Tested across multiple platforms and compilers

## Supported Formats

| Format | Fortran Read | Fortran Write | Python Read | Python Write |
|--------|--------------|---------------|-------------|--------------|
| **Tecplot ASCII** | ✅ | ✅ | ✅ | ✅ |
| **Tecplot Binary** (PLT/SZPLT) | ✅ | ✅ | ❌ | ❌ |
| **VTK** | ✅ | ✅ | ❌ | ❌ |
| **PLOT3D** | ✅ | ✅ | ❌ | ❌ |

## Quick Example

Convert a Tecplot file to VTK format:

```bash
ORION --input-format tecplot --input-file simulation.dat \
      --output-format vtk --output-file visualization.vtk
```

Read the same data in Fortran:

```fortran
use Lib_ORION_data
use Lib_Tecplot

type(ORION_data) :: IOfield
integer :: E_IO

E_IO = tec_read_structured_multiblock(filename='simulation.dat', orion=IOfield)
if (E_IO == 0) print *, 'Variables:', IOfield%varnames
```

Or in Python:

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('simulation.dat')
print(f"Loaded {len(varnames)} variables: {varnames}")
```

## Getting Started

New to ORION? Follow these steps:

1. **[Install ORION](getting-started/installation.md)** - Build from source or install via pip
2. **[Quick Start Tutorial](getting-started/quick-start.md)** - Learn the basics with a hands-on example

## Documentation Structure

This documentation is organized into several sections:

- **[Getting Started](getting-started/index.md)**: Installation, and first steps
- **[User Guide](user-guide/index.md)**: Detailed usage of the converter and APIs
- **[Development](development/index.md)**: Contributing, building, and versioning

## Use Cases

ORION is designed for:

- **CFD Workflows**: Convert between solver formats and visualization tools
- **Data Analysis**: Read simulation results in Python for post-processing

## Platform Support

ORION is tested and supported on:

- **Linux** (Ubuntu, OpenSUSE) with GNU and Intel compilers
- **macOS** (Intel and Apple Silicon) with GNU compilers
- **Windows** via WSL2 or MSYS2/MinGW

## Get Help

- **Questions?** Check the [User Guide](user-guide/index.md)
- **Found a bug?** [Open an issue](https://github.com/MarcoGrossi92/ORION/issues)
- **Want to contribute?** See the [Contributing Guide](development/contributing.md)

## License

ORION is free and open-source software licensed under the [GNU General Public License v3.0](about/license.md).
