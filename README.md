# ORION - Ordinary Repository for Input-Output Necessities

![License](https://img.shields.io/github/license/MarcoGrossi92/ORION)
![Fortran](https://img.shields.io/badge/Fortran-90%2B-blue)
![Python](https://img.shields.io/badge/Python-3.6%2B-yellow)
![Platform](https://img.shields.io/badge/platform-Linux%20%7C%20macOS-lightgrey)
![GitHub Stars](https://img.shields.io/github/stars/MarcoGrossi92/ORION?style=social)
![GitHub Forks](https://img.shields.io/github/forks/MarcoGrossi92/ORION?style=social)
![GitHub Downloads](https://img.shields.io/github/downloads/MarcoGrossi92/ORION/total)

A modular I/O toolkit for reading and writing structured, multi-block scientific data across multiple file formats. ORION provides both Fortran and Python interfaces designed for seamless integration into high-performance and scientific computing workflows.

## Features

- **Multi-format Support**: Tecplot, VTK, and PLOT3D file formats
- **Native Fortran API**: High-performance library for computational workflows
- **Python Interface**: Convenient scripting and post-processing capabilities
- **Command-line Converter**: Standalone utility for format conversion
- **Cross-platform**: Tested on Linux and macOS with GNU and Intel compilers

## Quick Start

### Installation

Clone and build with a single command:

```bash
git clone https://github.com/MarcoGrossi92/ORION.git
cd ORION
./install.sh build --compiler=gnu --use-tecio
```

For Python support, use this command in the ORION folder:

```bash
pip install .
```

### Basic Usage

**Convert files from command line:**

```bash
ORION --input-format tecplot --input-file data.dat \
      --output-format vtk --output-file output.vtk
```

**Read data in Fortran:**

```fortran
use Lib_ORION_data
use Lib_Tecplot

type(ORION_data) :: IOfield
integer :: E_IO

E_IO = tec_read_structured_multiblock(filename='data.tec', orion=IOfield)
```

**Read data in Python:**

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('data.tec')
```

## Documentation

📘 **Full documentation**: https://MarcoGrossi92.github.io/ORION/

Learn more about:
- [Installation & Requirements](https://MarcoGrossi92.github.io/ORION/getting-started/installation/)
- [Fortran API Guide](https://MarcoGrossi92.github.io/ORION/user-guide/fortran-api/)
- [Python API Guide](https://MarcoGrossi92.github.io/ORION/user-guide/python-api/)

## Contributing

Contributions are welcome! Please see our [Contributing Guide](https://MarcoGrossi92.github.io/ORION/development/contributing/) for details on how to get started.

## License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.

## Support

- **Issues & Feature Requests**: [GitHub Issues](https://github.com/MarcoGrossi92/ORION/issues)
- **Documentation**: [https://MarcoGrossi92.github.io/ORION/](https://MarcoGrossi92.github.io/ORION/)

---

**Acknowledgements**: Built upon [Lib_VTK_IO](https://github.com/victorsndvg/Lib_VTK_IO) and [OFF](https://github.com/szaghi/OFF)