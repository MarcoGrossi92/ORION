# Installation

This guide covers installing ORION for both Fortran and Python users.

## Prerequisites

Before installing ORION, ensure you have the required dependencies:

- **CMake** ≥ 3.13
- **Fortran compiler**: Intel (`ifort`/`ifx`) or GNU (`gfortran`)
- **C++ compiler**: Required for TecIO binary format support (optional)
- **Python** ≥ 3.6: For Python interface (optional)

## Installation Methods

Choose the installation method that best fits your needs:

=== "From Source (Fortran)"

    Build the Fortran library and command-line converter:

    ```bash
    git clone https://github.com/MarcoGrossi92/ORION.git
    cd ORION
    ./install.sh build --compiler=gnu --use-tecio
    ```

    This creates:
    - Static library: `lib/libORION.a`
    - Converter executable: `bin/app/ORION`
    - Test executables: `bin/test/`

=== "Via pip (Python)"

    Install the Python package:

    ```bash
    pip install ORION
    ```

    This provides Python bindings for Tecplot ASCII format only.

=== "Both Fortran and Python"

    Install Fortran library first, then Python package:

    ```bash
    # Build Fortran library
    git clone https://github.com/MarcoGrossi92/ORION.git
    cd ORION
    ./install.sh build --compiler=gnu --use-tecio
    
    # Install Python package
    pip install ORION
    ```

## Build Options

The `install.sh` script provides flexible build configuration:

### Basic Syntax

```bash
./install.sh [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]
```

### Global Options

| Option | Description |
|--------|-------------|
| `-v, --verbose` | Enable verbose output for debugging |

### Commands

**build**

Perform a complete build from scratch:

```bash
./install.sh build [OPTIONS]
```

**Options:**

| Option | Values | Description |
|--------|--------|-------------|
| `--compiler` | `gnu`, `intel` | Select compiler suite (default: `gnu`) |
| `--use-tecio` | — | Enable TecIO for binary Tecplot formats |

**Examples:**

```bash
# Build with GNU compiler, no TecIO
./install.sh build --compiler=gnu

# Build with Intel compiler and TecIO support
./install.sh build --compiler=intel --use-tecio

# Verbose build with all features
./install.sh -v build --compiler=gnu --use-tecio
```

**compile**

Recompile using existing CMake configuration stored in `CMakePresets.json`:

```bash
./install.sh compile
```

Use this after modifying source code to avoid reconfiguring the build system.

**setvars**

Set environment variables to use the converter from any directory:

```bash
./install.sh setvars
```

After running this command, you can call `ORION` from any shell location.

## Compiler-Specific Instructions

**GNU Compiler (gfortran)**

Most systems come with GNU compilers pre-installed:

```bash
# Check if gfortran is available
gfortran --version

# Build ORION
./install.sh build --compiler=gnu --use-tecio
```

**Intel Compiler**

For Intel oneAPI compilers:

```bash
# Source Intel environment (adjust path as needed)
source /opt/intel/oneapi/setvars.sh

# Build ORION
./install.sh build --compiler=intel --use-tecio
```

## TecIO Support

TecIO enables reading and writing binary Tecplot formats (`.plt`, `.szplt`). When you use `--use-tecio`, the build system automatically compiles TecIO and links it with ORION.

TecIO is a C++ library built using `boost`. If a boost system installation is present, that one is used to build TecIO, otherwise, the boost files shipped with ORION will be used. TecIO building provides two folders in `lib/TecIO`:

1. `tecio-build-<compilers>`
2. `tecio-install-<compilers>`

TecIO will not rebuilt unless these folders are removed by the user or the compilers are changed when recompiling ORION. 

!!! note "C++ Compiler Required"
    TecIO requires a C++ compiler. Ensure `g++` or Intel C++ compiler is available.

!!! warning "Large Build Time"
    TecIO compilation can take several minutes on first build.

## Verification

After installation, verify everything works:

**Test the Converter**

```bash
# Display help
ORION --help

# Test with sample data (if available)
ORION --input-format tecplot --input-file test.dat \
      --output-format vtk --output-file test.vtk
```

**Run Test Suite**

```bash
./scripts/test.sh
```

This executes all Fortran unit tests and reports results.

**Test Python Installation**

```python
# In Python shell
import ORION
print(ORION.__version__)

# Test reading a file
from ORION import read_TEC
# x, y, z, var, varnames = read_TEC('yourfile.dat')
```

## Environment Setup

To use the `ORION` converter from anywhere, add to your shell configuration:

**Bash/Zsh (~/.bashrc or ~/.zshrc):**

```bash
export PATH="/path/to/ORION/bin/app:$PATH"
```

Then reload your shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

This operation should be performed by `setvars` command of `install.sh`.

## Library Linking (Advanced)

To link ORION in external Fortran projects:

```bash
# Compile your program
gfortran -I/path/to/ORION/include \
         -L/path/to/ORION/lib \
         -lORION \
         your_program.f90 -o your_program
```

Or use CMake's `find_package`:

```cmake
find_package(ORION REQUIRED)
target_link_libraries(your_target ORION::ORION)
```

## Troubleshooting

**Build Fails with "CMake not found"**

Install CMake:

```bash
# Ubuntu/Debian
sudo apt-get install cmake

# macOS
brew install cmake

# Verify installation
cmake --version
```

**"Compiler not found" Error**

Ensure your compiler is in PATH:

```bash
# For GNU
which gfortran

# For Intel (after sourcing setvars.sh)
which ifort
```

**TecIO Build Errors**

If TecIO compilation fails:

1. Ensure C++ compiler is available: `which g++`
2. Try building without TecIO first: `./install.sh build --compiler=gnu`
3. Check compiler compatibility with TecIO documentation

**Python Import Fails**

```bash
# Ensure ORION is installed
pip list | grep ORION

# Reinstall if needed
pip uninstall ORION
pip install ORION
```

## Next Steps

- **[Quick Start Tutorial](quick-start.md)**: Learn ORION basics with a hands-on example
- **[User Guide](../user-guide/index.md)**: Explore the converter and API capabilities
