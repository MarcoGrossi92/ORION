# Installation

This guide covers installing ORION for both Fortran and Python users.

## Prerequisites

Before installing ORION, ensure you have the required dependencies:

- **CMake** ≥ 3.13
- **Fortran compiler**: GNU (`gfortran`) or Intel (`ifx`, `ifort`)
- **C++ compiler**: CMake enables C++ for the project, and TecIO (binary Tecplot support) is written in it
- **Python** ≥ 3.6 with **NumPy**: for the Python interface (optional)

## Installation Methods

Choose the installation method that best fits your needs:

=== "From Source (Fortran)"

    Build the Fortran library, the command-line converter and the tests:

    ```bash
    git clone https://github.com/MarcoGrossi92/ORION.git
    cd ORION
    ./install.sh build --compilers=gnu --use-tecio
    ```

    This creates:

    - Static library: `build/src/fortran/lib/libORION.a`, with its Fortran modules in `build/modules/`
    - Converter executable: `bin/app/converter`
    - Test executables: `bin/test/`

=== "Python"

    Install the Python package from a clone of the repository:

    ```bash
    git clone https://github.com/MarcoGrossi92/ORION.git
    cd ORION
    pip install .
    ```

    This installs NumPy, the only dependency, if it is missing, and provides Python reading and writing of Tecplot ASCII
    files and reading of PLOT3D grid files.

=== "Both Fortran and Python"

    Build the Fortran library first, then install the Python package, from the ORION folder:

    ```bash
    git clone https://github.com/MarcoGrossi92/ORION.git
    cd ORION
    ./install.sh build --compilers=gnu --use-tecio

    pip install .
    ```

## Build Options

The `install.sh` script provides the build configuration:

### Basic Syntax

```bash
./install.sh [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]
```

### Global Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show the help message |
| `-v, --verbose` | Enable verbose output for debugging |

### Commands

**build**

Perform a complete build from scratch, in `build/` (an existing `build/` is deleted): a Release build of the library, the
converter and the tests. It also writes `CMakePresets.json`, which `scripts/compile.sh` uses (see below).

```bash
./install.sh build [OPTIONS]
```

**Options:**

| Option | Values | Description |
|--------|--------|-------------|
| `--compilers=<name>` | `gnu`, `intel` | Select the compiler suite: `gnu` uses `gfortran` and `g++`, `intel` uses `ifx` and `icpx`. Without it CMake chooses the compilers (the environment variables `FC` and `CXX` apply) |
| `--use-tecio` | — | Build with TecIO, for binary Tecplot files. Without it the build has no binary Tecplot support |

**Examples:**

```bash
# Build with GNU compilers, no TecIO
./install.sh build --compilers=gnu

# Build with Intel compilers and TecIO support
./install.sh build --compilers=intel --use-tecio

# Verbose build with all features
./install.sh -v build --compilers=gnu --use-tecio
```

!!! note "Calling CMake directly"
    `install.sh` is a thin layer over CMake. When you run `cmake` yourself, the option `USE_TECIO` is **ON** by default
    (`-DUSE_TECIO=OFF` turns it off), while `install.sh` builds without TecIO unless `--use-tecio` is given.

**Rebuilding**

After modifying the source code, rebuild with the configuration stored in `CMakePresets.json`, without reconfiguring by
hand:

```bash
./scripts/compile.sh      # add -v for verbose output
```

**Setting the converter command**

The converter is the executable `bin/app/converter`. To use it from any directory as the command `ORION`, source the
script in your shell (bash):

```bash
source scripts/setvars.sh
```

It sets `ORIONDIR` to the ORION folder and defines the shell function `ORION`, which runs `bin/app/converter`, for the
current shell only: it does not modify your shell configuration files. The examples of this documentation use `ORION`.
See [Environment Setup](#environment-setup) to make it permanent.

## Compiler-Specific Instructions

**GNU Compiler (gfortran)**

Most systems come with GNU compilers pre-installed:

```bash
# Check if gfortran is available
gfortran --version

# Build ORION
./install.sh build --compilers=gnu --use-tecio
```

**Intel Compiler**

For Intel oneAPI compilers:

```bash
# Source Intel environment (adjust path as needed)
source /opt/intel/oneapi/setvars.sh

# Build ORION
./install.sh build --compilers=intel --use-tecio
```

## TecIO Support

TecIO enables reading and writing binary Tecplot files: ORION writes `.plt` and `.szplt` files and reads `.szplt` files.
When you use `--use-tecio`, the build system automatically compiles TecIO and links it with ORION.

TecIO is a C++ library built using `boost`. If a boost system installation is present, that one is used to build TecIO, otherwise, the boost files shipped with ORION will be used. TecIO building provides two folders in `lib/TecIO`:

1. `tecio-build-<compilers>`
2. `tecio-install-<compilers>`

TecIO will not rebuilt unless these folders are removed by the user or the compilers are changed when recompiling ORION. 

`-DORION_TECIO_DIR=<folder>` puts the two folders in `<folder>` instead of `lib/TecIO`, so that several builds of ORION
share one TecIO build. The rule above applies to them there.

!!! note "C++ Compiler Required"
    TecIO requires a C++ compiler. Ensure `g++` or Intel C++ compiler is available.

!!! warning "Large Build Time"
    TecIO compilation can take several minutes on first build.

## Verification

After installation, verify everything works:

**Test the Converter**

```bash
source scripts/setvars.sh

# Display help
ORION --help
```

[Quick Start](quick-start.md) converts a sample file step by step.

**Run Test Suite**

```bash
ctest --test-dir build --output-on-failure
```

This runs the Fortran tests, which `install.sh build` has compiled (`ORION.<name>` in the list of `ctest`), and the Python
tests, which are registered when CMake finds a Python 3 with NumPy. Run one test with `ctest --test-dir build -R tecplot_read`.

**Test Python Installation**

```python
# In Python shell
# Test reading a file
from ORION import read_TEC
# x, y, z, var, varnames = read_TEC('yourfile.tec')
```

## Environment Setup

To have the `ORION` command in every new bash shell, add this line to `~/.bashrc`:

```bash
source /path/to/ORION/scripts/setvars.sh
```

Another way, for any shell, is to put the folder of the executable in your `PATH`; the command is then `converter`, not
`ORION`:

```bash
export PATH="/path/to/ORION/bin/app:$PATH"
```

Then reload your shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

## Library Linking (Advanced)

**As a CMake subproject**

A CMake project adds the ORION folder as a subproject and links the target `ORION`. When ORION is not the top-level
project, CMake builds the library only, not the converter and the tests:

```cmake
project(your_project LANGUAGES Fortran CXX)

add_subdirectory(path/to/ORION ORION)

# ORION writes its Fortran modules to <top-level build directory>/modules
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

add_executable(your_program your_program.f90)
target_link_libraries(your_program ORION)
target_include_directories(your_program PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
```

The options `USE_TECIO` (default ON) and `ORION_TECIO_DIR` (see [TecIO Support](#tecio-support)) apply to the
subproject as well; set them before `add_subdirectory` to change them (`set(USE_TECIO OFF CACHE BOOL "" FORCE)`).

**With a compiler command**

Against a library built by `install.sh` without TecIO:

```bash
gfortran -I/path/to/ORION/build/modules your_program.f90 \
         /path/to/ORION/build/src/fortran/lib/libORION.a -o your_program
```

With TecIO, add the TecIO library and the C++ runtime:

```bash
gfortran -I/path/to/ORION/build/modules your_program.f90 \
         /path/to/ORION/build/src/fortran/lib/libORION.a \
         /path/to/ORION/lib/TecIO/tecio-install-GNU/lib/libtecio.a \
         -lstdc++ -lpthread -lm -o your_program
```

ORION has no `install` step, so it is not found with `find_package`.

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

**"Unknown option '--compiler=gnu'"**

The option is `--compilers` (with an `s`), e.g. `./install.sh build --compilers=gnu`.

**"Compiler not found" Error**

Ensure your compiler is in PATH:

```bash
# For GNU
which gfortran

# For Intel (after sourcing oneAPI's setvars.sh)
which ifx
```

**TecIO Build Errors**

If TecIO compilation fails:

1. Ensure C++ compiler is available: `which g++`
2. Try building without TecIO first: `./install.sh build --compilers=gnu`
3. Check compiler compatibility with TecIO documentation

**"ORION: command not found"**

The command exists in a shell after `source scripts/setvars.sh`; or call `bin/app/converter` directly.

**Python Import Fails**

```bash
# Ensure ORION is installed
pip list | grep ORION

# Reinstall if needed, from the ORION folder
pip uninstall ORION
pip install .
```

If the import of `numpy` fails, install NumPy: `pip install numpy` (`pip install .` installs it).

## Next Steps

- **[Quick Start Tutorial](quick-start.md)**: Learn ORION basics with a hands-on example
- **[User Guide](../user-guide/index.md)**: Explore the converter and API capabilities
