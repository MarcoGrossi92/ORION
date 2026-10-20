# Quick Start

This tutorial will get you up and running with ORION in just a few minutes. By the end, you'll know how to convert files and use the basic APIs.

## Prerequisites

Before starting, ensure you have:

- ✅ ORION installed ([Installation Guide](installation.md))
- ✅ A terminal/command prompt
- ✅ (Optional) Python 3.6+ and the ORION package (`pip install .` in the ORION folder) for the Python example

## Your First Conversion

Let's start with the simplest use case: converting a file using the command-line tool.

### Step 1: Verify Installation

Check that the converter is accessible. From the ORION folder, define the `ORION` command in your shell and ask for the
help:

```bash
source scripts/setvars.sh
ORION --help
```

You should see the help message with the available options:

```
 File format converter. Supported formats: PLOT3D, Tecplot, VTK

 Usage: ORION --in-file=<file> --out-file=<file> [options]
   The format of a file is that of its extension: .dat .tec .szplt (Tecplot), .p3d (PLOT3D), .vtm (VTK)

 Options:
   -h, --help              Display this help message
   ...
```

`ORION` runs the executable `bin/app/converter`; you can also call that executable directly (see
[Environment Setup](installation.md#environment-setup)).

### Step 2: Create Sample Data

We'll use a small Tecplot file: one block of 5 x 5 x 5 nodes (4 x 4 x 4 cells) with a variable, `Temperature`, at the
cells. The command below writes it with `awk`; any other way of writing the same lines is fine:

```bash
awk 'BEGIN {
  n = 5
  print "VARIABLES = x y z Temperature"
  printf "ZONE T=\"Sample\", I=%d, J=%d, K=%d, DATAPACKING=BLOCK, VARLOCATION=([4]=CELLCENTERED)\n", n, n, n
  # the x, then the y, then the z of the nodes (i fastest), one value per line
  for (c = 0; c < 3; c++) for (k = 0; k < n; k++) for (j = 0; j < n; j++) for (i = 0; i < n; i++)
    print (c == 0 ? i : (c == 1 ? j : k)) / (n - 1)
  # the temperature of the cells
  for (k = 0; k < n - 1; k++) for (j = 0; j < n - 1; j++) for (i = 0; i < n - 1; i++)
    print 300 + 5 * (i + j + k)
}' > sample.tec
```

The file starts with:

```
VARIABLES = x y z Temperature
ZONE T="Sample", I=5, J=5, K=5, DATAPACKING=BLOCK, VARLOCATION=([4]=CELLCENTERED)
0
0.25
```

### Step 3: Convert to VTK

Now convert the Tecplot file to VTK format. A VTK output is a `.vtm` file and one `.vts` file for each block, written next
to it, so give it a directory of its own:

```bash
mkdir vtk
ORION --in-file=sample.tec --out-file=vtk/sample.vtm
```

The converter takes the formats from the extensions of the files and prints:

```
 - Input file  : sample.tec
 - Output file : vtk/sample.vtm


 Done!
```

### Step 4: Verify Output

Check that the output files were created:

```bash
ls vtk
cat vtk/sample.vtm
```

```
Block1.vts  sample.vtm
<?xml version="1.0"?>
<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">
  <vtkMultiBlockDataSet>
    <Block index="0">
      <DataSet index="0" file="Block1.vts"/>
    </Block>
  </vtkMultiBlockDataSet>
</VTKFile>
```

`Temperature` was at the cells in the Tecplot file, so it is cell data in `Block1.vts`. The converter reads the VTK files
back as well:

```bash
ORION --in-file=vtk/sample.vtm --out-file=back.tec
head -2 back.tec
```

```
 VARIABLES = x y z  Temperature
 ZONE  T = Block1, I=5, J=5, K=5, DATAPACKING=BLOCK, VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED), SOLUTIONTIME=0.00000000000000000E+000, STRANDID=1
```

The variables keep their names (`Temperature` is the name of the array in `Block1.vts`) and the blocks are named `Block1`,
`Block2`, .... See the [Converter guide](../user-guide/converter.md) for the other formats and options.

## Using the Fortran API

Now let's write a simple Fortran program that reads and processes the same data.

### Create a Fortran Program

Create a file `my_first_program.f90`:

```fortran
program my_first_program
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none

  type(orion_data) :: IOfield
  integer :: E_IO, i

  E_IO = tec_read_structured_multiblock(filename='sample.tec', orion=IOfield)
  if (E_IO /= 0) stop 'Error reading sample.tec'

  print '(A,I0)', 'Number of blocks: ', size(IOfield%block)
  print '(A,*(1X,A))', 'Variables:', (trim(IOfield%varnames(i)), i = 1, size(IOfield%varnames))
  print '(A,3(1X,I0))', 'Cells of block 1 (Ni Nj Nk):', IOfield%block(1)%Ni, IOfield%block(1)%Nj, IOfield%block(1)%Nk
  ! The first solution variable (after x, y and z) is vars(1,...), at the cells: vars(1,1:Ni,1:Nj,1:Nk)
  print '(A,F6.1,A,F6.1)', 'Temperature range: ', minval(IOfield%block(1)%vars(1,:,:,:)), ' to ', &
                                                  maxval(IOfield%block(1)%vars(1,:,:,:))
end program my_first_program
```

### Compile and Run

Compile the program against the library and its modules (`ORIONDIR`, the ORION folder, is set by `scripts/setvars.sh`;
a library built without `--use-tecio` needs nothing else):

```bash
gfortran -I$ORIONDIR/build/modules my_first_program.f90 \
         $ORIONDIR/build/src/fortran/lib/libORION.a -o my_first_program

./my_first_program
```

With a library built with `--use-tecio`, link TecIO as well:

```bash
gfortran -I$ORIONDIR/build/modules my_first_program.f90 \
         $ORIONDIR/build/src/fortran/lib/libORION.a \
         $ORIONDIR/lib/TecIO/tecio-install-GNU/lib/libtecio.a -lstdc++ -lpthread -lm -o my_first_program
```

Expected output:

```
Number of blocks: 1
Variables: x y z Temperature
Cells of block 1 (Ni Nj Nk): 4 4 4
Temperature range:  300.0 to  345.0
```

## Using the Python API

Let's do the same thing with Python.

### Create a Python Script

Create `my_first_script.py`:

```python
import numpy as np
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('sample.tec')

print(f"Number of blocks: {len(x)}")
print(f"Variables: {varnames}")
print(f"Nodes of block 1: {x[0].shape}")

temperature = var[0][0]  # block 0, first variable after x, y and z: its values at the cells
print(f"Cells of block 1: {temperature.shape}")
print(f"Temperature range: {temperature.min():.1f} to {temperature.max():.1f}")
print(f"Mean temperature: {temperature.mean():.1f}")

# The cell of the maximum, and its first node
i, j, k = np.unravel_index(temperature.argmax(), temperature.shape)
print(f"Maximum in cell i={i}, j={j}, k={k}; first node at x={x[0][i, j, k]:.2f}, y={y[0][i, j, k]:.2f}, z={z[0][i, j, k]:.2f}")
```

### Run the Script

```bash
python3 my_first_script.py
```

Expected output:

```
Number of blocks: 1
Variables: ['x', 'y', 'z', 'Temperature']
Nodes of block 1: (5, 5, 5)
Cells of block 1: (4, 4, 4)
Temperature range: 300.0 to 345.0
Mean temperature: 322.5
Maximum in cell i=3, j=3, k=3; first node at x=0.75, y=0.75, z=0.75
```

## Troubleshooting

### "ORION: command not found"

**Solution:** The command exists in the shell where you ran `source scripts/setvars.sh`. Run it there (from the ORION folder),
or call the executable by its path, or put its folder in your `PATH`:

```bash
/path/to/ORION/bin/app/converter --help

export PATH="/path/to/ORION/bin/app:$PATH"   # then the command is `converter`
```

### An `ERROR` line from the converter

**Solution:** The converter prints a line that begins with `ERROR`, and ends with the exit status 1, when it cannot read
its input or write its output. Check that:

1. The file exists and is readable: `ls -l sample.tec`
2. The extension of the file is that of its format (`.dat`, `.tec`, `.szplt`, `.p3d`, `.vtm`)
3. The output directory exists (`mkdir vtk`)

The [Converter guide](../user-guide/converter.md#troubleshooting) lists the messages.

### "Cannot open module file" (Fortran)

**Solution:** Point the compiler to the modules of the library, and check that the library is there:

```bash
ls $ORIONDIR/build/modules/lib_tecplot.mod
ls $ORIONDIR/build/src/fortran/lib/libORION.a
```

### "ImportError: No module named ORION" (Python)

**Solution:** Install the Python package from the ORION folder (it installs NumPy if it is missing):

```bash
pip install .

# Verify
pip list | grep ORION
```

## Next Steps

Congratulations! You've completed the quick start tutorial. Here's what to explore next:

### Explore Specific Topics

- **[Fortran API](../user-guide/fortran-api.md)** - Deep dive into Fortran interface
- **[Python API](../user-guide/python-api.md)** - Python workflows and analysis
- **[Converter](../user-guide/converter.md)** - Advanced converter usage

### Try Real Data

Download sample datasets:
- [NASA Turbulence Modeling Database](https://turbmodels.larc.nasa.gov/)

---
