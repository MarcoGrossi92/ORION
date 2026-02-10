# Quick Start

This tutorial will get you up and running with ORION in just a few minutes. By the end, you'll know how to convert files and use the basic APIs.

## Prerequisites

Before starting, ensure you have:

- ✅ ORION installed ([Installation Guide](installation.md))
- ✅ A terminal/command prompt
- ✅ (Optional) Python 3.6+ for Python examples

## Your First Conversion

Let's start with the simplest use case: converting a file using the command-line tool.

### Step 1: Verify Installation

Check that ORION is accessible:

```bash
# Check if converter is in PATH
ORION --help

# If not found, use full path
/path/to/ORION/bin/app/ORION --help
```

You should see the help message with available options.

### Step 2: Download Sample Data

We'll use a simple Tecplot file for testing. Create a sample file:

```bash
cat > sample.dat << 'EOF'
VARIABLES = "X", "Y", "Z", "Temperature"
ZONE T="Sample Block", I=5, J=5, K=5
0.0 0.0 0.0 300.0
0.25 0.0 0.0 305.0
0.5 0.0 0.0 310.0
0.75 0.0 0.0 315.0
1.0 0.0 0.0 320.0
0.0 0.25 0.0 302.0
0.25 0.25 0.0 307.0
# ... (simplified for brevity)
EOF
```

Or download a real sample from the repository:

```bash
wget https://raw.githubusercontent.com/MarcoGrossi92/ORION/main/test/sample.dat
```

### Step 3: Convert to VTK

Now convert the Tecplot file to VTK format:

```bash
ORION --input-format tecplot --input-file sample.dat \
      --output-format vtk --output-file sample.vtk
```

You should see output like:
```
Reading Tecplot file: sample.dat
Writing VTK file: sample.vtk
Conversion complete!
```

### Step 4: Verify Output

Check that the output file was created:

```bash
ls -lh sample.vtk
head -20 sample.vtk  # View file header
```

The VTK file should contain the same data in VTK format.

## Using the Fortran API

Now let's write a simple Fortran program that reads and processes data.

### Create a Fortran Program

Create a file `my_first_program.f90`:

```fortran
program my_first_program
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(ORION_data) :: IOfield
  integer :: E_IO
  real :: max_temp, min_temp
  
  ! Read the sample file
  print *, '=== ORION Quick Start Example ==='
  print *, 'Reading sample.dat...'
  
  E_IO = tec_read_structured_multiblock(filename='sample.dat', orion=IOfield)
  
  if (E_IO /= 0) then
    print *, 'Error reading file!'
    stop 1
  endif
  
  ! Print file information
  print *, 'Successfully read file!'
  print *, 'Number of blocks:', IOfield%nblocks
  print *, 'Variables:', trim(IOfield%varnames)
  print *, 'Block 1 dimensions:', IOfield%blockdims(:, 1)
  
  ! Find min/max temperature (assuming it's the first variable)
  if (allocated(IOfield%var(1)%values)) then
    min_temp = minval(IOfield%var(1)%values(:,:,:,1))
    max_temp = maxval(IOfield%var(1)%values(:,:,:,1))
    
    print *, 'Temperature range:', min_temp, 'to', max_temp
  endif
  
  print *, '=== Done! ==='
  
end program my_first_program
```

### Compile and Run

Compile the program:

```bash
# With GNU compiler
gfortran -I/path/to/ORION/include \
         -L/path/to/ORION/lib \
         -lORION \
         my_first_program.f90 -o my_first_program

# Run it
./my_first_program
```

Expected output:
```
=== ORION Quick Start Example ===
Reading sample.dat...
Successfully read file!
Number of blocks: 1
Variables: Temperature
Block 1 dimensions: 5 5 5
Temperature range: 300.0 to 320.0
=== Done! ===
```

## Using the Python API

Let's do the same thing with Python.

### Create a Python Script

Create `my_first_script.py`:

```python
#!/usr/bin/env python3
"""ORION Quick Start Example - Python"""

from ORION import read_TEC
import numpy as np

print("=== ORION Quick Start Example (Python) ===")
print("Reading sample.dat...")

# Read the file
x, y, z, var, varnames = read_TEC('sample.dat')

print("Successfully read file!")
print(f"Variables: {varnames}")
print(f"Grid shape: {x.shape}")

# Analyze temperature (first variable)
temperature = var[0][0]  # Block 0, variable 0

print(f"Temperature range: {temperature.min():.2f} to {temperature.max():.2f}")
print(f"Mean temperature: {temperature.mean():.2f}")

# Find location of maximum temperature
max_idx = np.unravel_index(temperature.argmax(), temperature.shape)
print(f"Max temperature location: i={max_idx[0]}, j={max_idx[1]}, k={max_idx[2]}")
print(f"Coordinates: x={x[0][max_idx]:.2f}, y={y[0][max_idx]:.2f}, z={z[0][max_idx]:.2f}")

print("=== Done! ===")
```

### Run the Script

```bash
python3 my_first_script.py
```

Expected output:
```
=== ORION Quick Start Example (Python) ===
Reading sample.dat...
Successfully read file!
Variables: ['Temperature']
Grid shape: (1, 5, 5, 5)
Temperature range: 300.00 to 320.00
Mean temperature: 310.00
Max temperature location: i=4, j=4, k=4
Coordinates: x=1.00, y=1.00, z=1.00
=== Done! ===
```

## Troubleshooting

### "ORION: command not found"

**Solution:** Add ORION to your PATH or use the full path:

```bash
# Temporary
export PATH="/path/to/ORION/bin/app:$PATH"

# Permanent
echo 'export PATH="/path/to/ORION/bin/app:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### "Error reading file"

**Solution:** Check that:

1. File exists and is readable: `ls -l filename.dat`
2. File format is correct: `file filename.dat`
3. Format specification matches file type

### "Module not found" (Fortran)

**Solution:** Ensure you're linking correctly:

```bash
# Verify library exists
ls -l /path/to/ORION/lib/libORION.a

# Check include path
ls -l /path/to/ORION/include/
```

### "ImportError: No module named ORION" (Python)

**Solution:** Install the Python package:

```bash
pip install ORION

# Or if already installed, verify
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