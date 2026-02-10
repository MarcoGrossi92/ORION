# Python API

The Python API provides a convenient interface for reading and writing Tecplot ASCII files. It's designed for scripting, post-processing, and integration with scientific Python workflows.

## Installation

Install ORION via pip:

```bash
pip install ORION
```

This installs the Python package with support for Tecplot ASCII format.

## Supported Formats

The Python API currently supports:

| Format | Read | Write | Notes |
|--------|------|-------|-------|
| **Tecplot ASCII** | ✅ | ✅ | `.tec` files |
| **Tecplot Binary** | ❌ | ❌ | Use Fortran API for `.plt`/`.szplt` |
| **VTK** | ❌ | ❌ | Use Fortran API |
| **PLOT3D** | ❌ | ❌ | Use Fortran API |

!!! note "Binary Format Support"
    For Tecplot binary formats and other file types, use the [Fortran API](fortran-api.md) or the command-line converter.

## Reading Tecplot Files

### Basic Usage

The primary function for reading Tecplot files is `read_TEC`:

```python
from ORION import read_TEC

# Read Tecplot ASCII file
x, y, z, var, varnames = read_TEC('simulation.tec')

print(f"Variables: {varnames}")
print(f"Grid shape: {x.shape}")
print(f"Number of variables: {len(var)}")
```

### Return Values

`read_TEC` returns a tuple of five elements:

1. **x**: NumPy array of x-coordinates `(nblocks, imax, jmax, kmax)`
2. **y**: NumPy array of y-coordinates `(nblocks, imax, jmax, kmax)`
3. **z**: NumPy array of z-coordinates `(nblocks, imax, jmax, kmax)`
4. **var**: List of NumPy arrays, one per variable `[(nblocks, imax, jmax, kmax), ...]`
5. **varnames**: List of variable names `['velocity_x', 'velocity_y', ...]`

### Single Block Data

For single-block files:

```python
x, y, z, var, varnames = read_TEC('single_block.tec')

# Access data (squeeze out block dimension)
x_coords = x[0, :, :, :]  # Shape: (imax, jmax, kmax)
y_coords = y[0, :, :, :]
z_coords = z[0, :, :, :]

# Access first variable
first_var = var[0][0, :, :, :]  # Shape: (imax, jmax, kmax)

print(f"Grid dimensions: {x_coords.shape}")
print(f"Variable name: {varnames[0]}")
```

### Multi-Block Data

For multi-block structured grids:

```python
x, y, z, var, varnames = read_TEC('multiblock.tec')

nblocks = x.shape[0]
print(f"Number of blocks: {nblocks}")

# Iterate over blocks
for iblock in range(nblocks):
    print(f"\nBlock {iblock + 1}:")
    print(f"  Dimensions: {x[iblock].shape}")
    print(f"  X range: [{x[iblock].min():.3f}, {x[iblock].max():.3f}]")
    print(f"  Y range: [{y[iblock].min():.3f}, {y[iblock].max():.3f}]")
    print(f"  Z range: [{z[iblock].min():.3f}, {z[iblock].max():.3f}]")
    
    # Access variables for this block
    for ivar, name in enumerate(varnames):
        data = var[ivar][iblock]
        print(f"  {name}: [{data.min():.3e}, {data.max():.3e}]")
```

## Writing Tecplot Files

### Basic Usage

Use `write_TEC` to create Tecplot ASCII files:

```python
from ORION import write_TEC
import numpy as np

# Prepare data
x = np.array([...])  # Shape: (nblocks, imax, jmax, kmax)
y = np.array([...])
z = np.array([...])
var = [array1, array2, ...]  # List of arrays
varnames = ['pressure', 'temperature', 'density']

# Write file
write_TEC('output.tec', x, y, z, var, varnames)
```

### Creating Data from Scratch

Example: Generate a simple 3D grid with computed variables:

```python
import numpy as np
from ORION import write_TEC

# Create a 10x10x10 grid
nx, ny, nz = 10, 10, 10

# Single block (add dimension for nblocks=1)
x = np.linspace(0, 1, nx)[np.newaxis, :, np.newaxis, np.newaxis]
y = np.linspace(0, 1, ny)[np.newaxis, np.newaxis, :, np.newaxis]
z = np.linspace(0, 1, nz)[np.newaxis, np.newaxis, np.newaxis, :]

# Broadcast to full grid
x = np.broadcast_to(x, (1, nx, ny, nz))
y = np.broadcast_to(y, (1, nx, ny, nz))
z = np.broadcast_to(z, (1, nx, ny, nz))

# Compute variables
pressure = np.sin(np.pi * x) * np.cos(np.pi * y)
temperature = x**2 + y**2 + z**2

# Prepare variable list
var = [pressure, temperature]
varnames = ['pressure', 'temperature']

# Write file
write_TEC('generated.tec', x, y, z, var, varnames)
print("File written successfully!")
```

## Data Analysis Examples

### Extracting Slices

Extract a 2D slice from 3D data:

```python
from ORION import read_TEC
import matplotlib.pyplot as plt

# Read data
x, y, z, var, varnames = read_TEC('flow_field.tec')

# Extract mid-plane slice (k = nz//2)
iblock = 0
k_mid = x.shape[3] // 2

x_slice = x[iblock, :, :, k_mid]
y_slice = y[iblock, :, :, k_mid]
pressure_slice = var[0][iblock, :, :, k_mid]  # Assuming pressure is first variable

# Plot
plt.contourf(x_slice, y_slice, pressure_slice, levels=20)
plt.colorbar(label='Pressure')
plt.xlabel('X')
plt.ylabel('Y')
plt.title(f'{varnames[0]} at mid-plane')
plt.savefig('pressure_slice.png')
```

### Computing Derived Quantities

Calculate velocity magnitude:

```python
from ORION import read_TEC
import numpy as np

# Read velocity components
x, y, z, var, varnames = read_TEC('velocity_field.tec')

# Assuming varnames = ['u', 'v', 'w', 'pressure']
u = var[0]  # velocity_x
v = var[1]  # velocity_y
w = var[2]  # velocity_z

# Compute magnitude
vmag = np.sqrt(u**2 + v**2 + w**2)

# Statistics
print(f"Velocity magnitude:")
print(f"  Min: {vmag.min():.3e}")
print(f"  Max: {vmag.max():.3e}")
print(f"  Mean: {vmag.mean():.3e}")
print(f"  Std: {vmag.std():.3e}")
```

### Block-wise Analysis

Analyze each block separately:

```python
from ORION import read_TEC
import numpy as np

x, y, z, var, varnames = read_TEC('multiblock.tec')

nblocks = x.shape[0]

for iblock in range(nblocks):
    print(f"\n=== Block {iblock + 1} ===")
    
    # Compute volume (approximate)
    dx = np.diff(x[iblock], axis=0).mean()
    dy = np.diff(y[iblock], axis=1).mean()
    dz = np.diff(z[iblock], axis=2).mean()
    volume = dx * dy * dz * np.prod(x[iblock].shape)
    
    print(f"Approximate volume: {volume:.3e}")
    
    # Variable statistics
    for ivar, name in enumerate(varnames):
        data = var[ivar][iblock]
        print(f"{name}:")
        print(f"  Range: [{data.min():.3e}, {data.max():.3e}]")
        print(f"  Mean: {data.mean():.3e}")
```

## Integration with Scientific Python

### Using with Pandas

Convert to DataFrame for tabular analysis:

```python
from ORION import read_TEC
import pandas as pd
import numpy as np

# Read data
x, y, z, var, varnames = read_TEC('data.tec')

# Flatten arrays for single block
iblock = 0
data_dict = {
    'x': x[iblock].flatten(),
    'y': y[iblock].flatten(),
    'z': z[iblock].flatten()
}

# Add variables
for ivar, name in enumerate(varnames):
    data_dict[name] = var[ivar][iblock].flatten()

# Create DataFrame
df = pd.tecaFrame(data_dict)

# Analysis
print(df.describe())
print(df.corr())  # Correlation matrix
```

### Using with SciPy

Interpolate data to new grid:

```python
from ORION import read_TEC
from scipy.interpolate import RegularGridInterpolator
import numpy as np

# Read data
x, y, z, var, varnames = read_TEC('coarse_grid.tec')

iblock = 0
# Create interpolator for first variable
interp = RegularGridInterpolator(
    (x[iblock, :, 0, 0], y[iblock, 0, :, 0], z[iblock, 0, 0, :]),
    var[0][iblock],
    method='linear'
)

# Interpolate to finer grid
x_fine = np.linspace(x[iblock].min(), x[iblock].max(), 50)
y_fine = np.linspace(y[iblock].min(), y[iblock].max(), 50)
z_fine = np.linspace(z[iblock].min(), z[iblock].max(), 50)

xg, yg, zg = np.meshgrid(x_fine, y_fine, z_fine, indexing='ij')
points = np.array([xg.flatten(), yg.flatten(), zg.flatten()]).T

var_fine = interp(points).reshape(50, 50, 50)
```

### Visualization with Matplotlib

3D surface plot:

```python
from ORION import read_TEC
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Read data
x, y, z, var, varnames = read_TEC('surface.tec')

# Extract surface (k=0 plane)
iblock = 0
X = x[iblock, :, :, 0]
Y = y[iblock, :, :, 0]
Z = var[0][iblock, :, :, 0]  # Use first variable as height

# 3D surface plot
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(X, Y, Z, cmap='viridis')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel(varnames[0])
plt.colorbar(surf)
plt.savefig('surface_3d.png')
```

---