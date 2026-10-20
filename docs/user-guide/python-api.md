# Python API

The Python API provides a convenient interface for reading and writing Tecplot ASCII files, and for reading PLOT3D grid files. It's designed for scripting, post-processing, and integration with scientific Python workflows.

## Installation

Install the Python package from a clone of the repository, in the ORION folder:

```bash
pip install .
```

This installs the Python package with support for Tecplot ASCII and PLOT3D grid files, and NumPy, its only dependency.

## Supported Formats

The Python API currently supports:

| Format | Read | Write | Notes |
|--------|------|-------|-------|
| **Tecplot ASCII** | ✅ | ✅ | `.dat`, `.tec` files, in the form described under *Return Values*; other forms are refused |
| **Tecplot Binary** | ❌ | ❌ | Use the Fortran API or the converter for `.szplt`/`.plt` |
| **VTK** | ❌ | ❌ | Use the Fortran API or the converter |
| **PLOT3D** | ✅ | ❌ | Grid files, every common layout (ASCII and binary) |

!!! note "Binary Format Support"
    For Tecplot binary formats and other file types, use the [Fortran API](fortran-api.md) or the
    [command-line converter](converter.md).

## Reading Tecplot Files

### Basic Usage

The primary function for reading Tecplot files is `read_TEC`:

```python
from ORION import read_TEC

# Read Tecplot ASCII file
x, y, z, var, varnames = read_TEC('simulation.tec')

print(f"Variables: {varnames}")
print(f"Number of blocks: {len(x)}")
print(f"Nodes of the first block: {x[0].shape}")
print(f"Variables of the first block: {len(var[0])}")
```

### Return Values

`read_TEC` reads files of three coordinates, `x`, `y` and `z`, at the nodes, with the other variables at the cells, in
`BLOCK` packing: the files that the Fortran writer writes for three-dimensional blocks with its default location of the
variables (`orion%tec%node` false), and those of `write_TEC`. It returns a tuple of five elements; the first four have
one entry per zone (block):

1. **x**: list of NumPy arrays, the x-coordinates of the nodes of each zone, of shape `(imax, jmax, kmax)`
2. **y**: list of NumPy arrays, the y-coordinates, of the same shapes
3. **z**: list of NumPy arrays, the z-coordinates, of the same shapes
4. **var**: list, for each zone, of NumPy arrays, one per variable after `x`, `y` and `z`: the values at the cells, of
   shape `(max(imax-1,1), max(jmax-1,1), max(kmax-1,1))`
5. **varnames**: list of the names of the `VARIABLES` list, `x`, `y` and `z` included (`['x', 'y', 'z', 'u', ...]`)

!!! note "Variable names"
    `read_TEC` takes the names from the `VARIABLES` list, which may span several lines and ends at the first
    `ZONE` record (the keyword as a whole word: a name such as `zone_velocity` is a name). As in the Fortran reader, each name may be in double quotes (`"rho(1)"`, as `write_TEC`
    writes them), in single quotes, or bare (`rho(1)`, as the Fortran writer of ORION 1.7.0 and later writes
    them), and the three forms may be mixed in one list. Names are separated by blanks, tabs or commas; a name
    in quotes may contain them. An unclosed quote raises `ValueError`.

!!! warning "Files that `read_TEC` does not read"
    `read_TEC` reads the form described above and nothing else: three coordinates, `BLOCK` packing, one value per line, the
    other variables at the cells. It checks the header of each zone and the number of its values, and raises a
    `ValueError` that names the file, the zone and what is wrong for any other form: a `POINT` zone, a zone without `J` or
    `K`, a file of one or two coordinates (its third variable at the cells), variables at the nodes (what the Fortran
    writer writes with `orion%tec%node` set, and the converter for a field whose variables are at the nodes), several
    values on a line, or a number of values that is not that of the zone. Read such files with the Fortran API, which
    reads every form (see [Supported Formats](supported-formats.md#tecplot-format)).

!!! note "Zone dimensions"
    `read_TEC` takes the dimensions of a zone from its `I=`, `J=` and `K=` keywords outside text in quotes
    (`I = 4` with blanks is read too), so numbers in a zone title (`T = "Block 1"`) are not dimensions. On a
    `ZONE` record, a value that is not a whole number, such as the `I=***` a Fortran writer leaves when the
    number does not fit its field, raises a `ValueError` that names the file, the zone and the keyword.

### Single Block Data

For single-block files:

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('single_block.tec')

# Access the data of the first (and only) block
x_coords = x[0]  # Shape: (imax, jmax, kmax), the nodes
y_coords = y[0]
z_coords = z[0]

# Access the first variable after x, y and z
first_var = var[0][0]  # Shape: (imax-1, jmax-1, kmax-1), the cells

print(f"Grid dimensions: {x_coords.shape}")
print(f"Variable name: {varnames[3]}")
```

### Multi-Block Data

For multi-block structured grids:

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('multiblock.tec')

nblocks = len(x)
print(f"Number of blocks: {nblocks}")

# Iterate over blocks
for iblock in range(nblocks):
    print(f"\nBlock {iblock + 1}:")
    print(f"  Dimensions: {x[iblock].shape}")
    print(f"  X range: [{x[iblock].min():.3f}, {x[iblock].max():.3f}]")
    print(f"  Y range: [{y[iblock].min():.3f}, {y[iblock].max():.3f}]")
    print(f"  Z range: [{z[iblock].min():.3f}, {z[iblock].max():.3f}]")

    # Access the variables of this block (their names follow x, y and z)
    for ivar, name in enumerate(varnames[3:]):
        data = var[iblock][ivar]
        print(f"  {name}: [{data.min():.3e}, {data.max():.3e}]")
```

## Reading PLOT3D Files

`read_P3D` reads a PLOT3D grid file whatever its layout, which it detects from the file itself, by the
rules of the Fortran reader `p3d_read_multiblock` (see [Supported Formats](supported-formats.md#plot3d-format)):
ASCII, Fortran unformatted or C binary; either byte order; 4- or 8-byte integers and reals; multi-grid or
single grid; 3D or 2D; with or without iblank; whole or planes.

```python
from ORION import read_P3D

xb, yb, zb = read_P3D('grid.x')

print(f"Blocks: {len(xb)}")
for b, x in enumerate(xb):
    print(f"Block {b+1}: {x.shape}")      # (I, J, K)
```

### Return Values

`read_P3D` returns a tuple of three lists, with one NumPy array (`float64`) per block:

1. **xb**: x-coordinates of each block, shape `(I, J, K)`, indexed `[i, j, k]` as in `read_TEC`
2. **yb**: y-coordinates
3. **zb**: z-coordinates; `None` for a 2D file, whose arrays have shape `(I, J, 1)`

Coordinates of 4-byte reals are returned as `float64`. The iblank array of a file that has one is read
past, as in the Fortran reader.

### Errors

The file is read only when exactly one PLOT3D layout matches its size (the count of its values for ASCII)
and, for a Fortran unformatted file, its records. Otherwise `read_P3D` raises `ValueError` with the reason:

```python
try:
    xb, yb, zb = read_P3D('grid.x')
except ValueError as e:
    print(e)    # grid.x: no PLOT3D layout matches the file: its Fortran records break at byte 985 (a truncated file?)
```

A file that cannot be opened raises `OSError` (`FileNotFoundError` for a missing file).

## Writing Tecplot Files

### Basic Usage

Use `write_TEC` to create Tecplot ASCII files, with the data in the form that `read_TEC` returns:

```python
from ORION import write_TEC
import numpy as np

# Prepare data: one entry per block
x = [x1]          # the x-coordinates of the nodes of each block, of shape (imax, jmax, kmax)
y = [y1]
z = [z1]
var = [[p1, t1]]  # for each block, its variables at the cells, of shape (imax-1, jmax-1, kmax-1)
varnames = ['x', 'y', 'z', 'pressure', 'temperature']  # every name of the file, the coordinates first

# Write file
write_TEC('output.tec', x, y, z, var, varnames)
```

`write_TEC` names the zones `Block1`, `Block2`, ...; without `varnames` it names the variables `x`, `y`, `z`, `v1`,
`v2`, ... It writes every value of the arrays it is given, for any shape of zone (a block, a plane, a line or a point of
nodes: a direction with one plane of nodes has one layer of cells), and raises a `ValueError`, before it writes anything,
when the arrays do not fit: coordinates of different shapes, a variable that is not of the shape of the cells of its
block, blocks with different numbers of variables, or a number of names that is not that of the coordinates and
variables.

### Creating Data from Scratch

Example: Generate a simple 3D grid with computed variables:

```python
import numpy as np
from ORION import write_TEC

# A grid of 10 x 10 x 10 nodes (9 x 9 x 9 cells)
nx, ny, nz = 10, 10, 10
xg, yg, zg = np.meshgrid(np.linspace(0, 1, nx), np.linspace(0, 1, ny), np.linspace(0, 1, nz), indexing='ij')

# The cell centres (for this uniform grid, the mean of two opposite corners of each cell)
xc = 0.5 * (xg[:-1, :-1, :-1] + xg[1:, 1:, 1:])
yc = 0.5 * (yg[:-1, :-1, :-1] + yg[1:, 1:, 1:])
zc = 0.5 * (zg[:-1, :-1, :-1] + zg[1:, 1:, 1:])

# Compute variables at the cells
pressure = np.sin(np.pi * xc) * np.cos(np.pi * yc)
temperature = xc**2 + yc**2 + zc**2

# Write one block
write_TEC('generated.tec', [xg], [yg], [zg], [[pressure, temperature]], ['x', 'y', 'z', 'pressure', 'temperature'])
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

# The first variable after x, y and z (assumed to be the pressure) on the middle layer of cells
iblock = 0
pressure = var[iblock][0]
k_mid = pressure.shape[2] // 2
pressure_slice = pressure[:, :, k_mid]

# The centres of the cells in i and j, from the node plane k_mid (the mean of the four corners)
xn, yn = x[iblock][:, :, k_mid], y[iblock][:, :, k_mid]
x_slice = 0.25 * (xn[:-1, :-1] + xn[1:, :-1] + xn[:-1, 1:] + xn[1:, 1:])
y_slice = 0.25 * (yn[:-1, :-1] + yn[1:, :-1] + yn[:-1, 1:] + yn[1:, 1:])

# Plot
plt.contourf(x_slice, y_slice, pressure_slice, levels=20)
plt.colorbar(label='Pressure')
plt.xlabel('X')
plt.ylabel('Y')
plt.title(f'{varnames[3]} at mid-plane')
plt.savefig('pressure_slice.png')
```

### Computing Derived Quantities

Calculate velocity magnitude:

```python
from ORION import read_TEC
import numpy as np

# Read velocity components
x, y, z, var, varnames = read_TEC('velocity_field.tec')

# Assuming varnames = ['x', 'y', 'z', 'u', 'v', 'w', 'pressure']: the variables of the first block
u = var[0][0]  # velocity_x
v = var[0][1]  # velocity_y
w = var[0][2]  # velocity_z

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

nblocks = len(x)

for iblock in range(nblocks):
    print(f"\n=== Block {iblock + 1} ===")

    # Compute volume (approximate)
    dx = np.diff(x[iblock], axis=0).mean()
    dy = np.diff(y[iblock], axis=1).mean()
    dz = np.diff(z[iblock], axis=2).mean()
    volume = dx * dy * dz * np.prod([n - 1 for n in x[iblock].shape])

    print(f"Approximate volume: {volume:.3e}")

    # Variable statistics
    for ivar, name in enumerate(varnames[3:]):
        data = var[iblock][ivar]
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

# Read data
x, y, z, var, varnames = read_TEC('data.tec')

# The values at the cells of the first block, one column per variable
iblock = 0
data_dict = {name: var[iblock][ivar].flatten() for ivar, name in enumerate(varnames[3:])}

# Create DataFrame
df = pd.DataFrame(data_dict)

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
xn, yn, zn = x[iblock], y[iblock], z[iblock]
# The centres of the cells along each axis of a rectilinear grid
xc = 0.5 * (xn[:-1, 0, 0] + xn[1:, 0, 0])
yc = 0.5 * (yn[0, :-1, 0] + yn[0, 1:, 0])
zc = 0.5 * (zn[0, 0, :-1] + zn[0, 0, 1:])

# Create interpolator for the first variable, at the cells
interp = RegularGridInterpolator((xc, yc, zc), var[iblock][0], method='linear')

# Interpolate to a finer grid, within the cell centres
x_fine = np.linspace(xc.min(), xc.max(), 50)
y_fine = np.linspace(yc.min(), yc.max(), 50)
z_fine = np.linspace(zc.min(), zc.max(), 50)

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

# The first layer of cells (k = 0): the centres of its cells in i and j, from the node plane k = 0
iblock = 0
xn, yn = x[iblock][:, :, 0], y[iblock][:, :, 0]
X = 0.25 * (xn[:-1, :-1] + xn[1:, :-1] + xn[:-1, 1:] + xn[1:, 1:])
Y = 0.25 * (yn[:-1, :-1] + yn[1:, :-1] + yn[:-1, 1:] + yn[1:, 1:])
Z = var[iblock][0][:, :, 0]  # Use first variable as height

# 3D surface plot
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(X, Y, Z, cmap='viridis')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel(varnames[3])
plt.colorbar(surf)
plt.savefig('surface_3d.png')
```

---