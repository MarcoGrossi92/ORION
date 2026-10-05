# Fortran API

The Fortran API provides a high-performance interface for reading and writing scientific data in multiple formats. This guide covers the core concepts and usage patterns.

## Core Concepts

### The orion_data Type

All I/O operations use the `orion_data` derived type, which encapsulates all simulation data:

```fortran
use Lib_ORION_data

type(orion_data) :: IOfield
```

**Key Components:**

- `varnames(:)` - Array of variable name strings (allocatable)
- `solutiontime` - Solution time (real)
- `block(:)` - Array of computational blocks (allocatable)
- `tec` - Tecplot format options
- `vtk` - VTK format options
- `p3d` - PLOT3D format options
- `strandid` - STRANDID of the last zone read from a Tecplot ASCII file (integer, -1 when absent)

### The obj_block Type

Each block contains mesh and solution data:

```fortran
type :: obj_block
  character(len=128) :: name        ! Block name
  integer :: Ni, Nj, Nk             ! Dimensions (I, J, K)
  real(R8P), allocatable :: mesh(:,:,:,:)  ! Coordinates (Ni, Nj, Nk, 3)
  real(R8P), allocatable :: vars(:,:,:,:)  ! Variables (Ni, Nj, Nk, nvars)
end type
```

**Block structure:**
- `mesh(1,i,j,k)` - X coordinates
- `mesh(2,i,j,k)` - Y coordinates
- `mesh(3,i,j,k)` - Z coordinates
- `vars(n,i,j,k)` - nth solution variable

### Format Options

Each format has specific options:

**Tecplot (`Type_tec_Format`):**
```fortran
character(6) :: extension  ! File extension (default: '.tec')
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
logical :: node            ! Node or cell data location (default: .false.)
logical :: bc              ! Save boundary conditions (default: .false.)
logical :: double          ! Binary files (.plt/.szplt): .true. = 64-bit values, .false. = 32-bit (default: .true.)
```

`double` sets the precision of the values stored in a binary Tecplot file (TecIO `VIsDouble`); the data are
always handed to TecIO as double precision. A program that writes binary files and needs a given precision sets
the option before the write; otherwise its files follow this default. ASCII files are not affected.

**VTK (`Type_vtk_Format`):**
```fortran
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
logical :: node            ! Node or cell data location (default: .false.)
```

**PLOT3D (`Type_p3d_Format`):**
```fortran
character(6) :: format     ! 'binary' or 'ascii' (default: 'binary')
```

### Error Handling

All I/O functions return an integer error code:

```fortran
integer :: E_IO

E_IO = tec_read_structured_multiblock(filename='data.tec', orion=IOfield)

if (E_IO == 0) then
  print *, 'Success!'
else
  print *, 'Error occurred, code:', E_IO
endif
```

**Error Codes:**

- `0`: Success
- Non-zero: Error (specific codes vary by function)

## Format Modules

ORION provides three format-specific modules:

```fortran
use Lib_Tecplot ! For Tecplot ASCII and binary formats:
```
```fortran
use Lib_VTK ! For VTK ASCII and binary formats
```
```fortran
use Lib_PLOT3D ! For NASA PLOT3D grid files
```

## Reading Data

**Tecplot Files**

ASCII Format

```fortran
program read_tecplot_ascii
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO, iblock, nblocks, nvars
  
  ! Read file
  E_IO = tec_read_structured_multiblock(filename='solution.tec', orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read file'
    nblocks = size(IOfield%block)
    print *, 'Number of blocks:', nblocks
    print *, 'Variables:', IOfield%varnames
    
    ! Access block 1 information
    print *, 'Block 1 name:', trim(IOfield%block(1)%name)
    print *, 'Block 1 dimensions:', IOfield%block(1)%Ni, IOfield%block(1)%Nj, IOfield%block(1)%Nk
    
    ! Access mesh coordinates
    print *, 'X range:', minval(IOfield%block(1)%mesh(1,:,:,:)), &
                        maxval(IOfield%block(1)%mesh(1,:,:,:))
  endif
end program read_tecplot_ascii
```

Binary Format (with TecIO)

For `.plt` or `.szplt` files, use the same function—ORION auto-detects the format:

```fortran
E_IO = tec_read_structured_multiblock(filename='solution.plt', orion=IOfield)
```

!!! note "TecIO Requirement"
    Binary format support requires building with `--use-tecio` flag.

### VTK Files

```fortran
program read_vtk_structured
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Read VTK structured grid
  E_IO = vtk_read_file(filename='mesh.vtk', orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read VTK file'
    print *, 'Number of blocks:', size(IOfield%block)
    print *, 'Block 1 points:', IOfield%block(1)%Ni * &
                                 IOfield%block(1)%Nj * &
                                 IOfield%block(1)%Nk
  endif
end program read_vtk_structured
```

### Dimensions of what a reader returns

- **Tecplot ASCII, structured zones.** A zone header gives `I`, `J` and `K`; a header without `K` is one node
  plane (`K = 1`, Tecplot's default). When the first zone has `K = 1` the file is read as two-dimensional:
  `mesh` holds two coordinates (`mesh(1:2,...)`) and every block has `Nk = 0`; all zones of a file must then
  have `K = 1`. A plane of a three-dimensional field (a slice) is read with three coordinates instead: when
  the first three variables are named `x`, `y` and `z` (in any case) and `z` is nodal in every zone (not listed
  as `CELLCENTERED` in `VARLOCATION`), `mesh` holds `x`, `y` and `z` (`mesh(1:3,...)`, still one node plane,
  `Nk = 0`) and the solution variables start after `z`; all zones must again have `K = 1`. A file whose third
  variable has another name, or is cell-centred, is read as two-dimensional as above. The rule goes by name:
  a two-dimensional file whose third variable is nodal and named `z` (a mixture fraction `Z`, for example) is
  read as a slice, so rename that variable or write it cell-centred to read the file as two-dimensional.
  `varnames` holds the coordinate names followed by the variable names, so
  `size(varnames) = size(mesh,1) + size(vars,1)`.
- **Tecplot binary (`.szplt`, TecIO builds).** Each zone is read with the dimension of its node counts: a zone
  with one node plane (`K = 1`) has two coordinates (one when also `J = 1`) and `Nk = 0`, a volume zone three.
  A slice is read with three coordinates by the rule of the ASCII reader: when the first three variables are
  named `x`, `y` and `z` (in any case) and `z` is nodal in every zone, a zone with `K = 1` holds `x`, `y` and
  `z` (`mesh(1:3,...)`, `Nk = 0`) and the solution variables start after `z`. A file whose third variable has
  another name, or is cell-centred, is read with two coordinates; as in the ASCII reader, a two-dimensional
  file whose third variable is nodal and named `z` is read as a slice. `varnames` is filled with the names
  stored in the file, with the same convention as the ASCII reader (coordinates first).
- **PLOT3D grids.** The first dimensions record decides the dimension of the whole file: two integers
  (`Ni Nj`) give a two-dimensional grid (two coordinates, `Nk = 0`), three integers (`Ni Nj Nk`) a
  three-dimensional one. Each block has its own dimensions record; a node count below 1 is refused.
- **Solution time.** `solutiontime` is 0 in a new `orion_data`. The Tecplot ASCII reader stores the
  `SOLUTIONTIME` of the file, or -10 when its zone headers give none. The `.szplt` reader does not store the
  zone time of the file and leaves `solutiontime` as it was (0 for a new object); so does the PLOT3D reader,
  since a PLOT3D grid has no time.
- **Steady solutions and `strandid`.** `tec_write_structured_multiblock` treats a negative `solutiontime` as the
  mark of a steady solution (for instance minus the iteration count of the run): it writes the absolute value
  as `SOLUTIONTIME` and, in an ASCII file, adds `STRANDID = 0` (a static zone) to every zone header. The ASCII
  reader returns `SOLUTIONTIME` as written, so `solutiontime` is `|t|` for such a file, and stores the `STRANDID`
  of the last zone header in `strandid`: 0 for a steady file written by ORION, -1 when the header has none.
  A program that must know whether a file holds a steady solution tests `strandid == 0`. `strandid` is -1 in a
  new `orion_data` and only this reader sets it: the `.szplt`, VTK and PLOT3D readers and
  `tec_read_points_multivars` leave it as it was, and `copyORION` does not copy it (nor `solutiontime`). Binary
  files do not mark steady solutions: the binary writer stores StrandID 0 for every zone.

### Length of the Tecplot header and lines

- **VARIABLES header.** `tec_write_structured_multiblock` and `tec_write_points_multivars` build the list of
  variable names (the `VARIABLES` line of an ASCII file, the list handed to TecIO for `.plt` and `.szplt`) in
  a buffer of 32768 characters, and the ASCII reader reads up to 32768 characters of header: the `VARIABLES`
  line, the lines before it and its continuation lines, joined. A longer list is cut without a message.
- **Number of names.** The ASCII reader keeps at most 512 names, coordinates included. A header with more
  names is refused: the reader returns `err /= 0` and prints `no variables found in VARIABLES header`. The
  `.szplt` reader takes the names one by one from the file and has no such limit. Every name is kept to 32
  characters (`varnames` is `character(len=32)`); the ASCII reader warns when it shortens one, the `.szplt`
  reader shortens it without a message.
- **Zone headers and data lines.** The ASCII reader reads each zone-header line and each data line into a
  buffer of 1000 characters, and joins the lines of a zone header in another buffer of 1000 characters: each
  data line, and each zone header with all its lines joined, must fit in 1000 characters.
  `tec_write_structured_multiblock` writes one value per line; `tec_write_points_multivars` writes one point
  per line, with all its variables on that line.

### PLOT3D Files

PLOT3D typically uses separate grid and solution files. ORION is designed to handle just grids.

```fortran
program read_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Read both grid and solution files
  E_IO = plot3d_read(filename='grid.p3d',orion=IOfield)
  
  if (E_IO == 0) then
    print *, 'Successfully read PLOT3D files'
    print *, 'Number of blocks:', size(IOfield%block)
  endif
end program read_plot3d
```

## Writing Data

**Tecplot Files**

ASCII Format

```fortran
program write_tecplot
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO
  
  ! Assume IOfield is populated with data
  ! IOfield%block(:)%mesh contains coordinates
  ! IOfield%block(:)%vars contains solution variables
  
  ! Set format options
  IOfield%tec%format = 'ascii'  ! or 'binary'
  IOfield%tec%extension = '.tec'
  IOfield%tec%node = .false.     ! Node-centered data
  
  ! Write Tecplot ASCII file
  E_IO = tec_write_structured_multiblock( &
           orion=IOfield, &
           filename='output.tec')
  
  if (E_IO /= 0) then
    print *, 'Error writing file'
  endif
end program write_tecplot
```

!!! tip "Variable Names"
    Variable names are stored in the `varnames` array. Each element is a string containing the variable name.

Binary Format

```fortran
! Same function, different format option
IOfield%tec%format = 'binary'
IOfield%tec%extension = '.plt'

E_IO = tec_write_structured_multiblock( &
         orion=IOfield, &
         filename='output.szplt')
```

### Format Options

Control Tecplot output with format options:

```fortran
! ASCII output, node-centered, with boundary conditions
IOfield%tec%format = 'ascii'
IOfield%tec%extension = '.tec'
IOfield%tec%node = .true.   ! Node-centered (vs cell-centered)
IOfield%tec%bc = .true.     ! Include boundary condition cells

! Binary output
IOfield%tec%format = 'binary'
IOfield%tec%extension = '.plt'
IOfield%tec%node = .false.
IOfield%tec%bc = .false.
```

### VTK Files

```fortran
program write_vtk
  use Lib_ORION_data
  use Lib_VTK
  implicit none
  
  type(ORION_data) :: IOfield
  integer :: E_IO
  
  ! Write VTK structured multiblock
  E_IO = vtk_write_structured_multiblock( &
           orion=IOfield, &
           varnames='density,momentum_x,momentum_y,momentum_z,energy', &
           filename='output.vtk')
end program write_vtk
```

### PLOT3D Files

```fortran
program write_plot3d
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none
  
  type(ORION_data) :: IOfield
  integer :: E_IO
  
  ! Write grid and solution files
  E_IO = plot3d_write(filename='output_grid.p3d',orion=IOfield)

end program write_plot3d
```

## Multi-Block Data

ORION natively supports multi-block structured grids:

```fortran
program multiblock_example
  use Lib_ORION_data
  use Lib_Tecplot
  implicit none
  
  type(orion_data) :: IOfield
  integer :: E_IO, iblock, nblocks
  
  ! Read multi-block file
  E_IO = tec_read_structured_multiblock(filename='multiblock.tec', orion=IOfield)
  
  ! Get number of blocks
  nblocks = size(IOfield%block)
  
  ! Iterate over blocks
  do iblock = 1, nblocks
    print *, 'Block', iblock, ':', trim(IOfield%block(iblock)%name)
    print *, '  Dimensions (I,J,K):', IOfield%block(iblock)%Ni, &
                                       IOfield%block(iblock)%Nj, &
                                       IOfield%block(iblock)%Nk
    
    ! Access mesh coordinates
    print *, '  X range:', minval(IOfield%block(iblock)%mesh(1,:,:,:)), &
                            maxval(IOfield%block(iblock)%mesh(1,:,:,:))
    
    ! Access solution variables (if present)
    if (allocated(IOfield%block(iblock)%vars)) then
      print *, '  Number of variables:', size(IOfield%block(iblock)%vars, 4)
    endif
  enddo
end program multiblock_example
```

## Data Manipulation

### Accessing Mesh Coordinates

```fortran
! For block 'iblock'
real(R8P), allocatable :: x(:,:,:), y(:,:,:), z(:,:,:)
integer :: Ni, Nj, Nk

! Get dimensions
Ni = IOfield%block(iblock)%Ni
Nj = IOfield%block(iblock)%Nj
Nk = IOfield%block(iblock)%Nk

! Allocate and extract coordinates
allocate(x(Ni, Nj, Nk))
allocate(y(Ni, Nj, Nk))
allocate(z(Ni, Nj, Nk))

x = IOfield%block(iblock)%mesh(1,:,:,:)
y = IOfield%block(iblock)%mesh(2,:,:,:)
z = IOfield%block(iblock)%mesh(3,:,:,:)
```

### Accessing Solution Variables

```fortran
! Variables are stored as vars(i, j, k, var_index)
real(R8P), allocatable :: variables(:,:,:,:)
integer :: nvars

! Extract all variables for a block
variables = IOfield%block(iblock)%vars
nvars = size(variables, 4)

! Extract specific variable (e.g., pressure at index 4)
real(R8P), allocatable :: pressure(:,:,:)
allocate(pressure(Ni, Nj, Nk))
pressure = IOfield%block(iblock)%vars(4,:,:,:)
```

## Best Practices

1. **Always check error codes**: Don't assume I/O operations succeed
2. **Deallocate memory**: Free `IOfield` components when done
3. **Match variable names**: Ensure `varnames` matches actual data
4. **Use appropriate formats**: Choose format based on downstream tool requirements
5. **Test with small datasets**: Verify logic before scaling up

## Tests and Examples

For full examples, refers to the `src/test/` folder.

---