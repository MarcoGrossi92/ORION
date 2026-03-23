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
```

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