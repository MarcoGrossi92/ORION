  program vtk_node_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_write_structured_multiblock and vtk_read_structured_multiblock do not handle variables at the nodes: the writer returns 1
  !> and writes nothing when orion%vtk%node is set and the blocks have variables, and the reader returns 1 for a block file whose
  !> variables are point data. Fields with cell variables, and meshes without variables, are written and read as before. The
  !> program checks, in the ascii, binary and raw formats: a field with variables and orion%vtk%node set (return value 1, no .vtm
  !> file written; a .vtm file of that name left by an earlier run is removed first); the same field with cell variables (return
  !> value 0, read back as written); a mesh without variables and orion%vtk%node set (return value 0, read back with its nodes);
  !> and it reads a block file with one variable as point data, 8 values on 8 nodes, written here in ascii (return value 1). Exit
  !> status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: checks = 0, failures = 0
  integer :: f

  do f = 1, size(formats)
    call write_and_read(trim(formats(f)))
  enddo
  call read_point_data

  write(*,'(A,I0,A,I0,A)') 'vtk_node_data: ', checks, ' checks, ', failures, ' failed'
  if (failures > 0) stop 1

  contains

  subroutine check(condition, message)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Count a check and report it when it fails.
  !---------------------------------------------------------------------------------------------------------------------------------
  logical,          intent(in) :: condition
  character(len=*), intent(in) :: message
  !---------------------------------------------------------------------------------------------------------------------------------

  checks = checks + 1
  if (.not.condition) then
    failures = failures + 1
    write(*,'(A)') '  FAIL: '//message
  endif
  end subroutine check

  subroutine field(o, vars)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< One 3-D block of 2 x 3 x 2 cells, with two cell variables when vars is true.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(inout) :: o
  logical,          intent(in)    :: vars
  integer(I4P)                    :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(o%block(1))
  o%block(1)%name = 'N1'
  o%block(1)%Ni = 2; o%block(1)%Nj = 3; o%block(1)%Nk = 2
  allocate(o%block(1)%mesh(1:3, 0:2, 0:3, 0:2))
  do k = 0, 2; do j = 0, 3; do i = 0, 2
    o%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P) + 0.5_R8P, real(k + 1, R8P)]
  enddo; enddo; enddo
  if (vars) then
    allocate(o%block(1)%vars(1:2, 1:2, 1:3, 1:2))
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      o%block(1)%vars(:,i,j,k) = [real(i + 10*j + 100*k, R8P), -1.5_R8P]
    enddo; enddo; enddo
  endif
  end subroutine field

  subroutine write_and_read(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The writes and reads of one format.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w1, w2, w3, r2, r3
  character(len=16)            :: varnames
  integer(I4P)                 :: err
  logical                      :: there
  integer                      :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  ! Variables at the nodes: refused. A .vtm file left here by an earlier run would hide a write: it is removed first
  open(newunit=u, file='node_'//fmt//'.vtm')
  close(u, status='delete')
  call field(w1, .true.)
  w1%vtk%format = fmt
  w1%vtk%node = .true.
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w1, vtspath='node_'//fmt//'_', vtmpath='node_'//fmt, varnames=varnames)
  call check(err == 1, fmt//': variables at the nodes refused by the writer (return value 1)')
  inquire(file='node_'//fmt//'.vtm', exist=there)
  call check(.not.there, fmt//': no .vtm file written for variables at the nodes')

  ! Cell variables: written and read back as before
  call field(w2, .true.)
  w2%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w2, vtspath='cell_'//fmt//'_', vtmpath='cell_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': cell variables written')
  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='cell_'//fmt, vtspath='')
  call check(err == 0, fmt//': cell variables read back')
  if (allocated(r2%block)) then
    call check(all(shape(r2%block(1)%vars) == [2, 2, 3, 2]), fmt//': cell variables read back with their shape')
    if (all(shape(r2%block(1)%vars) == [2, 2, 3, 2])) &
      call check(all(r2%block(1)%vars == w2%block(1)%vars), fmt//': cell variables read back as written')
  endif

  ! A mesh without variables, orion%vtk%node set: written and read back as before
  call field(w3, .false.)
  w3%vtk%format = fmt
  w3%vtk%node = .true.
  varnames = ''
  err = vtk_write_structured_multiblock(orion=w3, vtspath='mesh_'//fmt//'_', vtmpath='mesh_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': mesh without variables written with orion%vtk%node set')
  r3%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r3, vtmpath='mesh_'//fmt, vtspath='')
  call check(err == 0, fmt//': mesh without variables read back')
  if (allocated(r3%block)) then
    call check(all(shape(r3%block(1)%mesh) == [3, 3, 4, 3]), fmt//': mesh read back with its nodes')
    if (all(shape(r3%block(1)%mesh) == [3, 3, 4, 3])) &
      call check(all(r3%block(1)%mesh == w3%block(1)%mesh), fmt//': nodes read back as written')
  endif
  end subroutine write_and_read

  subroutine read_point_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read a block file with one variable as point data: refused.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: r
  integer(I4P)     :: u, err
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file='pointdata_P1.vts', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '    <Piece Extent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '      <Points>'
  write(u,'(A)') '        <DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
  write(u,'(A)') '          0 0 1 1 0 1 0 1 1 1 1 1 0 0 2 1 0 2 0 1 2 1 1 2'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '      </Points>'
  write(u,'(A)') '      <PointData>'
  write(u,'(A)') '        <DataArray type="Float64" Name="a" NumberOfComponents="1" format="ascii">'
  write(u,'(A)') '          1 2 3 4 5 6 7 8'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '      </PointData>'
  write(u,'(A)') '    </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  open(newunit=u, file='pointdata.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="pointdata_P1.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='pointdata', vtspath='')
  call check(err == 1, 'a block file with point data refused by the reader (return value 1)')
  end subroutine read_point_data
  endprogram vtk_node_data
