  program vtk_read_errors
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_read_structured_multiblock returns an error, and stops reading, when a block file cannot be opened or holds no mesh it
  !> can read; it does not go on with the coordinates it has not read. The program writes a field of two 3-D blocks in the ascii,
  !> binary and raw formats and reads it back four ways: as written (no error, every node and value as written: the control);
  !> with a path to the block files that does not exist; from a .vtm that lists a third block file that does not exist; and from a
  !> .vtm whose second block file holds a StructuredGrid without points. Exit status 0 when every check passes, 1 otherwise.
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

  write(*,'(A,I0,A,I0,A)') 'vtk_read_errors: ', checks, ' checks, ', failures, ' failed'
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

  subroutine write_and_read(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field in the format given, and read it back as written and from the three broken forms.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w, r1, r2, r3, r4
  character(len=16)            :: varnames
  integer(I4P)                 :: b, i, j, k, err, u
  logical                      :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'E'//trim(str(.true., b))
    w%block(b)%Ni = 2; w%block(b)%Nj = 3; w%block(b)%Nk = 1 + b
    allocate(w%block(b)%mesh(1:3, 0:2, 0:3, 0:1+b), w%block(b)%vars(1:2, 1:2, 1:3, 1:1+b))
    do k = 0, 1 + b; do j = 0, 3; do i = 0, 2
      w%block(b)%mesh(:,i,j,k) = [real(i + 10*b, R8P), real(j, R8P), real(k, R8P) + 0.5_R8P]
    enddo; enddo; enddo
    do k = 1, 1 + b; do j = 1, 3; do i = 1, 2
      w%block(b)%vars(:,i,j,k) = [real(i + 10*j + 100*k, R8P), -real(b, R8P)]
    enddo; enddo; enddo
  enddo
  w%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath='errors_'//fmt//'_', vtmpath='errors_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': written without error')

  ! The control: the field as written
  r1%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  same = size(r1%block) == 2
  if (same) then
    do b = 1, 2
      same = same .and. all(shape(r1%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
             all(shape(r1%block(b)%vars) == shape(w%block(b)%vars))
      if (same) same = all(r1%block(b)%mesh == w%block(b)%mesh) .and. all(r1%block(b)%vars == w%block(b)%vars)
    enddo
  endif
  call check(same, fmt//': read back as written')

  ! A path to the block files that does not exist
  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='errors_'//fmt, vtspath='no_such_folder/')
  call check(err /= 0, fmt//': error for block files that do not exist')

  ! A .vtm that lists a third block file that does not exist
  open(newunit=u, file='errors_missing_'//fmt//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="errors_'//fmt//'_E1.vts"/>'
  write(u,'(A)') '      <DataSet index="1" file="errors_'//fmt//'_E2.vts"/>'
  write(u,'(A)') '      <DataSet index="2" file="errors_'//fmt//'_E3.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  r3%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r3, vtmpath='errors_missing_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file listed in the .vtm that does not exist')

  ! A .vtm whose second block file holds a StructuredGrid without points
  open(newunit=u, file='errors_nopoints_'//fmt//'_E2.vts', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="+0 +2 +0 +3 +0 +3">'
  write(u,'(A)') '    <Piece Extent="+0 +2 +0 +3 +0 +3">'
  write(u,'(A)') '    </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  open(newunit=u, file='errors_nopoints_'//fmt//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="errors_'//fmt//'_E1.vts"/>'
  write(u,'(A)') '      <DataSet index="1" file="errors_nopoints_'//fmt//'_E2.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  r4%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r4, vtmpath='errors_nopoints_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file without points')
  end subroutine write_and_read
  endprogram vtk_read_errors
