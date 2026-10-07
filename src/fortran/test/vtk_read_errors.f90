  program vtk_read_errors
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_read_structured_multiblock returns an error, and stops reading, at the first thing it cannot read: a .vtm file that does
  !> not exist, that has no block list or that lists no block file, a block file that cannot be opened, points or variable values
  !> that cannot be read; it does not go on with the coordinates or values it has not read. It closes each block file once read,
  !> also when it stops on an error in that file. The program writes a field of two 3-D blocks in the ascii, binary and raw
  !> formats and reads it back: as written (no error, every node and value as written: the control), with both block files
  !> closed after the read, and a second time in the same program; with a path to the block files that does not exist; from a
  !> .vtm that lists a third block file that does not exist; from a .vtm whose second block file holds a StructuredGrid without
  !> points, closed after the error. Then, in each format, from a .vtm whose block file is cut in the middle of the values of
  !> its last variable (an error, and the file closed after it), and once from a .vtm that does not exist, from one without a
  !> block list and from one whose block list holds no block file. These last reads come last: a reader that stops the program
  !> on them cannot hide the other checks. Exit status 0 when every check passes, 1 otherwise.
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
  do f = 1, size(formats)
    call read_cut(trim(formats(f)))
  enddo
  call broken_vtm

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

  logical function is_open(name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Whether the file is connected to a unit of this program.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file=name, opened=is_open)
  end function is_open

  logical function same_field(r, w)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Whether r holds the blocks of w, every node and value as in w.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: r, w
  integer                      :: b
  !---------------------------------------------------------------------------------------------------------------------------------

  same_field = allocated(r%block)
  if (same_field) same_field = size(r%block) == size(w%block)
  if (same_field) then
    do b = 1, size(w%block)
      same_field = same_field .and. allocated(r%block(b)%mesh) .and. allocated(r%block(b)%vars)
      if (same_field) same_field = all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
                                   all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
      if (same_field) same_field = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
    enddo
  endif
  end function same_field

  subroutine write_vtm(name, files)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a .vtm file that lists the block files given.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name, files(:)
  integer                      :: u, n
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=name//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  do n = 1, size(files)
    write(u,'(A,I0,A)') '      <DataSet index="', n - 1, '" file="'//trim(files(n))//'"/>'
  enddo
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  end subroutine write_vtm

  subroutine cut_copy(src, dst, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Copy the block file src, written in the format fmt, to dst cut in the middle of the values of its last variable, b: in the
  !< ascii and binary formats between the start tag and the end tag of its data array, in the raw format 80 bytes before the
  !< end tag of the appended data, inside the 4 + 18*8 bytes of b, the last array of the appended data.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: src, dst, fmt
  character(len=:), allocatable :: buf
  integer                       :: u, n, p1, p2, p3, keep
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=src, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  if (fmt == 'raw') then
    keep = index(buf, '</AppendedData>', back=.true.) - 80
  else
    p1 = index(buf, 'Name="b"', back=.true.)
    p2 = p1 + index(buf(p1:), '>')
    p3 = p2 + index(buf(p2:), '</DataArray>') - 1
    keep = (p2 + p3)/2
  endif
  open(newunit=u, file=dst, access='stream', form='unformatted', status='replace', action='write')
  write(u) buf(1:keep)
  close(u)
  end subroutine cut_copy

  subroutine write_and_read(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field in the format given, read it back as written, a second time, and from the broken forms.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w, r1, r2, r3, r4, r5
  character(len=16)            :: varnames
  character(len=64)            :: files(3)
  integer(I4P)                 :: b, i, j, k, err, u
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

  ! The control: the field as written, both block files closed after the read
  r1%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  call check(same_field(r1, w), fmt//': read back as written')
  call check(.not.is_open('errors_'//fmt//'_E1.vts') .and. .not.is_open('errors_'//fmt//'_E2.vts'), &
             fmt//': block files closed after the read')

  ! The same field read a second time in the same program
  r5%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r5, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0 .and. same_field(r5, w), fmt//': read a second time as written')

  ! A path to the block files that does not exist
  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='errors_'//fmt, vtspath='no_such_folder/')
  call check(err /= 0, fmt//': error for block files that do not exist')

  ! A .vtm that lists a third block file that does not exist
  files(1) = 'errors_'//fmt//'_E1.vts'
  files(2) = 'errors_'//fmt//'_E2.vts'
  files(3) = 'errors_'//fmt//'_E3.vts'
  call write_vtm('errors_missing_'//fmt, files)
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
  files(2) = 'errors_nopoints_'//fmt//'_E2.vts'
  call write_vtm('errors_nopoints_'//fmt, files(1:2))
  r4%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r4, vtmpath='errors_nopoints_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file without points')
  call check(.not.is_open('errors_nopoints_'//fmt//'_E2.vts'), fmt//': block file without points closed after the error')
  end subroutine write_and_read

  subroutine read_cut(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read from a .vtm whose only block file is the second block of the field written in the format given, cut in the middle of
  !< the values of its last variable.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: r
  character(len=64)            :: files(1)
  integer(I4P)                 :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  call cut_copy('errors_'//fmt//'_E2.vts', 'errors_cut_'//fmt//'_E2.vts', fmt)
  files(1) = 'errors_cut_'//fmt//'_E2.vts'
  call write_vtm('errors_cut_'//fmt, files)
  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='errors_cut_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file cut in the values of a variable')
  call check(.not.is_open('errors_cut_'//fmt//'_E2.vts'), fmt//': block file cut in its values closed after the error')
  end subroutine read_cut

  subroutine broken_vtm
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read from a .vtm that does not exist (a file of that name left by an earlier run is removed first), from one without a block
  !< list and from one whose block list holds no block file.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: r1, r2, r3
  character(len=64) :: none(0)
  integer(I4P)      :: err
  integer           :: u
  logical           :: there
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file='errors_no_such_file.vtm', exist=there)
  if (there) then
    open(newunit=u, file='errors_no_such_file.vtm')
    close(u, status='delete')
  endif
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='errors_no_such_file', vtspath='')
  call check(err /= 0, 'error for a .vtm that does not exist')

  open(newunit=u, file='errors_nolist.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='errors_nolist', vtspath='')
  call check(err /= 0, 'error for a .vtm without a block list')

  call write_vtm('errors_noblocks', none)
  err = vtk_read_structured_multiblock(orion=r3, vtmpath='errors_noblocks', vtspath='')
  call check(err /= 0, 'error for a .vtm that lists no block file')
  end subroutine broken_vtm
  endprogram vtk_read_errors
