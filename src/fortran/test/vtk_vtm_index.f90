  program vtk_vtm_index
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_write_structured_multiblock lists the block files in the .vtm file as the DataSet elements of one block, with the
  !> indices 0 to nb-1 in the order of the blocks: a reader places each DataSet at its index, so two DataSet elements with the
  !> same index cannot both be read. For fields of 1, 2 and 17 blocks in the ascii, binary and raw formats the program writes the
  !> field, reads the .vtm file as text and checks the index and the file of each DataSet element, and reads the field back with
  !> vtk_read_structured_multiblock (every block, node and value as written). Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: sizes(3) = [1, 2, 17]
  integer :: checks = 0, failures = 0
  integer :: f, n

  do f = 1, size(formats)
    do n = 1, size(sizes)
      call write_and_read(trim(formats(f)), sizes(n))
    enddo
  enddo

  write(*,'(A,I0,A,I0,A)') 'vtk_vtm_index: ', checks, ' checks, ', failures, ' failed'
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

  subroutine write_and_read(fmt, nb)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of nb blocks in the format given, check the DataSet elements of its .vtm file and read it back.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fmt
  integer(I4P),     intent(in)  :: nb
  type(orion_data)              :: w, r
  character(len=16)             :: varnames
  character(len=64)             :: tag, expected
  character(len=:), allocatable :: buf
  integer(I4P)                  :: b, i, j, k, err
  integer                       :: u, n, p, q, ndata, index_value, ios
  logical                       :: indices_ok, files_ok, same
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = 'vtmindex_'//fmt//'_'//trim(str(.true., nb))
  allocate(w%block(nb))
  do b = 1, nb
    w%block(b)%name = 'X'//trim(str(.true., b))
    w%block(b)%Ni = 1; w%block(b)%Nj = 2; w%block(b)%Nk = 1
    allocate(w%block(b)%mesh(1:3, 0:1, 0:2, 0:1), w%block(b)%vars(1:1, 1:1, 1:2, 1:1))
    do k = 0, 1; do j = 0, 2; do i = 0, 1
      w%block(b)%mesh(:,i,j,k) = [real(i + 2*b, R8P), real(j, R8P), real(k + 1, R8P)]
    enddo; enddo; enddo
    w%block(b)%vars(1,1,:,1) = [real(b, R8P) + 0.5_R8P, -real(b, R8P)]
  enddo
  w%vtk%format = fmt
  varnames = 'a'
  err = vtk_write_structured_multiblock(orion=w, vtspath=trim(tag)//'_', vtmpath=trim(tag), varnames=varnames)
  call check(err == 0, trim(tag)//': written without error')

  ! The DataSet elements of the .vtm file: index 0 to nb-1, in the order of the blocks, and their files
  open(newunit=u, file=trim(tag)//'.vtm', access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  ndata = 0; indices_ok = .true.; files_ok = .true.
  p = index(buf, '<DataSet ')
  do while (p > 0)
    ndata = ndata + 1
    q = index(buf(p:), 'index="') + p - 1 + len('index="')
    read(buf(q:q+index(buf(q:), '"')-2), *, iostat=ios) index_value
    indices_ok = indices_ok .and. ios == 0 .and. index_value == ndata - 1
    q = index(buf(p:), 'file="') + p - 1 + len('file="')
    expected = trim(tag)//'_X'//trim(str(.true., int(ndata, I4P)))//'.vts'
    files_ok = files_ok .and. buf(q:q+index(buf(q:), '"')-2) == trim(expected)
    n = index(buf(p+1:), '<DataSet ')
    if (n > 0) then
      p = p + n
    else
      p = 0
    endif
  enddo
  call check(ndata == nb, trim(tag)//': '//trim(str(.true., int(ndata, I4P)))//' DataSet elements, '// &
             trim(str(.true., nb))//' expected')
  call check(indices_ok, trim(tag)//': DataSet indices 0 to '//trim(str(.true., nb-1))//' in the order of the blocks')
  call check(files_ok, trim(tag)//': DataSet files in the order of the blocks')

  ! The field read back
  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(tag), vtspath='')
  call check(err == 0, trim(tag)//': read back without error')
  same = .false.
  if (err == 0 .and. allocated(r%block)) then
    same = size(r%block) == nb
    if (same) then
      do b = 1, nb
        same = same .and. all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
               all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
        if (same) same = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
      enddo
    endif
  endif
  call check(same, trim(tag)//': read back as written')
  end subroutine write_and_read
  endprogram vtk_vtm_index
