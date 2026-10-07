  program vtk_one_dimensional
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_write_structured_multiblock and vtk_read_structured_multiblock handle meshes of one, two and three coordinates. A mesh of
  !> one coordinate (mesh(1:1,0:Ni,0:0,0:0), as the Tecplot readers return a file of lines) is written as two lines of nodes in j
  !> and two planes in k, with y = z = 0, one layer of cells in j and k, as a 2-D mesh is written as two planes of nodes in k with
  !> z = 0; the reader takes that form back as a mesh of one coordinate. A mesh of another number of coordinates is refused. For
  !> the ascii, binary and raw formats and fields of one block of 1 and 3 cells with two cell variables, the program writes the
  !> field, checks the extent and the points of the block file (y and z 0), and reads it back: one coordinate, every node and
  !> every value as written. It also writes a mesh of four coordinates (return value 1, no .vtm file written; a .vtm file of that
  !> name left by an earlier run is removed first). Every value is an integer or a half. Exit status 0 when every check passes, 1
  !> otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: cells(2) = [1, 3]
  integer :: checks = 0, failures = 0
  integer :: f, n

  do f = 1, size(formats)
    do n = 1, size(cells)
      call write_and_read(trim(formats(f)), cells(n))
    enddo
  enddo
  call four_coordinates

  write(*,'(A,I0,A,I0,A)') 'vtk_one_dimensional: ', checks, ' checks, ', failures, ' failed'
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

  subroutine write_and_read(fmt, ni)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of one block of ni cells, mesh of one coordinate, check its block file and read it back.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fmt
  integer(I4P),     intent(in)  :: ni
  type(orion_data)              :: w, r
  character(len=16)             :: varnames
  character(len=32)             :: tag
  character(len=:), allocatable :: buf
  integer(I4P)                  :: i, err, ext(6)
  integer                       :: u, n, p, q, ios
  logical                       :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = 'line_'//fmt//'_'//trim(str(.true., ni))
  allocate(w%block(1))
  w%block(1)%name = 'L'
  w%block(1)%Ni = ni; w%block(1)%Nj = 0; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:1, 0:ni, 0:0, 0:0), w%block(1)%vars(1:2, 1:ni, 1:1, 1:1))
  do i = 0, ni
    w%block(1)%mesh(1,i,0,0) = real(i, R8P) + 0.5_R8P
  enddo
  do i = 1, ni
    w%block(1)%vars(:,i,1,1) = [real(10*i, R8P) + 0.5_R8P, -real(i, R8P)]
  enddo
  w%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath=trim(tag)//'_', vtmpath=trim(tag), varnames=varnames)
  call check(err == 0, trim(tag)//': written without error')

  ! The block file: extent 0..ni, 0..1, 0..1 (two lines of nodes in j, two planes in k)
  open(newunit=u, file=trim(tag)//'_L.vts', access='stream', form='unformatted', status='old', action='read', iostat=ios)
  call check(ios == 0, trim(tag)//': block file written')
  if (ios == 0) then
    inquire(unit=u, size=n)
    allocate(character(len=n) :: buf)
    read(u) buf
    close(u)
    ext = -1
    p = index(buf, 'WholeExtent="')
    if (p > 0) then
      p = p + len('WholeExtent="')
      q = index(buf(p:), '"') + p - 2
      read(buf(p:q), *, iostat=ios) ext
    endif
    call check(all(ext == [0, ni, 0, 1, 0, 1]), trim(tag)//': extent 0 '//trim(str(.true., ni))//' 0 1 0 1')
  endif

  ! Read back: a mesh of one coordinate, the nodes and the values as written
  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(tag), vtspath='')
  call check(err == 0, trim(tag)//': read back without error')
  same = .false.
  if (err == 0 .and. allocated(r%block)) then
    if (size(r%block) == 1 .and. allocated(r%block(1)%mesh) .and. allocated(r%block(1)%vars)) then
      call check(size(r%block(1)%mesh,1) == 1, trim(tag)//': read back with one coordinate')
      same = all(shape(r%block(1)%mesh) == shape(w%block(1)%mesh)) .and. all(shape(r%block(1)%vars) == shape(w%block(1)%vars))
      if (same) same = all(r%block(1)%mesh == w%block(1)%mesh) .and. all(r%block(1)%vars == w%block(1)%vars)
    endif
  endif
  call check(same, trim(tag)//': nodes and values read back as written')
  end subroutine write_and_read

  subroutine four_coordinates
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A mesh of four coordinates is refused: return value 1 and no .vtm file.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: w
  character(len=16) :: varnames
  integer(I4P)      :: err
  integer           :: u
  logical           :: there
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file='line_four.vtm')
  close(u, status='delete')
  allocate(w%block(1))
  w%block(1)%name = 'F'
  w%block(1)%Ni = 1; w%block(1)%Nj = 1; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(1:4, 0:1, 0:1, 0:1), w%block(1)%vars(1:1, 1:1, 1:1, 1:1))
  w%block(1)%mesh = 1.0_R8P; w%block(1)%vars = 2.0_R8P
  w%vtk%format = 'ascii'
  varnames = 'a'
  err = vtk_write_structured_multiblock(orion=w, vtspath='line_four_', vtmpath='line_four', varnames=varnames)
  call check(err == 1, 'four coordinates: refused (return value 1)')
  inquire(file='line_four.vtm', exist=there)
  call check(.not.there, 'four coordinates: no .vtm file written')
  end subroutine four_coordinates
  endprogram vtk_one_dimensional
