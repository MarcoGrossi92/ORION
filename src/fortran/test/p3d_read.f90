program p3d_read
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing p3d_read_multiblock on the grids that p3d_write writes (mesh.p3d, two blocks of 10 x 10 x 10 and
  !> 20 x 10 x 10 cells; mesh2D.p3d, two blocks of 10 x 10 and 20 x 10 cells), and on files of its own. Each subroutine says what
  !> it checks, the change of main it checks and its basis. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_PLOT3D
  use Lib_ORION_data
  implicit none
  integer :: checks = 0, failures = 0
  !---------------------------------------------------------------------------------------------------------------------------------

  call grids_of_p3d_write
  call counts_refused
  call read_again

  write(*,'(A,I0,A,I0,A)') 'p3d_read: ', checks, ' checks, ', failures, ' failed'
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
  else
    write(*,'(A)') '  ok    '//message
  endif
  end subroutine check

  subroutine grids_of_p3d_write
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The two grids of p3d_write read back: two blocks each, the number of coordinates (3, then 2), the cells of each block and,
  !< for the 3-D grid, every coordinate (x = i in the first block and 10 + i in the second, y = j, z = k).
  !< Change of main checked (3ee1522): the number of coordinates is taken from the first dimensions record by a probe that
  !< does not depend on the compiler; before it, gfortran read the 3-D grid with two coordinates and Nk = 0.
  !< Basis: P, by construction: the grids are those that p3d_write writes.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, parameter :: ni(2) = [10, 20]
  type(orion_data) :: d
  integer          :: err, b, i, j, k
  logical          :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  d%p3d%format = 'ascii'
  err = p3d_read_multiblock(orion=d, filename='mesh.p3d')
  call check(err == 0, 'mesh.p3d: read without error')
  call check(allocated(d%block), 'mesh.p3d: blocks read')
  if (.not.allocated(d%block)) return
  call check(size(d%block) == 2, 'mesh.p3d: two blocks')
  if (size(d%block) /= 2) return
  do b = 1, 2
    call check(d%block(b)%Ni == ni(b) .and. d%block(b)%Nj == 10 .and. d%block(b)%Nk == 10, &
               'mesh.p3d: the cells of block '//trim(str(no_sign=.true., n=b)))
    call check(size(d%block(b)%mesh, 1) == 3, 'mesh.p3d: three coordinates in block '//trim(str(no_sign=.true., n=b)))
    if (size(d%block(b)%mesh, 1) /= 3 .or. d%block(b)%Ni /= ni(b) .or. d%block(b)%Nj /= 10 .or. d%block(b)%Nk /= 10) cycle
    same = .true.
    do k = 0, 10; do j = 0, 10; do i = 0, ni(b)
      same = same .and. d%block(b)%mesh(1,i,j,k) == real(10*(b-1) + i, R8P) .and. &
             d%block(b)%mesh(2,i,j,k) == real(j, R8P) .and. d%block(b)%mesh(3,i,j,k) == real(k, R8P)
    enddo; enddo; enddo
    call check(same, 'mesh.p3d: the coordinates of block '//trim(str(no_sign=.true., n=b)))
  enddo
  deallocate(d%block)
  err = p3d_read_multiblock(orion=d, filename='mesh2D.p3d')
  call check(err == 0, 'mesh2D.p3d: read without error')
  if (.not.allocated(d%block)) return
  call check(size(d%block) == 2, 'mesh2D.p3d: two blocks')
  if (size(d%block) /= 2) return
  do b = 1, 2
    call check(d%block(b)%Ni == ni(b) .and. d%block(b)%Nj == 10 .and. d%block(b)%Nk == 0, &
               'mesh2D.p3d: the cells of block '//trim(str(no_sign=.true., n=b)))
    call check(size(d%block(b)%mesh, 1) == 2, 'mesh2D.p3d: two coordinates in block '//trim(str(no_sign=.true., n=b)))
  enddo
  end subroutine grids_of_p3d_write

  subroutine counts_refused
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Files whose counts cannot be those of a grid are refused before any coordinate is read: a block count of 0 and a
  !< dimensions record with a node count of 0, in 3-D and in 2-D, all with return value 1 (no PLOT3D layout matches).
  !< Change of main checked: error codes of p3d_read_multiblock, a node count below 1 is now included in return value 1
  !< (formerly 2); before it, the reader allocated the blocks of such counts and went on (a dimensions record of the
  !< one-line layout read from a coordinate).
  !< Basis: P: a grid has at least one block, and a block at least one node in each direction.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=8),  parameter :: records(3) = [character(len=8) :: '0', '3 0 2', '3 0']
  integer,           parameter :: expected(3) = [1, 1, 1]
  character(len=40), parameter :: what(3) = [character(len=40) :: 'a block count of 0', &
                                             'a node count of 0 in a 3-D record', 'a node count of 0 in a 2-D record']
  type(orion_data) :: d
  integer          :: err, u, c
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, 3
    open(newunit=u, file='p3d_counts.p3d', status='replace', action='write')
    if (c == 1) then
      write(u,'(A)') trim(records(c))
    else
      write(u,'(A)') '1'
      write(u,'(A)') trim(records(c))
      write(u,'(A)') '0.5'
    endif
    close(u)
    d%p3d%format = 'ascii'
    if (allocated(d%block)) deallocate(d%block)
    err = p3d_read_multiblock(orion=d, filename='p3d_counts.p3d')
    write(*,'(A,I0)') trim(what(c))//': return value ', err
    call check(err == expected(c), trim(what(c))//': refused (return value '//trim(str(no_sign=.true., n=expected(c)))//')')
  enddo
  end subroutine counts_refused

  subroutine read_again
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A grid read into an orion_data that already holds blocks: the blocks of the file take their place (return value 0, the
  !< two blocks of mesh2D.p3d after those of mesh.p3d), as in the .szplt and VTK readers.
  !< Defect: the reader allocated the blocks of the file over those already allocated, and the program stopped with "Attempting
  !< to allocate already allocated variable".
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 9.7.1.3, paragraph 1 (p. 135): allocating an allocated
  !< variable "causes an error condition in the ALLOCATE statement".
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: d
  integer          :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  d%p3d%format = 'ascii'
  err = p3d_read_multiblock(orion=d, filename='mesh.p3d')
  call check(err == 0, 'read again: the 3-D grid read')
  err = p3d_read_multiblock(orion=d, filename='mesh2D.p3d')
  call check(err == 0, 'read again: the 2-D grid read into the same object, without error')
  if (err == 0 .and. allocated(d%block)) &
    call check(size(d%block) == 2 .and. d%block(1)%Nk == 0 .and. size(d%block(1)%mesh, 1) == 2, &
               'read again: the blocks of the 2-D grid')
  end subroutine read_again

end program p3d_read
