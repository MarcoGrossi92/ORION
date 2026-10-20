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
  call digits_ascii
  call write_shapes
  call write_names

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

  subroutine digits_ascii
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Every 64-bit coordinate written by p3d_write_multiblock comes back, bit for bit, when the file is read. A 3-D grid of two
  !< blocks (3 x 2 x 2 and 2 x 2 x 3 cells) and a 2-D grid of one block (3 x 2 cells), filled with reals that 15 significant
  !< digits do not give back (hard_real), are written and read back: every coordinate must be the real written (==).
  !< Defect: the writer used a list-directed WRITE, whose digits are the compiler's: 17 with gfortran, 16 with ifx, so that a
  !< grid written and read again was not the grid written.
  !< Basis: the Fortran API guide (the derivation of the 17 significant digits): the decimal numbers of n significant digits are
  !< closer to one another than the 64-bit reals (53-bit significand) in every range when 10**(n-1) > 2**53, n >= 17, so the
  !< nearest decimal number of 17 digits reads back as the real it was written from.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, parameter :: ni(2) = [3, 2], nj(2) = [2, 2], nk(2) = [2, 3]
  type(orion_data)   :: w, r
  integer            :: err, b, d, i, j, k, n, nd
  logical            :: same
  character(len=12)  :: file
  !---------------------------------------------------------------------------------------------------------------------------------

  do nd = 3, 2, -1
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(merge(2, 1, nd == 3)))
    n = 0
    do b = 1, size(w%block)
      w%block(b)%Ni = ni(b); w%block(b)%Nj = nj(b); w%block(b)%Nk = merge(nk(b), 0, nd == 3)
      allocate(w%block(b)%mesh(1:nd,0:w%block(b)%Ni,0:w%block(b)%Nj,0:w%block(b)%Nk))
      do k = 0, w%block(b)%Nk; do j = 0, w%block(b)%Nj; do i = 0, w%block(b)%Ni; do d = 1, nd
        n = n + 1
        w%block(b)%mesh(d,i,j,k) = hard_real(n)
      enddo; enddo; enddo; enddo
    enddo
    file = merge('digits3D.p3d', 'digits2D.p3d', nd == 3)
    w%p3d%format = 'ascii'
    err = p3d_write_multiblock(orion=w, filename=trim(file))
    call check(err == 0, trim(file)//': written without error')
    r%p3d%format = 'ascii'
    err = p3d_read_multiblock(orion=r, filename=trim(file))
    call check(err == 0, trim(file)//': read without error')
    if (err /= 0) cycle
    same = size(r%block) == size(w%block)
    if (same) then
      do b = 1, size(w%block)
        same = same .and. all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh))
      enddo
    endif
    call check(same, trim(file)//': the blocks and their dimensions')
    if (.not.same) cycle
    same = .true.
    do b = 1, size(w%block)
      same = same .and. all(r%block(b)%mesh == w%block(b)%mesh)
    enddo
    call check(same, trim(file)//': every coordinate, bit for bit')
  enddo
  end subroutine digits_ascii

  subroutine write_shapes
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The grid p3d_write_multiblock writes is that of the meshes of the blocks, mesh(1:ndir,:,:,:), not that of Nk. A mesh of two
  !< coordinates with Nk = 1 (the block of a .vtm file, of two planes of nodes in k) is written as a two-dimensional grid of the
  !< nodes of its first plane, and read back as one; so is a mesh of two coordinates with two planes. A three-dimensional mesh
  !< whose indices start at 1 is written whole. A mesh that a grid cannot hold is refused (return value 1, nothing written): of
  !< one coordinate, of blocks with a different number of coordinates, a block without a mesh, and a format other than ascii
  !< (binary included).
  !< Defect: a block of Nk /= 0 was taken for a three-dimensional one, and a mesh of two coordinates with Nk = 1 was written with
  !< a third coordinate and a second plane that it has not, past its bounds; a mesh of one coordinate was written past its
  !< bounds too, and a format other than ascii and binary left the output unit unopened.
  !< Basis: P, by construction: the values read back are those of the nodes written.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: w, r
  integer          :: err, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  ! two coordinates, Nk = 1 as the VTK reader gives it, one plane of nodes in k
  allocate(w%block(1))
  w%block(1)%Ni = 3; w%block(1)%Nj = 2; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(1:2,0:3,0:2,0:0))
  do j = 0, 2; do i = 0, 3
    w%block(1)%mesh(:,i,j,0) = [real(10*i + j, R8P), 0.5_R8P*i - j]
  enddo; enddo
  w%p3d%format = 'ascii'
  err = p3d_write_multiblock(orion=w, filename='write_2d_nk1.p3d')
  call check(err == 0, 'write 2-D, Nk = 1: written without error')
  err = p3d_read_multiblock(orion=r, filename='write_2d_nk1.p3d')
  call check(err == 0, 'write 2-D, Nk = 1: read back without error')
  if (err == 0) then
    call check(size(r%block) == 1 .and. size(r%block(1)%mesh, 1) == 2 .and. r%block(1)%Ni == 3 .and. &
               r%block(1)%Nj == 2 .and. r%block(1)%Nk == 0, 'write 2-D, Nk = 1: one block of two coordinates, 4 x 3 nodes')
    if (size(r%block(1)%mesh, 1) == 2) &
      call check(all(r%block(1)%mesh(:,0:3,0:2,0) == w%block(1)%mesh(:,0:3,0:2,0)), 'write 2-D, Nk = 1: the coordinates')
  endif
  ! two coordinates, two planes in k (the first one is written)
  deallocate(w%block(1)%mesh)
  allocate(w%block(1)%mesh(1:2,0:3,0:2,0:1))
  do k = 0, 1; do j = 0, 2; do i = 0, 3
    w%block(1)%mesh(:,i,j,k) = [real(10*i + j, R8P) + 100*k, 0.5_R8P*i - j]
  enddo; enddo; enddo
  err = p3d_write_multiblock(orion=w, filename='write_2d_2k.p3d')
  err = p3d_read_multiblock(orion=r, filename='write_2d_2k.p3d')
  call check(err == 0, 'write 2-D, two planes: read back without error')
  if (err == 0) then
    call check(size(r%block(1)%mesh, 1) == 2 .and. r%block(1)%Nk == 0, 'write 2-D, two planes: a two-dimensional grid')
    if (size(r%block(1)%mesh, 1) == 2) &
      call check(all(r%block(1)%mesh(:,0:3,0:2,0) == w%block(1)%mesh(:,0:3,0:2,0)), 'write 2-D, two planes: the first plane')
  endif
  ! three coordinates, indices from 1
  deallocate(w%block(1)%mesh)
  allocate(w%block(1)%mesh(1:3,1:3,1:2,1:2))
  do k = 1, 2; do j = 1, 2; do i = 1, 3
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P)/3._R8P, real(k, R8P)]
  enddo; enddo; enddo
  err = p3d_write_multiblock(orion=w, filename='write_3d_from1.p3d')
  err = p3d_read_multiblock(orion=r, filename='write_3d_from1.p3d')
  call check(err == 0, 'write 3-D, indices from 1: read back without error')
  if (err == 0) then
    call check(size(r%block(1)%mesh, 1) == 3 .and. r%block(1)%Ni == 2 .and. r%block(1)%Nj == 1 .and. r%block(1)%Nk == 1, &
               'write 3-D, indices from 1: 3 x 2 x 2 nodes')
    if (size(r%block(1)%mesh, 1) == 3 .and. r%block(1)%Ni == 2 .and. r%block(1)%Nj == 1 .and. r%block(1)%Nk == 1) &
      call check(all(r%block(1)%mesh(:,0:2,0:1,0:1) == w%block(1)%mesh(:,1:3,1:2,1:2)), &
                 'write 3-D, indices from 1: the coordinates')
  endif
  ! meshes that a grid cannot hold
  deallocate(w%block(1)%mesh)
  allocate(w%block(1)%mesh(1:1,0:3,0:0,0:0))
  w%block(1)%mesh = 1._R8P
  call refused(w, 'one coordinate', 'write_1d.p3d')
  deallocate(w%block); allocate(w%block(2))
  allocate(w%block(1)%mesh(1:3,0:1,0:1,0:1), w%block(2)%mesh(1:2,0:1,0:1,0:0))
  w%block(1)%mesh = 1._R8P; w%block(2)%mesh = 1._R8P
  call refused(w, 'blocks of two and of three coordinates', 'write_mixed.p3d')
  deallocate(w%block(2)%mesh)
  call refused(w, 'a block without a mesh', 'write_nomesh.p3d')
  allocate(w%block(2)%mesh(1:3,0:1,0:1,0:1))
  w%block(2)%mesh = 1._R8P
  w%p3d%format = 'ascii3'
  call refused(w, 'a format that is not ascii', 'write_format.p3d')
  w%p3d%format = 'binary'
  call refused(w, 'the format binary', 'write_binary.p3d')
  end subroutine write_shapes

  subroutine write_names
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The file p3d_write_multiblock writes. A name with no extension gets .p3d, also when a directory of the path has a dot
  !< (v1.2/grid becomes v1.2/grid.p3d); a name with an extension is not changed. A file that cannot be opened (a directory that
  !< does not exist) gives a nonzero return value, with no file fort.N.
  !< Defect: the extension was added when the whole name had no dot, so that v1.2/grid was written as it was; the format
  !< binary stopped the program (with the exit status 0), and a file that could not be opened was written to all the same.
  !< Basis: P, by construction.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: w
  integer          :: err, es
  logical          :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%Ni = 1; w%block(1)%Nj = 1; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(1:3,0:1,0:1,0:1))
  w%block(1)%mesh = 0.5_R8P
  call execute_command_line('mkdir -p write_v1.2', exitstat=es)
  call check(es == 0, 'write names: directory made')
  err = p3d_write_multiblock(orion=w, filename='write_v1.2/grid')
  inquire(file='write_v1.2/grid.p3d', exist=exists)
  call check(err == 0 .and. exists, 'write names: v1.2/grid is written as v1.2/grid.p3d')
  inquire(file='write_v1.2/grid', exist=exists)
  call check(.not.exists, 'write names: and not as v1.2/grid')
  err = p3d_write_multiblock(orion=w, filename='write_v1.2/grid.x')
  inquire(file='write_v1.2/grid.x', exist=exists)
  call check(err == 0 .and. exists, 'write names: a name with an extension is not changed')
  err = p3d_write_multiblock(orion=w, filename='no_such_dir/grid.p3d')
  call check(err /= 0, 'write names: a directory that does not exist: a nonzero return value')
  call execute_command_line('ls fort.* > /dev/null 2>&1', exitstat=es)
  call check(es /= 0, 'write names: a directory that does not exist: no file fort.N')
  end subroutine write_names

  subroutine refused(w, what, file)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< p3d_write_multiblock refuses to write w (return value 1) and leaves no file.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: w
  character(len=*), intent(in) :: what, file
  integer                      :: err, u
  logical                      :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=file, status='replace', action='write')
  close(u, status='delete')
  err = p3d_write_multiblock(orion=w, filename=file)
  call check(err == 1, 'write: '//what//': refused (return value 1)')
  inquire(file=file, exist=exists)
  call check(.not.exists, 'write: '//what//': no file written')
  end subroutine refused

  pure function hard_real(n) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The n-th of a list of 64-bit reals that 15 significant digits do not give back: 0.1 + 0.2, the thirds, pi, the reals next to
  !< 1, the smallest normal real, and multiples of 1/7 from about 1e-297 to 1e297, of both signs.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: n
  real(R8P)           :: x
  !---------------------------------------------------------------------------------------------------------------------------------

  select case(mod(n, 8))
  case(0)
    x = 0.1_R8P + 0.2_R8P
  case(1)
    x = 1._R8P/3._R8P
  case(2)
    x = -2._R8P/3._R8P
  case(3)
    x = acos(-1._R8P)
  case(4)
    x = nearest(1._R8P, 1._R8P)
  case(5)
    x = -nearest(1._R8P, -1._R8P)
  case(6)
    x = tiny(1._R8P)
  case default
    x = real(n, R8P)/7._R8P*10._R8P**(27*mod(n, 23) - 297)
  end select
  end function hard_real

end program p3d_read
