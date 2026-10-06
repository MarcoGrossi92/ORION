  program vtk_read_face_z0
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_read_structured_multiblock reads a block with one plane of nodes in k whose nodes all lie on the plane z = 0, such as
  !> the face of a 3-D block on a wall at z = 0, as a 3-D block: its nodes keep their z, and the block has no cell in k, as its
  !> extent gives. Only a block with two planes of nodes in k and z = 0 at every node, the form in which
  !> vtk_write_structured_multiblock writes a 2-D mesh, is read as 2-D. For each of the ascii, binary and raw formats the program
  !> writes a 3-D field of two blocks, a face on the plane z = 0 (3 x 4 x 0 cells, its cell variables with one layer of cells) and
  !> a volume block (2 x 2 x 2 cells) next to it, reads it back and checks for each block Ni, Nj, Nk, the shape of the mesh and
  !> of the variables, every node, and every cell value of the volume block, bit for bit (every value is an integer or a half).
  !> Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: dims(3,2) = reshape([3,4,0, 2,2,2], [3,2])
  integer :: checks = 0, failures = 0
  integer :: f

  do f = 1, size(formats)
    call write_and_read(trim(formats(f)))
  enddo

  write(*,'(A,I0,A,I0,A)') 'vtk_read_face_z0: ', checks, ' checks, ', failures, ' failed'
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

  pure function node(b, i, j, k) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Coordinates of node (i, j, k) of block b: the face (b = 1) on the plane z = 0, the volume block (b = 2) from z = 0 to z = 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  x = [real(i + 10*(b-1), R8P), real(j, R8P) + 0.5_R8P*i, real(k, R8P)]
  end function node

  pure function cell(b, i, j, k) result(v)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Values of the two cell variables in cell (i, j, k) of block b.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: v(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  v = [real(i + 10*j + 100*k, R8P), -real(b, R8P) - 0.5_R8P]
  end function cell

  subroutine write_and_read(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field in the format given, read it back and check it.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w, r
  character(len=16)            :: varnames
  character(len=64)            :: label
  integer(I4P)                 :: b, i, j, k, err
  logical                      :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'F'//trim(str(.true., b))
    w%block(b)%Ni = dims(1,b); w%block(b)%Nj = dims(2,b); w%block(b)%Nk = dims(3,b)
    allocate(w%block(b)%mesh(1:3, 0:dims(1,b), 0:dims(2,b), 0:dims(3,b)))
    allocate(w%block(b)%vars(1:2, 1:max(dims(1,b),1), 1:max(dims(2,b),1), 1:max(dims(3,b),1)))
    do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
      w%block(b)%mesh(:,i,j,k) = node(b, i, j, k)
    enddo; enddo; enddo
    do k = 1, ubound(w%block(b)%vars,4); do j = 1, ubound(w%block(b)%vars,3); do i = 1, ubound(w%block(b)%vars,2)
      w%block(b)%vars(:,i,j,k) = cell(b, i, j, k)
    enddo; enddo; enddo
  enddo
  w%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath='face_z0_'//fmt//'_', vtmpath='face_z0_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': written without error')

  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='face_z0_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  call check(size(r%block) == 2, fmt//': 2 blocks read back')
  if (size(r%block) /= 2) return
  do b = 1, 2
    label = fmt//' block '//trim(str(.true., b))
    call check(r%block(b)%Ni == dims(1,b) .and. r%block(b)%Nj == dims(2,b) .and. r%block(b)%Nk == dims(3,b), &
               trim(label)//': Ni, Nj, Nk')
    same = all(shape(r%block(b)%mesh) == [3, dims(1,b)+1, dims(2,b)+1, dims(3,b)+1])
    call check(same, trim(label)//': shape of the mesh, 3 coordinates and every node')
    if (same) then
      do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
        same = same .and. all(r%block(b)%mesh(:,i,j,k) == node(b, i, j, k))
      enddo; enddo; enddo
      call check(same, trim(label)//': nodes')
    endif
    ! The reader counts the cells of a block from its extent: none in k for the face
    same = all(shape(r%block(b)%vars) == [2, dims(1,b), dims(2,b), dims(3,b)])
    call check(same, trim(label)//': shape of the variables, one cell per extent interval')
    if (same .and. size(r%block(b)%vars) > 0) then
      do k = 1, dims(3,b); do j = 1, dims(2,b); do i = 1, dims(1,b)
        same = same .and. all(r%block(b)%vars(:,i,j,k) == cell(b, i, j, k))
      enddo; enddo; enddo
      call check(same, trim(label)//': cell values')
    endif
  enddo
  end subroutine write_and_read
  endprogram vtk_read_face_z0
