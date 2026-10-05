  program vtk_read_mesh_dims
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_read_structured_multiblock tells a 2-D mesh from a 3-D one by its z coordinates: vtk_write_structured_multiblock
  !> writes a 2-D mesh (mesh(1:2,...)) as one layer of cells between two layers of nodes with z = 0. A 3-D mesh whose z
  !> coordinates are not all 0 must come back as 3-D, also when they add up to 0, as for a slab around z = 0.
  !>
  !> For each of the ascii, binary and raw formats the program writes and reads back, without time:
  !>   sym : a 3-D field of two blocks around z = 0, z in {-1, 1} (one layer of cells) and z in {-1, 0, 1} (two layers);
  !>   pos : a 3-D field of one block with z in {0, 1};
  !>   2d  : a 2-D field of one block;
  !> and checks the dimension of the mesh read, its bounds and every coordinate, the block sizes and every variable. Every
  !> value is an integer, so it reads back bit for bit in every format. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: checks = 0, failures = 0
  integer :: f

  do f = 1, size(formats)
    call round_trip('sym', trim(formats(f)))
    call round_trip('pos', trim(formats(f)))
    call round_trip('2d', trim(formats(f)))
  enddo

  write(*,'(A,I0,A,I0,A)') 'vtk_read_mesh_dims: ', checks, ' checks, ', failures, ' failed'
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

  subroutine make_field(kind, fmt, orion)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Field of the given kind: block sizes, nodes at x = i (+ an offset per block), y = j, z of the kind, two cell variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)    :: kind, fmt
  type(orion_data), intent(inout) :: orion
  integer(I4P)                    :: nb, b, i, j, k, ni(2), nj(2), nk(2), ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  select case(kind)
  case('sym')
    nb = 2; ni = [3, 2]; nj = [2, 3]; nk = [1, 2]; ndir = 3
  case('pos')
    nb = 1; ni = [3, 0]; nj = [2, 0]; nk = [1, 0]; ndir = 3
  case default
    nb = 1; ni = [3, 0]; nj = [2, 0]; nk = [1, 0]; ndir = 2
  end select
  allocate(orion%block(1:nb))
  do b = 1, nb
    orion%block(b)%name = 'mesh_dims_'//kind//'_'//fmt//'_B'//trim(str(.true.,b))
    orion%block(b)%Ni = ni(b); orion%block(b)%Nj = nj(b); orion%block(b)%Nk = nk(b)
    if (ndir == 3) then
      allocate(orion%block(b)%mesh(1:3,0:ni(b),0:nj(b),0:nk(b)))
    else
      allocate(orion%block(b)%mesh(1:2,0:ni(b),0:nj(b),0:0))
    endif
    allocate(orion%block(b)%vars(1:2,1:ni(b),1:nj(b),1:nk(b)))
    do k = lbound(orion%block(b)%mesh,4), ubound(orion%block(b)%mesh,4)
      do j = 0, nj(b)
        do i = 0, ni(b)
          orion%block(b)%mesh(1,i,j,k) = real(i + 10*(b-1), R8P)
          orion%block(b)%mesh(2,i,j,k) = real(j, R8P)
          if (ndir == 3) then
            if (kind == 'sym') then
              ! z in {-1, 1} for one layer of cells, in {-1, 0, 1} for two: they add up to 0 in any order
              orion%block(b)%mesh(3,i,j,k) = real(2*k - nk(b), R8P) / real(nk(b), R8P)
            else
              orion%block(b)%mesh(3,i,j,k) = real(k, R8P)
            endif
          endif
        enddo
      enddo
    enddo
    do k = 1, nk(b)
      do j = 1, nj(b)
        do i = 1, ni(b)
          orion%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          orion%block(b)%vars(2,i,j,k) = -real(i*j*k + b, R8P)
        enddo
      enddo
    enddo
  enddo
  orion%vtk%format = fmt
  end subroutine make_field

  subroutine round_trip(kind, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of the given kind in the given format, read it back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: kind, fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  call make_field(kind, fmt, written)
  vtm = 'mesh_dims_'//kind//'_'//fmt
  label = kind//' '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == size(written%block), trim(label)//': number of blocks')
  if (size(back%block) /= size(written%block)) return
  ndir = size(written%block(1)%mesh,1)
  do b = 1, size(written%block)
    associate(w => written%block(b), r => back%block(b))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': block size')
    call check(size(r%mesh,1) == ndir, trim(label)//': mesh read as '//trim(str(.true.,size(r%mesh,1)))// &
                                       '-D instead of '//trim(str(.true.,ndir))//'-D')
    if (size(r%mesh,1) /= ndir) cycle
    call check(all(lbound(r%mesh) == lbound(w%mesh)) .and. all(ubound(r%mesh) == ubound(w%mesh)), &
               trim(label)//': mesh bounds')
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) cycle
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates')
    call check(all(lbound(r%vars) == lbound(w%vars)) .and. all(ubound(r%vars) == ubound(w%vars)), &
               trim(label)//': variable bounds')
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) cycle
    call check(all(r%vars == w%vars), trim(label)//': variables')
    end associate
  enddo
  end subroutine round_trip
  endprogram vtk_read_mesh_dims
