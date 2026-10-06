  program vtk_surface_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_write_structured_multiblock writes surface blocks: blocks with one plane of nodes in one direction (Ni, Nj or Nk = 0),
  !> such as the faces of a volume block, carry one layer of cells there, as VTK readers count them (and as the Tecplot writer
  !> writes them). For each of the ascii, binary and raw formats the program writes a field of three surface blocks, one per
  !> direction (i: 0 x 4 x 3 cells, j: 5 x 0 x 2, k: 3 x 4 x 0), with two cell variables, and a fourth i-face whose variables
  !> hold no layer of cells (as a block read back from a file with no cell value), and reads each block file back as text and
  !> bytes: the extent of the block (WholeExtent and Piece Extent), the number of points and their coordinates, and for each
  !> cell variable the number of values (one per cell, a direction with one plane of nodes counting one cell; none for the
  !> fourth block, written as before) and the values, bit for bit (every value is an integer or a half). It then reads each
  !> field back with vtk_read_structured_multiblock, which reads the cell variables of every block, and checks the blocks, their
  !> sizes and their nodes. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_Base64, only: b64_decode
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: dims(3,4) = reshape([0,4,3, 5,0,2, 3,4,0, 0,4,3], [3,4])
  logical,          parameter :: layer(4)  = [.true., .true., .true., .false.]  ! variables with the layer of cells
  integer :: checks = 0, failures = 0
  integer :: f

  do f = 1, size(formats)
    call write_and_check(trim(formats(f)))
  enddo

  write(*,'(A,I0,A,I0,A)') 'vtk_surface_blocks: ', checks, ' checks, ', failures, ' failed'
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

  subroutine write_and_check(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the three surface blocks in the format given and check each block file.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: orion
  character(len=16)            :: varnames
  integer(I4P)                 :: b, i, j, k, err
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(orion%block(4))
  do b = 1, 4
    orion%block(b)%name = 'B'//trim(str(.true., b))
    orion%block(b)%Ni = dims(1,b); orion%block(b)%Nj = dims(2,b); orion%block(b)%Nk = dims(3,b)
    allocate(orion%block(b)%mesh(1:3, 0:dims(1,b), 0:dims(2,b), 0:dims(3,b)))
    if (layer(b)) then
      allocate(orion%block(b)%vars(1:2, 1:max(dims(1,b),1), 1:max(dims(2,b),1), 1:max(dims(3,b),1)))
    else
      allocate(orion%block(b)%vars(1:2, 1:dims(1,b), 1:dims(2,b), 1:dims(3,b)))
    endif
    do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
      orion%block(b)%mesh(:,i,j,k) = node(b, i, j, k)
    enddo; enddo; enddo
    do k = 1, ubound(orion%block(b)%vars,4); do j = 1, ubound(orion%block(b)%vars,3); do i = 1, ubound(orion%block(b)%vars,2)
      orion%block(b)%vars(:,i,j,k) = cell(b, i, j, k)
    enddo; enddo; enddo
  enddo
  orion%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=orion, vtspath='surface_'//fmt//'_', vtmpath='surface_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': written without error')
  do b = 1, 4
    call check_block(fmt, b)
  enddo
  call read_back(fmt)
  end subroutine write_and_check

  subroutine read_back(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read the field written in the format given back with vtk_read_structured_multiblock and check the blocks, their sizes and
  !< their nodes.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: r
  integer(I4P)                 :: b, i, j, k, err
  logical                      :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='surface_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  call check(size(r%block) == 4, fmt//': 4 blocks read back')
  if (size(r%block) /= 4) return
  do b = 1, 4
    call check(r%block(b)%Ni == dims(1,b) .and. r%block(b)%Nj == dims(2,b) .and. r%block(b)%Nk == dims(3,b), &
               fmt//' block '//trim(str(.true., b))//': Ni, Nj, Nk read back')
    same = all(shape(r%block(b)%mesh) == [3, dims(1,b)+1, dims(2,b)+1, dims(3,b)+1])
    if (same) then
      do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
        same = same .and. all(r%block(b)%mesh(:,i,j,k) == node(b, i, j, k))
      enddo; enddo; enddo
    endif
    call check(same, fmt//' block '//trim(str(.true., b))//': nodes read back')
  enddo
  end subroutine read_back

  pure function node(b, i, j, k) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Coordinates of node (i, j, k) of block b: integers and halves, exact in every format. No block has z = 0 at every node:
  !< the reader takes such a block for a 2-D field.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  x = [real(i + 10*b, R8P), real(j, R8P) + 0.5_R8P*k, real(k + b, R8P)]
  end function node

  pure function cell(b, i, j, k) result(v)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Values of the two cell variables in cell (i, j, k) of block b.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: v(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  v = [real(i + 10*j + 100*k, R8P), -real(b, R8P)]
  end function cell

  subroutine check_block(fmt, b)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read the file of block b as text and bytes and check extent, points and cell values.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fmt
  integer(I4P),     intent(in)  :: b
  character(len=:), allocatable :: buf
  character(len=64)             :: label
  integer(I4P)                  :: ext(6), want(6), u, n, i, j, k, m, s, ncell, npoint
  real(R8P), allocatable        :: values(:), expected(:)
  real(R8P)                     :: v(2)
  character(len=8)              :: names(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  label = fmt//' block '//trim(str(.true., b))
  open(newunit=u, file='surface_'//fmt//'_B'//trim(str(.true., b))//'.vts', access='stream', form='unformatted', status='old')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  want = [0, dims(1,b), 0, dims(2,b), 0, dims(3,b)]
  call extent(buf, '<StructuredGrid', ext); call check(all(ext == want), trim(label)//': WholeExtent')
  call extent(buf, '<Piece', ext);          call check(all(ext == want), trim(label)//': Piece Extent')
  npoint = (dims(1,b)+1)*(dims(2,b)+1)*(dims(3,b)+1)
  ncell  = max(dims(1,b),1)*max(dims(2,b),1)*max(dims(3,b),1)
  if (.not.layer(b)) ncell = 0
  names = ['Points  ', 'a       ', 'b       ']
  do s = 1, 3
    call array_values(buf, fmt, trim(names(s)), values)
    if (s == 1) then
      allocate(expected(3*npoint)); m = 0
      do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
        expected(m+1:m+3) = node(b, i, j, k); m = m + 3
      enddo; enddo; enddo
    else
      allocate(expected(ncell)); m = 0
      if (ncell > 0) then
        do k = 1, max(dims(3,b),1); do j = 1, max(dims(2,b),1); do i = 1, max(dims(1,b),1)
          m = m + 1; v = cell(b, i, j, k); expected(m) = v(s-1)
        enddo; enddo; enddo
      endif
    endif
    call check(size(values) == size(expected), trim(label)//': number of values of '//trim(names(s))//' ('// &
               trim(str(.true., size(values)))//', '//trim(str(.true., size(expected)))//' expected)')
    if (size(values) == size(expected)) call check(all(values == expected), trim(label)//': values of '//trim(names(s)))
    deallocate(values, expected)
  enddo
  end subroutine check_block

  subroutine extent(buf, element, ext)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The Extent attribute (WholeExtent for StructuredGrid) of the first element given.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: buf, element
  integer(I4P),     intent(out) :: ext(6)
  integer(I4P)                  :: p, q, r, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  ext = -1
  p = index(buf, element); if (p == 0) return
  q = index(buf(p:), '>') + p - 1
  r = index(buf(p:q), 'Extent="'); if (r == 0) return
  r = p + r - 1 + len('Extent="')
  read(buf(r:r+index(buf(r:q), '"')-2), *, iostat=ios) ext
  end subroutine extent

  subroutine array_values(buf, fmt, name, values)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The values of the DataArray of the given name: from its text (ascii), from base64 (binary, after the 4-byte size), or from
  !< the raw appended data at its offset (after the 4-byte size).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*),       intent(in)  :: buf, fmt, name
  real(R8P), allocatable, intent(out) :: values(:)
  integer(I4P)                        :: p, q, e, r, ios, nbyte, offset, start, ntok, i
  integer(I1P), allocatable           :: bytes(:)
  character(len=:), allocatable       :: text
  logical                             :: blank
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(values(0))
  p = index(buf, 'Name="'//name//'"'); if (p == 0) return
  p = index(buf(:p), '<DataArray', back=.true.)
  q = index(buf(p:), '>') + p - 1
  select case(fmt)
  case('ascii')
    e = index(buf(q:), '</DataArray>') + q - 1
    text = buf(q+1:e-1)
    ! Line breaks become blanks: a list-directed read of one record does not take them for separators
    ntok = 0; blank = .true.
    do i = 1, len(text)
      if (text(i:i) < ' ') text(i:i) = ' '
      if (blank .and. text(i:i) > ' ') ntok = ntok + 1
      blank = text(i:i) <= ' '
    enddo
    deallocate(values); allocate(values(ntok))
    if (ntok > 0) read(text, *, iostat=ios) values
  case('binary')
    e = index(buf(q:), '</DataArray>') + q - 1
    ! The base64 text, without the line breaks and blanks around it
    r = q + 1
    do while (r < e .and. buf(r:r) <= ' ')
      r = r + 1
    enddo
    i = e - 1
    do while (i > r .and. buf(i:i) <= ' ')
      i = i - 1
    enddo
    text = buf(r:i)
    allocate(bytes(3*len(text)/4 - count([text(len(text):len(text)) == '=', text(len(text)-1:len(text)-1) == '='])))
    call b64_decode(code=text, n=bytes)
    nbyte = transfer(bytes(1:4), nbyte)
    deallocate(values); allocate(values(nbyte/8))
    if (nbyte > 0) values = transfer(bytes(5:4+nbyte), values)
  case('raw')
    r = index(buf(p:q), 'offset="'); if (r == 0) return
    r = p + r - 1 + len('offset="')
    read(buf(r:r+index(buf(r:q), '"')-2), *, iostat=ios) offset
    start = index(buf, '<AppendedData'); start = index(buf(start:), '_') + start
    nbyte = transfer(buf(start+offset:start+offset+3), nbyte)
    deallocate(values); allocate(values(nbyte/8))
    if (nbyte > 0) values = transfer(buf(start+offset+4:start+offset+3+nbyte), values)
  end select
  end subroutine array_values
  endprogram vtk_surface_blocks
