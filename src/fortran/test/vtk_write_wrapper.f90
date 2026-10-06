  program vtk_write_multiblock
  USE IR_Precision
  USE Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  use Lib_ORION_data
  USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
  use Lib_Base64, only: b64_decode
  use strings, only: simplified_relative_path
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks writing.
  !> It writes the fields that vtk_read_wrapper reads back, then checks vtk_write_structured_multiblock, one subroutine per
  !> case, each with what it checks, the defect it closes and its basis. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data)       :: orion
  integer(I4P), parameter:: nx1=0_I4P,nx2=90_I4P,ny1=0_I4P,ny2=50_I4P
  integer(I4P)           :: nz1 ,nz2
  integer(I4P)           :: i,j,k,b,E_IO
  character(len=10)      :: varnames
  integer :: checks = 0, failures = 0
  ! for the subroutine surface_blocks and its procedures
  integer(I4P),     parameter :: dims(3,4) = reshape([0,4,3, 5,0,2, 3,4,0, 0,4,3], [3,4])
  logical,          parameter :: layer(4)  = [.true., .true., .true., .false.]  ! variables with the layer of cells
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! 3D test

  nz1 = 0_I4P; nz2 = 50_I4P
  allocate(orion%block(1:4))
  do b = 1, 4
    orion%block(b)%name = 'B3D'//trim(str(.true.,b))
    orion%block(b)%Ni = nx2
    orion%block(b)%Nj = ny2
    orion%block(b)%Nk = nz2
    allocate(orion%block(b)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
    allocate(orion%block(b)%vars(1:2,nx1+1:nx2,ny1+1:ny2,nz1+1:nz2))
    do k=nz1,nz2
      do j=ny1,ny2
        do i=nx1,nx2
        orion%block(b)%mesh(1,i,j,k) = i*1._R8P
        orion%block(b)%mesh(2,i,j,k) = j*1._R8P
        orion%block(b)%mesh(3,i,j,k) = k*1._R8P
        enddo
      enddo
    enddo
    do k=nz1+1,nz2
      do j=ny1+1,ny2
        do i=nx1+1,nx2
        orion%block(b)%vars(1,i,j,k) = (i*j*k)*1._R8P
        orion%block(b)%vars(2,i,j,k) = -(i*j*k)*1._R8P
        enddo
      enddo
    enddo
    if (b>1) then
      orion%block(b)%mesh(1,:,:,:) = orion%block(b-1)%mesh(1,:,:,:) + nx2*1._R8P
    endif
  enddo

  varnames='var1 var2'
  orion%vtk%format='raw'
  E_IO = vtk_write_structured_multiblock(orion=orion,vtspath='',vtmpath='field3D',varnames=varnames)

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Pure 2D test
  
  deallocate(orion%block)
  allocate(orion%block(1:4))
  do b = 1, 4
    orion%block(b)%name = 'B2D'//trim(str(.true.,b))
    orion%block(b)%Ni = nx2
    orion%block(b)%Nj = ny2
    orion%block(b)%Nk = 1_I4P
    allocate(orion%block(b)%mesh(1:2,nx1:nx2,ny1:ny2,0:0))
    allocate(orion%block(b)%vars(1:2,nx1+1:nx2,ny1+1:ny2,1:1))
    do j=ny1,ny2
      do i=nx1,nx2
      orion%block(b)%mesh(1,i,j,0) = i*1._R8P
      orion%block(b)%mesh(2,i,j,0) = j*1._R8P
      enddo
    enddo
    do j=ny1+1,ny2
      do i=nx1+1,nx2
      orion%block(b)%vars(1,i,j,1) = (i*j*k)*1._R8P
      orion%block(b)%vars(2,i,j,1) = -(i*j*k)*1._R8P
      enddo
    enddo
    if (b>1) then
      orion%block(b)%mesh(1,:,:,:) = orion%block(b-1)%mesh(1,:,:,:) + nx2*1._R8P
    endif
  enddo

  varnames='var1 var2'
  orion%vtk%format='raw'
  E_IO = vtk_write_structured_multiblock(orion=orion,vtspath='',vtmpath='field2D',varnames=varnames)

  call many_blocks
  call surface_blocks
  call relative_paths

  write(*,'(A,I0,A,I0,A)') 'vtk_write_wrapper: ', checks, ' checks, ', failures, ' failed'
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

  subroutine many_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Any number of blocks, written. A field of 130 blocks (3-D, a few cells each, different sizes) is written in the ascii
  !< format without time and read back, and the number of blocks, their names and sizes, every coordinate and every variable
  !< are checked, bit for bit. One format is enough: VTK_INI_XML stores the index of a block file before it looks at the
  !< format. 130 rather than 100: past the array, a gfortran release build of the old writer went on for a few blocks without
  !< any sign, then crashed (from 110 blocks in our runs).
  !< Defect: vtk_write_structured_multiblock kept the file index of each block in an array of 99, so a field of more blocks was
  !< written past its end.
  !< Basis: Fortran 2018, 9.5.3.1 (p. 128): the value of a subscript shall be within the bounds of its dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  call round_trip(130_I4P, 'ascii')
  end subroutine many_blocks

  subroutine round_trip(nb, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write nb blocks in the given format, read them back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     intent(in) :: nb
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, i, j, k, ni, nj, nk
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(written%block(1:nb))
  do b = 1, nb
    ni = 1 + mod(b, 3); nj = 1 + mod(b, 2); nk = 1 + mod(b, 4)
    written%block(b)%name = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt//'_B'//trim(str(.true.,b))
    written%block(b)%Ni = ni; written%block(b)%Nj = nj; written%block(b)%Nk = nk
    allocate(written%block(b)%mesh(1:3,0:ni,0:nj,0:nk))
    allocate(written%block(b)%vars(1:2,1:ni,1:nj,1:nk))
    do k = 0, nk
      do j = 0, nj
        do i = 0, ni
          written%block(b)%mesh(1,i,j,k) = real(i + 10*b, R8P)
          written%block(b)%mesh(2,i,j,k) = real(j, R8P)
          written%block(b)%mesh(3,i,j,k) = real(k + 1, R8P)
        enddo
      enddo
    enddo
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          written%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          written%block(b)%vars(2,i,j,k) = -real(b, R8P)
        enddo
      enddo
    enddo
  enddo
  written%vtk%format = fmt
  vtm = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt
  label = trim(str(.true.,nb))//' blocks '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == nb, trim(label)//': '//trim(str(.true.,size(back%block)))//' blocks read')
  if (size(back%block) /= nb) return
  do b = 1, nb
    associate(w => written%block(b), r => back%block(b))
    call check(trim(r%name) == trim(w%name), trim(label)//': name of block '//trim(str(.true.,b)))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': size of block '//trim(str(.true.,b)))
    call check(size(r%mesh,1) == 3, trim(label)//': mesh of block '//trim(str(.true.,b))//' not 3-D')
    if (size(r%mesh,1) /= 3) cycle
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) then
      call check(.false., trim(label)//': mesh bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates of block '//trim(str(.true.,b)))
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) then
      call check(.false., trim(label)//': variable bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%vars == w%vars), trim(label)//': variables of block '//trim(str(.true.,b)))
    end associate
  enddo
  end subroutine round_trip

  subroutine surface_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Surface blocks. vtk_write_structured_multiblock writes surface blocks, blocks with one plane of nodes in one direction
  !< (Ni, Nj or Nk = 0) such as the faces of a volume block, with one layer of cells there, as VTK readers count them (and as
  !< the Tecplot writer writes them). In each of the ascii, binary and raw formats a field of three surface blocks, one per
  !< direction (i: 0 x 4 x 3 cells, j: 5 x 0 x 2, k: 3 x 4 x 0), with two cell variables, and a fourth i-face whose variables
  !< hold no layer of cells (as a block read back from a file with no cell value), is written, and each block file is read back
  !< as text and bytes: the extent of the block (WholeExtent and Piece Extent), the number of points and their coordinates, and
  !< for each cell variable the number of values (one per cell, a direction with one plane of nodes counting one cell; none for
  !< the fourth block, written as before) and the values, bit for bit (every value is an integer or a half). Each field is then
  !< read back with vtk_read_structured_multiblock, which reads the cell variables of every block, and the blocks, their sizes
  !< and their nodes are checked.
  !< Defect: the writer counted the cells of a block as Ni*Nj*Nk, none for a surface block, whose cell arrays it wrote empty.
  !< Basis: Tecplot 360 Data Format Guide, section 2-1 (p. 15), and VTK 9.7.1 (vtkStructuredData::GetNumberOfCells): max(n-1,1)
  !< cells in each direction of n points.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: f
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    call write_and_check(trim(formats(f)))
  enddo
  end subroutine surface_blocks

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

  subroutine relative_paths
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The paths of the block files in the .vtm. simplified_relative_path(path1, path2) gives the prefix with which
  !< vtk_write_structured_multiblock lists the block files in the .vtm file: path1 is the path of the .vtm file without its
  !< extension, path2 the prefix of the block files (vtspath), and the result R is such that dir(path1)//R//name names the file
  !< path2//name, dir(path1) being path1 up to its last '/' (empty when it has none), since a reader takes the file of a
  !< DataSet relative to the directory of the .vtm file. Both paths are relative to the same directory, or both absolute; an
  !< absolute path2 is also taken with a relative path1, and is then the answer itself. The result is checked for every form of
  !< the two paths: no common prefix, an empty path on one side or on both, a common directory, a common prefix that is not a
  !< directory, equal paths, a prefix of file names without a trailing '/', paths that start with './' or '../', absolute
  !< paths. The answer of each case was written from the rule above before the checks were run. The function is also called on
  !< the forms it cannot answer, which need the current directory (an absolute path1 with a relative path2, the same directory
  !< written in two forms, a path1 that climbs out of the common directory): it returns, and the result is written to the
  !< output, not checked. Then a field of two blocks
  !< is written with vtk_write_structured_multiblock three ways, the .vtm file and the block files sharing no prefix: block
  !< files with a name prefix next to the .vtm (blk_relpath_, fld_relpath); the .vtm in a subdirectory and the block files in
  !< the current directory (vtspath = '', a listed path, '../', longer than vtspath); the block files in a subdirectory and the
  !< .vtm in the current directory. Each time the .vtm lists the block files as they are on disk, seen from its directory, and
  !< vtk_read_structured_multiblock, given the directory of the .vtm as vtspath, reads the field back as written. The
  !< subdirectory is made with execute_command_line('mkdir ...').
  !< Defect: simplified_relative_path used last_slash, which it set only when the paths share a prefix, so for paths without a
  !< common prefix the .vtm listed what the memory held; the writer also kept the prefix in a string as long as vtspath, so a
  !< longer one was cut ('../' became '').
  !< Basis: Fortran 2018, 9.2 (p. 123) and 19.6.4 (p. 512): a variable that is not initially defined is undefined, and a
  !< reference to it is not permitted; the reader of VTK 9.7.1 takes the file of a DataSet relative to the directory of the
  !< .vtm file (vtkXMLCompositeDataReader::GetFilePath).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer :: exitstat, cmdstat
  !---------------------------------------------------------------------------------------------------------------------------------

  ! No common prefix
  call case('fld',          '',             '',               'no directory, block files next to the .vtm')
  call case('',             '',             '',               'both empty')
  call case('',             'vtk/',         'vtk/',           'empty path1')
  call case('fld',          'vtk/',         'vtk/',           'block files in a subdirectory')
  call case('out/fld',      '',             '../',            '.vtm in a subdirectory, block files in the current one')
  call case('a/b/fld',      '',             '../../',         '.vtm two levels down')
  call case('out/fld',      'vtk/',         '../vtk/',        'two subdirectories')
  call case('fld',          'pre',          'pre',            'block files with a name prefix')
  call case('fld',          './',           './',             'block files in ./')
  call case('out/fld',      '../vtk/',      '../../vtk/',     'block files above the current directory')
  ! Common directory, common prefix
  call case('out/fld',      'out/vtk/',     'vtk/',           'common directory')
  call case('out/fld',      'out/',         '',               'block files in the directory of the .vtm')
  call case('a/b/fld',      'a/c/',         '../c/',          'common directory, sibling subdirectories')
  call case('a/fld',        'a/b/c/',       'b/c/',           'block files two levels below the .vtm')
  call case('a/b/c/fld',    'a/',           '../../',         'block files two levels above the .vtm')
  call case('out1/fld',     'out2/vtk/',    '../out2/vtk/',   'common prefix that is not a directory')
  call case('out/field',    'out/fieldvtk/','fieldvtk/',      'common prefix past the common directory')
  call case('out/fld',      'out/fld',      'fld',            'equal paths')
  call case('out/fld',      'out/vtk',      'vtk',            'block files with a name prefix, no trailing /')
  call case('./out/fld',    './vtk/',       '../vtk/',        'both paths starting with ./')
  call case('../out/fld',   '../out/vtk/',  'vtk/',           'both paths starting with ../')
  ! Absolute paths
  call case('/d/out/fld',   '/d/out/vtk/',  'vtk/',           'absolute, common directory')
  call case('/a/fld',       '/b/vtk/',      '../b/vtk/',      'absolute, common root only')
  call case('/fld',         '/vtk/',        'vtk/',           'absolute, at the root')
  call case('fld',          '/d/vtk/',      '/d/vtk/',        'relative path1 without a directory, absolute path2')
  call case('out/fld',      '/d/vtk/',      '/d/vtk/',        'relative path1 with a directory, absolute path2')
  ! Forms that need the current directory: only that the function returns
  call returns('/d/out/fld', 'vtk/',     'absolute path1, relative path2')
  call returns('./out/fld',  'out/vtk/', 'the same directory written as ./out and as out')
  call returns('../out/fld', 'vtk/',     'path1 climbing out of the common directory')
  call execute_command_line('mkdir relpath_sub', exitstat=exitstat, cmdstat=cmdstat)  ! it may already be there
  call write_and_read_paths('blk_relpath_', 'fld_relpath', '', 'block files with a name prefix')
  call write_and_read_paths('', 'relpath_sub/fld', 'relpath_sub/', '.vtm in a subdirectory, vtspath empty')
  call write_and_read_paths('relpath_sub/blk_', 'fld2_relpath', '', 'block files in a subdirectory')
  end subroutine relative_paths

  subroutine case(path1, path2, expected, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The relative path of path2 seen from the directory of path1 is the one expected.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: path1, path2, expected, what
  character(len=256)           :: r
  !---------------------------------------------------------------------------------------------------------------------------------

  r = simplified_relative_path(path1, path2)
  call check(trim(r) == expected, what//': "'//path1//'", "'//path2//'" -> "'//trim(r)//'", "'//expected//'" expected')
  end subroutine case

  subroutine returns(path1, path2, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The function returns on a form it cannot answer: the result is written to the output, not checked (it would need the
  !< current directory).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: path1, path2, what
  character(len=:), allocatable :: r
  !---------------------------------------------------------------------------------------------------------------------------------

  r = simplified_relative_path(path1, path2)
  write(*,'(A)') what//': returns "'//r//'"'
  end subroutine returns

  subroutine write_and_read_paths(vtspath, vtmpath, vtmdir, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of two blocks with the block files vtspath//<block>.vts and the .vtm file vtmpath.vtm, whose directory is
  !< vtmdir; check that the .vtm lists the block files as they are on disk, seen from vtmdir, and read the field back, with
  !< vtmdir as vtspath.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: vtspath, vtmpath, vtmdir, what
  type(orion_data)              :: w, r
  character(len=16)             :: varnames
  character(len=:), allocatable :: buf
  integer(I4P)                  :: b, i, j, k, err
  integer                       :: u, n, p, q, nfiles
  logical                       :: there, same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'RP'//trim(str(.true., b))
    w%block(b)%Ni = 2; w%block(b)%Nj = 1 + b; w%block(b)%Nk = 1
    allocate(w%block(b)%mesh(1:3, 0:2, 0:1+b, 0:1), w%block(b)%vars(1:1, 1:2, 1:1+b, 1:1))
    do k = 0, 1; do j = 0, 1 + b; do i = 0, 2
      w%block(b)%mesh(:,i,j,k) = [real(i + 10*b, R8P), real(j, R8P), real(k, R8P) + 0.5_R8P]
    enddo; enddo; enddo
    w%block(b)%vars(1,:,:,1) = real(b, R8P)
  enddo
  w%vtk%format = 'ascii'
  varnames = 'a'
  err = vtk_write_structured_multiblock(orion=w, vtspath=vtspath, vtmpath=vtmpath, varnames=varnames)
  call check(err == 0, what//': field written without error')
  open(newunit=u, file=vtmpath//'.vtm', access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  nfiles = 0
  p = index(buf, 'file="')
  do while (p > 0)
    p = p + len('file="')
    q = index(buf(p:), '"') + p - 2
    inquire(file=vtmdir//buf(p:q), exist=there)
    call check(there, what//': the .vtm lists a block file that is on disk: '//buf(p:q))
    nfiles = nfiles + 1
    n = index(buf(q+1:), 'file="')
    p = 0; if (n > 0) p = q + n
  enddo
  call check(nfiles == 2, what//': the .vtm lists two block files')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath=vtmpath, vtspath=vtmdir)
  call check(err == 0, what//': field read back without error')
  same = .false.
  if (err == 0 .and. allocated(r%block)) then
    same = size(r%block) == 2
    if (same) then
      do b = 1, 2
        same = same .and. all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
               all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
        if (same) same = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
      enddo
    endif
  endif
  call check(same, what//': field read back as written')
  end subroutine write_and_read_paths
  end program vtk_write_multiblock