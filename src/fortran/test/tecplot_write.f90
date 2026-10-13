program tecplot_write_multiblock
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks writing of Tecplot files: the checks of tec_write_structured_multiblock, one subroutine
  !> per case, each with what it checks, the defect it closes and its basis, then the files that tecplot_read reads back. Exit
  !> status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use Lib_Tecplot
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
#ifdef TECIO
  use, intrinsic :: iso_c_binding
#endif
  use IR_Precision
  implicit none
#ifdef TECIO
  include "tecio.f90"
#endif
  integer, parameter :: nx1=0                              !< X lower bound extent.
  integer, parameter :: nx2=10                             !< X upper bound extent.
  integer, parameter :: ny1=0                              !< Y lower bound extent.
  integer, parameter :: ny2=10                             !< Y upper bound extent.
  integer, parameter :: nz1=0                              !< Z lower bound extent.
  integer, parameter :: nz2=10                             !< Z upper bound extent.
  type(orion_data)   :: data
  integer            :: error                              !< Status error.
  integer            :: i, j, k                            !< Counter.
  integer :: checks = 0, failures = 0

  ! The checks of the writer come first. The binary files are written only with TecIO: without it the writer stops the
  ! program with error termination. The ASCII files that tecplot_read and the Python reader read back are written in every
  ! build.
#ifdef TECIO
  call surface_blocks
#endif
  call variable_bounds
  call ascii_title_round_trip
  write(*,'(A,I0,A,I0,A)') 'tecplot_write: ', checks, ' checks, ', failures, ' failed'
  if (failures > 0) stop 1

  data%tec%node = .false.
  data%tec%bc = .false.

  allocate(data%block(1:2))

  data%block(1)%name = 'blocco-A'
  data%block(2)%name = 'blocco-B'

  allocate(data%block(1)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(data%block(1)%vars(1:2,nx1+1:nx2,ny1+1:ny2,nz1+1:nz2))
  allocate(data%block(2)%mesh(1:3,nx1:nx2+10,ny1:ny2,nz1:nz2))
  allocate(data%block(2)%vars(1:2,nx1+1:nx2+10,ny1+1:ny2,nz1+1:nz2))

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2
        data%block(1)%mesh(1,i,j,k) = dble(i)
        data%block(1)%mesh(2,i,j,k) = dble(j)
        data%block(1)%mesh(3,i,j,k) = dble(k)
      enddo
    enddo
  enddo
  do k=nz1+1, nz2
    do j=ny1+1, ny2
      do i=nx1+1, nx2
        data%block(1)%vars(1,i,j,k) = dble(i*j*k)
        data%block(1)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2+10
        data%block(2)%mesh(1,i,j,k) = data%block(1)%mesh(1,nx2,j,k)+dble(i)
        data%block(2)%mesh(2,i,j,k) = dble(j)
        data%block(2)%mesh(3,i,j,k) = dble(k)
      enddo
    enddo
  enddo
  do k=nz1+1, nz2
    do j=ny1+1, ny2
      do i=nx1+1, nx2+10
        data%block(2)%vars(1,i,j,k) = dble(i*j*k)
        data%block(2)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

#ifdef TECIO
  data%tec%format = 'binary'
  error = tec_write_structured_multiblock(orion=data,varnames='variable1 variable2',filename='tecfile.szplt')

  data%tec%format = 'binary'
  error = tec_write_structured_multiblock(orion=data,varnames='variable1 variable2',filename='tecfile.plt')
#endif

  data%tec%format = 'ascii'
  error = tec_write_structured_multiblock(orion=data,varnames='"variable1"',filename='tecfile.tec',Nvars=1)

  deallocate(data%block)

  allocate(data%block(1:2))

  data%block(1)%name = 'blocco-A'
  data%block(2)%name = 'blocco-B'

  allocate(data%block(1)%mesh(1:2,nx1:nx2,ny1:ny2,0:0))
  allocate(data%block(1)%vars(1:2,nx1+1:nx2,ny1+1:ny2,1:1))
  allocate(data%block(2)%mesh(1:2,nx1:nx2+10,ny1:ny2,0:0))
  allocate(data%block(2)%vars(1:2,nx1+1:nx2+10,ny1+1:ny2,1:1))

  do j=ny1, ny2
    do i=nx1, nx2
      data%block(1)%mesh(1,i,j,0) = dble(i)
      data%block(1)%mesh(2,i,j,0) = dble(j)
    enddo
  enddo

  do j=ny1+1, ny2
    do i=nx1+1, nx2
      data%block(1)%vars(1,i,j,1) = dble(i*j)
      data%block(1)%vars(2,i,j,1) = -dble(i*j)
    enddo
  enddo

  do j=ny1, ny2
    do i=nx1, nx2+10
      data%block(2)%mesh(1,i,j,0) = data%block(1)%mesh(1,nx2,j,0)+dble(i)
      data%block(2)%mesh(2,i,j,0) = dble(j)
    enddo
  enddo
  do j=ny1+1, ny2
    do i=nx1+1, nx2+10
      data%block(2)%vars(1,i,j,1) = dble(i*j)
      data%block(2)%vars(2,i,j,1) = -dble(i*j)
    enddo
  enddo

#ifdef TECIO
  data%tec%format = 'binary'
  error = tec_write_structured_multiblock(orion=data,varnames='variable1 variable2',filename='tec2D.szplt')
#endif

  data%tec%format = 'ascii'
  error = tec_write_structured_multiblock(orion=data,varnames='"variable1"',filename='tec2D.tec',Nvars=1)

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

  integer function keyword(line, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The integer after the keyword key (such as ' I=') in a zone header, up to the next comma; -1 when it is not there.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: line, key
  integer                      :: p, q, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  keyword = -1
  p = index(line, key); if (p == 0) return
  p = p + len(key)
  q = index(line(p:), ',') + p - 2; if (q < p) q = len_trim(line)
  read(line(p:q), *, iostat=ios) keyword
  if (ios /= 0) keyword = -1
  end function keyword
#ifdef TECIO

  subroutine surface_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Surface blocks and lines of nodes, with TecIO only. tec_write_structured_multiblock writes surface blocks, blocks with one
  !< plane of nodes in one direction (Ni, Nj or Nk = 0) such as the faces of a volume block, and lines of nodes, as ordered
  !< zones with one node plane there. Their variables at the cells hold one layer of cells in that direction,
  !< max(Ni,1)*max(Nj,1)*max(Nk,1) values, and their variables at the nodes one value per node, as an ordered zone stores them
  !< (Tecplot 360 Data Format Guide, section 2-1). Three fields are written: a three-dimensional one of a surface block per
  !< direction (i: 0 x 4 x 3 cells, j: 5 x 0 x 2, k: 3 x 4 x 0), a line of nodes (4 x 0 x 0) and a volume block (2 x 3 x 2)
  !< with two variables at the cells; the same field with two variables at the nodes (orion%tec%node); a two-dimensional one
  !< (two coordinates) of a block of 4 x 3 cells and two lines of nodes (5 x 0 and 0 x 3) with two variables at the cells. Each
  !< field goes to a .szplt, a .plt and an ASCII file. Each .szplt file is read back with the TecIO reader: number of zones and
  !< variables, I, J and K of each zone, every coordinate and, for each variable, its location, its number of values and the
  !< values, bit for bit (every value is an integer, a half or a quarter); the values at the cells of a zone with JMax = 1 and
  !< KMax > 1 (the face j) are read one layer of cells in K at a time, as the TecIO reader gives them right only so. The TecIO
  !< reader reads .szplt files only: of the .plt files the checks are that they are written without error. Of the ASCII files
  !< the I, J and K of each zone header and the values that follow it are read, the same as in the .szplt file.
  !< Defect: the binary writer counted no cell value for a block with one plane of nodes in i or j and copied the values of its
  !< layer of cells into a buffer of 0 values, past its end; variables at the nodes of such blocks were taken over two planes,
  !< past the end of their arrays.
  !< Basis: Tecplot 360 Data Format Guide, section 2-1 (p. 15): max(IMAX-1,1)*max(JMAX-1,1)*max(KMAX-1,1) cell values, as TecIO
  !< computes them; Fortran 2018, 9.5.3.1 (p. 128): a subscript within the bounds of its dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), parameter :: dims3(3,5) = reshape([0,4,3, 5,0,2, 3,4,0, 4,0,0, 2,3,2], [3,5])
  integer(I4P), parameter :: dims2(3,3) = reshape([4,3,0, 5,0,0, 0,3,0], [3,3])
  !---------------------------------------------------------------------------------------------------------------------------------

  call write_and_check(3, dims3, .false., 'tecsurface')
  call write_and_check(3, dims3, .true., 'tecsurface_nodes')
  call write_and_check(2, dims2, .false., 'tecsurface_2d')
  end subroutine surface_blocks

  pure function node(b, i, j, k) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Coordinates of node (i, j, k) of block b: integers and halves, exact in every format. A two-dimensional field takes x and y.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  x = [real(i + 10*b, R8P), real(j, R8P) + 0.5_R8P*k, real(k + b, R8P)]
  end function node

  pure function value(b, i, j, k) result(v)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Values of the two variables in cell or node (i, j, k) of block b.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: v(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  v = [real(i + 10*j + 100*k, R8P) + 0.25_R8P, -real(b, R8P) - 0.5_R8P]
  end function value

  pure subroutine bounds(nodal, d, lo, hi)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Index bounds of the variables of a block of d cells: the nodes, or the cells with one layer where a direction has one plane
  !< of nodes (the k layer of a two-dimensional block).
  !---------------------------------------------------------------------------------------------------------------------------------
  logical,      intent(in)  :: nodal
  integer(I4P), intent(in)  :: d(3)
  integer(I4P), intent(out) :: lo(3), hi(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  if (nodal) then
    lo = 0; hi = d
  else
    lo = 1; hi = max(d, 1)
  endif
  end subroutine bounds

  subroutine write_and_check(nd, dims, nodal, base)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field of nd coordinates and blocks of dims cells, with variables at the cells or at the nodes, to base.szplt,
  !< base.plt and base.tec and check the files.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer,          intent(in) :: nd
  integer(I4P),     intent(in) :: dims(:,:)
  logical,          intent(in) :: nodal
  character(len=*), intent(in) :: base
  type(orion_data)             :: orion
  integer(I4P)                 :: b, i, j, k, err, lo(3), hi(3)
  real(R8P)                    :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(orion%block(size(dims,2)))
  do b = 1, size(dims,2)
    orion%block(b)%name = 'B'//trim(str(.true., b))
    orion%block(b)%Ni = dims(1,b); orion%block(b)%Nj = dims(2,b); orion%block(b)%Nk = dims(3,b)
    allocate(orion%block(b)%mesh(1:nd, 0:dims(1,b), 0:dims(2,b), 0:dims(3,b)))
    do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
      x = node(b, i, j, k)
      orion%block(b)%mesh(:,i,j,k) = x(1:nd)
    enddo; enddo; enddo
    call bounds(nodal, dims(:,b), lo, hi)
    allocate(orion%block(b)%vars(1:2, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)))
    do k = lo(3), hi(3); do j = lo(2), hi(2); do i = lo(1), hi(1)
      orion%block(b)%vars(:,i,j,k) = value(b, i, j, k)
    enddo; enddo; enddo
  enddo
  orion%tec%node = nodal

  orion%tec%format = 'binary'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename=base//'.szplt')
  call check(err == 0, base//'.szplt: written without error')
  call read_szplt(base//'.szplt', nd, dims, nodal)
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename=base//'.plt')
  call check(err == 0, base//'.plt: written without error')

  orion%tec%format = 'ascii'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename=base//'.tec')
  call check(err == 0, base//'.tec: written without error')
  call read_ascii(base//'.tec', nd, dims, nodal)
  end subroutine write_and_check

  subroutine read_szplt(fname, nd, dims, nodal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read the .szplt file back with the TecIO reader and check zones, nodes and the values of the variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname
  integer,          intent(in)  :: nd
  integer(I4P),     intent(in)  :: dims(:,:)
  logical,          intent(in)  :: nodal
  type(c_ptr)                   :: h
  integer(c_int32_t)            :: nzones, nvars, z, v, loc, e
  integer(c_int64_t)            :: imax, jmax, kmax, nval
  real(c_double), allocatable   :: vals(:)
  real(R8P),      allocatable   :: expected(:)
  real(R8P)                     :: x(3), c(2)
  character(len=48)             :: label
  integer(I4P)                  :: i, j, k, lo(3), hi(3)
  integer                       :: m, nl
  !---------------------------------------------------------------------------------------------------------------------------------

  h = c_null_ptr
  e = tecFileReaderOpen(fname//c_null_char, h)
  call check(e == 0, fname//': opened by the TecIO reader')
  if (e /= 0) return
  e = tecDataSetGetNumZones(h, nzones)
  call check(e == 0 .and. nzones == size(dims,2), fname//': '//trim(str(.true., size(dims,2,kind=I4P)))//' zones')
  e = tecDataSetGetNumVars(h, nvars)
  call check(e == 0 .and. nvars == nd + 2, fname//': '//trim(str(.true., int(nd + 2, I4P)))//' variables (coordinates, a, b)')
  if (nzones /= size(dims,2) .or. nvars /= nd + 2) then
    e = tecFileReaderClose(h)
    return
  endif
  do z = 1, nzones
    label = fname//' zone '//trim(str(.true., int(z, I4P)))
    e = tecZoneGetIJK(h, z, imax, jmax, kmax)
    call check(e == 0 .and. imax == dims(1,z)+1 .and. jmax == dims(2,z)+1 .and. kmax == dims(3,z)+1, trim(label)//': I, J, K')
    call bounds(nodal, dims(:,z), lo, hi)
    do v = 1, nd + 2
      e = tecZoneVarGetValueLocation(h, z, v, loc)
      if (v <= nd) then
        call check(e == 0 .and. loc == 1, trim(label)//': variable '//trim(str(.true., int(v, I4P)))//' at the nodes')
        allocate(expected((dims(1,z)+1)*(dims(2,z)+1)*(dims(3,z)+1))); m = 0
        do k = 0, dims(3,z); do j = 0, dims(2,z); do i = 0, dims(1,z)
          m = m + 1; x = node(z, i, j, k); expected(m) = x(v)
        enddo; enddo; enddo
      else
        call check(e == 0 .and. loc == merge(1, 0, nodal), trim(label)//': variable '//trim(str(.true., int(v, I4P)))// &
                   merge(' at the nodes', ' at the cells', nodal))
        allocate(expected(product(hi - lo + 1))); m = 0
        do k = lo(3), hi(3); do j = lo(2), hi(2); do i = lo(1), hi(1)
          m = m + 1; c = value(z, i, j, k); expected(m) = c(v-nd)
        enddo; enddo; enddo
      endif
      e = tecZoneVarGetNumValues(h, z, v, nval)
      call check(e == 0 .and. nval == size(expected), trim(label)//': number of values of variable '// &
                 trim(str(.true., int(v, I4P)))//' ('//trim(str(.true., int(nval, I4P)))//', '// &
                 trim(str(.true., size(expected, kind=I4P)))//' expected)')
      if (e == 0 .and. nval == size(expected)) then
        allocate(vals(nval))
        if (v > nd .and. .not.nodal .and. jmax == 1 .and. kmax > 1) then
          ! The TecIO reader gives the values at the cells of a zone with JMax = 1 and KMax > 1 right one layer of cells in K at a
          ! time, not in one call (the values after the first layer are those of the next variable): read them layer by layer
          nl = int(max(imax-1, 1_c_int64_t))
          do k = 1, int(kmax-1, I4P)
            e = tecZoneVarGetDoubleValues(h, z, v, int((k-1)*nl+1, c_int64_t), int(nl, c_int64_t), vals((k-1)*nl+1:k*nl))
          enddo
        else
          e = tecZoneVarGetDoubleValues(h, z, v, 1_c_int64_t, nval, vals)
        endif
        call check(e == 0 .and. all(vals == expected), trim(label)//': values of variable '//trim(str(.true., int(v, I4P))))
        deallocate(vals)
      endif
      deallocate(expected)
    enddo
  enddo
  e = tecFileReaderClose(h)
  end subroutine read_szplt

  subroutine read_ascii(fname, nd, dims, nodal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read the ASCII file back as text: the I, J and K of each zone header and the values that follow it, the coordinates at the
  !< nodes and the two variables, block by block.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname
  integer,          intent(in)  :: nd
  integer(I4P),     intent(in)  :: dims(:,:)
  logical,          intent(in)  :: nodal
  character(len=*), parameter   :: names(2:3) = ['VARIABLES = x y a b  ', 'VARIABLES = x y z a b']
  character(len=1024)           :: line
  character(len=48)             :: label
  real(R8P),        allocatable :: vals(:), expected(:)
  real(R8P)                     :: x(3), c(2)
  integer(I4P)                  :: i, j, k, lo(3), hi(3)
  integer                       :: u, ios, z, v, m, np, nc, ijk(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=fname, status='old', action='read')
  read(u,'(A)',iostat=ios) line
  call check(ios == 0 .and. index(line, trim(names(nd))) > 0, fname//': '//trim(names(nd)))
  do z = 1, size(dims,2)
    label = fname//' zone '//trim(str(.true., int(z, I4P)))
    read(u,'(A)',iostat=ios) line
    call check(ios == 0 .and. index(line, 'ZONE') > 0, trim(label)//': zone header')
    if (ios /= 0 .or. index(line, 'ZONE') == 0) exit
    ijk = [keyword(line, ' I='), keyword(line, ' J='), keyword(line, ' K=')]
    call check(all(ijk == dims(:,z) + 1), trim(label)//': I, J, K')
    call bounds(nodal, dims(:,z), lo, hi)
    np = (dims(1,z)+1)*(dims(2,z)+1)*(dims(3,z)+1)
    nc = product(hi - lo + 1)
    allocate(vals(nd*np + 2*nc), expected(nd*np + 2*nc))
    m = 0
    do v = 1, nd
      do k = 0, dims(3,z); do j = 0, dims(2,z); do i = 0, dims(1,z)
        m = m + 1; x = node(z, i, j, k); expected(m) = x(v)
      enddo; enddo; enddo
    enddo
    do v = 1, 2
      do k = lo(3), hi(3); do j = lo(2), hi(2); do i = lo(1), hi(1)
        m = m + 1; c = value(z, i, j, k); expected(m) = c(v)
      enddo; enddo; enddo
    enddo
    read(u,*,iostat=ios) vals
    call check(ios == 0 .and. all(vals == expected), trim(label)//': nodes and values of the variables')
    deallocate(vals, expected)
  enddo
  close(u)
  end subroutine read_ascii
#endif

  subroutine variable_bounds
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The index ranges of the variables. tec_write_structured_multiblock takes the variables of a block from the index range of
  !< the values to write in each direction, 1:max(N,1) for variables at the cells and 0:N for variables at the nodes (1:1 in k
  !< for a two-dimensional block), or from the range the variables have there when it has the same length but other bounds: the
  !< node plane of a two-dimensional field is vars(:,:,:,0:0) as the Tecplot readers return it, or vars(:,:,:,1:1); variables
  !< with ghost cells are written without them. ASCII files of one block with two variables are written: a two-dimensional
  !< field of 4 x 3 nodes with the variables at the nodes in vars(:,0:3,0:2,0:0) and in vars(:,0:3,0:2,1:1), and with the
  !< variables at the cells; a three-dimensional field of 3 x 2 x 2 cells with the variables at the cells in
  !< vars(:,1:3,1:2,1:2) and with a layer of ghost cells, vars(:,0:4,0:3,0:3), and with the variables at the nodes in
  !< vars(:,0:3,0:2,0:2) and in vars(:,1:4,1:3,1:3). Of each file the zone header and the values that follow it are read: I, J
  !< and K, the coordinates and the values of the variables, each the value of its node or cell. The two files of each field
  !< must be the same, byte for byte. A two-dimensional field with variables at the nodes is also written with
  !< vtk_write_structured_multiblock, read with vtk_read_structured_multiblock (variables in vars(:,0:3,0:2,0:0)) and written
  !< to a Tecplot ASCII file, and with TecIO to a .szplt file read back: the values must be those written to VTK. Every value
  !< is an integer, a half or a quarter.
  !< Defect: the writer took the variables from fixed index ranges, 1:1 in k for a two-dimensional block at the nodes too, so a
  !< two-dimensional field read from a VTK or a Tecplot file (variables at the nodes in vars(:,:,:,0:0)) was written from past
  !< the end of its variables.
  !< Basis: Fortran 2018, 9.5.3.1 (p. 128): the value of a subscript shall be within the bounds of its dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  ! Two-dimensional field, 4 x 3 nodes (3 x 2 cells), variables at the nodes: the plane at k = 0 and at k = 1
  call write_and_check_bounds('bounds_2d_nodes_k0', 2, [3, 2, 0], .true., [0, 0, 0], [3, 2, 0])
  call write_and_check_bounds('bounds_2d_nodes_k1', 2, [3, 2, 0], .true., [0, 0, 1], [3, 2, 1])
  call same_files('bounds_2d_nodes_k0.tec', 'bounds_2d_nodes_k1.tec')
  ! Two-dimensional field, variables at the cells
  call write_and_check_bounds('bounds_2d_cells', 2, [3, 2, 0], .false., [1, 1, 1], [3, 2, 1])
  ! Three-dimensional field, 3 x 2 x 2 cells, variables at the cells: exact and with a layer of ghost cells
  call write_and_check_bounds('bounds_3d_cells', 3, [3, 2, 2], .false., [1, 1, 1], [3, 2, 2])
  call write_and_check_bounds('bounds_3d_cells_ghosts', 3, [3, 2, 2], .false., [0, 0, 0], [4, 3, 3])
  call same_files('bounds_3d_cells.tec', 'bounds_3d_cells_ghosts.tec')
  ! Three-dimensional field, variables at the nodes: from 0 and from 1
  call write_and_check_bounds('bounds_3d_nodes_0', 3, [3, 2, 2], .true., [0, 0, 0], [3, 2, 2])
  call write_and_check_bounds('bounds_3d_nodes_1', 3, [3, 2, 2], .true., [1, 1, 1], [4, 3, 3])
  call same_files('bounds_3d_nodes_0.tec', 'bounds_3d_nodes_1.tec')
  ! A two-dimensional field with variables at the nodes, read from VTK and written to Tecplot
  call read_and_write_again
  end subroutine variable_bounds

  pure function node_bounds(i, j, k) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Coordinates of node (i, j, k): integers and halves.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: i, j, k
  real(R8P)           :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  x = [real(i, R8P) + 0.5_R8P, real(j, R8P) + 10.0_R8P, real(k, R8P) + 100.0_R8P]
  end function node_bounds

  pure function value_bounds(i, j, k) result(v)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Values of the two variables at node or cell (i, j, k), counted from the first node or cell of the block (0 or 1).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: i, j, k
  real(R8P)           :: v(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  v = [real(1000 + i + 10*j + 100*k, R8P) + 0.25_R8P, -real(2000 + i + 10*j + 100*k, R8P) - 0.5_R8P]
  end function value_bounds

  subroutine write_and_check_bounds(base, nd, cells, nodal, lo, hi)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of one block of cells(1:3) cells (cells(3) = 0 for two dimensions) with nd coordinates and two variables
  !< allocated as vars(:,lo(1):hi(1),lo(2):hi(2),lo(3):hi(3)), at the nodes or at the cells, to base.tec; check the file.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: base
  integer,          intent(in) :: nd, cells(3), lo(3), hi(3)
  logical,          intent(in) :: nodal
  type(orion_data)             :: orion
  real(R8P)                    :: x(3)
  integer                      :: i, j, k, first, err, n(3), shift(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(orion%block(1))
  orion%block(1)%name = 'B1'
  orion%block(1)%Ni = cells(1); orion%block(1)%Nj = cells(2); orion%block(1)%Nk = cells(3)
  allocate(orion%block(1)%mesh(1:nd, 0:cells(1), 0:cells(2), 0:cells(3)))
  do k = 0, cells(3); do j = 0, cells(2); do i = 0, cells(1)
    x = node_bounds(i, j, k)
    orion%block(1)%mesh(:,i,j,k) = x(1:nd)
  enddo; enddo; enddo
  allocate(orion%block(1)%vars(1:2, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)))
  ! The place of a node is counted from 0 and that of a cell from 1, as the writer counts them (cells(d)+1 nodes and
  ! max(cells(d),1) cells in direction d). An array of that length holds the value of each place at lo(d) plus the place less
  ! the first place; a longer one, with ghost cells, at the place itself, and its ghost cells hold the values of other places
  first = merge(0, 1, nodal)
  do i = 1, 3
    n(i) = merge(cells(i) + 1, max(cells(i), 1), nodal)
    shift(i) = 0
    if (hi(i) - lo(i) + 1 == n(i)) shift(i) = lo(i) - first
  enddo
  do k = lo(3), hi(3); do j = lo(2), hi(2); do i = lo(1), hi(1)
    orion%block(1)%vars(:,i,j,k) = value_bounds(i - shift(1), j - shift(2), k - shift(3))
  enddo; enddo; enddo
  orion%tec%node = nodal
  orion%tec%format = 'ascii'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename=base//'.tec')
  call check(err == 0, base//'.tec: written without error')
  call read_ascii_bounds(base//'.tec', nd, cells, nodal)
  end subroutine write_and_check_bounds

  subroutine read_ascii_bounds(fname, nd, cells, nodal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read the ASCII file as text: I, J and K of the zone header and the values that follow it, the coordinates at the nodes and
  !< the two variables, each the value of its node (counted from 0) or cell (counted from 1).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fname
  integer,          intent(in) :: nd, cells(3)
  logical,          intent(in) :: nodal
  character(len=1024)          :: line
  real(R8P),       allocatable :: vals(:), expected(:)
  real(R8P)                    :: x(3), c(2)
  integer                      :: u, ios, i, j, k, v, m, np, nc, lo(3), hi(3), ijk(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  if (nodal) then
    lo = 0; hi = cells
  else
    lo = 1; hi = max(cells, 1)
  endif
  if (nd == 2) then
    lo(3) = 0; hi(3) = 0
    if (.not.nodal) then
      lo(3) = 1; hi(3) = 1
    endif
  endif
  np = product(cells + 1)
  nc = product(hi - lo + 1)
  open(newunit=u, file=fname, status='old', action='read')
  read(u,'(A)',iostat=ios) line
  read(u,'(A)',iostat=ios) line
  call check(ios == 0 .and. index(line, 'ZONE') > 0, fname//': zone header')
  ijk = [keyword(line, ' I='), keyword(line, ' J='), keyword(line, ' K=')]
  call check(all(ijk == cells + 1), fname//': I, J, K')
  allocate(vals(nd*np + 2*nc), expected(nd*np + 2*nc))
  m = 0
  do v = 1, nd
    do k = 0, cells(3); do j = 0, cells(2); do i = 0, cells(1)
      m = m + 1; x = node_bounds(i, j, k); expected(m) = x(v)
    enddo; enddo; enddo
  enddo
  do v = 1, 2
    do k = lo(3), hi(3); do j = lo(2), hi(2); do i = lo(1), hi(1)
      m = m + 1; c = value_bounds(i, j, k); expected(m) = c(v)
    enddo; enddo; enddo
  enddo
  read(u,*,iostat=ios) vals
  call check(ios == 0 .and. all(vals == expected), fname//': coordinates and values of the variables')
  close(u)
  end subroutine read_ascii_bounds

  subroutine same_files(a, b)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The two files are the same, byte for byte.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: a, b
  character(len=:), allocatable :: sa, sb
  integer                       :: u, n, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  sa = ''; sb = ''
  open(newunit=u, file=a, access='stream', form='unformatted', status='old', action='read', iostat=ios)
  if (ios == 0) then
    inquire(unit=u, size=n); deallocate(sa); allocate(character(len=n) :: sa); read(u) sa; close(u)
  endif
  open(newunit=u, file=b, access='stream', form='unformatted', status='old', action='read', iostat=ios)
  if (ios == 0) then
    inquire(unit=u, size=n); deallocate(sb); allocate(character(len=n) :: sb); read(u) sb; close(u)
  endif
  call check(len(sa) > 0 .and. sa == sb .and. len(sa) == len(sb), a//' and '//b//': the same bytes')
  end subroutine same_files

  subroutine read_and_write_again
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A two-dimensional field of one block of 4 x 3 nodes with the variables at the nodes, written with
  !< vtk_write_structured_multiblock in the raw format, read with vtk_read_structured_multiblock (variables in
  !< vars(:,0:3,0:2,0:0)) and written to a Tecplot ASCII file: the values written to VTK. With TecIO the same field is also
  !< written to a .szplt file, by the binary branch of the writer, and read back with tec_read_structured_multiblock: the
  !< values written to VTK, one per node.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: w, orion
  character(len=8) :: varnames
  real(R8P)        :: x(3)
  integer          :: i, j, err
#ifdef TECIO
  type(orion_data) :: back
  logical          :: same
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'bounds_vtk_B1'
  w%block(1)%Ni = 3; w%block(1)%Nj = 2; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(2,0:3,0:2,0:0), w%block(1)%vars(2,0:3,0:2,0:0))
  do j = 0, 2; do i = 0, 3
    x = node_bounds(i, j, 0)
    w%block(1)%mesh(:,i,j,0) = x(1:2)
    w%block(1)%vars(:,i,j,0) = value_bounds(i, j, 0)
  enddo; enddo
  w%vtk%format = 'raw'
  w%vtk%node = .true.
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='bounds_vtk', varnames=varnames)
  call check(err == 0, 'bounds_vtk.vtm: written without error')
  orion%vtk%format = 'raw'
  err = vtk_read_structured_multiblock(orion=orion, vtmpath='bounds_vtk', vtspath='')
  call check(err == 0, 'bounds_vtk.vtm: read without error')
  if (err /= 0) return
  call check(orion%vtk%node .and. size(orion%block(1)%mesh,1) == 2 .and. lbound(orion%block(1)%vars,4) == 0 .and. &
             ubound(orion%block(1)%vars,4) == 0, 'bounds_vtk.vtm: two coordinates, variables at the nodes in vars(:,:,:,0:0)')
  orion%tec%node = orion%vtk%node
  orion%tec%format = 'ascii'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename='bounds_written_again.tec')
  call check(err == 0, 'bounds_written_again.tec: written without error')
  call read_ascii_bounds('bounds_written_again.tec', 2, [3, 2, 0], .true.)
#ifdef TECIO
  orion%tec%format = 'binary'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename='bounds_written_again.szplt')
  call check(err == 0, 'bounds_written_again.szplt: written without error')
  back%tec%format = 'binary'
  err = tec_read_structured_multiblock(orion=back, filename='bounds_written_again.szplt')
  call check(err == 0, 'bounds_written_again.szplt: read without error')
  if (err /= 0) return
  same = size(back%block) == 1
  if (same) same = all(shape(back%block(1)%vars) == [2, 4, 3, 1])
  if (same) same = all(back%block(1)%vars(:,:,:,lbound(back%block(1)%vars,4)) == orion%block(1)%vars(:,:,:,0))
  call check(same, 'bounds_written_again.szplt: the values written to VTK, one per node')
#endif
  end subroutine read_and_write_again
  subroutine ascii_title_round_trip
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A Tecplot ASCII file read and written again keeps its zone titles. A two-dimensional file of one zone of 4 x 3 nodes titled
  !< Z1, with the variables at the nodes, written by hand, is read with the Tecplot ASCII reader (the block named Z1, the
  !< variables in vars(:,0:3,0:2,0:0)) and written again with tec_write_structured_multiblock: the zone header holds T = Z1, and
  !< I, J and K and the values are those written by hand.
  !< Defect: the ASCII reader left the names of the blocks undefined, so a file read and written again got zone titles made of
  !< what the memory held, and a zone header that could not be read back.
  !< Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2 (p. 123): T is the zone title.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: orion
  real(R8P)          :: x(3), c(2)
  character(len=256) :: line
  integer            :: u, v, i, j, err, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file='bounds_by_hand.tec', status='replace', action='write')
  write(u,'(A)') 'VARIABLES = x y a b'
  write(u,'(A)') 'ZONE T = Z1, I=4, J=3, K=1, DATAPACKING=BLOCK'
  do v = 1, 4
    do j = 0, 2; do i = 0, 3
      x = node_bounds(i, j, 0); c = value_bounds(i, j, 0)
      if (v <= 2) then
        write(u,'(ES25.17)') x(v)
      else
        write(u,'(ES25.17)') c(v-2)
      endif
    enddo; enddo
  enddo
  close(u)
  err = tec_read_structured_multiblock(orion=orion, filename='bounds_by_hand.tec')
  call check(err == 0, 'bounds_by_hand.tec: read without error')
  if (err /= 0) return
  call check(trim(orion%block(1)%name) == 'Z1', 'bounds_by_hand.tec: the block named Z1, the title of its zone')
  call check(orion%tec%node .and. size(orion%block(1)%mesh,1) == 2, 'bounds_by_hand.tec: two coordinates, variables at the nodes')
  orion%tec%format = 'ascii'
  err = tec_write_structured_multiblock(orion=orion, varnames='a b', filename='bounds_written_again_ascii.tec')
  call check(err == 0, 'bounds_written_again_ascii.tec: written without error')
  line = ''
  open(newunit=u, file='bounds_written_again_ascii.tec', status='old', action='read')
  read(u,'(A)',iostat=ios) line
  read(u,'(A)',iostat=ios) line
  close(u)
  call check(index(line, 'T = Z1,') > 0, 'bounds_written_again_ascii.tec: the zone title Z1')
  call read_ascii_bounds('bounds_written_again_ascii.tec', 2, [3, 2, 0], .true.)
  end subroutine ascii_title_round_trip
end program tecplot_write_multiblock