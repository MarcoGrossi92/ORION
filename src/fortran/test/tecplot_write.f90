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
  call precision_szplt
  call binary_name
  call binary_status
  call extension_names
#endif
  call variable_bounds
  call ascii_title_round_trip
  call digits_ascii
  call names_count
  call mesh_only_counts
  call nvars_range
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

  subroutine digits_ascii
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Every 64-bit real of an ASCII file comes back, bit for bit, when the file is read. A two-dimensional field of one block of
  !< 3 x 2 cells with two variables at the cells and solutiontime = 0.1 + 0.2 is written to an ASCII file and read back with
  !< tec_read_structured_multiblock: every coordinate, every value of the variables and the solution time must be the real
  !< written (==). They are reals that 15 significant digits do not give back (hard_real).
  !< Defect: the writer wrote 15 significant digits (format E23.15E3), and a real that needs more came back as another real: a
  !< field written and read again, as a solution from which a run continues, was not the field written.
  !< Basis: the Fortran API guide (the derivation of the 17 significant digits): the decimal numbers of n significant digits are
  !< closer to one another than the 64-bit reals (53-bit significand) in every range when 10**(n-1) > 2**53, n >= 17, so the
  !< nearest decimal number of 17 digits reads back as the real it was written from.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: w, r
  integer          :: i, j, n, err
  logical          :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 3; w%block(1)%Nj = 2; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:3,0:2,0:0), w%block(1)%vars(1:2,1:3,1:2,1:1))
  n = 0
  do j = 0, 2; do i = 0, 3
    n = n + 1; w%block(1)%mesh(1,i,j,0) = hard_real(n)
    n = n + 1; w%block(1)%mesh(2,i,j,0) = hard_real(n)
  enddo; enddo
  do j = 1, 2; do i = 1, 3
    n = n + 1; w%block(1)%vars(1,i,j,1) = hard_real(n)
    n = n + 1; w%block(1)%vars(2,i,j,1) = hard_real(n)
  enddo; enddo
  w%solutiontime = 0.1_R8P + 0.2_R8P
  w%tec%node = .false.
  w%tec%format = 'ascii'
  err = tec_write_structured_multiblock(orion=w, varnames='a b', filename='digits.tec')
  call check(err == 0, 'digits.tec: written without error')
  r%tec%format = 'ascii'
  err = tec_read_structured_multiblock(orion=r, filename='digits.tec')
  call check(err == 0, 'digits.tec: read without error')
  if (err /= 0) return
  same = size(r%block) == 1
  if (same) same = all(shape(r%block(1)%mesh) == shape(w%block(1)%mesh)) .and. &
                   all(shape(r%block(1)%vars) == shape(w%block(1)%vars))
  call check(same, 'digits.tec: one block of two coordinates and two variables at the cells')
  if (.not.same) return
  call check(all(r%block(1)%mesh == w%block(1)%mesh), 'digits.tec: every coordinate, bit for bit')
  call check(all(r%block(1)%vars == w%block(1)%vars), 'digits.tec: every value of the variables, bit for bit')
  call check(r%solutiontime == w%solutiontime, 'digits.tec: the solution time, bit for bit')
  end subroutine digits_ascii

#ifdef TECIO
  subroutine precision_szplt
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The precision of the values of a .szplt file follows orion%tec%double: a two-dimensional field of one block of 3 x 2 cells,
  !< two variables at the cells, every coordinate and value 0.1*n + 1/3 (n = 1, 2, ..., none of them a 32-bit real), is written
  !< with tec%double true and false and read back with tec_read_structured_multiblock: with true every real is the one written
  !< (==); with false every real is the one written rounded to 32 bits, real(real(x, R4P), R8P), and not the one written.
  !< Change of main checked (99e843a): the writer asks TecIO for 64-bit values when tec%double is true (VIsDouble = 1); before
  !< it, every binary file held 32-bit values.
  !< Basis: Tecplot 360 EX 2023 R1 Data Format Guide, TECINI142 (p. 45): VIsDouble is "the integer flag for specifying whether
  !< field data generated in future calls to TECDAT142 are to be written in single or double precision. 0=Single 1=Double".
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w, r
  integer                       :: i, j, n, err, f
  logical                       :: same, rounded, other
  character(len=*), parameter   :: files(2) = ['precision_double.szplt', 'precision_single.szplt']
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 3; w%block(1)%Nj = 2; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:3,0:2,0:0), w%block(1)%vars(1:2,1:3,1:2,1:1))
  n = 0
  do j = 0, 2; do i = 0, 3
    n = n + 1; w%block(1)%mesh(1,i,j,0) = 0.1_R8P*n + 1._R8P/3._R8P
    n = n + 1; w%block(1)%mesh(2,i,j,0) = 0.1_R8P*n + 1._R8P/3._R8P
  enddo; enddo
  do j = 1, 2; do i = 1, 3
    n = n + 1; w%block(1)%vars(1,i,j,1) = 0.1_R8P*n + 1._R8P/3._R8P
    n = n + 1; w%block(1)%vars(2,i,j,1) = 0.1_R8P*n + 1._R8P/3._R8P
  enddo; enddo
  w%tec%node = .false.
  w%tec%format = 'binary'
  do f = 1, 2
    w%tec%double = (f == 1)
    err = tec_write_structured_multiblock(orion=w, varnames='a b', filename=files(f))
    call check(err == 0, files(f)//': written without error')
    if (allocated(r%block)) deallocate(r%block)
    r%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=r, filename=files(f))
    call check(err == 0, files(f)//': read without error')
    if (err /= 0) cycle
    same = size(r%block) == 1
    if (same) same = all(shape(r%block(1)%mesh) == shape(w%block(1)%mesh)) .and. &
                     all(shape(r%block(1)%vars) == shape(w%block(1)%vars))
    call check(same, files(f)//': the shape of the field')
    if (.not.same) cycle
    if (f == 1) then
      call check(all(r%block(1)%mesh == w%block(1)%mesh) .and. all(r%block(1)%vars == w%block(1)%vars), &
                 files(f)//': every real as written (64 bits)')
    else
      rounded = all(r%block(1)%mesh == real(real(w%block(1)%mesh, R4P), R8P)) .and. &
                all(r%block(1)%vars == real(real(w%block(1)%vars, R4P), R8P))
      other = any(r%block(1)%vars /= w%block(1)%vars)
      call check(rounded .and. other, files(f)//': every real rounded to 32 bits')
    endif
  enddo
  end subroutine precision_szplt

  subroutine binary_name
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A binary Tecplot file is a .plt or a .szplt file: a two-dimensional field of one block of 2 x 1 cells with one variable at
  !< the cells, asked for in the binary format under the name tecfile.bin, gives the return value 1 and no file.
  !< Defect: the writer printed its message and returned without giving its result a value: the value returned was undefined
  !< (-77 in a build where an integer without a value is -77), as that of the reader for a name without the extensions it reads.
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 15.6.2.2, paragraph 4 (p. 319): "If the function
  !< result is not a pointer, its value shall be defined by the function".
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: w
  integer          :: i, j, err, u
  logical          :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:1,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  w%tec%node = .false.
  w%tec%format = 'binary'
  ! No file of that name from an earlier run, so that the check measures this write
  inquire(file='tecfile.bin', exist=exists)
  if (exists) then
    open(newunit=u, file='tecfile.bin', status='old')
    close(u, status='delete')
  endif
  err = tec_write_structured_multiblock(orion=w, varnames='a', filename='tecfile.bin')
  write(*,'(A,I0)') 'tecfile.bin: return value ', err
  call check(err == 1, 'tecfile.bin: a binary file name without .plt or .szplt refused with the return value 1')
  inquire(file='tecfile.bin', exist=exists)
  call check(.not.exists, 'tecfile.bin: no file written')
  end subroutine binary_name

  subroutine binary_status
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A binary write that TecIO refuses is reported: a two-dimensional field of one block of 2 x 1 cells with one variable at the
  !< cells and two named scalars, ok_name and mass-flow, is written to a .szplt and a .plt file. The writer takes both names
  !< (aux_names_valid), but TecIO refuses mass-flow (TECAUXSTR142: a name begins with a letter or _ and goes on with letters,
  !< digits, _ and ., in the TecIO of this repository, lib/TecIO/teciosrc/TecioSZL.cpp and TecioPLT.cpp), so the writer must
  !< return a nonzero value. The same field with ok_name only is written with 0.
  !< Defect: the writer kept the status of each TecIO call in its result and returned the last one, that of TECEND142, which
  !< closes the file: a call that failed was forgotten, and the file, without the named scalar that TecIO refused, came back
  !< with the return value 0.
  !< Basis: Tecplot 360 EX 2023 R1 Data Format Guide, TECAUXSTR142 and TECDAT142 (p. 30), TECEND142 (p. 32), TECINI142 (p. 45),
  !< TECZNE142 (p. 60): "Return Value: 0 if successful, -1 if unsuccessful."
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)            :: w
  integer                     :: i, j, err, f
  character(len=*), parameter :: files(2) = ['binary_status.szplt', 'binary_status.plt  ']
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:1,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  w%tec%node = .false.
  w%tec%format = 'binary'
  do f = 1, 2
    err = tec_write_structured_multiblock(orion=w, varnames='a', filename=trim(files(f)), fldnames=['ok_name'], &
                                          fldvalues=[1.5_R8P])
    call check(err == 0, trim(files(f))//': the named scalar ok_name written with 0')
    err = tec_write_structured_multiblock(orion=w, varnames='a', filename=trim(files(f)), fldnames=['ok_name  ', 'mass-flow'], &
                                          fldvalues=[1.5_R8P, 2.5_R8P])
    write(*,'(A,I0)') trim(files(f))//' with mass-flow: return value ', err
    call check(err /= 0, trim(files(f))//': the named scalar mass-flow, which TecIO refuses, gives a nonzero return value')
  enddo
  end subroutine binary_status

  subroutine extension_names
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The format of a file is that of the extension of its name, the text after the last dot of its last component: a
  !< two-dimensional field of one block of 2 x 1 cells with one variable at the cells is written in the binary format as
  !< ext.dat.szplt and as ext.plt.szplt, and each file is read back, with 0, as the field written (a .szplt file); a binary
  !< write and a read of ext.szplt/field, a name without an extension (ext.szplt is a directory name, not made), give the
  !< return value 1, as a name without the extensions of the format.
  !< Defect: the reader and the binary writer took the format from any part of the name: ext.dat.szplt went to the ASCII
  !< reader, which refused it; the writer wrote a .plt file under the name ext.plt.szplt, which the .szplt reader could not
  !< read; ext.szplt/field went to the .szplt reader and writer, which got the error of a file that cannot be opened.
  !< Basis: the Fortran API guide, Reading Data: "ORION takes the format from the extension".
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)            :: w, r
  integer                     :: i, j, err, f
  logical                     :: same
  character(len=*), parameter :: files(2) = ['ext.dat.szplt', 'ext.plt.szplt']
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:1,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  w%tec%node = .false.
  w%tec%format = 'binary'
  do f = 1, 2
    err = tec_write_structured_multiblock(orion=w, varnames='a', filename=files(f))
    call check(err == 0, files(f)//': written without error')
    if (allocated(r%block)) deallocate(r%block)
    r%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=r, filename=files(f))
    write(*,'(A,I0)') files(f)//': read with the return value ', err
    call check(err == 0, files(f)//': read without error, a .szplt file')
    if (err /= 0) cycle
    same = size(r%block) == 1
    if (same) same = all(shape(r%block(1)%mesh) == shape(w%block(1)%mesh)) .and. &
                     all(shape(r%block(1)%vars) == shape(w%block(1)%vars))
    if (same) same = all(r%block(1)%mesh == w%block(1)%mesh) .and. all(r%block(1)%vars == w%block(1)%vars)
    call check(same, files(f)//': the field written')
  enddo
  err = tec_write_structured_multiblock(orion=w, varnames='a', filename='ext.szplt/field')
  write(*,'(A,I0)') 'ext.szplt/field written: return value ', err
  call check(err == 1, 'ext.szplt/field: a name without an extension refused by the binary writer with the return value 1')
  if (allocated(r%block)) deallocate(r%block)
  err = tec_read_structured_multiblock(orion=r, filename='ext.szplt/field')
  write(*,'(A,I0)') 'ext.szplt/field read: return value ', err
  call check(err == 1, 'ext.szplt/field: a name without an extension refused by the reader with the return value 1')
  end subroutine extension_names
#endif

  subroutine names_count
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The names of varnames are as many as the variables written: a two-dimensional field of one block of 2 x 1 cells with two
  !< variables at the cells, written with one name (a) or three (a b c), gives the return value 1 and no file, in an ASCII file
  !< and, with TecIO, in a .plt and a .szplt file; so does the field of two such blocks written to a .plt file with one name
  !< (a). With TecIO the names a,b, separated by a comma, give a .szplt file read back with the variables x, y, a and b.
  !< Defect: the writer did not count the names. TecIO takes the number of names, with the coordinates, as the number of values
  !< of each zone: with one name too few it was handed more values than that, which in a .plt file it wrote past the end of
  !< its buffers (the field of two blocks stopped the program on a corrupted heap) and in a .szplt file it refused; an ASCII
  !< file got a VARIABLES line of fewer names than its values. TecIO splits a list with a comma at its commas, so the list
  !< x y a,b was two names.
  !< Basis: TecIO of this repository, lib/TecIO/teciosrc/TecioPLT.cpp (TECINI142: the number of variables is the number of
  !< names, split at the line ends, or else the commas, or else the blanks of the list); Tecplot 360 EX 2023 R1 Data Format
  !< Guide, TECDAT142 (p. 30): TECDAT142 writes the values of the variables of the zone, in the order of the variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w
  integer                       :: i, j, err, f, n
  character(len=*), parameter   :: lists(2) = ['a    ', 'a b c']
  character(len=:), allocatable :: name
#ifdef TECIO
  type(orion_data)              :: r, w2
  character(len=*), parameter   :: exts(3) = ['.tec  ', '.plt  ', '.szplt']
#else
  character(len=*), parameter   :: exts(1) = ['.tec  ']
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:2,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  w%block(1)%vars(2,:,1,1) = [3._R8P, 4._R8P]
  w%tec%node = .false.
  do f = 1, size(exts)
    w%tec%format = merge('ascii ', 'binary', f == 1)
    do n = 1, 2
      name = 'names_'//trim(itoa(n))//trim(exts(f))
      call remove_file(name)
      err = tec_write_structured_multiblock(orion=w, varnames=trim(lists(n)), filename=name)
      write(*,'(A,I0)') name//' ('//trim(lists(n))//' for 2 variables): return value ', err
      call check(err == 1, name//': '//trim(lists(n))//' for 2 variables refused with the return value 1')
      call check(.not.exists_file(name), name//': no file written')
    enddo
  enddo
#ifdef TECIO
  allocate(w2%block(2))
  w2%block(1) = w%block(1)
  w2%block(2) = w%block(1)
  w2%block(2)%name = 'B2'
  w2%tec = w%tec
  w2%tec%format = 'binary'
  call remove_file('names_two_blocks.plt')
  err = tec_write_structured_multiblock(orion=w2, varnames='a', filename='names_two_blocks.plt')
  write(*,'(A,I0)') 'names_two_blocks.plt (a for 2 variables, two blocks): return value ', err
  call check(err == 1, 'names_two_blocks.plt: a for 2 variables of two blocks refused with the return value 1')
  call check(.not.exists_file('names_two_blocks.plt'), 'names_two_blocks.plt: no file written')
  w%tec%format = 'binary'
  err = tec_write_structured_multiblock(orion=w, varnames='a,b', filename='names_comma.szplt')
  call check(err == 0, 'names_comma.szplt: a,b for 2 variables written with 0')
  r%tec%format = 'binary'
  err = tec_read_structured_multiblock(orion=r, filename='names_comma.szplt')
  call check(err == 0, 'names_comma.szplt: read without error')
  if (err /= 0) return
  call check(size(r%varnames) == 4, 'names_comma.szplt: four variables')
  if (size(r%varnames) /= 4) return
  call check(r%varnames(1) == 'x' .and. r%varnames(2) == 'y' .and. r%varnames(3) == 'a' .and. r%varnames(4) == 'b', &
             'names_comma.szplt: the variables x, y, a and b')
#endif
  end subroutine names_count

  subroutine mesh_only_counts
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A block 1 without vars, a write of the mesh only, writes no variable: a two-dimensional field of one block of 2 x 1 cells
  !< without vars, written with Nvars = 1 or Nvars = -1, or with varnames of two names (a b), gives the return value 1 and no
  !< file, in an ASCII file and, with TecIO, in a .szplt file; written with Nvars = 0 it gives 0 and a file read back with the
  !< two coordinates and no variable.
  !< Defect: the writer did not count the names: a write of the mesh only with names wrote the mesh, with the return value 0,
  !< and left the names out. It took Nvars as it came: a write of the mesh only with Nvars = 1 or Nvars = -1 wrote the mesh,
  !< with the return value 0.
  !< Basis: docs/user-guide/fortran-api.md, Names given to the writer: Nvars is from 0 to size(vars,1), 0 when block 1 has no
  !< vars, and varnames holds as many names as the variables written, otherwise the return value 1 and no file.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w, r
  integer                       :: i, j, err, f, n
  integer, parameter            :: counts(2) = [1, -1]
  character(len=:), allocatable :: name
#ifdef TECIO
  character(len=*), parameter   :: exts(2) = ['.tec  ', '.szplt']
#else
  character(len=*), parameter   :: exts(1) = ['.tec  ']
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%tec%node = .false.
  do f = 1, size(exts)
    w%tec%format = merge('ascii ', 'binary', f == 1)
    do n = 1, 2
      name = 'mesh_only_nvars_'//trim(itoa(n))//trim(exts(f))
      call remove_file(name)
      err = tec_write_structured_multiblock(orion=w, filename=name, Nvars=counts(n))
      write(*,'(A,I0,A,I0)') name//' (no vars, Nvars = ', counts(n), '): return value ', err
      call check(err == 1, name//': Nvars = '//trim(itoa(counts(n)))//' without vars refused with the return value 1')
      call check(.not.exists_file(name), name//': no file written')
    enddo
    name = 'mesh_only_names'//trim(exts(f))
    call remove_file(name)
    err = tec_write_structured_multiblock(orion=w, varnames='a b', filename=name)
    write(*,'(A,I0)') name//' (no vars, varnames a b): return value ', err
    call check(err == 1 .and. .not.exists_file(name), name//': two names without vars refused with the return value 1, '// &
               'no file written')
    name = 'mesh_only_zero'//trim(exts(f))
    call remove_file(name)
    err = tec_write_structured_multiblock(orion=w, filename=name, Nvars=0)
    call check(err == 0, name//': Nvars = 0 without vars written with 0')
    r%tec%format = w%tec%format
    err = tec_read_structured_multiblock(orion=r, filename=name)
    call check(err == 0, name//': read back with 0')
    if (err == 0) call check(size(r%block(1)%mesh,1) == 2 .and. size(r%varnames) == 2, &
                             name//': two coordinates and no variable')
  enddo
  end subroutine mesh_only_counts

  subroutine nvars_range
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Nvars is a number of the variables that vars holds, from 0 to size(vars,1): a two-dimensional field of one block of 2 x 1
  !< cells with two variables at the cells, written with Nvars = 3 (varnames a b c) or Nvars = -1, gives the return value 1
  !< and no file, in an ASCII file and, with TecIO, in a .szplt file. So does a field of two such blocks whose second block
  !< holds one variable only (vars(1:1,...)), written without Nvars (varnames a b).
  !< Defect: the writer took Nvars as it came. Nvars = 3 read a third variable past the end of vars: the ASCII file got the
  !< values that followed the array in memory, with the return value 0, and a build with bounds checking or AddressSanitizer
  !< stopped there (index 3 of dimension 1 of vars above its upper bound 2; a heap-buffer-overflow read in tec_dat).
  !< Nvars = -1 wrote past the end of the locations of the variables of a .szplt file (index 2 of tecvarloc, of 1 element), and
  !< gave an ASCII file the location [3-1]=CELLCENTERED, with the return value 0. The writer checked the variables of block 1
  !< only: the second block, of one variable, was read past the end of its vars for the second variable, in the ASCII and in
  !< the .szplt file, with the return value 0.
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 9.5.3.1 (p. 128): a subscript within the bounds of its
  !< dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w, w2
  integer                       :: i, j, err, f, n
  integer, parameter            :: counts(2) = [3, -1]
  character(len=:), allocatable :: name
#ifdef TECIO
  character(len=*), parameter   :: exts(2) = ['.tec  ', '.szplt']
#else
  character(len=*), parameter   :: exts(1) = ['.tec  ']
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
  allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:2,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    w%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  w%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  w%block(1)%vars(2,:,1,1) = [3._R8P, 4._R8P]
  w%tec%node = .false.
  do f = 1, size(exts)
    w%tec%format = merge('ascii ', 'binary', f == 1)
    do n = 1, 2
      name = 'nvars_'//trim(itoa(n))//trim(exts(f))
      call remove_file(name)
      if (counts(n) == 3) then
        err = tec_write_structured_multiblock(orion=w, varnames='a b c', filename=name, Nvars=counts(n))
      else
        err = tec_write_structured_multiblock(orion=w, filename=name, Nvars=counts(n))
      endif
      write(*,'(A,I0,A,I0)') name//' (Nvars = ', counts(n), ' for 2 variables): return value ', err
      call check(err == 1, name//': Nvars = '//trim(itoa(counts(n)))//' for 2 variables refused with the return value 1')
      call check(.not.exists_file(name), name//': no file written')
    enddo
  enddo
  ! Two blocks, the second with one variable only
  allocate(w2%block(2))
  w2%block(1) = w%block(1)
  w2%block(2) = w%block(1)
  w2%block(2)%name = 'B2'
  deallocate(w2%block(2)%vars)
  allocate(w2%block(2)%vars(1:1,1:2,1:1,1:1))
  w2%block(2)%vars(1,:,1,1) = [5._R8P, 6._R8P]
  w2%tec = w%tec
  do f = 1, size(exts)
    w2%tec%format = merge('ascii ', 'binary', f == 1)
    name = 'nvars_short_block'//trim(exts(f))
    call remove_file(name)
    err = tec_write_structured_multiblock(orion=w2, varnames='a b', filename=name)
    write(*,'(A,I0)') name//' (block 2 with 1 of the 2 variables): return value ', err
    call check(err == 1 .and. .not.exists_file(name), name//': a block with fewer variables than block 1 refused with the '// &
               'return value 1, no file written')
  enddo
  end subroutine nvars_range

  function itoa(n) result(s)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The digits of n.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: n
  character(len=16)   :: s
  !---------------------------------------------------------------------------------------------------------------------------------

  write(s,'(I0)') n
  end function itoa

  subroutine remove_file(name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove the file name if it is there, so that a check that no file is written measures the write that follows.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name
  integer                      :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  if (.not.exists_file(name)) return
  open(newunit=u, file=name, status='old')
  close(u, status='delete')
  end subroutine remove_file

  logical function exists_file(name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Is the file name there?
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file=name, exist=exists_file)
  end function exists_file

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
end program tecplot_write_multiblock