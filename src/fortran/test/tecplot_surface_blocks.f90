  program tecplot_surface_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !> tec_write_structured_multiblock writes surface blocks, blocks with one plane of nodes in one direction (Ni, Nj or Nk = 0) such
  !> as the faces of a volume block, and lines of nodes, as ordered zones with one node plane there. Their variables at the cells
  !> hold one layer of cells in that direction, max(Ni,1)*max(Nj,1)*max(Nk,1) values, and their variables at the nodes one value
  !> per node, as an ordered zone stores them (Tecplot 360 Data Format Guide, section 2-1). The program writes three fields: a
  !> three-dimensional one of a surface block per direction (i: 0 x 4 x 3 cells, j: 5 x 0 x 2, k: 3 x 4 x 0), a line of nodes
  !> (4 x 0 x 0) and a volume block (2 x 3 x 2) with two variables at the cells; the same field with two variables at the nodes
  !> (orion%tec%node); a two-dimensional one (two coordinates) of a block of 4 x 3 cells and two lines of nodes (5 x 0 and 0 x 3)
  !> with two variables at the cells. Each field goes to a .szplt, a .plt and an ASCII file. The program reads each .szplt file
  !> back with the TecIO reader: number of zones and variables, I, J and K of each zone, every coordinate and, for each variable,
  !> its location, its number of values and the values, bit for bit (every value is an integer, a half or a quarter); the values
  !> at the cells of a zone with JMax = 1 and KMax > 1 (the face j) are read one layer of cells in K at a time, as the TecIO
  !> reader gives them right only so. The TecIO reader reads .szplt files only: of the .plt files the program checks that they
  !> are written without error. Of the ASCII files
  !> it reads the I, J and K of each zone header and the values that follow it, the same as in the .szplt file. Built only with
  !> TecIO. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_c_binding
  use IR_Precision
  use Lib_ORION_data
  use Lib_Tecplot, only: tec_write_structured_multiblock
  implicit none
  include "tecio.f90"
  integer(I4P), parameter :: dims3(3,5) = reshape([0,4,3, 5,0,2, 3,4,0, 4,0,0, 2,3,2], [3,5])
  integer(I4P), parameter :: dims2(3,3) = reshape([4,3,0, 5,0,0, 0,3,0], [3,3])
  integer :: checks = 0, failures = 0

  call write_and_check(3, dims3, .false., 'tecsurface')
  call write_and_check(3, dims3, .true., 'tecsurface_nodes')
  call write_and_check(2, dims2, .false., 'tecsurface_2d')

  write(*,'(A,I0,A,I0,A)') 'tecplot_surface_blocks: ', checks, ' checks, ', failures, ' failed'
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
  endprogram tecplot_surface_blocks
