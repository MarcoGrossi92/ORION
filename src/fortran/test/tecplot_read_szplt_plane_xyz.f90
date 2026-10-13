!< Tecplot binary (.szplt) zones with one node plane (K = 1).
!< A slice of a 3-D field that carries x, y and z is read with three coordinates and every
!< solution variable keeps its own band, cell-centred or nodal, also next to a volume zone;
!< the header-only pass gives the same variable count. A pure 2-D file (x, y), a 3-D file,
!< and K = 1 files whose third variable is a cell-centred z or has another name read as before.
!< The files are written with the ORION writer or with the TecIO calls it uses (TecIO build).
!< The values of every shape of ordered zone, read row by row where TecIO needs it (layers).
!< The number of coordinates of the file for every zone (zone_shapes).
!< One location for the solution variables (variable_location).
!< Files of lines named x and y, read with two coordinates (lines_xy).
!< Zones whose solution variables are at different locations, each read with its own (zone_locations).
!< A passive variable does not decide the location of a zone; a zone of passive variables takes that of the file
!< (passive_variables).
!< Files with fewer variables than the coordinates of their zones, refused (too_few_variables).
!< Stops with a non-zero code when a check fails.
program tecplot_read_szplt_plane_xyz
  use Lib_Tecplot
  use Lib_ORION_data
  use, intrinsic :: iso_c_binding
  implicit none
  include "tecio.f90"
  integer :: nfail

  nfail = 0
  ! the files that read as before come first, so that a failure on a slice cannot hide them
  call pure_2d()
  call volume_3d()
  call z_cellcentered()
  call third_named_w()
  call slice_nodal()
  call slice_dims_only()
  call slice_and_volume()
  call slice_cellcentered()
  call layers()
  call zone_shapes()
  call variable_location()
  call lines_xy()
  call zone_locations()
  call passive_variables()
  call too_few_variables()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_szplt_plane_xyz: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_szplt_plane_xyz: all checks passed'

contains

  subroutine check(ok, what)
    logical,          intent(in) :: ok
    character(len=*), intent(in) :: what
    if (ok) then
      write(*,'(A)') '  ok    '//what
    else
      write(*,'(A)') '  FAIL  '//what
      nfail = nfail + 1
    endif
  end subroutine check

  ! A .szplt file written with the TecIO calls of the ORION writer. Zone z has dims(1:3,z)
  ! nodes; variable v of zone z has location loc(v,z) (1 nodal, 0 cell-centred) and its
  ! nval(v,z) values in vals(1:nval(v,z),v,z).
  subroutine write_szplt(fname, names, dims, loc, nval, vals)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: dims(:,:), loc(:,:), nval(:,:)
    real(8),          intent(in) :: vals(:,:,:)
    integer, external :: tecini142, teczne142, tecdatd142, tecend142
    integer, allocatable :: nul(:), vloc(:)
    real(8), allocatable :: buf(:)
    integer :: err, v, z, nv
    character(len=1), parameter :: z0 = achar(0)
    nv = size(loc,1)
    allocate(nul(nv), vloc(nv))
    nul = 0
    err = tecini142('test'//z0, names//z0, fname//z0, '.'//z0, 1, 0, 0, 1)
    do z = 1, size(dims,2)
      vloc = loc(:,z)
      err = teczne142('zone'//z0, 0, dims(1,z), dims(2,z), dims(3,z), 0, 0, 0, 0.0d0, 0, 0, 1, &
                      0, 0, 0, 0, 0, nul, vloc, nul, 0)
      do v = 1, nv
        buf = vals(1:nval(v,z),v,z)
        err = tecdatd142(nval(v,z), buf)
      enddo
    enddo
    err = tecend142()
  end subroutine write_szplt

  ! x y + u v, cell-centred, written by the ORION writer (I = 3, J = 2).
  subroutine pure_2d()
    type(orion_data) :: w, d
    integer :: i, j, err
    allocate(w%block(1))
    w%block(1)%name = 'plane'
    w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
    allocate(w%block(1)%mesh(1:2,0:2,0:1,0:0), w%block(1)%vars(1:2,1:2,1:1,1:1))
    do j = 0, 1; do i = 0, 2
      w%block(1)%mesh(1,i,j,0) = real(i,8); w%block(1)%mesh(2,i,j,0) = real(j,8)
    enddo; enddo
    w%block(1)%vars(1,:,1,1) = [11.0d0, 12.0d0]
    w%block(1)%vars(2,:,1,1) = [21.0d0, 22.0d0]
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='u v', filename='szplt_pure2d.szplt')
    write(*,'(A)') 'pure 2-D: x y nodal, u v cell-centred (ORION writer)'
    call check(err == 0, 'written')
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_pure2d.szplt')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%varnames) == 4, 'four names, coordinates first')
    call check(size(d%block(1)%mesh,1) == 2, 'two coordinates')
    call check(d%block(1)%Nk == 0, 'Nk = 0')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(all(d%block(1)%vars(1,1:2,1,1) == [11.0d0, 12.0d0]), 'u in band 1')
    call check(all(d%block(1)%vars(2,1:2,1,1) == [21.0d0, 22.0d0]), 'v in band 2')
  end subroutine pure_2d

  ! x y z + u v, cell-centred, one cell (I = J = K = 2), written by the ORION writer.
  subroutine volume_3d()
    type(orion_data) :: w, d
    integer :: i, j, k, err
    allocate(w%block(1))
    w%block(1)%name = 'volume'
    w%block(1)%Ni = 1; w%block(1)%Nj = 1; w%block(1)%Nk = 1
    allocate(w%block(1)%mesh(1:3,0:1,0:1,0:1), w%block(1)%vars(1:2,1:1,1:1,1:1))
    do k = 0, 1; do j = 0, 1; do i = 0, 1
      w%block(1)%mesh(:,i,j,k) = [real(i,8), real(j,8), 3.0d0 + real(k,8)]
    enddo; enddo; enddo
    w%block(1)%vars(1,1,1,1) = 7.0d0
    w%block(1)%vars(2,1,1,1) = 8.0d0
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='u v', filename='szplt_volume.szplt')
    write(*,'(A)') 'volume: x y z nodal, u v cell-centred, K = 2 (ORION writer)'
    call check(err == 0, 'written')
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_volume.szplt')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 3, 'three coordinates')
    call check(d%block(1)%Nk == 1, 'Nk = 1')
    if (size(d%block(1)%mesh,1) /= 3) return
    call check(d%block(1)%mesh(3,1,1,1) == 4.0d0, 'z of the last node')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(d%block(1)%vars(1,1,1,1) == 7.0d0 .and. d%block(1)%vars(2,1,1,1) == 8.0d0, 'u and v in bands 1 and 2')
  end subroutine volume_3d

  ! K = 1, x y nodal, then z u v cell-centred: z is a solution variable, as before.
  subroutine z_cellcentered()
    type(orion_data) :: d
    integer :: dims(3,1), loc(5,1), nval(5,1), err
    real(8) :: vals(6,5,1)
    dims(:,1) = [3, 2, 1]
    loc(:,1) = [1, 1, 0, 0, 0]
    nval(:,1) = [6, 6, 2, 2, 2]
    vals = 0.0d0
    vals(1:6,1,1) = [0.0d0, 1.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0]
    vals(1:6,2,1) = [0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0]
    vals(1:2,3,1) = [5.0d0, 6.0d0]
    vals(1:2,4,1) = [11.0d0, 12.0d0]
    vals(1:2,5,1) = [21.0d0, 22.0d0]
    call write_szplt('szplt_zcell.szplt', 'x y z u v', dims, loc, nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_zcell.szplt')
    write(*,'(A)') 'K = 1 with a cell-centred z: x y nodal, z u v cell-centred'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 2, 'two coordinates')
    call check(size(d%block(1)%vars,1) == 3, 'three solution variables, z first')
    if (size(d%block(1)%vars,1) /= 3) return
    call check(all(d%block(1)%vars(1,1:2,1,1) == [5.0d0, 6.0d0]), 'z in band 1')
    call check(all(d%block(1)%vars(2,1:2,1,1) == [11.0d0, 12.0d0]), 'u in band 2')
    call check(all(d%block(1)%vars(3,1:2,1,1) == [21.0d0, 22.0d0]), 'v in band 3')
  end subroutine z_cellcentered

  ! K = 1, every variable nodal, third variable named w: a 2-D file, as before.
  subroutine third_named_w()
    type(orion_data) :: d
    integer :: dims(3,1), loc(4,1), nval(4,1), err, n
    real(8) :: vals(6,4,1)
    dims(:,1) = [3, 2, 1]
    loc = 1
    nval = 6
    vals(1:6,1,1) = [0.0d0, 1.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0]
    vals(1:6,2,1) = [0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0]
    vals(1:6,3,1) = [(100.0d0 + n, n = 1, 6)]
    vals(1:6,4,1) = [(200.0d0 + n, n = 1, 6)]
    call write_szplt('szplt_xyw.szplt', 'x y w u', dims, loc, nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_xyw.szplt')
    write(*,'(A)') 'K = 1 with a third variable named w, all nodal'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 2, 'two coordinates')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables, w first')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(d%block(1)%vars(1,0,0,0) == 101.0d0 .and. d%block(1)%vars(1,2,1,0) == 106.0d0, 'w in band 1')
    call check(d%block(1)%vars(2,0,0,0) == 201.0d0 .and. d%block(1)%vars(2,2,1,0) == 206.0d0, 'u in band 2')
  end subroutine third_named_w

  ! A slice with every variable nodal and the names in upper case: X Y Z u v (I = 3, J = 2, K = 1).
  subroutine slice_nodal()
    type(orion_data) :: d
    integer :: dims(3,1), loc(5,1), nval(5,1), err, n
    real(8) :: vals(6,5,1)
    dims(:,1) = [3, 2, 1]
    loc = 1
    nval = 6
    vals(1:6,1,1) = [0.0d0, 1.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0]
    vals(1:6,2,1) = [0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0]
    vals(1:6,3,1) = [5.0d0, 5.25d0, 5.5d0, 5.0d0, 5.25d0, 5.5d0]
    vals(1:6,4,1) = [(100.0d0 + n, n = 1, 6)]
    vals(1:6,5,1) = [(200.0d0 + n, n = 1, 6)]
    call write_szplt('szplt_slice_nodal.szplt', 'X Y Z u v', dims, loc, nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_slice_nodal.szplt')
    write(*,'(A)') 'slice: X Y Z u v, all nodal'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%varnames) == 5, 'five names, coordinates first')
    call check(size(d%block(1)%mesh,1) == 3, 'three coordinates')
    if (size(d%block(1)%mesh,1) /= 3) return
    call check(d%block(1)%Ni == 2 .and. d%block(1)%Nj == 1 .and. d%block(1)%Nk == 0, 'Ni = 2, Nj = 1, Nk = 0')
    call check(d%block(1)%mesh(3,0,0,0) == 5.0d0 .and. d%block(1)%mesh(3,2,1,0) == 5.5d0, 'z as the third coordinate')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(d%block(1)%vars(1,0,0,0) == 101.0d0 .and. d%block(1)%vars(1,2,1,0) == 106.0d0, 'u in band 1')
    call check(d%block(1)%vars(2,0,0,0) == 201.0d0 .and. d%block(1)%vars(2,2,1,0) == 206.0d0, 'v in band 2')
  end subroutine slice_nodal

  ! The header-only pass on the nodal slice gives the same counts as the full read.
  subroutine slice_dims_only()
    type(orion_data) :: d
    integer :: err
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_slice_nodal.szplt', dims_only=.true.)
    write(*,'(A)') 'slice, header-only pass'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(d%block(1)%Nk == 0, 'Nk = 0')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
  end subroutine slice_dims_only

  ! A slice zone (K = 1) and a volume zone (K = 2) in one file, x y z u all nodal:
  ! both zones have three coordinates and one band.
  subroutine slice_and_volume()
    type(orion_data) :: d
    integer :: dims(3,2), loc(4,2), nval(4,2), err, n
    real(8) :: vals(8,4,2)
    dims(:,1) = [3, 2, 1]
    dims(:,2) = [2, 2, 2]
    loc = 1
    nval(:,1) = 6
    nval(:,2) = 8
    vals = 0.0d0
    vals(1:6,1,1) = [0.0d0, 1.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0]
    vals(1:6,2,1) = [0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0]
    vals(1:6,3,1) = 5.0d0
    vals(1:6,4,1) = [(100.0d0 + n, n = 1, 6)]
    vals(1:8,1,2) = [0.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0]
    vals(1:8,2,2) = [0.0d0, 0.0d0, 1.0d0, 1.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0]
    vals(1:8,3,2) = [3.0d0, 3.0d0, 3.0d0, 3.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0]
    vals(1:8,4,2) = [(300.0d0 + n, n = 1, 8)]
    call write_szplt('szplt_slice_volume.szplt', 'x y z u', dims, loc, nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_slice_volume.szplt')
    write(*,'(A)') 'slice zone and volume zone, x y z u all nodal'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block) == 2, 'two blocks')
    call check(size(d%block(1)%mesh,1) == 3 .and. size(d%block(2)%mesh,1) == 3, 'three coordinates in both zones')
    call check(size(d%block(1)%vars,1) == 1 .and. size(d%block(2)%vars,1) == 1, 'one solution variable in both zones')
    if (size(d%block(1)%vars,1) /= 1 .or. size(d%block(1)%mesh,1) /= 3) return
    call check(d%block(1)%vars(1,0,0,0) == 101.0d0 .and. d%block(1)%vars(1,2,1,0) == 106.0d0, 'u of the slice in band 1')
    call check(d%block(2)%vars(1,1,1,1) == 308.0d0, 'u of the volume in band 1')
  end subroutine slice_and_volume

  ! A slice written by the ORION writer: x y z nodal, u v w p cell-centred (I = 3, J = 2, K = 1),
  ! z varying along x. Last, since before the fix the reader overran the cell values here.
  subroutine slice_cellcentered()
    type(orion_data) :: w, d
    integer :: i, j, s, err
    allocate(w%block(1))
    w%block(1)%name = 'slice'
    w%block(1)%Ni = 2; w%block(1)%Nj = 1; w%block(1)%Nk = 0
    allocate(w%block(1)%mesh(1:3,0:2,0:1,0:0), w%block(1)%vars(1:4,1:2,1:1,1:1))
    do j = 0, 1; do i = 0, 2
      w%block(1)%mesh(:,i,j,0) = [real(i,8), real(j,8), 5.0d0 + 0.25d0*i]
    enddo; enddo
    do s = 1, 4
      w%block(1)%vars(s,:,1,1) = [10.0d0*s + 1.0d0, 10.0d0*s + 2.0d0]
    enddo
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='u v w p', filename='szplt_slice_cc.szplt')
    write(*,'(A)') 'slice: x y z nodal, u v w p cell-centred (ORION writer)'
    call check(err == 0, 'written')
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='szplt_slice_cc.szplt')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%varnames) == 7, 'seven names, coordinates first')
    call check(size(d%block(1)%mesh,1) == 3, 'three coordinates')
    if (size(d%block(1)%mesh,1) /= 3) return
    call check(d%block(1)%Ni == 2 .and. d%block(1)%Nj == 1 .and. d%block(1)%Nk == 0, 'Ni = 2, Nj = 1, Nk = 0')
    call check(d%block(1)%mesh(3,0,1,0) == 5.0d0 .and. d%block(1)%mesh(3,2,1,0) == 5.5d0, 'z as the third coordinate')
    call check(size(d%block(1)%vars,1) == 4, 'four solution variables')
    if (size(d%block(1)%vars,1) /= 4) return
    call check(all(d%block(1)%vars(1,1:2,1,1) == [11.0d0, 12.0d0]), 'u in band 1')
    call check(all(d%block(1)%vars(2,1:2,1,1) == [21.0d0, 22.0d0]), 'v in band 2')
    call check(all(d%block(1)%vars(3,1:2,1,1) == [31.0d0, 32.0d0]), 'w in band 3')
    call check(all(d%block(1)%vars(4,1:2,1,1) == [41.0d0, 42.0d0]), 'p in band 4')
  end subroutine slice_cellcentered


  ! The values of every shape of ordered zone. Read in one call, the TecIO reader gives wrong the
  ! values at the cells of some zones with one node plane or line in a direction (JMax = 1 with IMax
  ! >= 2 and KMax >= 3; IMax = 2 with JMax >= 3 and KMax = 1; IMax = 1 with JMax = 2 and KMax >= 3);
  ! one row of cells in I per call it gives every value right. So tec_read_szplt reads a variable at
  ! the cells of a zone with IMax, JMax or KMax = 1 one row of cells in I per call, and any other
  ! variable in one call. The checks cover the 216 shapes with IMax, JMax and KMax in {1, 2, 3, 4,
  ! 6, 9}: files are written, with the TecIO calls that the ORION writer uses, of three coordinates
  ! (x y z a b: a volume first, then every shape with KMax > 1 and every shape with KMax = 1 and
  ! JMax > 1), of two coordinates (x y a b: every shape with KMax = 1 and JMax > 1) and of one
  ! coordinate (x a b: every shape with JMax = KMax = 1), with a and b at the cells and at the
  ! nodes, in 64-bit and 32-bit values (12 files), read with tec_read_structured_multiblock and
  ! every zone is checked against the field written: the number of coordinates, one value of a and b
  ! per node or per cell (max(N-1,1) cells in each direction) and every value. Every value is an
  ! integer or a quarter, exact in 32 bits.
  ! Defect: the reader read each variable in one call and returned those wrong values without an
  ! error.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 5-5 (p. 170): the values of a variable
  ! may be read in chunks; section 2-1 (p. 15): the number of cell values of an ordered zone.
  subroutine layers()
    integer, parameter :: sizes(6) = [1, 2, 3, 4, 6, 9]
    integer, allocatable :: d3(:,:), d2(:,:), d1(:,:)
    integer :: prec, loc, a, b, c, n3, n2, n1
    ! the zones of each file: three coordinates (a volume first), two, one
    allocate(d3(3,217), d2(3,36), d1(3,6))
    n3 = 1; d3(:,1) = [4, 3, 5]; n2 = 0; n1 = 0
    do c = 1, 6
      do b = 1, 6
        do a = 1, 6
          if (sizes(c) > 1 .or. sizes(b) > 1) then
            n3 = n3 + 1; d3(:,n3) = [sizes(a), sizes(b), sizes(c)]
          endif
          if (sizes(c) == 1 .and. sizes(b) > 1) then
            n2 = n2 + 1; d2(:,n2) = [sizes(a), sizes(b), sizes(c)]
          endif
          if (sizes(c) == 1 .and. sizes(b) == 1) then
            n1 = n1 + 1; d1(:,n1) = [sizes(a), sizes(b), sizes(c)]
          endif
        enddo
      enddo
    enddo
    do prec = 1, 0, -1
      do loc = 0, 1
        call case_layers('layers3', 'x y z a b', 3, d3(:,1:n3), loc, prec)
        call case_layers('layers2', 'x y a b', 2, d2(:,1:n2), loc, prec)
        call case_layers('layers1', 'x a b', 1, d1(:,1:n1), loc, prec)
      enddo
    enddo
  end subroutine layers

  ! Value of variable v at node or cell (i, j, k) of zone z: coordinate v (v <= nd) or a, b (v = nd+1, nd+2).
  pure real(8) function value(v, nd, z, i, j, k)
    integer, intent(in) :: v, nd, z, i, j, k
    if (v <= nd) then
      value = merge(i, merge(j, k, v == 2), v == 1)
    elseif (v == nd + 1) then
      value = 1000*z + i + 20*j + 400*k + 0.25d0
    else
      value = -(1000*z + i + 20*j + 400*k) - 0.5d0
    endif
  end function value

  ! Write the file of the zones dims (nd coordinates, a and b at the nodes when loc is 1, at the cells otherwise, 64-bit values
  ! when prec is 1), read it with the ORION reader and check every zone against the field written.
  subroutine case_layers(base, names, nd, dims, loc, prec)
    character(len=*), intent(in) :: base, names
    integer,          intent(in) :: nd, dims(:,:), loc, prec
    integer, external :: tecini142, teczne142, tecdatd142, tecend142
    character(len=1), parameter :: z0 = achar(0)
    character(len=64) :: fname, what
    type(orion_data) :: d
    real(8), allocatable :: buf(:)
    integer, allocatable :: nul(:), vloc(:)
    integer :: e, z, v, i, j, k, m, err, n(3), lo(3), nbad
    logical :: same
    fname = base//merge('_nodes', '_cells', loc == 1)//trim(merge('_double', '_float ', prec == 1))//'.szplt'
    allocate(nul(nd+2), vloc(nd+2))
    nul = 0; vloc = 1; vloc(nd+1:nd+2) = loc
    e = tecini142('layers'//z0, names//z0, trim(fname)//z0, '.'//z0, 1, 0, 0, prec)
    do z = 1, size(dims,2)
      e = teczne142('zone'//z0, 0, dims(1,z), dims(2,z), dims(3,z), 0, 0, 0, 0.0d0, 0, 0, 1, 0, 0, 0, 0, 0, nul, vloc, nul, 0)
      do v = 1, nd + 2
        if (allocated(buf)) deallocate(buf)
        if (v <= nd .or. loc == 1) then
          allocate(buf(product(dims(:,z)))); m = 0
          do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
            m = m + 1; buf(m) = value(v, nd, z, i, j, k)
          enddo; enddo; enddo
        else
          allocate(buf(product(max(dims(:,z)-1,1)))); m = 0
          do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
            m = m + 1; buf(m) = value(v, nd, z, i, j, k)
          enddo; enddo; enddo
        endif
        e = tecdatd142(size(buf), buf)
      enddo
    enddo
    e = tecend142()
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=trim(fname))
    call check(err == 0, trim(fname)//': read without error')
    if (err /= 0) return
    call check(size(d%block) == size(dims,2), trim(fname)//': every zone read')
    if (size(d%block) /= size(dims,2)) return
    nbad = 0
    do z = 1, size(dims,2)
      write(what,'(A,A,I0,A,3(1X,I0))') trim(fname), ' zone ', z, ', IJK', dims(:,z)
      call check(size(d%block(z)%mesh,1) == nd, trim(what)//': number of coordinates')
      if (size(d%block(z)%mesh,1) /= nd) cycle
      ! nodes, or cells: max(N-1,1) in each direction; the first index from the array
      if (loc == 1) then
        n = dims(:,z)
      else
        n = max(dims(:,z)-1,1)
      endif
      same = size(d%block(z)%vars,1) == 2 .and. all(shape(d%block(z)%vars) == [2, n])
      call check(same, trim(what)//': one value of a and b per '//merge('node', 'cell', loc == 1))
      if (.not.same) cycle
      lo = [lbound(d%block(z)%vars,2), lbound(d%block(z)%vars,3), lbound(d%block(z)%vars,4)]
      do v = nd + 1, nd + 2
        same = .true.
        do k = 1, n(3); do j = 1, n(2); do i = 1, n(1)
          if (d%block(z)%vars(v-nd, lo(1)+i-1, lo(2)+j-1, lo(3)+k-1) /= &
              value(v, nd, z, i-loc, j-loc, k-loc)) same = .false.
        enddo; enddo; enddo
        call check(same, trim(what)//': values of '//merge('a', 'b', v == nd+1))
        if (.not.same) nbad = nbad + 1
      enddo
    enddo
    write(*,'(A,I0,A,I0,A)') trim(fname)//': ', size(dims,2), ' zones, ', nbad, ' variables with wrong values'
  end subroutine case_layers

  ! The number of coordinates of every zone, for every shape of zone. The number of coordinates is
  ! that of the file, the same for every zone (the variables are those of the file): three when a
  ! zone has more than one node plane in K, or when the first three variables are named x, y and z
  ! and z is nodal in every zone; otherwise two when a zone has more than one node line in J;
  ! otherwise one (a file of lines). No zone takes it from the zone before it. A zone with one node
  ! plane or line in a direction has one layer of cells there (Tecplot 360 Data Format Guide,
  ! section 2-1). Files, written with the TecIO calls that the ORION writer uses: x y z a with zones
  ! of every shape (IMax, JMax, KMax equal to 1 or 3), a at the nodes and a at the cells, and again
  ! with a face j as the first zone (three coordinates); x y a with zones of one node plane or line
  ! (two); x y z a with zones of one node plane or line, z at the nodes (three, by the names), and
  ! with lines only (three, by the names); x y z with planes, z at the cells (two: a cell-centred z
  ! is no coordinate); p q r a with planes and lines (two) and with a volume (three); x a with lines
  ! (one). For each zone: the number of coordinates, every coordinate and every value of the last
  ! variable.
  ! Defect: the reader gave each zone the number of coordinates of its own shape, and the face j of
  ! a volume block that of the zone before it, or an undefined one.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 1-3, item 7 (p. 9): the same variables
  ! in every zone; section 4-3.1 (pp. 120-121): one list of variables for the file.
  subroutine zone_shapes()
    integer :: shapes(3,8), z
    ! the eight shapes: IMax, JMax and KMax equal to 1 or 3
    do z = 1, 8
      shapes(:,z) = [merge(3, 1, btest(z-1,0)), merge(3, 1, btest(z-1,1)), merge(3, 1, btest(z-1,2))]
    enddo
    call case_shapes('szplt_shapes_nodal.szplt',   'x y z a', shapes,                   1, 3)
    call case_shapes('szplt_shapes_cells.szplt',   'x y z a', shapes,                   0, 3)
    call case_shapes('szplt_shapes_jfirst.szplt',  'x y z a', shapes(:,[6,5,8,2]),      0, 3)
    call case_shapes('szplt_shapes_2d.szplt',      'x y a',   shapes(:,[4,2,3,1]),      0, 2)
    call case_shapes('szplt_shapes_planes.szplt',  'x y z a', shapes(:,[4,2,3,1]),      1, 3)
    call case_shapes('szplt_shapes_xyzlines.szplt','x y z a', shapes(:,[2,1]),          0, 3)
    call case_shapes('szplt_shapes_zcell.szplt',   'x y z',   shapes(:,[4,3]),          0, 2)
    call case_shapes('szplt_shapes_pqr2.szplt',    'p q r a', shapes(:,[4,2]),          1, 2)
    call case_shapes('szplt_shapes_pqr3.szplt',    'p q r a', shapes(:,[8,4,6]),        1, 3)
    call case_shapes('szplt_shapes_lines.szplt',   'x a',     shapes(:,[2,1]),          0, 1)
  end subroutine zone_shapes

  ! Write a file with the zones of the shapes given (the first ncoord variables are the coordinates by the rule, the
  ! last one, a, is at the nodes when anodal is 1 and at the cells otherwise), read it with the ORION reader and check
  ! each zone: ncoord coordinates, the coordinates and the values of a.
  subroutine case_shapes(fname, names, dims, anodal, ncoord)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: dims(:,:), anodal, ncoord
    type(orion_data) :: d
    integer, allocatable :: loc(:), nval(:,:)
    real(8), allocatable :: vals(:,:,:), expected(:)
    integer :: nv, nz, z, v, i, j, k, m, err, lo
    character(len=96) :: what
    nv = count_names(names)
    nz = size(dims,2)
    allocate(loc(nv), nval(nv,nz), vals(27,nv,nz))
    loc = 1; loc(nv) = anodal
    vals = 0.0d0
    do z = 1, nz
      do v = 1, nv
        m = 0
        if (loc(v) == 1) then
          do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
            m = m + 1; vals(m,v,z) = value_shapes(v, nv, i, j, k)
          enddo; enddo; enddo
        else
          do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
            m = m + 1; vals(m,v,z) = value_shapes(v, nv, i, j, k)
          enddo; enddo; enddo
        endif
        nval(v,z) = m
      enddo
    enddo
    call write_szplt(fname, names, dims, spread(loc, 2, size(dims,2)), nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A)') fname//': '//names//', a '//merge('at the nodes', 'at the cells', anodal == 1)
    call check(err == 0, fname//': read without error')
    if (err /= 0) return
    call check(size(d%block) == nz, fname//': every zone read')
    if (size(d%block) /= nz) return
    do z = 1, nz
      write(what,'(A,I0,A,3(I0,1X))') fname//' zone ', z, ', IJK ', dims(:,z)
      call check(size(d%block(z)%mesh,1) == ncoord, trim(what)//': number of coordinates')
      if (size(d%block(z)%mesh,1) /= ncoord) cycle
      call check(size(d%block(z)%vars,1) == nv - ncoord, trim(what)//': number of solution variables')
      if (size(d%block(z)%vars,1) /= nv - ncoord) cycle
      do v = 1, ncoord
        call check(all(reshape(d%block(z)%mesh(v,:,:,:), [nval(v,z)]) == vals(1:nval(v,z),v,z)), &
                   trim(what)//': coordinate '//names(2*v-1:2*v-1))
      enddo
      lo = nv - ncoord
      expected = vals(1:nval(nv,z),nv,z)
      call check(size(d%block(z)%vars(lo,:,:,:)) == size(expected), trim(what)//': number of values of a')
      if (size(d%block(z)%vars(lo,:,:,:)) == size(expected)) &
        call check(all(reshape(d%block(z)%vars(lo,:,:,:), [size(expected)]) == expected), trim(what)//': values of a')
    enddo
  end subroutine case_shapes

  ! Value of variable v of nv at node or cell (i, j, k): coordinates for the first variables, then the variable a.
  pure real(8) function value_shapes(v, nv, i, j, k)
    integer, intent(in) :: v, nv, i, j, k
    select case(v)
    case(1)
      value_shapes = i + 0.5d0
    case(2)
      value_shapes = j + 10.0d0
    case(3)
      value_shapes = k + 100.0d0
    case default
      value_shapes = 1000.0d0*v + i + 10*j + 100*k + 0.25d0
    end select
    if (v == nv) value_shapes = 5000.0d0 + i + 10*j + 100*k + 0.25d0
  end function value_shapes

  ! The location of the variables. ORION holds the coordinates at the nodes and one location for all
  ! solution variables of a block, orion%tec%node: the reader sets it when every solution variable
  ! of every zone is at the nodes and clears it when every one is at the cells, leaves it as it is
  ! for a file without solution variables, and refuses a zone with a coordinate at the cells or with
  ! solution variables at the nodes and at the cells (return value 1); zones whose locations differ
  ! are read by zone_locations. Files written with the TecIO calls that the ORION writer uses: a
  ! two-dimensional field (x y a b) with a and b at the nodes, read with orion%tec%node set to false
  ! first; three-dimensional fields (x y z a) of a volume and a face j with a at the nodes, read
  ! with it false first, and with a at the cells, read with it true first; a mesh (x y z), read with
  ! orion%tec%node false and then true; and, refused, a two-dimensional field with a at the nodes and
  ! b at the cells, and a three-dimensional field with z and a at the cells. The fields with the
  ! variables at the nodes are also written again with tec_write_structured_multiblock, as read, and
  ! their .szplt files read with the TecIO reader: the variables at the nodes, with the values of the
  ! file read. Every value is an integer, a half or a quarter.
  ! Defect: the reader never set orion%tec%node, read a zone with variables at the nodes and at the
  ! cells past the end of the buffer of the values at the cells, and put a coordinate at the cells
  ! at index 0 of the variables.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2, VARLOCATION (p. 124): each
  ! variable may be at the nodes or at the cells; ORION holds one location for all the solution
  ! variables of a field.
  subroutine variable_location()
    integer :: loc2(4,1), loc3n(4,2), loc3c(4,2), locin(4,1), locmesh(3,1), loczc(4,1)
    loc2 = 1
    loc3n = 1
    loc3c = 1; loc3c(4,:) = 0
    locin = 1; locin(4,1) = 0
    locmesh = 1
    loczc = 1; loczc(3:4,1) = 0
    call case_location('loc_2d_nodes.szplt',    'x y a b', reshape([4,3,1], [3,1]),       loc2,    .false., 0, 1)
    call case_location('loc_3d_nodes.szplt',    'x y z a', reshape([3,3,3, 3,1,3], [3,2]), loc3n,   .false., 0, 1)
    call case_location('loc_3d_cells.szplt',    'x y z a', reshape([3,3,3, 3,1,3], [3,2]), loc3c,   .true.,  0, 0)
    call case_location('loc_mesh_false.szplt',  'x y z',   reshape([3,3,3], [3,1]),       locmesh, .false., 0, 0)
    call case_location('loc_mesh_true.szplt',   'x y z',   reshape([3,3,3], [3,1]),       locmesh, .true.,  0, 1)
    ! last: before the change the reader read the values at the cells of this zone past the end of TecIO's buffer
    call case_location('loc_2d_in_zone.szplt',  'x y a b', reshape([4,3,1], [3,1]),       locin,   .false., 1, -1)
    ! and a coordinate at the cells, which the reader put in vars(0,...)
    call case_location('loc_3d_zcell.szplt',    'x y z a', reshape([3,3,3], [3,1]),       loczc,   .false., 1, -1)
  end subroutine variable_location

  ! Value of variable v at node or cell (i, j, k) of zone z: coordinates for the first variables, then the solution variables.
  pure real(8) function value_location(v, nd, z, i, j, k)
    integer, intent(in) :: v, nd, z, i, j, k
    if (v <= nd) then
      select case(v)
      case(1); value_location = i + 0.5d0
      case(2); value_location = j + 10.0d0
      case default; value_location = k + 100.0d0
      end select
    else
      value_location = 1000.0d0*v + 100*z + i + 10*j + 20*k + 0.25d0
    endif
  end function value_location

  ! Write a .szplt file with write_szplt: zone z has dims(:,z) nodes, variable v of zone z is at the nodes when loc(v,z) is 1
  ! and at the cells otherwise, with the values of value_location. The first nd variables are the coordinates.
  subroutine write_szplt_location(fname, names, nd, dims, loc)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: nd, dims(:,:), loc(:,:)
    integer, allocatable :: nval(:,:)
    real(8), allocatable :: vals(:,:,:)
    integer :: v, z, i, j, k, m, nv
    nv = size(loc,1)
    allocate(nval(nv,size(dims,2)), vals(maxval(product(dims,1)),nv,size(dims,2)))
    vals = 0
    do z = 1, size(dims,2)
      do v = 1, nv
        m = 0
        if (loc(v,z) == 1) then
          do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
            m = m + 1; vals(m,v,z) = value_location(v, nd, z, i, j, k)
          enddo; enddo; enddo
        else
          do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
            m = m + 1; vals(m,v,z) = value_location(v, nd, z, i, j, k)
          enddo; enddo; enddo
        endif
        nval(v,z) = m
      enddo
    enddo
    call write_szplt(fname, names, dims, loc, nval, vals)
  end subroutine write_szplt_location

  ! Write the file, read it with the ORION reader with orion%tec%node set to node0 first, and check the return value (expected
  ! err0) and orion%tec%node (expected 1 true, 0 false, -1 not checked). A file with solution variables at the nodes is written
  ! again as read, and the new file is checked with the TecIO reader.
  subroutine case_location(fname, names, dims, loc, node0, err0, node1)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: dims(:,:), loc(:,:), err0, node1
    logical,          intent(in) :: node0
    type(orion_data) :: d
    integer :: nd, err
    nd = merge(3, 2, index(names, 'z') > 0)
    call write_szplt_location(fname, names, nd, dims, loc)
    d%tec%node = node0
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A,I0,A,L1)') fname//': return value ', err, ', orion%tec%node ', d%tec%node
    call check((err == 0) .eqv. (err0 == 0), fname//': return value')
    if (err /= 0 .or. node1 < 0) return
    call check(d%tec%node .eqv. (node1 == 1), fname//': orion%tec%node')
    if (node1 == 1 .and. size(loc,1) > nd) call write_again(fname, d, nd, dims)
  end subroutine case_location

  ! Write the field read from fname again as it is, to 'again_'//fname, and check that file with the TecIO reader: every
  ! solution variable at the nodes, with the values written in fname.
  subroutine write_again(fname, d, nd, dims)
    character(len=*), intent(in) :: fname
    type(orion_data), intent(in) :: d
    integer,          intent(in) :: nd, dims(:,:)
    type(c_ptr) :: h
    integer(c_int32_t) :: e, nv, z, v, l
    integer(c_int64_t) :: n
    real(c_double), allocatable :: x(:)
    real(8), allocatable :: expected(:)
    integer :: err, i, j, k, m
    character(len=12) :: names
    names = merge('x y z a     ', 'x y a b     ', nd == 3)
    err = tec_write_structured_multiblock(orion=d, varnames=trim(names(2*nd+1:)), filename='again_'//fname)
    call check(err == 0, 'again_'//fname//': written without error')
    h = c_null_ptr
    e = tecFileReaderOpen('again_'//fname//c_null_char, h)
    call check(e == 0, 'again_'//fname//': opened by the TecIO reader')
    if (e /= 0) return
    e = tecDataSetGetNumVars(h, nv)
    do z = 1, size(dims,2)
      do v = int(nd,c_int32_t)+1, nv
        e = tecZoneVarGetValueLocation(h, z, v, l)
        call check(e == 0 .and. l == 1, 'again_'//fname//': solution variable at the nodes')
        e = tecZoneVarGetNumValues(h, z, v, n)
        call check(e == 0 .and. n == product(dims(:,z)), 'again_'//fname//': one value per node')
        if (n /= product(dims(:,z))) cycle
        allocate(x(n), expected(n)); m = 0
        do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
          m = m + 1; expected(m) = value_location(int(v), nd, int(z), i, j, k)
        enddo; enddo; enddo
        e = tecZoneVarGetDoubleValues(h, z, v, 1_c_int64_t, n, x)
        call check(e == 0 .and. all(x == expected), 'again_'//fname//': values of the file read')
        deallocate(x, expected)
      enddo
    enddo
    e = tecFileReaderClose(h)
  end subroutine write_again

  ! Files of lines of nodes (JMax = KMax = 1 in every zone) with x and y. The first two variables
  ! named x and y (in any case, as the rule of slices compares x, y and z), y nodal in every zone,
  ! are the coordinates of the file: the lines of a two-dimensional field, as
  ! tec_write_structured_multiblock writes a two-dimensional mesh of lines. Files written with the
  ! TecIO calls that the ORION writer uses, of two zones of lines (4 and 3 nodes): x y a b with a
  ! and b at the cells and at the nodes, and X Y a b (two coordinates); x a b at the cells (one
  ! coordinate); x y a with y and a at the cells (one coordinate, y a solution variable); x q a with
  ! q at the nodes and a at the cells (refused: solution variables at the nodes and at the cells). A
  ! two-dimensional field of two lines written with tec_write_structured_multiblock to a .szplt
  ! file, at the cells and at the nodes, is read back with two coordinates and the values written.
  ! For the files read: the return value, the number of coordinates, every coordinate and every
  ! value of the last variable. Every value is an integer or a quarter.
  ! Defect: the reader took the coordinates of a file of lines from its shape only, one, so a
  ! two-dimensional field of lines written by ORION was refused (variables at the cells) or read
  ! with y as a solution variable (variables at the nodes).
  ! Basis: by construction, as the Tecplot 360 EX 2023 R1 Data Format Guide leaves the choice of the
  ! coordinates to the plot (section 4-3.1, p. 121): x and y as the first two variables, y nodal,
  ! are the coordinates of the lines of a two-dimensional field, the file that
  ! tec_write_structured_multiblock writes.
  subroutine lines_xy()
    call case_lines('szlines_xy_cells.szplt',  'x y a b', [1,1,0,0], 0, 2)
    call case_lines('szlines_xy_nodes.szplt',  'x y a b', [1,1,1,1], 0, 2)
    call case_lines('szlines_XY_nodes.szplt',  'X Y a b', [1,1,1,1], 0, 2)
    call case_lines('szlines_x_cells.szplt',   'x a b',   [1,0,0],   0, 1)
    call case_lines('szlines_ycell.szplt',     'x y a',   [1,0,0],   0, 1)
    call case_lines('szlines_xq_mixed.szplt',  'x q a',   [1,1,0],   1, 0)
    call written_again(.false.)
    call written_again(.true.)
  end subroutine lines_xy

  ! Value of variable v at node or cell i of zone z.
  pure real(8) function value_lines(v, z, i)
    integer, intent(in) :: v, z, i
    select case(v)
    case(1); value_lines = i + 0.5d0 + 10*z
    case(2); value_lines = z + 0.25d0
    case default; value_lines = 1000*v + 100*z + i + 0.25d0
    end select
  end function value_lines

  ! Write, with write_szplt, a file of two zones of lines (4 and 3 nodes) with the variables names, at the nodes where loc is 1;
  ! read it and check the return value (0 read, 1 refused) and, when read, the number of coordinates nd, the coordinates and the
  ! last variable.
  subroutine case_lines(fname, names, loc, err0, nd)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: loc(:), err0, nd
    integer, parameter :: ni(2) = [4, 3]
    type(orion_data) :: d
    integer, allocatable :: nval(:,:)
    real(8), allocatable :: vals(:,:,:)
    integer :: z, v, i, nv, err, n
    logical :: same
    nv = size(loc)
    allocate(nval(nv,2), vals(maxval(ni),nv,2))
    vals = 0
    do z = 1, 2
      do v = 1, nv
        nval(v,z) = merge(ni(z), ni(z) - 1, loc(v) == 1)
        vals(1:nval(v,z),v,z) = [(value_lines(v, z, i), i = 1, nval(v,z))]
      enddo
    enddo
    call write_szplt(fname, names, reshape([ni(1),1,1, ni(2),1,1], [3,2]), spread(loc, 2, 2), nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A,I0)') fname//': return value ', err
    call check((err == 0) .eqv. (err0 == 0), fname//': return value')
    if (err /= 0 .or. err0 /= 0) return
    call check(size(d%block) == 2, fname//': two zones')
    if (size(d%block) /= 2) return
    do z = 1, 2
      call check(size(d%block(z)%mesh,1) == nd, fname//': number of coordinates')
      if (size(d%block(z)%mesh,1) /= nd) cycle
      same = .true.
      do v = 1, nd
        do i = 1, ni(z)
          if (d%block(z)%mesh(v, i-1, 0, 0) /= value_lines(v, z, i)) same = .false.
        enddo
      enddo
      call check(same, fname//': coordinates')
      n = merge(ni(z), ni(z) - 1, loc(nv) == 1)
      same = size(d%block(z)%vars(nv-nd,:,:,:)) == n
      if (same) same = all(reshape(d%block(z)%vars(nv-nd,:,:,:), [n]) == [(value_lines(nv, z, i), i = 1, n)])
      call check(same, fname//': values of the last variable')
    enddo
  end subroutine case_lines

  ! A two-dimensional field of two lines of nodes (Nj = Nk = 0; mesh(1:2,...)) written with tec_write_structured_multiblock to a
  ! .szplt file with the variables at the cells or at the nodes, read back: two coordinates and the values written.
  subroutine written_again(nodal)
    logical, intent(in) :: nodal
    integer, parameter :: ni(2) = [3, 2]
    type(orion_data) :: w, r
    character(len=40) :: fname
    integer :: z, i, err, f
    logical :: same
    fname = 'szlines_written_'//trim(merge('nodes', 'cells', nodal))//'.szplt'
    f = merge(0, 1, nodal)
    allocate(w%block(2))
    do z = 1, 2
      w%block(z)%name = 'L'//achar(48+z)
      w%block(z)%Ni = ni(z); w%block(z)%Nj = 0; w%block(z)%Nk = 0
      allocate(w%block(z)%mesh(1:2, 0:ni(z), 0:0, 0:0), w%block(z)%vars(1:2, f:ni(z), merge(0,1,nodal):merge(0,1,nodal), &
               merge(0,1,nodal):merge(0,1,nodal)))
      do i = 0, ni(z)
        w%block(z)%mesh(:, i, 0, 0) = [value_lines(1, z, i+1), value_lines(2, z, i+1)]
      enddo
      do i = f, ni(z)
        w%block(z)%vars(:, i, lbound(w%block(z)%vars,3), lbound(w%block(z)%vars,4)) = [value_lines(3, z, i), value_lines(4, z, i)]
      enddo
    enddo
    w%tec%node = nodal
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='a b', filename=trim(fname))
    call check(err == 0, trim(fname)//': written without error')
    r%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=r, filename=trim(fname))
    call check(err == 0, trim(fname)//': read back without error')
    if (err /= 0) return
    do z = 1, 2
      call check(size(r%block(z)%mesh,1) == 2, trim(fname)//': two coordinates')
      if (size(r%block(z)%mesh,1) /= 2) cycle
      same = all(r%block(z)%mesh(:, 0:ni(z), 0, 0) == w%block(z)%mesh(:, 0:ni(z), 0, 0))
      call check(same, trim(fname)//': coordinates read back as written')
      same = size(r%block(z)%vars,1) == 2 .and. size(r%block(z)%vars,2) == ni(z) + 1 - f
      if (same) same = all(reshape(r%block(z)%vars, [2*(ni(z)+1-f)]) == reshape(w%block(z)%vars, [2*(ni(z)+1-f)]))
      call check(same, trim(fname)//': values read back as written')
    enddo
  end subroutine written_again

  ! Zones whose solution variables are at different locations. ORION holds one location for all
  ! solution variables of a block: each block takes the location of its zone, vars from 0 at the
  ! nodes and from 1 at the cells, so the zones of a file may differ; orion%tec%node, which tells one
  ! location for a whole field, is then left as it is. Files written with the TecIO calls that the
  ! ORION writer uses, of two volume zones of 3 x 3 x 3 nodes (x y z a): a at the nodes in the first
  ! zone and at the cells in the second, and the other way round; each read with orion%tec%node set
  ! to false first and to true first. For each: the return value, orion%tec%node as it was, and for
  ! each zone the bounds of its variables and every value of a. Every value is an integer or a
  ! quarter.
  ! Defect: none; the checks state that a file whose zones differ in location is read with each zone
  ! at its own location, as before: only a zone that holds both locations cannot be held.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2, VARLOCATION (p. 124): "Each
  ! variable in each zone in a data file may be located at the nodes or the cell-centers"; ORION
  ! holds one location for all the solution variables of a block.
  subroutine zone_locations()
    integer :: loc(4,2), f, s
    character(len=40) :: fname
    logical :: node0
    do f = 1, 2
      loc = 1
      loc(4, f) = 0
      write(fname,'(A,I0,A)') 'zones_loc_', f, '.szplt'
      call write_szplt_location(trim(fname), 'x y z a', 3, reshape([3,3,3, 3,3,3], [3,2]), loc)
      do s = 1, 2
        node0 = (s == 2)
        call read_zones(trim(fname), loc, node0)
      enddo
    enddo
  end subroutine zone_locations

  ! Read a file of zone_locations with orion%tec%node set to node0 first, and check it.
  subroutine read_zones(fname, loc, node0)
    character(len=*), intent(in) :: fname
    integer,          intent(in) :: loc(:,:)
    logical,          intent(in) :: node0
    type(orion_data) :: d
    real(8), allocatable :: expected(:)
    character(len=80) :: what
    integer :: err, z, i, j, k, m, lo
    d%tec%node = node0
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(what,'(A,L1)') fname//', orion%tec%node first ', node0
    write(*,'(A,I0)') trim(what)//': return value ', err
    call check(err == 0, trim(what)//': read (return value 0)')
    if (err /= 0) return
    call check(d%tec%node .eqv. node0, trim(what)//': orion%tec%node as it was')
    do z = 1, 2
      lo = merge(0, 1, loc(4,z) == 1)
      call check(all(lbound(d%block(z)%vars) == [1, lo, lo, lo]) .and. all(ubound(d%block(z)%vars) == [1, 2, 2, 2]), &
                 trim(what)//': bounds of the variables of zone '//achar(48+z))
      if (any(lbound(d%block(z)%vars) /= [1, lo, lo, lo])) cycle
      allocate(expected((3-lo)**3)); m = 0
      do k = lo, 2; do j = lo, 2; do i = lo, 2
        m = m + 1; expected(m) = value_location(4, 3, z, i, j, k)
      enddo; enddo; enddo
      call check(all(reshape(d%block(z)%vars(1,:,:,:), [size(expected)]) == expected), &
                 trim(what)//': values of a in zone '//achar(48+z))
      deallocate(expected)
    enddo
  end subroutine read_zones

  ! Passive variables, which hold no values in a zone: in a zone with values (passive_one_zone), and
  ! as all the solution variables of a zone, declared at the nodes and at the cells (passive_zone).
  subroutine passive_variables()
    call passive_one_zone()
    call passive_zone(.false.)
    call passive_zone(.true.)
  end subroutine passive_variables

  ! A passive variable, which holds no values in a zone, does not decide the location of the zone,
  ! and reads as zero: the block takes the location of its solution variables that hold values.
  ! File written with the TecIO calls that the ORION writer uses, one volume zone of 3 x 3 x 3 nodes
  ! (x y z p a): x, y and z at the nodes, p passive (declared at the nodes, no values), a at the
  ! cells; read with orion%tec%node set to true first. Checks: the return value, orion%tec%node
  ! cleared, the bounds of the variables (from 1, the cells), every value of a, and p zero.
  ! Defect: the reader gave the zone the bounds of the nodes, as if p held values at the nodes, read
  ! the 8 values of a over 27 nodes, past the end of TecIO's buffer, and left p as the memory held.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 1-3, item 7 "Passive Variables" (p. 9):
  ! "A passive variable is one that will always return the value zero if queried"; ORION holds one
  ! location for all the solution variables of a block, here that of a, the one that holds values.
  subroutine passive_one_zone()
    integer, external :: tecini142, teczne142, tecdatd142, tecend142
    character(len=1), parameter :: z0 = achar(0)
    type(orion_data) :: d
    integer :: e, pas(5), vloc(5), nul(5), m, i, err
    real(8), allocatable :: buf(:)
    pas = [0, 0, 0, 1, 0]; vloc = [1, 1, 1, 1, 0]; nul = 0
    e = tecini142('passive'//z0, 'x y z p a'//z0, 'passive.szplt'//z0, '.'//z0, 1, 0, 0, 1)
    e = teczne142('zone'//z0, 0, 3, 3, 3, 0, 0, 0, 0.0d0, 0, 0, 1, 0, 0, 0, 0, 0, pas, vloc, nul, 0)
    do m = 1, 3
      buf = [(dble(100*m + i), i = 1, 27)]
      e = tecdatd142(27, buf)
    enddo
    buf = [(dble(1000 + i) + 0.25d0, i = 1, 8)]
    e = tecdatd142(8, buf)
    e = tecend142()
    d%tec%format = 'binary'
    d%tec%node = .true.
    err = tec_read_structured_multiblock(orion=d, filename='passive.szplt')
    write(*,'(A,I0,A,L1)') 'passive.szplt: return value ', err, ', orion%tec%node ', d%tec%node
    call check(err == 0, 'passive.szplt: read (return value 0)')
    if (err /= 0) return
    call check(.not.d%tec%node, 'passive.szplt: orion%tec%node cleared (a at the cells)')
    call check(all(d%block(1)%vars(1,:,:,:) == 0.0d0), 'passive.szplt: p, passive, zero')
    call check(all(lbound(d%block(1)%vars) == [1, 1, 1, 1]) .and. all(ubound(d%block(1)%vars) == [2, 2, 2, 2]), &
               'passive.szplt: the variables at the cells, vars(1:2,1:2,1:2,1:2)')
    if (.not.all(shape(d%block(1)%vars) == [2, 2, 2, 2])) return
    call check(all(reshape(d%block(1)%vars(2,:,:,:), [8]) == [(dble(1000 + i) + 0.25d0, i = 1, 8)]), &
               'passive.szplt: every value of a')
  end subroutine passive_one_zone

  ! A zone whose solution variables are all passive, a zone of coordinates only, takes the location of
  ! the file, orion%tec%node, at which tec_write_structured_multiblock writes every block. File
  ! written with the TecIO calls that the ORION writer uses, two volume zones of 3 x 3 x 3 nodes
  ! (x y z a): a at the nodes with values in zone 1, passive in zone 2, declared at the cells (cellp)
  ! or at the nodes; read with orion%tec%node cleared first, written again to an ASCII file with
  ! tec_write_structured_multiblock and read back. Checks: the return value, orion%tec%node set (a at
  ! the nodes in zone 1), the bounds of the variables of both zones (from 0, the nodes), every value of
  ! a in zone 1, a zero in zone 2; the file written again: the return value, the return value of the
  ! reading back, and every value of a in both zones as read.
  ! Defect: the reader gave zone 2 the bounds of the cells, or those of the location declared for a,
  ! while it set orion%tec%node, so the writer read the variables of zone 2 from index 0, below their
  ! lower bound, and wrote what the memory held.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 1-3, item 7 "Passive Variables" (p. 9):
  ! a passive variable is one that "will always return the value zero if queried", for zones where it
  ! is "not important or not calculated"; ORION writes every block of a field at the location of
  ! orion%tec%node.
  subroutine passive_zone(cellp)
    logical, intent(in) :: cellp
    integer, external :: tecini142, teczne142, tecdatd142, tecend142
    character(len=1), parameter :: z0 = achar(0)
    type(orion_data) :: d, r
    character(len=40) :: fname, ftec
    integer :: e, pas(4), vloc(4), nul(4), m, i, j, k, n, z, err
    real(8) :: buf(27)
    logical :: same
    fname = 'passive_zone_'//merge('cells', 'nodes', cellp)//'.szplt'
    ftec = 'passive_zone_'//merge('cells', 'nodes', cellp)//'.tec'
    nul = 0
    e = tecini142('passive zone'//z0, 'x y z a'//z0, trim(fname)//z0, '.'//z0, 1, 0, 0, 1)
    do z = 1, 2
      pas = 0; vloc = 1
      if (z == 2) pas(4) = 1
      if (z == 2 .and. cellp) vloc(4) = 0
      e = teczne142('zone'//achar(48+z)//z0, 0, 3, 3, 3, 0, 0, 0, 0.0d0, 0, 0, 1, 0, 0, 0, 0, 0, pas, vloc, nul, 0)
      do m = 1, 3
        n = 0
        do k = 1, 3; do j = 1, 3; do i = 1, 3
          n = n + 1; buf(n) = dble(merge(i, merge(j, k, m == 2), m == 1) + 10*z)
        enddo; enddo; enddo
        e = tecdatd142(27, buf)
      enddo
      if (z == 1) then
        buf = [(dble(1000 + i) + 0.25d0, i = 1, 27)]
        e = tecdatd142(27, buf)
      endif
    enddo
    e = tecend142()
    d%tec%format = 'binary'
    d%tec%node = .false.
    err = tec_read_structured_multiblock(orion=d, filename=trim(fname))
    write(*,'(A,I0,A,L1)') trim(fname)//': return value ', err, ', orion%tec%node ', d%tec%node
    call check(err == 0, trim(fname)//': read (return value 0)')
    if (err /= 0) return
    call check(d%tec%node, trim(fname)//': orion%tec%node set (a at the nodes in zone 1)')
    same = .true.
    do z = 1, 2
      same = same .and. all(lbound(d%block(z)%vars) == [1, 0, 0, 0]) .and. all(ubound(d%block(z)%vars) == [1, 2, 2, 2])
    enddo
    call check(same, trim(fname)//': the variables of both zones at the nodes, vars(1:1,0:2,0:2,0:2)')
    if (.not.same) return
    call check(all(reshape(d%block(1)%vars(1,:,:,:), [27]) == [(dble(1000 + i) + 0.25d0, i = 1, 27)]), &
               trim(fname)//': every value of a in zone 1')
    call check(all(d%block(2)%vars == 0.0d0), trim(fname)//': a, passive in zone 2, zero')
    d%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=d, varnames='a', filename=trim(ftec))
    call check(err == 0, trim(ftec)//': written again (return value 0)')
    r%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=r, filename=trim(ftec))
    call check(err == 0, trim(ftec)//': read back (return value 0)')
    if (err /= 0) return
    same = size(r%block) == 2
    if (same) same = all(shape(r%block(1)%vars) == [1, 3, 3, 3]) .and. all(shape(r%block(2)%vars) == [1, 3, 3, 3])
    if (same) same = all(r%block(1)%vars == d%block(1)%vars) .and. all(r%block(2)%vars == 0.0d0)
    call check(same, trim(ftec)//': every value of a in both zones, as read')
  end subroutine passive_zone

  ! Files with fewer variables than the coordinates of their zones cannot be held and are refused
  ! (return value 1), as the ASCII reader refuses them. File written with the TecIO calls that the
  ! ORION writer uses: a zone of 3 x 3 x 1 nodes with the variable x only (two coordinates, one
  ! variable). TecIO does not write a volume zone with fewer than three variables ("Variables 1,2
  ! and 3 are needed to calculate subzones"), so that case is not written here.
  ! Defect: the reader returned 0, with the coordinate it had no variable for never set.
  ! Basis: by construction: a zone with more than one node line in J has two coordinates, and the
  ! coordinates are the first variables of the file.
  subroutine too_few_variables()
    call few_variables('few_vars_2d.szplt', 'x', [3, 3, 1])
  end subroutine too_few_variables

  ! Write a file of one zone of dims nodes with the variables names, all at the nodes, read it and check that it is refused.
  subroutine few_variables(fname, names, dims)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: dims(3)
    type(orion_data) :: d
    integer, allocatable :: loc(:,:), nval(:,:)
    real(8), allocatable :: vals(:,:,:)
    integer :: v, nv, err
    nv = count_names(names)
    allocate(loc(nv,1), nval(nv,1), vals(product(dims),nv,1))
    loc = 1; nval = product(dims)
    do v = 1, nv
      vals(:,v,1) = 1.5d0*v
    enddo
    call write_szplt(fname, names, reshape(dims, [3,1]), loc, nval, vals)
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A,I0)') fname//': return value ', err
    call check(err == 1, fname//': refused (return value 1)')
  end subroutine few_variables

  ! The number of names in names, separated by blanks.
  pure integer function count_names(names)
    character(len=*), intent(in) :: names
    integer :: i
    count_names = count([(names(i:i) /= ' ' .and. (i == 1 .or. names(max(i-1,1):max(i-1,1)) == ' '), i = 1, len(names))])
  end function count_names
end program tecplot_read_szplt_plane_xyz
