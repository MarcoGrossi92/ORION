!< Tecplot binary (.szplt) zones with one node plane (K = 1).
!< A slice of a 3-D field that carries x, y and z is read with three coordinates and every
!< solution variable keeps its own band, cell-centred or nodal, also next to a volume zone;
!< the header-only pass gives the same variable count. A pure 2-D file (x, y), a 3-D file,
!< and K = 1 files whose third variable is a cell-centred z or has another name read as before.
!< The files are written with the ORION writer or with the TecIO calls it uses (TecIO build).
!< The values of every shape of ordered zone, read row by row where TecIO needs it (layers).
!< The number of coordinates of the file for every zone (zone_shapes).
!< Stops with a non-zero code when a check fails.
program tecplot_read_szplt_plane_xyz
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
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

  ! The number of names in names, separated by blanks.
  pure integer function count_names(names)
    character(len=*), intent(in) :: names
    integer :: i
    count_names = count([(names(i:i) /= ' ' .and. (i == 1 .or. names(max(i-1,1):max(i-1,1)) == ' '), i = 1, len(names))])
  end function count_names
end program tecplot_read_szplt_plane_xyz
