!< Tecplot binary (.szplt) zones with one node plane (K = 1).
!< A slice of a 3-D field that carries x, y and z is read with three coordinates and every
!< solution variable keeps its own band, cell-centred or nodal, also next to a volume zone;
!< the header-only pass gives the same variable count. A pure 2-D file (x, y), a 3-D file,
!< and K = 1 files whose third variable is a cell-centred z or has another name read as before.
!< The files are written with the ORION writer or with the TecIO calls it uses (TecIO build).
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

end program tecplot_read_szplt_plane_xyz
