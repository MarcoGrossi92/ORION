!< Tecplot ASCII zones with one node plane (K = 1).
!< A slice of a 3-D field that carries x, y and z is read with three coordinates and every
!< solution variable keeps its own band, cell-centred or nodal. A pure 2-D file (x, y), a file
!< whose third variable is a cell-centred z, and a slice next to a volume zone read as before.
!< Stops with a non-zero code when a check fails.
program tecplot_read_plane_xyz
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer :: nfail

  nfail = 0
  call slice_cellcentered()
  call slice_nodal()
  call pure_2d()
  call z_cellcentered()
  call slice_and_volume()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_plane_xyz: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_plane_xyz: all checks passed'

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

  ! Six nodes (I = 3, J = 2), two cells; x y z nodal, u v w p cell-centred.
  subroutine slice_cellcentered()
    type(orion_data) :: d
    integer :: u, err
    open(newunit=u, file='plane_xyz_cc.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "z" "u" "v" "w" "p"'
    write(u,'(A)') ' ZONE  T = slice, I=3, J=2, K=1, DATAPACKING=BLOCK, '// &
                   'VARLOCATION=([1-3]=NODAL,[4-7]=CELLCENTERED), SOLUTIONTIME=0.5'
    write(u,'(A)') ' 0.0 1.0 2.0 0.0 1.0 2.0'
    write(u,'(A)') ' 0.0 0.0 0.0 1.0 1.0 1.0'
    write(u,'(A)') ' 5.0 5.0 5.0 5.0 5.0 5.0'
    write(u,'(A)') ' 11.0 12.0'
    write(u,'(A)') ' 21.0 22.0'
    write(u,'(A)') ' 31.0 32.0'
    write(u,'(A)') ' 41.0 42.0'
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='plane_xyz_cc.tec')
    write(*,'(A)') 'slice: x y z nodal, u v w p cell-centred'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block) == 1, 'one block')
    call check(size(d%varnames) == 7, 'seven names, coordinates first')
    call check(size(d%block(1)%mesh,1) == 3, 'three coordinates')
    if (size(d%block(1)%mesh,1) /= 3) return
    call check(d%block(1)%Ni == 2 .and. d%block(1)%Nj == 1 .and. d%block(1)%Nk == 0, 'Ni = 2, Nj = 1, Nk = 0')
    call check(all(d%block(1)%mesh(3,:,:,:) == 5.0d0), 'z = 5 on every node')
    call check(d%block(1)%mesh(1,2,1,0) == 2.0d0 .and. d%block(1)%mesh(2,2,1,0) == 1.0d0, 'x and y of the last node')
    call check(size(d%block(1)%vars,1) == 4, 'four solution variables')
    if (size(d%block(1)%vars,1) /= 4) return
    call check(d%block(1)%vars(1,1,1,1) == 11.0d0 .and. d%block(1)%vars(1,2,1,1) == 12.0d0, 'u in band 1')
    call check(d%block(1)%vars(2,1,1,1) == 21.0d0 .and. d%block(1)%vars(2,2,1,1) == 22.0d0, 'v in band 2')
    call check(d%block(1)%vars(3,1,1,1) == 31.0d0 .and. d%block(1)%vars(3,2,1,1) == 32.0d0, 'w in band 3')
    call check(d%block(1)%vars(4,1,1,1) == 41.0d0 .and. d%block(1)%vars(4,2,1,1) == 42.0d0, 'p in band 4')
  end subroutine slice_cellcentered

  ! The same plane, every variable nodal (no VARLOCATION), names in upper case.
  subroutine slice_nodal()
    type(orion_data) :: d
    integer :: u, err
    open(newunit=u, file='plane_xyz_node.tec', status='replace')
    write(u,'(A)') ' VARIABLES = "X" "Y" "Z" "u" "p"'
    write(u,'(A)') ' ZONE T="slice", I=3, J=2, K=1, DATAPACKING=BLOCK'
    write(u,'(A)') ' 0.0 1.0 2.0 0.0 1.0 2.0'
    write(u,'(A)') ' 0.0 0.0 0.0 1.0 1.0 1.0'
    write(u,'(A)') ' 7.0 7.0 7.0 7.0 7.0 7.0'
    write(u,'(A)') ' 1.0 2.0 3.0 4.0 5.0 6.0'
    write(u,'(A)') ' 11.0 12.0 13.0 14.0 15.0 16.0'
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='plane_xyz_node.tec')
    write(*,'(A)') 'slice: X Y Z u p, all nodal'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 3, 'three coordinates')
    if (size(d%block(1)%mesh,1) /= 3) return
    call check(all(d%block(1)%mesh(3,:,:,:) == 7.0d0), 'z = 7 on every node')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(d%block(1)%vars(1,0,0,0) == 1.0d0 .and. d%block(1)%vars(1,2,1,0) == 6.0d0, 'u in band 1')
    call check(d%block(1)%vars(2,0,0,0) == 11.0d0 .and. d%block(1)%vars(2,2,1,0) == 16.0d0, 'p in band 2')
  end subroutine slice_nodal

  ! Pure 2-D file (x, y): two coordinates, as before.
  subroutine pure_2d()
    type(orion_data) :: d
    integer :: u, err
    open(newunit=u, file='plane_xy.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "u" "v" "p"'
    write(u,'(A)') ' ZONE  T = plane, I=3, J=2, K=1, DATAPACKING=BLOCK, '// &
                   'VARLOCATION=([1-2]=NODAL,[3-5]=CELLCENTERED), SOLUTIONTIME=0'
    write(u,'(A)') ' 0.0 1.0 2.0 0.0 1.0 2.0'
    write(u,'(A)') ' 0.0 0.0 0.0 1.0 1.0 1.0'
    write(u,'(A)') ' 11.0 12.0'
    write(u,'(A)') ' 21.0 22.0'
    write(u,'(A)') ' 41.0 42.0'
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='plane_xy.tec')
    write(*,'(A)') 'pure 2-D: x y, u v p cell-centred'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 2, 'two coordinates')
    call check(d%block(1)%Ni == 2 .and. d%block(1)%Nj == 1 .and. d%block(1)%Nk == 0, 'Ni = 2, Nj = 1, Nk = 0')
    call check(size(d%block(1)%vars,1) == 3, 'three solution variables')
    if (size(d%block(1)%vars,1) /= 3) return
    call check(d%block(1)%vars(1,2,1,1) == 12.0d0 .and. d%block(1)%vars(2,1,1,1) == 21.0d0 .and. &
               d%block(1)%vars(3,2,1,1) == 42.0d0, 'u, v, p in bands 1, 2, 3')
  end subroutine pure_2d

  ! Third variable named z but cell-centred: a solution variable, two coordinates, as before.
  subroutine z_cellcentered()
    type(orion_data) :: d
    integer :: u, err
    open(newunit=u, file='plane_xy_zcell.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "z" "u"'
    write(u,'(A)') ' ZONE  T = plane, I=3, J=2, K=1, DATAPACKING=BLOCK, '// &
                   'VARLOCATION=([1-2]=NODAL,[3-4]=CELLCENTERED), SOLUTIONTIME=0'
    write(u,'(A)') ' 0.0 1.0 2.0 0.0 1.0 2.0'
    write(u,'(A)') ' 0.0 0.0 0.0 1.0 1.0 1.0'
    write(u,'(A)') ' 3.0 4.0'
    write(u,'(A)') ' 11.0 12.0'
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='plane_xy_zcell.tec')
    write(*,'(A)') 'pure 2-D: x y, cell-centred z and u'
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block(1)%mesh,1) == 2, 'two coordinates')
    call check(size(d%block(1)%vars,1) == 2, 'two solution variables')
    if (size(d%block(1)%vars,1) /= 2) return
    call check(d%block(1)%vars(1,1,1,1) == 3.0d0 .and. d%block(1)%vars(2,2,1,1) == 12.0d0, 'z in band 1, u in band 2')
  end subroutine z_cellcentered

  ! A slice next to a volume zone, in both orders: refused, as before.
  subroutine slice_and_volume()
    type(orion_data) :: d
    integer :: u, err
    open(newunit=u, file='plane_and_volume.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "z" "p"'
    call zone(u, 1)
    call zone(u, 2)
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='plane_and_volume.tec')
    write(*,'(A)') 'slice zone, then volume zone'
    call check(err /= 0, 'refused')
    if (allocated(d%block)) deallocate(d%block)
    if (allocated(d%varnames)) deallocate(d%varnames)
    open(newunit=u, file='volume_and_plane.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "z" "p"'
    call zone(u, 2)
    call zone(u, 1)
    close(u)
    err = tec_read_structured_multiblock(orion=d, filename='volume_and_plane.tec')
    write(*,'(A)') 'volume zone, then slice zone'
    call check(err /= 0, 'refused')
  end subroutine slice_and_volume

  ! A nodal zone of 2 x 2 x nk nodes: bands x, y, z, p.
  subroutine zone(u, nk)
    integer, intent(in) :: u, nk
    integer :: n, i
    write(u,'(A,I0,A)') ' ZONE T="z", I=2, J=2, K=', nk, ', DATAPACKING=BLOCK'
    do n = 1, 4
      write(u,'(*(1X,F6.1))') (dble(n), i = 1, 4*nk)
    enddo
  end subroutine zone

end program tecplot_read_plane_xyz
