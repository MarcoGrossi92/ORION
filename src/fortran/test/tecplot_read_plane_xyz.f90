!< Tecplot ASCII zones with one node plane (K = 1).
!< A slice of a 3-D field that carries x, y and z is read with three coordinates and every
!< solution variable keeps its own band, cell-centred or nodal. A pure 2-D file (x, y) and a file
!< whose third variable is a cell-centred z read as before. A slice next to a volume zone, in
!< both orders, is read with three coordinates, each zone with its own number of node planes.
!< Zones of every shape, with the coordinates of the file (zone_shapes).
!< One location for the solution variables (variable_location).
!< Files of lines named x and y, read with two coordinates (lines_xy).
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
  call zone_shapes()
  call variable_location()
  call lines_xy()
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

  ! A slice next to a volume zone, in both orders: read with three coordinates, the slice with one
  ! node plane and the volume with two.
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
    call check_slice_volume(d, err, 1, 2)
    if (allocated(d%block)) deallocate(d%block)
    if (allocated(d%varnames)) deallocate(d%varnames)
    open(newunit=u, file='volume_and_plane.tec', status='replace')
    write(u,'(A)') ' VARIABLES ="x" "y" "z" "p"'
    call zone(u, 2)
    call zone(u, 1)
    close(u)
    err = tec_read_structured_multiblock(orion=d, filename='volume_and_plane.tec')
    write(*,'(A)') 'volume zone, then slice zone'
    call check_slice_volume(d, err, 2, 1)
  end subroutine slice_and_volume

  ! The file of a slice (block bs) and a volume zone (block bv): three coordinates in both, one node
  ! plane in the slice (Nk = 0) and two in the volume (Nk = 1), p = 4 at every node.
  subroutine check_slice_volume(d, err, bs, bv)
    type(orion_data), intent(in) :: d
    integer,          intent(in) :: err, bs, bv
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block) == 2, 'two blocks')
    if (size(d%block) /= 2) return
    call check(size(d%block(bs)%mesh,1) == 3 .and. size(d%block(bv)%mesh,1) == 3, 'three coordinates in both zones')
    call check(d%block(bs)%Nk == 0 .and. d%block(bv)%Nk == 1, 'one node plane in the slice, two in the volume')
    call check(all(d%block(bs)%vars == 4.0d0) .and. all(d%block(bv)%vars == 4.0d0), 'p = 4 at every node')
  end subroutine check_slice_volume

  ! A nodal zone of 2 x 2 x nk nodes: bands x, y, z, p.
  subroutine zone(u, nk)
    integer, intent(in) :: u, nk
    integer :: n, i
    write(u,'(A,I0,A)') ' ZONE T="z", I=2, J=2, K=', nk, ', DATAPACKING=BLOCK'
    do n = 1, 4
      write(u,'(*(1X,F6.1))') (dble(n), i = 1, 4*nk)
    enddo
  end subroutine zone


  ! Zones of every shape. The number of coordinates is that of the file, the same for every zone
  ! (the variables are those of the file): three when a zone has more than one node plane in K, or
  ! when the first three variables are named x, y and z and z is NODAL in every zone; otherwise two
  ! when a zone has more than one node line in J; otherwise one (a file of lines). A zone with one
  ! node plane or line in a direction holds one layer of cells there: a cell-centred variable has
  ! max(IMax-1,1)*max(JMax-1,1)*max(KMax-1,1) values (Tecplot 360 Data Format Guide, section 2-1,
  ! and 4-4.1 to 4-4.3 for the ASCII zone data), a nodal one IMax*JMax*KMax. Files, written here in
  ! BLOCK packing: x y z a with zones of every shape (IMax, JMax, KMax equal to 1 or 3), a at the
  ! cells and a at the nodes, and again with a line as the first zone (three coordinates); x y z a
  ! with zones of one node plane or line only (three, by the names), and with lines only (three, by
  ! the names); x y z with planes, z at the cells (two: a cell-centred z is no coordinate); x y a
  ! with planes and lines, also with a line as the first zone (two); p q r a with a volume (three)
  ! and with planes and lines (two); x a with lines (one); x y a with lines only (one, and a
  ! warning: the second variable, named y, is read as a solution variable). The solution variables
  ! of a file are all at the nodes or all at the cells (one location for all of them, as ORION holds
  ! them). For each zone: the number of coordinates, every coordinate and every value of the last
  ! variable.
  ! Defect: the reader gave no cell values to zones with one node plane or line in I or J, without
  ! an error, took the number of coordinates from the first zone, and refused with no message a zone
  ! whose shape did not agree with it.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 2-1 (p. 15) and sections 4-4.1 to 4-4.3
  ! (pp. 137-138): one layer of cells where a dimension is 1; section 1-3, item 7 (p. 9) and section
  ! 4-3.1 (pp. 120-121): the variables, and so the coordinates, are those of the file.
  subroutine zone_shapes()
    integer :: shapes(3,8), z
    ! the eight shapes: IMax, JMax and KMax equal to 1 or 3
    do z = 1, 8
      shapes(:,z) = [merge(3, 1, btest(z-1,0)), merge(3, 1, btest(z-1,1)), merge(3, 1, btest(z-1,2))]
    enddo
    call case_shapes('shapes_cells.tec',      'x y z a', shapes,                 .true.,  3)
    call case_shapes('shapes_nodes.tec',      'x y z a', shapes,                 .false., 3)
    call case_shapes('shapes_linefirst.tec',  'x y z a', shapes(:,[2,8,6,7,5]),  .true.,  3)
    call case_shapes('shapes_planes.tec',     'x y z a', shapes(:,[4,2,3,1]),    .true.,  3)
    call case_shapes('shapes_xyz_lines.tec',  'x y z a', shapes(:,[2,1]),        .true.,  3)
    call case_shapes('shapes_zcell.tec',      'x y z',   shapes(:,[4,3]),        .true.,  2)
    call case_shapes('shapes_2d.tec',         'x y a',   shapes(:,[4,3,2,1]),    .true.,  2)
    call case_shapes('shapes_2d_linefirst.tec','x y a',  shapes(:,[2,4,3]),      .true.,  2)
    call case_shapes('shapes_pqr3.tec',       'p q r a', shapes(:,[8,6,4]),      .true.,  3)
    call case_shapes('shapes_pqr2.tec',       'p q r a', shapes(:,[4,2]),        .true.,  2)
    call case_shapes('shapes_1d.tec',         'x a',     shapes(:,[2,1]),        .true.,  1)
    call case_shapes('shapes_xy_lines.tec',   'x y a',   shapes(:,[2]),          .true.,  1)
  end subroutine zone_shapes

  ! Value of variable v of nv at node or cell (i, j, k): coordinates for the first variables, then the last variable.
  pure real(8) function value(v, nv, i, j, k)
    integer, intent(in) :: v, nv, i, j, k
    select case(v)
    case(1)
      value = i + 0.5d0
    case(2)
      value = j + 10.0d0
    case(3)
      value = k + 100.0d0
    case default
      value = 1000.0d0*v + i + 10*j + 100*k + 0.25d0
    end select
    if (v == nv) value = 5000.0d0 + i + 10*j + 100*k + 0.25d0
  end function value

  ! Write a Tecplot ASCII file in BLOCK packing with one zone per shape given; the coordinates are nodal, the solution
  ! variables (after the first ncoord) cell-centred when acell is true. Read it with the ORION reader and check each zone.
  subroutine case_shapes(fname, names, dims, acell, ncoord)
    character(len=*), intent(in) :: fname, names
    integer,          intent(in) :: dims(:,:), ncoord
    logical,          intent(in) :: acell
    type(orion_data) :: d
    real(8), allocatable :: expected(:)
    integer :: nv, nz, z, v, i, j, k, u, err, lo
    character(len=96) :: what
    nv = count_names(names)
    nz = size(dims,2)
    open(newunit=u, file=fname, status='replace', action='write')
    write(u,'(A)') 'VARIABLES = '//names
    do z = 1, nz
      if (acell) then
        write(u,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)') 'ZONE T = Z', z, ', I=', dims(1,z), ', J=', dims(2,z), ', K=', dims(3,z), &
          ', DATAPACKING=BLOCK, VARLOCATION=([', ncoord+1, '-', nv, ']=CELLCENTERED)'
      else
        write(u,'(A,I0,A,I0,A,I0,A,I0,A)') 'ZONE T = Z', z, ', I=', dims(1,z), ', J=', dims(2,z), ', K=', dims(3,z), &
          ', DATAPACKING=BLOCK'
      endif
      do v = 1, nv
        if (v > ncoord .and. acell) then
          do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
            write(u,'(ES25.17)') value(v, nv, i, j, k)
          enddo; enddo; enddo
        else
          do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
            write(u,'(ES25.17)') value(v, nv, i, j, k)
          enddo; enddo; enddo
        endif
      enddo
    enddo
    close(u)
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A)') fname//': '//names//', a '//merge('at the cells', 'at the nodes', acell)
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
        allocate(expected(product(dims(:,z))))
        call fill(expected, v, nv, dims(:,z), .false.)
        call check(all(reshape(d%block(z)%mesh(v,:,:,:), [size(expected)]) == expected), &
                   trim(what)//': coordinate '//names(2*v-1:2*v-1))
        deallocate(expected)
      enddo
      lo = nv - ncoord
      if (acell) then
        allocate(expected(product(max(dims(:,z)-1,1))))
      else
        allocate(expected(product(dims(:,z))))
      endif
      call fill(expected, nv, nv, dims(:,z), acell)
      call check(size(d%block(z)%vars(lo,:,:,:)) == size(expected), trim(what)//': number of values of the last variable')
      if (size(d%block(z)%vars(lo,:,:,:)) == size(expected)) &
        call check(all(reshape(d%block(z)%vars(lo,:,:,:), [size(expected)]) == expected), &
                   trim(what)//': values of the last variable')
      deallocate(expected)
    enddo
  end subroutine case_shapes

  ! The values of variable v at the nodes (or at the cells) of a zone of nodes n, in the order of the file.
  pure subroutine fill(e, v, nv, n, cells)
    real(8), intent(out) :: e(:)
    integer, intent(in)  :: v, nv, n(3)
    logical, intent(in)  :: cells
    integer :: i, j, k, m
    m = 0
    if (cells) then
      do k = 1, max(n(3)-1,1); do j = 1, max(n(2)-1,1); do i = 1, max(n(1)-1,1)
        m = m + 1; e(m) = value(v, nv, i, j, k)
      enddo; enddo; enddo
    else
      do k = 0, n(3)-1; do j = 0, n(2)-1; do i = 0, n(1)-1
        m = m + 1; e(m) = value(v, nv, i, j, k)
      enddo; enddo; enddo
    endif
  end subroutine fill

  ! The location of the variables. ORION holds the coordinates at the nodes and one location for all
  ! solution variables of a file, orion%tec%node: at the nodes when the zone headers list no
  ! variable as CELLCENTERED, at the cells otherwise. A file with a coordinate at the cells, or with
  ! solution variables at the nodes and at the cells, in a zone or between zones, or with variables
  ! at the cells in POINT packing, is refused (return value 1, with a message on the standard
  ! error). Files written here, in BLOCK packing unless said: a two-dimensional field (x y a b) with
  ! every variable at the nodes and with a and b at the cells; a three-dimensional field (x y z a)
  ! with a at the cells; a two-dimensional field (x y z a, one node plane) with z and a at the
  ! cells, so that z is a solution variable; a mesh (x y z); and, refused, a two-dimensional field
  ! with a at the nodes and b at the cells, a three-dimensional field with a at the nodes in one
  ! zone and at the cells in the other, a three-dimensional field with z at the cells, and a
  ! two-dimensional field in POINT packing with a and b at the cells. For the files read: the return
  ! value, orion%tec%node, the number of coordinates and every value of the last variable. Every
  ! value is an integer, a half or a quarter.
  ! Defect: the reader read a zone with solution variables at the nodes and at the cells as if all
  ! were at the cells, its values shifted, with no error, and a coordinate at the cells with one
  ! value per node.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2, VARLOCATION (p. 124): each
  ! variable may be at the nodes or at the cells, and cell-centered variables need BLOCK packing;
  ! ORION holds one location for all the solution variables of a field.
  subroutine variable_location()
    ! name, variables, zones (I J K), CELLCENTERED list of each zone ('' = none), POINT packing, return value, node, coordinates
    call case_location('loc_2d_nodes.tec',   'x y a b', reshape([4,3,1], [3,1]),       ['      '],           .false., 0, 1, 2)
    call case_location('loc_2d_cells.tec',   'x y a b', reshape([4,3,1], [3,1]),       ['[3-4] '],           .false., 0, 0, 2)
    call case_location('loc_3d_cells.tec',   'x y z a', reshape([3,3,3, 3,2,3], [3,2]), ['[4]   ', '[4]   '], .false., 0, 0, 3)
    call case_location('loc_2d_zcell.tec',   'x y z a', reshape([4,3,1], [3,1]),       ['[3,4] '],           .false., 0, 0, 2)
    call case_location('loc_mesh.tec',       'x y z',   reshape([3,3,3], [3,1]),       ['      '],           .false., 0, 1, 3)
    call case_location('loc_2d_in_zone.tec', 'x y a b', reshape([4,3,1], [3,1]),       ['[4]   '],           .false., 1, -1, 0)
    call case_location('loc_3d_zones.tec',   'x y z a', reshape([3,3,3, 3,2,3], [3,2]), ['      ', '[4]   '], .false., 1, -1, 0)
    call case_location('loc_3d_zcell.tec',   'x y z a', reshape([3,3,3], [3,1]),       ['[3-4] '],           .false., 1, -1, 0)
    call case_location('loc_2d_point.tec',   'x y a b', reshape([4,3,1], [3,1]),       ['[3-4] '],           .true.,  1, -1, 0)
  end subroutine variable_location

  ! Value of variable v at node or cell (i, j, k) of zone z.
  pure real(8) function value_location(v, z, i, j, k)
    integer, intent(in) :: v, z, i, j, k
    select case(v)
    case(1); value_location = i + 0.5d0
    case(2); value_location = j + 10.0d0
    case(3); value_location = k + 100.0d0 + 1000.0d0*z
    case default; value_location = 1000.0d0*v + 100*z + i + 10*j + 20*k + 0.25d0
    end select
  end function value_location

  ! Is variable v in the CELLCENTERED list cc ('[3-4]', '[4]', '[3,4]' or '')?
  logical function is_cell(cc, v)
    character(len=*), intent(in) :: cc
    integer,          intent(in) :: v
    character(len=1) :: c
    write(c,'(I1)') v
    is_cell = index(cc, c) > 0 .or. (index(cc, '3-4') > 0 .and. (v == 3 .or. v == 4))
  end function is_cell

  ! Write the file, read it, check the return value err0 and, for a file read, orion%tec%node (node1: 1 true, 0 false), the
  ! number of coordinates nd1 and every value of the last variable of each zone.
  subroutine case_location(fname, names, dims, cc, point, err0, node1, nd1)
    character(len=*), intent(in) :: fname, names, cc(:)
    integer,          intent(in) :: dims(:,:), err0, node1, nd1
    logical,          intent(in) :: point
    type(orion_data) :: d
    real(8), allocatable :: expected(:)
    integer :: u, z, v, i, j, k, m, nv, err
    logical :: cell
    nv = count_names(names)
    open(newunit=u, file=fname, status='replace', action='write')
    write(u,'(A)') 'VARIABLES = '//names
    do z = 1, size(dims,2)
      write(u,'(A,I0,A,I0,A,I0,A,I0,A)', advance='no') 'ZONE T = Z', z, ', I=', dims(1,z), ', J=', dims(2,z), ', K=', &
        dims(3,z), ', DATAPACKING='//merge('POINT', 'BLOCK', point)
      if (len_trim(cc(z)) > 0) write(u,'(A)', advance='no') ', VARLOCATION=('//trim(cc(z))//'=CELLCENTERED)'
      write(u,'(A)') ''
      if (point) then
        ! POINT: one tuple per node (a file that the reader refuses: the cells cannot be written so)
        do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
          write(u,'(*(ES25.17))') (value_location(v, z, i, j, k), v = 1, nv)
        enddo; enddo; enddo
        cycle
      endif
      do v = 1, nv
        if (is_cell(cc(z), v)) then
          do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
            write(u,'(ES25.17)') value_location(v, z, i, j, k)
          enddo; enddo; enddo
        else
          do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
            write(u,'(ES25.17)') value_location(v, z, i, j, k)
          enddo; enddo; enddo
        endif
      enddo
    enddo
    close(u)
    d%tec%node = (node1 /= 1)
    err = tec_read_structured_multiblock(orion=d, filename=fname)
    write(*,'(A,I0,A,L1)') fname//': return value ', err, ', orion%tec%node ', d%tec%node
    call check((err == 0) .eqv. (err0 == 0), fname//': return value')
    if (err /= 0 .or. err0 /= 0) return
    call check(d%tec%node .eqv. (node1 == 1), fname//': orion%tec%node')
    call check(size(d%block(1)%mesh,1) == nd1, fname//': number of coordinates')
    if (size(d%block(1)%mesh,1) /= nd1 .or. nv == nd1) return
    do z = 1, size(dims,2)
      cell = is_cell(cc(z), nv)
      if (cell) then
        allocate(expected(product(max(dims(:,z)-1,1))))
      else
        allocate(expected(product(dims(:,z))))
      endif
      m = 0
      if (cell) then
        do k = 1, max(dims(3,z)-1,1); do j = 1, max(dims(2,z)-1,1); do i = 1, max(dims(1,z)-1,1)
          m = m + 1; expected(m) = value_location(nv, z, i, j, k)
        enddo; enddo; enddo
      else
        do k = 0, dims(3,z)-1; do j = 0, dims(2,z)-1; do i = 0, dims(1,z)-1
          m = m + 1; expected(m) = value_location(nv, z, i, j, k)
        enddo; enddo; enddo
      endif
      call check(size(d%block(z)%vars(nv-nd1,:,:,:)) == size(expected), fname//': number of values of the last variable')
      if (size(d%block(z)%vars(nv-nd1,:,:,:)) == size(expected)) &
        call check(all(reshape(d%block(z)%vars(nv-nd1,:,:,:), [size(expected)]) == expected), &
                   fname//': values of the last variable')
      deallocate(expected)
    enddo
  end subroutine case_location

  ! Files of lines of nodes (J = K = 1 in every zone) with x and y. The first two variables named x
  ! and y (in any case, as the rule of slices compares x, y and z), y NODAL in every zone, are the
  ! coordinates of the file: the lines of a two-dimensional field, as
  ! tec_write_structured_multiblock writes a two-dimensional mesh of lines. Files written here, in
  ! BLOCK packing, of two zones of lines (4 and 3 nodes): x y a b with a and b at the cells and at
  ! the nodes, and X Y a b (two coordinates); x a b at the cells (one coordinate); x y a with y and
  ! a at the cells (one coordinate, y a solution variable); x q a with q at the nodes and a at the
  ! cells (refused: solution variables at the nodes and at the cells). A two-dimensional field of
  ! two lines written with tec_write_structured_multiblock, at the cells and at the nodes, is read
  ! back with two coordinates and the values written. For the files read: the return value, the
  ! number of coordinates, every coordinate and every value of the last variable. Every value is an
  ! integer, a half or a quarter.
  ! Defect: the reader took the coordinates of a file of lines from its shape only, one, so a
  ! two-dimensional field of lines written by ORION was refused (variables at the cells) or read
  ! with y as a solution variable (variables at the nodes).
  ! Basis: by construction, as the Tecplot 360 EX 2023 R1 Data Format Guide leaves the choice of the
  ! coordinates to the plot (section 4-3.1, p. 121): x and y as the first two variables, y nodal,
  ! are the coordinates of the lines of a two-dimensional field, the file that
  ! tec_write_structured_multiblock writes.
  subroutine lines_xy()
    call case_lines('lines_xy_cells.tec',  'x y a b', '[3-4]', 0, 2)
    call case_lines('lines_xy_nodes.tec',  'x y a b', '',      0, 2)
    call case_lines('lines_XY_nodes.tec',  'X Y a b', '',      0, 2)
    call case_lines('lines_x_cells.tec',   'x a b',   '[2-3]', 0, 1)
    call case_lines('lines_ycell.tec',     'x y a',   '[2-3]', 0, 1)
    call case_lines('lines_xq_mixed.tec',  'x q a',   '[3]',   1, 0)
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

  ! Write a file of two zones of lines (4 and 3 nodes) with the variables names, those listed in cc CELLCENTERED; read it and
  ! check the return value (0 read, 1 refused) and, when read, the number of coordinates nd, the coordinates and the last variable.
  subroutine case_lines(fname, names, cc, err0, nd)
    character(len=*), intent(in) :: fname, names, cc
    integer,          intent(in) :: err0, nd
    integer, parameter :: ni(2) = [4, 3]
    type(orion_data) :: d
    integer :: u, z, v, i, nv, err, n
    logical :: cell, same
    nv = count_names(names)
    open(newunit=u, file=fname, status='replace', action='write')
    write(u,'(A)') 'VARIABLES = '//names
    do z = 1, 2
      if (len(cc) > 0) then
        write(u,'(A,I0,A,I0,A)') 'ZONE T = L', z, ', I=', ni(z), &
          ', J=1, K=1, DATAPACKING=BLOCK, VARLOCATION=('//cc//'=CELLCENTERED)'
      else
        write(u,'(A,I0,A,I0,A)') 'ZONE T = L', z, ', I=', ni(z), ', J=1, K=1, DATAPACKING=BLOCK'
      endif
      do v = 1, nv
        cell = is_cell_lines(cc, v)
        n = merge(ni(z) - 1, ni(z), cell)
        do i = 1, n
          write(u,'(ES25.17)') value_lines(v, z, i)
        enddo
      enddo
    enddo
    close(u)
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
      cell = is_cell_lines(cc, nv)
      n = merge(ni(z) - 1, ni(z), cell)
      same = size(d%block(z)%vars(nv-nd,:,:,:)) == n
      if (same) same = all(reshape(d%block(z)%vars(nv-nd,:,:,:), [n]) == [(value_lines(nv, z, i), i = 1, n)])
      call check(same, fname//': values of the last variable')
    enddo
  end subroutine case_lines

  ! Is variable v listed in cc ('[3-4]', '[2-3]', '[3]' or '')?
  logical function is_cell_lines(cc, v)
    character(len=*), intent(in) :: cc
    integer,          intent(in) :: v
    character(len=1) :: c
    write(c,'(I1)') v
    is_cell_lines = index(cc, c) > 0 .or. (index(cc, '3-4') > 0 .and. v == 4) .or. (index(cc, '2-3') > 0 .and. v == 3)
  end function is_cell_lines

  ! A two-dimensional field of two lines of nodes (Nj = Nk = 0; mesh(1:2,...)) written with tec_write_structured_multiblock with
  ! the variables at the cells or at the nodes, read back: two coordinates and the values written.
  subroutine written_again(nodal)
    logical, intent(in) :: nodal
    integer, parameter :: ni(2) = [3, 2]
    type(orion_data) :: w, r
    character(len=32) :: fname
    integer :: z, i, err, f
    logical :: same
    fname = 'lines_written_'//trim(merge('nodes', 'cells', nodal))//'.tec'
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
    w%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=w, varnames='a b', filename=trim(fname))
    call check(err == 0, trim(fname)//': written without error')
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

  ! The number of names in names, separated by blanks.
  pure integer function count_names(names)
    character(len=*), intent(in) :: names
    integer :: i
    count_names = count([(names(i:i) /= ' ' .and. (i == 1 .or. names(max(i-1,1):max(i-1,1)) == ' '), i = 1, len(names))])
  end function count_names
end program tecplot_read_plane_xyz
