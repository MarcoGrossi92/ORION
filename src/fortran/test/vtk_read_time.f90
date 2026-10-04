!< VTK multi-block files written with a solution time are read back, time included.
!< vtk_write_structured_multiblock stores the time it is given as the TIME field data of every .vts
!< file. vtk_read_structured_multiblock must read such files back, with or without its optional
!< time argument, and with it return the time written: a positive time (time-accurate run) and a
!< negative one (a steady run that stores minus its iteration count). A file written without a
!< time reads back as time 0, and an empty <FieldData/> element does not hide the variables.
!< Checked on a 2-D and a 3-D field of two blocks: bit for bit in the binary and raw formats, to
!< the 15 significant digits of the writer in the ascii format. Stops with a non-zero code when a
!< check fails.
program vtk_read_time
  use IR_Precision
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  use Lib_ORION_data
  implicit none
  character(len=6), parameter :: formats(3) = ['binary', 'raw   ', 'ascii ']
  integer :: nfail, f, ndir

  nfail = 0
  do f = 1, size(formats)
    do ndir = 2, 3
      call round_trip(trim(formats(f)), ndir, 1._R8P/3._R8P, 'pos')
      call round_trip(trim(formats(f)), ndir, -5._R8P, 'neg')
    enddo
    call no_time(trim(formats(f)))
  enddo
  call empty_field_data()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'vtk_read_time: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'vtk_read_time: all checks passed'

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

  ! Relative tolerance of a format: binary and raw keep every bit, ascii 15 significant digits.
  pure function tolerance(fmt) result(tol)
    character(len=*), intent(in) :: fmt
    real(R8P) :: tol
    tol = 0._R8P
    if (fmt == 'ascii') tol = 1.e-14_R8P
  end function tolerance

  ! Two blocks of 3 x 2 (x 2) cells side by side, two cell-centred variables whose values are not
  ! short decimals, so that every bit must survive.
  subroutine make_field(d, ndir, tag)
    type(orion_data), intent(inout) :: d
    integer,          intent(in)    :: ndir
    character(len=*), intent(in)    :: tag
    integer :: b, i, j, k, nk
    nk = 1
    if (ndir == 3) nk = 2
    allocate(d%block(2))
    do b = 1, 2
      d%block(b)%name = 'time_'//tag//'_B'//trim(str(.true., b))
      d%block(b)%Ni = 3
      d%block(b)%Nj = 2
      d%block(b)%Nk = nk
      if (ndir == 2) then
        allocate(d%block(b)%mesh(2,0:3,0:2,0:0))
      else
        allocate(d%block(b)%mesh(3,0:3,0:2,0:nk))
      endif
      allocate(d%block(b)%vars(2,1:3,1:2,1:nk))
      do k = lbound(d%block(b)%mesh,4), ubound(d%block(b)%mesh,4)
        do j = 0, 2
          do i = 0, 3
            d%block(b)%mesh(1,i,j,k) = real(i + 3*(b-1), R8P) + 0.1_R8P
            d%block(b)%mesh(2,i,j,k) = real(j, R8P)/3._R8P
            if (ndir == 3) d%block(b)%mesh(3,i,j,k) = real(k, R8P)/7._R8P
          enddo
        enddo
      enddo
      do k = 1, nk
        do j = 1, 2
          do i = 1, 3
            d%block(b)%vars(1,i,j,k) = 1._R8P/real(i + 10*j + 100*k + 1000*b, R8P)
            d%block(b)%vars(2,i,j,k) = -sqrt(real(i + j + k + b, R8P))
          enddo
        enddo
      enddo
    enddo
  end subroutine make_field

  ! The mesh and the variables read are those written: bit for bit, or within tol relative.
  subroutine check_field(r, w, tol)
    type(orion_data), intent(in) :: r, w
    real(R8P),        intent(in) :: tol
    integer :: b
    if (.not. allocated(r%block)) then
      call check(.false., 'blocks read')
      return
    endif
    call check(size(r%block) == size(w%block), 'number of blocks')
    if (size(r%block) /= size(w%block)) return
    do b = 1, size(w%block)
      call check(allocated(r%block(b)%mesh), 'mesh of block '//trim(str(.true., b))//' read')
      if (allocated(r%block(b)%mesh)) then
        call check(all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)), 'shape of the mesh of block '//trim(str(.true., b)))
        if (all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh))) &
          call check(all(abs(r%block(b)%mesh - w%block(b)%mesh) <= tol*abs(w%block(b)%mesh)), &
                     'mesh of block '//trim(str(.true., b))//' as written')
      endif
      call check(allocated(r%block(b)%vars), 'variables of block '//trim(str(.true., b))//' read')
      if (.not. allocated(r%block(b)%vars)) cycle
      call check(all(shape(r%block(b)%vars) == shape(w%block(b)%vars)), 'shape of block '//trim(str(.true., b)))
      if (any(shape(r%block(b)%vars) /= shape(w%block(b)%vars))) cycle
      call check(all(abs(r%block(b)%vars - w%block(b)%vars) <= tol*abs(w%block(b)%vars)), &
                 'variables of block '//trim(str(.true., b))//' as written')
    enddo
  end subroutine check_field

  subroutine round_trip(fmt, ndir, t0, sign_tag)
    character(len=*), intent(in) :: fmt
    integer,          intent(in) :: ndir
    real(R8P),        intent(in) :: t0
    character(len=*), intent(in) :: sign_tag
    type(orion_data)  :: w, r, r2
    character(len=16) :: varnames
    character(len=32) :: tag
    real(R8P)         :: t
    integer           :: err

    tag = trim(fmt)//trim(str(.true., ndir))//'D_'//sign_tag
    write(*,'(A,ES24.16E3)') trim(tag)//': written with time = ', t0
    call make_field(w, ndir, trim(tag))
    varnames = 'u p'
    w%vtk%format = fmt
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='time_'//trim(tag), varnames=varnames, time=t0)
    call check(err == 0, 'written without error')

    ! read with the time argument
    r%vtk%format = fmt
    t = 12345._R8P
    err = vtk_read_structured_multiblock(orion=r, vtmpath='time_'//trim(tag), vtspath='', time=t)
    call check(err == 0, 'read with time: no error')
    call check(abs(t - t0) <= tolerance(fmt)*abs(t0), 'read with time: the time written')
    call check_field(r, w, tolerance(fmt))

    ! read without the time argument: the field data are not taken for variables
    r2%vtk%format = fmt
    err = vtk_read_structured_multiblock(orion=r2, vtmpath='time_'//trim(tag), vtspath='')
    call check(err == 0, 'read without time: no error')
    call check_field(r2, w, tolerance(fmt))
  end subroutine round_trip

  ! A file written without a time reads back as time 0.
  subroutine no_time(fmt)
    character(len=*), intent(in) :: fmt
    type(orion_data)  :: w, r
    character(len=16) :: varnames
    character(len=32) :: tag
    real(R8P)         :: t
    integer           :: err

    tag = trim(fmt)//'2D_notime'
    write(*,'(A)') trim(tag)//': written without time'
    call make_field(w, 2, trim(tag))
    varnames = 'u p'
    w%vtk%format = fmt
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='time_'//trim(tag), varnames=varnames)
    call check(err == 0, 'written without error')
    r%vtk%format = fmt
    t = 12345._R8P
    err = vtk_read_structured_multiblock(orion=r, vtmpath='time_'//trim(tag), vtspath='', time=t)
    call check(err == 0, 'read with time: no error')
    call check(t == 0._R8P, 'read with time: 0 when the file holds none')
    call check_field(r, w, tolerance(fmt))
  end subroutine no_time

  ! An empty <FieldData/> element (valid XML, not written by ORION) before the piece: the variables
  ! are still read, and the time is 0. The ascii files written by ORION are copied line by line with
  ! that element added after the StructuredGrid start tag.
  subroutine empty_field_data()
    type(orion_data)    :: w, r
    character(len=16)   :: varnames
    character(len=4096) :: line
    real(R8P)           :: t
    integer             :: err, b, uin, uout, ios

    write(*,'(A)') 'ascii2D_emptyfd: an empty <FieldData/> element in every block file'
    call make_field(w, 2, 'ascii2D_emptyfd')
    varnames = 'u p'
    w%vtk%format = 'ascii'
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='time_ascii2D_emptyfd', varnames=varnames)
    call check(err == 0, 'written without error')
    do b = 1, 2
      open(newunit=uin, file=trim(w%block(b)%name)//'.vts', action='read', status='old')
      open(newunit=uout, file=trim(w%block(b)%name)//'.tmp', action='write', status='replace')
      do
        read(uin, '(A)', iostat=ios) line
        if (ios /= 0) exit
        write(uout, '(A)') trim(line)
        if (index(line, '<StructuredGrid') > 0) write(uout, '(A)') '    <FieldData/>'
      enddo
      close(uin)
      close(uout)
      call rename_file(trim(w%block(b)%name)//'.tmp', trim(w%block(b)%name)//'.vts')
    enddo
    r%vtk%format = 'ascii'
    t = 12345._R8P
    err = vtk_read_structured_multiblock(orion=r, vtmpath='time_ascii2D_emptyfd', vtspath='', time=t)
    call check(err == 0, 'read with time: no error')
    call check(t == 0._R8P, 'read with time: 0 when the file holds none')
    call check_field(r, w, tolerance('ascii'))
  end subroutine empty_field_data

  ! Replace file new_name with file old_name (standard Fortran: copy the lines, delete the source).
  subroutine rename_file(old_name, new_name)
    character(len=*), intent(in) :: old_name, new_name
    character(len=4096) :: line
    integer :: uin, uout, ios
    open(newunit=uin, file=old_name, action='read', status='old')
    open(newunit=uout, file=new_name, action='write', status='replace')
    do
      read(uin, '(A)', iostat=ios) line
      if (ios /= 0) exit
      write(uout, '(A)') trim(line)
    enddo
    close(uout)
    close(uin, status='delete')
  end subroutine rename_file

end program vtk_read_time
