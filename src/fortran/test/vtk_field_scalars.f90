!< Scalars in the field data of VTK multi-block files: TIME, CYCLE and named values.
!< vtk_write_structured_multiblock stores its optional time, cycle and named scalars (fldnames,
!< fldvalues) as field data of every .vts file, and vtk_read_structured_multiblock returns them:
!< time and cycle are 0 when the files hold none, a named scalar is 0 with fldfound .false. when the
!< files do not hold it. A file written with the time only, as before these arguments existed, reads
!< back with no cycle and no named scalar; the field data are never taken for variables. Names and
!< values of different sizes are refused by both functions. Checked on 2-D and 3-D fields of two
!< blocks in the binary, raw and ascii formats: bit for bit in binary and raw, to the 15 significant
!< digits of the writer in ascii. Stops with a non-zero code when a check fails.
program vtk_field_scalars
  use IR_Precision
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  use Lib_ORION_data
  implicit none
  character(len=6), parameter  :: formats(3) = ['binary', 'raw   ', 'ascii ']
  character(len=20), parameter :: names(3) = [character(len=20) :: 'MOSE_ITERATION', 'MOSE_OUTPUT_NUMBER', &
                                              'LAST_OUTPUT_TIME']
  integer :: nfail, f, ndir

  nfail = 0
  do f = 1, size(formats)
    do ndir = 2, 3
      call all_scalars(trim(formats(f)), ndir)
    enddo
    call time_only(trim(formats(f)))
    call cycle_only(trim(formats(f)))
  enddo
  call misuse()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'vtk_field_scalars: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'vtk_field_scalars: all checks passed'

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
      d%block(b)%name = 'fld_'//tag//'_B'//trim(str(.true., b))
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

  ! Time, cycle and three named scalars written; read back all of them, in another order and with
  ! a name the files do not hold, and read back without any of them.
  subroutine all_scalars(fmt, ndir)
    character(len=*), intent(in) :: fmt
    integer,          intent(in) :: ndir
    type(orion_data)  :: w, r, r2
    character(len=16) :: varnames
    character(len=32) :: tag
    character(len=20) :: asked(4)
    real(R8P)         :: t0, t, values(3), got(4)
    integer(I4P)      :: n0, n
    logical           :: found(4)
    integer           :: err

    tag = trim(fmt)//trim(str(.true., ndir))//'D_all'
    t0 = 1._R8P/3._R8P
    n0 = 123456_I4P
    values = [37._R8P, 4._R8P, 0.1_R8P/3._R8P]
    write(*,'(A)') trim(tag)//': written with time, cycle and three named scalars'
    call make_field(w, ndir, trim(tag))
    varnames = 'u p'
    w%vtk%format = fmt
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, &
                                          time=t0, cycle=n0, fldnames=names, fldvalues=values)
    call check(err == 0, 'written without error')

    r%vtk%format = fmt
    asked = [character(len=20) :: 'LAST_OUTPUT_TIME', 'NOT_WRITTEN', 'MOSE_ITERATION', 'MOSE_OUTPUT_NUMBER']
    t = 12345._R8P
    n = -1_I4P
    got = 12345._R8P
    found = [.false., .true., .false., .false.]
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_'//trim(tag), vtspath='', time=t, cycle=n, &
                                         fldnames=asked, fldvalues=got, fldfound=found)
    call check(err == 0, 'read with all the field data: no error')
    call check(abs(t - t0) <= tolerance(fmt)*abs(t0), 'the time written')
    call check(n == n0, 'the cycle written')
    call check(found(1) .and. (.not. found(2)) .and. found(3) .and. found(4), 'found exactly the names written')
    call check(abs(got(1) - values(3)) <= tolerance(fmt)*abs(values(3)), 'LAST_OUTPUT_TIME as written')
    call check(got(2) == 0._R8P, 'a name the files do not hold reads as 0')
    call check(got(3) == values(1) .and. got(4) == values(2), 'MOSE_ITERATION and MOSE_OUTPUT_NUMBER as written')
    call check_field(r, w, tolerance(fmt))

    r2%vtk%format = fmt
    err = vtk_read_structured_multiblock(orion=r2, vtmpath='fld_'//trim(tag), vtspath='')
    call check(err == 0, 'read without the field data: no error')
    call check_field(r2, w, tolerance(fmt))
  end subroutine all_scalars

  ! Written with the time only, as before the cycle and the named scalars existed: the cycle and
  ! the named scalars read back as absent.
  subroutine time_only(fmt)
    character(len=*), intent(in) :: fmt
    type(orion_data)  :: w, r
    character(len=16) :: varnames
    character(len=32) :: tag
    real(R8P)         :: t, got(3)
    integer(I4P)      :: n
    logical           :: found(3)
    integer           :: err

    tag = trim(fmt)//'3D_time'
    write(*,'(A)') trim(tag)//': written with the time only'
    call make_field(w, 3, trim(tag))
    varnames = 'u p'
    w%vtk%format = fmt
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, time=-5._R8P)
    call check(err == 0, 'written without error')
    r%vtk%format = fmt
    n = -1_I4P
    got = 12345._R8P
    found = .true.
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_'//trim(tag), vtspath='', time=t, cycle=n, &
                                         fldnames=names, fldvalues=got, fldfound=found)
    call check(err == 0, 'read with all the field data: no error')
    call check(t == -5._R8P, 'the time written')
    call check(n == 0_I4P, 'no cycle: 0')
    call check(.not. any(found) .and. all(got == 0._R8P), 'no named scalar: not found, 0')
    call check_field(r, w, tolerance(fmt))
  end subroutine time_only

  ! Written with the cycle only: the time reads back as 0 and the cycle as written.
  subroutine cycle_only(fmt)
    character(len=*), intent(in) :: fmt
    type(orion_data)  :: w, r
    character(len=16) :: varnames
    character(len=32) :: tag
    real(R8P)         :: t
    integer(I4P)      :: n
    integer           :: err

    tag = trim(fmt)//'3D_cycle'
    write(*,'(A)') trim(tag)//': written with the cycle only'
    call make_field(w, 3, trim(tag))
    varnames = 'u p'
    w%vtk%format = fmt
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, cycle=7_I4P)
    call check(err == 0, 'written without error')
    r%vtk%format = fmt
    t = 12345._R8P
    n = -1_I4P
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_'//trim(tag), vtspath='', time=t, cycle=n)
    call check(err == 0, 'read with time and cycle: no error')
    call check(t == 0._R8P, 'no time: 0')
    call check(n == 7_I4P, 'the cycle written')
    call check_field(r, w, tolerance(fmt))
  end subroutine cycle_only

  ! Names without values, or of another size, are refused: return value 1.
  subroutine misuse()
    type(orion_data)  :: w, r
    character(len=16) :: varnames
    real(R8P)         :: got(2)
    logical           :: found(3)
    integer           :: err

    write(*,'(A)') 'misuse: names and values that do not match'
    call make_field(w, 3, 'binary3D_misuse')
    varnames = 'u p'
    w%vtk%format = 'binary'
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, &
                                          fldnames=names)
    call check(err == 1, 'write: names without values refused')
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, &
                                          fldnames=names, fldvalues=[1._R8P, 2._R8P])
    call check(err == 1, 'write: names and values of different sizes refused')
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, time=1._R8P)
    call check(err == 0, 'write: time only, no error')
    r%vtk%format = 'binary'
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_binary3D_misuse', vtspath='', fldnames=names, fldvalues=got)
    call check(err == 1, 'read: names and values of different sizes refused')
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_binary3D_misuse', vtspath='', fldnames=names(1:2), &
                                         fldvalues=got, fldfound=found)
    call check(err == 1, 'read: found of another size refused')
  end subroutine misuse

end program vtk_field_scalars
