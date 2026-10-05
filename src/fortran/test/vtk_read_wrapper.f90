  program vtk_read_multiblock
  USE IR_Precision
  USE Lib_VTK, only: vtk_read_structured_multiblock, vtk_write_structured_multiblock
  use Lib_ORION_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks reading.
  !> It reads back the fields that vtk_write_wrapper writes, then checks vtk_read_structured_multiblock on files of its own,
  !> one subroutine per case, each with what it checks, the defect it closes and its basis. Exit status 0 when every check
  !> passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data):: orion
  integer(I4P):: E_IO, i
  integer :: checks = 0, failures = 0
  ! for the subroutine field_scalars and its procedures
  character(len=20), parameter :: names(3) = [character(len=20) :: 'ITERATION', 'OUTPUT_NUMBER', &
                                              'LAST_OUTPUT_TIME']
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  orion%vtk%format='raw'
  E_IO = vtk_read_structured_multiblock(orion=orion,vtmpath='field3D',vtspath='')

  write(*,*) 'Blocks number = ', size(orion%block)
  write(*,*) 'Mesh type = ', size(orion%block(1)%mesh,1),'D'
  do i = 1, size(orion%block)
    write(*,*) 'Block name    = ', trim(orion%block(i)%name)
    write(*,*) 'Block size    = ', orion%block(i)%Ni, orion%block(i)%Nj, orion%block(i)%Nk
  enddo

  !---------------------------------------------------------------------------------------------------------------------------------
  write(*,*)
  orion%vtk%format='raw'
  deallocate(orion%block)
  E_IO = vtk_read_structured_multiblock(orion=orion,vtmpath='field2D',vtspath='')

  write(*,*) 'Blocks number = ', size(orion%block)
  write(*,*) 'Mesh type = ', size(orion%block(1)%mesh,1),'D'
  do i = 1, size(orion%block)
    write(*,*) 'Block name    = ', trim(orion%block(i)%name)
    write(*,*) 'Block size    = ', orion%block(i)%Ni, orion%block(i)%Nj, orion%block(i)%Nk
  enddo

  call files_with_time
  call field_scalars

  write(*,'(A,I0,A,I0,A)') 'vtk_read_wrapper: ', checks, ' checks, ', failures, ' failed'
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

  subroutine files_with_time
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Files written with a solution time are read back, time included. vtk_write_structured_multiblock stores the time it is
  !< given as the TIME field data of every .vts file; vtk_read_structured_multiblock must read such files back, with or without
  !< its optional time argument, and with it return the time written: a positive time (time-accurate run) and a negative one (a
  !< steady run that stores minus its iteration count). A file written without a time reads back as time 0, and an empty
  !< <FieldData/> element does not hide the variables. Checked on a 2-D and a 3-D field of two blocks: bit for bit in the
  !< binary and raw formats, to the 15 significant digits of the writer in the ascii format.
  !< Defect: the reader took every Name attribute of a block file for a variable name, the field data included, and stopped
  !< with a segmentation fault on a file written with a time; with the time argument it called the field-data writer on the
  !< unit it was reading, so the time was never set.
  !< Basis: the readers of VTK 9.7.1 read the arrays of a FieldData element as field data of the dataset
  !< (vtkXMLReader::ReadFieldData), and VTK File Formats describes the variables of a dataset as its PointData and CellData
  !< elements (p. 12).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['binary', 'raw   ', 'ascii ']
  integer :: f, ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    do ndir = 2, 3
      call round_trip(trim(formats(f)), ndir, 1._R8P/3._R8P, 'pos')
      call round_trip(trim(formats(f)), ndir, -5._R8P, 'neg')
    enddo
    call no_time(trim(formats(f)))
  enddo
  call empty_field_data()
  end subroutine files_with_time

  pure function tolerance(fmt) result(tol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Relative tolerance of a format: binary and raw keep every bit, ascii 15 significant digits.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  real(R8P) :: tol
  !---------------------------------------------------------------------------------------------------------------------------------

  tol = 0._R8P
  if (fmt == 'ascii') tol = 1.e-14_R8P
  end function tolerance

  subroutine make_field(d, ndir, tag)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Two blocks of 3 x 2 (x 2) cells side by side, two cell-centred variables whose values are not
  !< short decimals, so that every bit must survive.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(inout) :: d
  integer,          intent(in)    :: ndir
  character(len=*), intent(in)    :: tag
  integer :: b, i, j, k, nk
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine check_field(r, w, tol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The mesh and the variables read are those written: bit for bit, or within tol relative.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: r, w
  real(R8P),        intent(in) :: tol
  integer :: b
  !---------------------------------------------------------------------------------------------------------------------------------

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
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  integer,          intent(in) :: ndir
  real(R8P),        intent(in) :: t0
  character(len=*), intent(in) :: sign_tag
  type(orion_data)  :: w, r, r2
  character(len=16) :: varnames
  character(len=32) :: tag
  real(R8P)         :: t
  integer           :: err
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine no_time(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A file written without a time reads back as time 0.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  character(len=32) :: tag
  real(R8P)         :: t
  integer           :: err
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine empty_field_data()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< An empty <FieldData/> element (valid XML, not written by ORION) before the piece: the variables
  !< are still read, and the time is 0. The ascii files written by ORION are copied line by line with
  !< that element added after the StructuredGrid start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)    :: w, r
  character(len=16)   :: varnames
  character(len=4096) :: line
  real(R8P)           :: t
  integer             :: err, b, uin, uout, ios
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine rename_file(old_name, new_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Replace file new_name with file old_name (standard Fortran: copy the lines, delete the source).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: old_name, new_name
  character(len=4096) :: line
  integer :: uin, uout, ios
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine field_scalars
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Scalars in the field data: TIME, CYCLE and named values. vtk_write_structured_multiblock stores its optional time, cycle
  !< and named scalars (fldnames, fldvalues) as field data of every .vts file, and vtk_read_structured_multiblock returns them:
  !< time and cycle are 0 when the files hold none, a named scalar is 0 with fldfound .false. when the files do not hold it. A
  !< file written with the time only reads back with no cycle and no named scalar; the field data are never taken for
  !< variables. Names and values of different sizes are refused by both functions. Checked on 2-D and 3-D fields of two blocks
  !< in the binary, raw and ascii formats: bit for bit in binary and raw, to the 15 significant digits of the writer in ascii.
  !< An addition, not a defect: cycle, fldnames, fldvalues and fldfound are new optional arguments.
  !< Basis: the readers of VTK 9.7.1 read the arrays of a FieldData element as values of the whole dataset
  !< (vtkXMLReader::ReadFieldData); TIME and CYCLE are the names that ORION writes.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter  :: formats(3) = ['binary', 'raw   ', 'ascii ']
  integer :: f, ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    do ndir = 2, 3
      call all_scalars(trim(formats(f)), ndir)
    enddo
    call time_only(trim(formats(f)))
    call cycle_only(trim(formats(f)))
  enddo
  call misuse()
  end subroutine field_scalars

  subroutine make_field_scalars(d, ndir, tag)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Two blocks of 3 x 2 (x 2) cells side by side, two cell-centred variables whose values are not
  !< short decimals, so that every bit must survive.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(inout) :: d
  integer,          intent(in)    :: ndir
  character(len=*), intent(in)    :: tag
  integer :: b, i, j, k, nk
  !---------------------------------------------------------------------------------------------------------------------------------

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
  end subroutine make_field_scalars

  subroutine all_scalars(fmt, ndir)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Time, cycle and three named scalars written; read back all of them, in another order and with
  !< a name the files do not hold, and read back without any of them.
  !---------------------------------------------------------------------------------------------------------------------------------
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
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = trim(fmt)//trim(str(.true., ndir))//'D_all'
  t0 = 1._R8P/3._R8P
  n0 = 123456_I4P
  values = [37._R8P, 4._R8P, 0.1_R8P/3._R8P]
  write(*,'(A)') trim(tag)//': written with time, cycle and three named scalars'
  call make_field_scalars(w, ndir, trim(tag))
  varnames = 'u p'
  w%vtk%format = fmt
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, &
                                        time=t0, cycle=n0, fldnames=names, fldvalues=values)
  call check(err == 0, 'written without error')

  r%vtk%format = fmt
  asked = [character(len=20) :: 'LAST_OUTPUT_TIME', 'NOT_WRITTEN', 'ITERATION', 'OUTPUT_NUMBER']
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
  call check(got(3) == values(1) .and. got(4) == values(2), 'ITERATION and OUTPUT_NUMBER as written')
  call check_field(r, w, tolerance(fmt))

  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='fld_'//trim(tag), vtspath='')
  call check(err == 0, 'read without the field data: no error')
  call check_field(r2, w, tolerance(fmt))
  end subroutine all_scalars

  subroutine time_only(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Written with the time only, as before the cycle and the named scalars existed: the cycle and
  !< the named scalars read back as absent.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  character(len=32) :: tag
  real(R8P)         :: t, got(3)
  integer(I4P)      :: n
  logical           :: found(3)
  integer           :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = trim(fmt)//'3D_time'
  write(*,'(A)') trim(tag)//': written with the time only'
  call make_field_scalars(w, 3, trim(tag))
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

  subroutine cycle_only(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Written with the cycle only: the time reads back as 0 and the cycle as written.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  character(len=32) :: tag
  real(R8P)         :: t
  integer(I4P)      :: n
  integer           :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = trim(fmt)//'3D_cycle'
  write(*,'(A)') trim(tag)//': written with the cycle only'
  call make_field_scalars(w, 3, trim(tag))
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

  subroutine misuse()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Names without values, or of another size, are refused: return value 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  real(R8P)         :: got(2)
  logical           :: found(3)
  integer           :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  write(*,'(A)') 'misuse: names and values that do not match'
  call make_field_scalars(w, 3, 'binary3D_misuse')
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
  end program vtk_read_multiblock