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
  ! for the subroutine face_on_z0 and its procedures
  integer(I4P),     parameter :: dims(3,2) = reshape([3,4,0, 2,2,2], [3,2])
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! A child run of split_start_tag: the reads of its fields only
  if (command_argument_count() > 0) then
    call split_start_tag_child
    stop
  endif
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
  call large_file_without_field_data
  call field_data_one_line
  call mesh_dimension
  call many_blocks
  call face_on_z0
  call read_errors
  call point_data
  call arrays_before_points
  call name_in_appended_data
  call name_in_binary_values
  call split_start_tag
  call name_with_celldata
  call name_with_gt
  call cut_after_start_tag
  call name_in_single_quotes
  call attributes_in_single_quotes
  call scalars_without_components
  call vector_variables_refused
  call attribute_blanks
  call names_holding_attributes
  call tags_holding_gt
  call byte_order_values

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
  !< <FieldData/> element does not hide the variables. Checked on a 2-D and a 3-D field of two blocks, bit for bit in the
  !< binary, raw and ascii formats (the ascii format writes 17 significant digits; with the 15 it wrote, coordinates, values
  !< and times that are not short decimals came back as other reals).
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

  subroutine check_field(r, w)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The mesh and the variables read are those written, bit for bit.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: r, w
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
        call check(all(r%block(b)%mesh == w%block(b)%mesh), 'mesh of block '//trim(str(.true., b))//' as written')
    endif
    call check(allocated(r%block(b)%vars), 'variables of block '//trim(str(.true., b))//' read')
    if (.not. allocated(r%block(b)%vars)) cycle
    call check(all(shape(r%block(b)%vars) == shape(w%block(b)%vars)), 'shape of block '//trim(str(.true., b)))
    if (any(shape(r%block(b)%vars) /= shape(w%block(b)%vars))) cycle
    call check(all(r%block(b)%vars == w%block(b)%vars), 'variables of block '//trim(str(.true., b))//' as written')
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
  call check(t == t0, 'read with time: the time written')
  call check_field(r, w)

  ! read without the time argument: the field data are not taken for variables
  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='time_'//trim(tag), vtspath='')
  call check(err == 0, 'read without time: no error')
  call check_field(r2, w)
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
  call check_field(r, w)
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
  call check_field(r, w)
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
  !< in the binary, raw and ascii formats, bit for bit in each.
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
  call check(t == t0, 'the time written')
  call check(n == n0, 'the cycle written')
  call check(found(1) .and. (.not. found(2)) .and. found(3) .and. found(4), 'found exactly the names written')
  call check(got(1) == values(3), 'LAST_OUTPUT_TIME as written')
  call check(got(2) == 0._R8P, 'a name the files do not hold reads as 0')
  call check(got(3) == values(1) .and. got(4) == values(2), 'ITERATION and OUTPUT_NUMBER as written')
  call check_field(r, w)

  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='fld_'//trim(tag), vtspath='')
  call check(err == 0, 'read without the field data: no error')
  call check_field(r2, w)
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
  call check_field(r, w)
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
  call check_field(r, w)
  end subroutine cycle_only

  subroutine field_data_one_line
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A FieldData element on one line, as XML allows: the time, the cycle and the named scalars are read back as written, and so
  !< is the field. The files that vtk_write_structured_multiblock writes with a time only (a FieldData line of about 100
  !< characters) and with a time, a cycle and three named scalars (about 600), in the ascii, binary and raw formats, get their
  !< FieldData element on one line (the line breaks between its tags, and the blanks after them, taken out), and each data array
  !< of their CellData element on one line too (start tag, values and end tag), and are read with vtk_read_structured_multiblock.
  !< A block file in the layout of VTK with its FieldData element on one line longer than 256 characters is refused, as the same
  !< file with the element on lines of their own (arrays_before_points).
  !< Defect: the reader looked for the field data from the line after the FieldData start tag and took one tag per line, so the
  !< arrays of a FieldData element on one line were not found: the time and the cycle read as 0 and the scalars as absent, the
  !< values of a file that holds none, without an error; a FieldData line longer than 256 characters hid the names of the
  !< variables after it (read_variables_name read 256 characters of a line); and the values of a data array on the line of its
  !< start tag were taken in upper case, so a base64 code (binary format) was decoded wrong. The first array name of a block file
  !< was looked for in the first 256 characters of each line too, so a block file in the layout of VTK with a longer FieldData
  !< line named no array and was not refused: it was read, with the return value 0.
  !< Basis: XML 1.0 (Fifth Edition), W3C Recommendation 26 November 2008, 2.3, production [3]: a line feed is white space;
  !< 3.1, production [43]: the content of an element is its elements and its character data one after the other, with no
  !< white space needed between them.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: f
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    call one_line(trim(formats(f)), .false.)
    call one_line(trim(formats(f)), .true.)
  enddo
  call vtk_layout_one_line
  end subroutine field_data_one_line

  subroutine one_line(fmt, scalars)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The field of make_field_scalars written in the format fmt with a time, and with a cycle and three named scalars when scalars
  !< is true, read back with its FieldData element on one line.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  logical,          intent(in) :: scalars
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  character(len=32) :: tag
  character(len=20) :: asked(3)
  real(R8P)         :: t0, t, values(3), got(3)
  integer(I4P)      :: n0, n
  logical           :: found(3), joined
  integer           :: err, b
  !---------------------------------------------------------------------------------------------------------------------------------

  tag = fmt//'2D_oneline_time'
  if (scalars) tag = fmt//'2D_oneline'
  t0 = 1._R8P/3._R8P
  n0 = 42_I4P
  values = [37._R8P, 4._R8P, 0.1_R8P/3._R8P]
  call make_field_scalars(w, 2, trim(tag))
  varnames = 'u p'
  w%vtk%format = fmt
  if (scalars) then
    write(*,'(A)') trim(tag)//': written with time, cycle and three named scalars, FieldData put on one line'
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, &
                                          time=t0, cycle=n0, fldnames=names, fldvalues=values)
  else
    write(*,'(A)') trim(tag)//': written with time, FieldData put on one line'
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_'//trim(tag), varnames=varnames, time=t0)
  endif
  call check(err == 0, 'written without error')
  joined = .true.
  do b = 1, 2
    joined = joined .and. field_data_on_one_line(trim(w%block(b)%name)//'.vts')
    joined = joined .and. arrays_on_one_line(trim(w%block(b)%name)//'.vts')
  enddo
  call check(joined, 'FieldData, and each data array of CellData, on one line in every block file')
  r%vtk%format = fmt
  t = 12345._R8P
  if (scalars) then
    asked = [character(len=20) :: 'LAST_OUTPUT_TIME', 'ITERATION', 'OUTPUT_NUMBER']
    n = -1_I4P
    got = 12345._R8P
    found = .false.
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_'//trim(tag), vtspath='', time=t, cycle=n, &
                                         fldnames=asked, fldvalues=got, fldfound=found)
  else
    err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_'//trim(tag), vtspath='', time=t)
  endif
  call check(err == 0, 'read without error')
  call check(t == t0, 'the time written')
  if (scalars) then
    call check(n == n0, 'the cycle written')
    call check(all(found) .and. got(1) == values(3) .and. got(2) == values(1) .and. got(3) == values(2), &
               'the named scalars written')
  endif
  call check_field(r, w)
  end subroutine one_line

  subroutine vtk_layout_one_line
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A block file in the layout of VTK, its variable c as cell data before its Points (write_vtk_layout), with a FieldData element
  !< on one line of more than 256 characters (TIME and eight named scalars) after the StructuredGrid start tag, where VTK writes
  !< it: refused before any block is read (return value 1), as the same file with its FieldData element on lines of their own.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: r
  character(len=:), allocatable :: fd
  integer(I4P)                  :: err
  integer                       :: i
  !---------------------------------------------------------------------------------------------------------------------------------

  fd = '  <FieldData><DataArray type="Float64" Name="TIME" NumberOfTuples="1" format="ascii">0.5</DataArray>'
  do i = 1, 8
    fd = fd//'<DataArray type="Float64" Name="SCALAR_'//achar(48 + i)//'" NumberOfTuples="1" format="ascii">'// &
         achar(48 + i)//'</DataArray>'
  enddo
  fd = fd//'</FieldData>'
  call write_vtk_layout('vtklayout_oneline_V1.vts', .false., .true., fd)
  call write_vtm('vtklayout_oneline', [character(len=24) :: 'vtklayout_oneline_V1.vts'])
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='vtklayout_oneline', vtspath='')
  write(*,'(A,I0,A,I0)') 'vtklayout_oneline: a FieldData line of ', len(fd), ' characters, return value ', err
  call check(len(fd) > 256 .and. err == 1, 'vtklayout_oneline: a block file in the layout of VTK refused (return value 1)')
  if (allocated(r%block)) call check(.not.allocated(r%block(1)%mesh), 'vtklayout_oneline: no block read')
  end subroutine vtk_layout_one_line

  logical function arrays_on_one_line(fname) result(done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Rewrite the file fname with each data array of its CellData element on one line: the line breaks between '<DataArray' and
  !< '</DataArray>', and the blanks after them, are taken out. A self-closing array (raw format) has none. The file is read and
  !< written as a stream of bytes, as in field_data_on_one_line.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname
  character(len=:), allocatable :: s, t
  integer                       :: u, n, cs, ce, k, a, z
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: s)
  read(u) s
  close(u)
  cs = index(s, '<CellData>')
  ce = index(s, '</CellData>')
  if (cs == 0 .or. ce < cs) return
  t = s(1:cs-1)
  k = cs
  do while (k < ce)
    a = index(s(k:ce-1), '<DataArray')
    if (a == 0) exit
    a = k + a - 1
    z = index(s(a:ce-1), '</DataArray>')
    t = t//s(k:a-1)
    if (z == 0) then
      k = a
      exit
    endif
    z = a + z - 1
    ! the array from its start tag to its end tag, without the line breaks and the blanks after them
    k = a
    do while (k < z)
      if (s(k:k) == achar(10)) then
        k = k + 1
        do while (k < z .and. s(k:k) == ' ')
          k = k + 1
        enddo
      else
        t = t//s(k:k)
        k = k + 1
      endif
    enddo
  enddo
  t = t//s(k:)
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) t
  close(u)
  done = .true.
  end function arrays_on_one_line

  logical function field_data_on_one_line(fname) result(done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Rewrite the file fname with its FieldData element on one line: the line breaks between '<FieldData>' and '</FieldData>', and
  !< the blanks after them, are taken out. The file is read and written as a stream of bytes, so the appended data of the raw
  !< format, after the element, are kept as they are.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname
  character(len=:), allocatable :: s, t
  integer                       :: u, n, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: s)
  read(u) s
  close(u)
  i = index(s, '<FieldData>')
  j = index(s, '</FieldData>')
  if (i == 0 .or. j < i) return
  t = s(1:i-1)
  k = i
  do while (k < j)
    if (s(k:k) == achar(10)) then
      k = k + 1
      do while (k < j .and. s(k:k) == ' ')
        k = k + 1
      enddo
    else
      t = t//s(k:k)
      k = k + 1
    endif
  enddo
  t = t//s(j:)
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) t
  close(u)
  done = .true.
  end function field_data_on_one_line

  subroutine misuse()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Names without values, or of another size, and blank names are refused: return value 1, and the writer writes nothing.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: w, r
  character(len=16) :: varnames
  real(R8P)         :: got(2)
  logical           :: found(3), exists
  integer           :: err, b
  !---------------------------------------------------------------------------------------------------------------------------------

  write(*,'(A)') 'misuse: names and values that do not match'
  call remove_file('fld_binary3D_misuse.vtm')
  call remove_file('fld_binary3D_blank.vtm')
  call make_field_scalars(w, 3, 'binary3D_misuse')
  varnames = 'u p'
  w%vtk%format = 'binary'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, &
                                        fldnames=names)
  call check(err == 1, 'write: names without values refused')
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, &
                                        fldnames=names, fldvalues=[1._R8P, 2._R8P])
  call check(err == 1, 'write: names and values of different sizes refused')
  inquire(file='fld_binary3D_misuse.vtm', exist=exists)
  call check(.not.exists, 'write: nothing written for the names refused')
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_misuse', varnames=varnames, time=1._R8P)
  call check(err == 0, 'write: time only, no error')
  r%vtk%format = 'binary'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_binary3D_misuse', vtspath='', fldnames=names, fldvalues=got)
  call check(err == 1, 'read: names and values of different sizes refused')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_binary3D_misuse', vtspath='', fldnames=names(1:2), &
                                       fldvalues=got, fldfound=found)
  call check(err == 1, 'read: found of another size refused')
  ! A blank name found the first field data of a file whatever its name, in the reader, and was written in the writer; the
  ! blocks get names of their own, so that no block file of this write can be one of the write above
  do b = 1, size(w%block)
    w%block(b)%name = 'fld_binary3D_blank_B'//trim(str(.true., b))
    call remove_file(trim(w%block(b)%name)//'.vts')
  enddo
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='fld_binary3D_blank', varnames=varnames, &
                                        fldnames=[character(len=20) :: ' ', 'LAST_OUTPUT_TIME'], fldvalues=[1._R8P, 2._R8P])
  write(*,'(A,I0)') 'misuse: write with a blank name, return value ', err
  call check(err == 1, 'write: a blank name refused')
  inquire(file='fld_binary3D_blank.vtm', exist=exists)
  if (.not.exists) inquire(file='fld_binary3D_blank_B1.vts', exist=exists)
  call check(.not.exists, 'write: nothing written for a blank name')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='fld_binary3D_misuse', vtspath='', &
                                       fldnames=[character(len=20) :: ' ', 'LAST_OUTPUT_TIME'], fldvalues=got, fldfound=found(1:2))
  write(*,'(A,I0)') 'misuse: read with a blank name, return value ', err
  call check(err == 1, 'read: a blank name refused')
  end subroutine misuse

  subroutine remove_file(fname)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove the file fname left by an earlier run, if there is one: it would hide a write that writes nothing.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fname
  integer                      :: u
  logical                      :: there
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file=fname, exist=there)
  if (there) then
    open(newunit=u, file=fname)
    close(u, status='delete')
  endif
  end subroutine remove_file

  subroutine large_file_without_field_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A block file without field data, read with time, cycle and a name of field data, is read as one read without them: one
  !< block of 48 x 48 x 48 cells with two variables at the cells, written in the raw format without a time (a block file of
  !< 4.6 MB), read with time, cycle and fldnames = ['ABSENT']: return value 0, time, cycle and the value 0, the
  !< name not found, the field as written. The test program has a time limit (TIMEOUT of ORION.vtk_read_wrapper in
  !< src/fortran/test/CMakeLists.txt), which a read of minutes exceeds.
  !< Defect: the reader looked for TIME, CYCLE and each name in every block file through VTK_FLD_XML_READ, which reads a file
  !< record by record, one character at a time, until it meets the FieldData element: in a file without it, the whole file,
  !< the raw appended data included, at a cost that grows with the square of the length of the records.
  !< Basis: the reader returns 0 for the field data that the block files do not hold (Fortran API guide), and a block file
  !< without a FieldData element holds none.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, parameter :: n = 48
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  real(R8P)          :: t, v(1)
  integer(I4P)       :: c, err
  logical            :: found(1)
  integer            :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'nofld_B1'
  w%block(1)%Ni = n; w%block(1)%Nj = n; w%block(1)%Nk = n
  allocate(w%block(1)%mesh(3,0:n,0:n,0:n), w%block(1)%vars(2,1:n,1:n,1:n))
  do k = 0, n; do j = 0, n; do i = 0, n
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]/real(n, R8P)
  enddo; enddo; enddo
  do k = 1, n; do j = 1, n; do i = 1, n
    w%block(1)%vars(1,i,j,k) = real(i + n*(j + n*k), R8P)
    w%block(1)%vars(2,i,j,k) = -1._R8P/real(i + j + k, R8P)
  enddo; enddo; enddo
  w%vtk%format = 'raw'
  varnames = 'u p'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='nofld', varnames=varnames)
  call check(err == 0, 'nofld: written without time')
  r%vtk%format = 'raw'
  t = 12345._R8P; c = 12345; v = 12345._R8P; found = .true.
  err = vtk_read_structured_multiblock(orion=r, vtmpath='nofld', vtspath='', time=t, cycle=c, fldnames=['ABSENT'], &
                                       fldvalues=v, fldfound=found)
  write(*,'(A,I0)') 'nofld: read with time, cycle and a name, return value ', err
  call check(err == 0, 'nofld: read with time, cycle and a name: no error')
  call check(t == 0._R8P .and. c == 0 .and. v(1) == 0._R8P .and. .not.found(1), &
             'nofld: time, cycle and the value 0, the name not found')
  call check_field(r, w)
  end subroutine large_file_without_field_data

  subroutine mesh_dimension
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The dimension of a mesh. vtk_write_structured_multiblock writes a 2-D mesh (mesh(1:2,...)) as one layer of cells between
  !< two layers of nodes with z = 0, and vtk_read_structured_multiblock tells a 2-D mesh from a 3-D one by its z coordinates: a
  !< 3-D mesh whose z coordinates are not all 0 must come back as 3-D, also when they add up to 0, as for a slab around z = 0.
  !< In each of the ascii, binary and raw formats, written and read back without time: sym, a 3-D field of two blocks around z
  !< = 0, z in {-1, 1} (one layer of cells) and z in {-1, 0, 1} (two layers); pos, a 3-D field of one block with z in {0, 1};
  !< 2d, a 2-D field of one block; the checks are the dimension of the mesh read, its bounds and every coordinate, the block
  !< sizes and every variable. Every value is an integer, so it reads back bit for bit in every format.
  !< Defect: the reader took a block whose z add up to 0 for a 2-D one (sum(z) == 0) and dropped z.
  !< Basis: VTK File Formats, Points (p. 14): the points of a StructuredGrid have three coordinates and nothing marks a block
  !< as 2-D; Fortran 2018, 10.1.5.2.4 (p. 147): mathematically equivalent expressions can give different floating-point
  !< results, so sum(z) == 0 tests no geometry.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: f
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    call round_trip_dims('sym', trim(formats(f)))
    call round_trip_dims('pos', trim(formats(f)))
    call round_trip_dims('2d', trim(formats(f)))
  enddo
  end subroutine mesh_dimension

  subroutine make_field_dims(kind, fmt, orion)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Field of the given kind: block sizes, nodes at x = i (+ an offset per block), y = j, z of the kind, two cell variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)    :: kind, fmt
  type(orion_data), intent(inout) :: orion
  integer(I4P)                    :: nb, b, i, j, k, ni(2), nj(2), nk(2), ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  select case(kind)
  case('sym')
    nb = 2; ni = [3, 2]; nj = [2, 3]; nk = [1, 2]; ndir = 3
  case('pos')
    nb = 1; ni = [3, 0]; nj = [2, 0]; nk = [1, 0]; ndir = 3
  case default
    nb = 1; ni = [3, 0]; nj = [2, 0]; nk = [1, 0]; ndir = 2
  end select
  allocate(orion%block(1:nb))
  do b = 1, nb
    orion%block(b)%name = 'mesh_dims_'//kind//'_'//fmt//'_B'//trim(str(.true.,b))
    orion%block(b)%Ni = ni(b); orion%block(b)%Nj = nj(b); orion%block(b)%Nk = nk(b)
    if (ndir == 3) then
      allocate(orion%block(b)%mesh(1:3,0:ni(b),0:nj(b),0:nk(b)))
    else
      allocate(orion%block(b)%mesh(1:2,0:ni(b),0:nj(b),0:0))
    endif
    allocate(orion%block(b)%vars(1:2,1:ni(b),1:nj(b),1:nk(b)))
    do k = lbound(orion%block(b)%mesh,4), ubound(orion%block(b)%mesh,4)
      do j = 0, nj(b)
        do i = 0, ni(b)
          orion%block(b)%mesh(1,i,j,k) = real(i + 10*(b-1), R8P)
          orion%block(b)%mesh(2,i,j,k) = real(j, R8P)
          if (ndir == 3) then
            if (kind == 'sym') then
              ! z in {-1, 1} for one layer of cells, in {-1, 0, 1} for two: they add up to 0 in any order
              orion%block(b)%mesh(3,i,j,k) = real(2*k - nk(b), R8P) / real(nk(b), R8P)
            else
              orion%block(b)%mesh(3,i,j,k) = real(k, R8P)
            endif
          endif
        enddo
      enddo
    enddo
    do k = 1, nk(b)
      do j = 1, nj(b)
        do i = 1, ni(b)
          orion%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          orion%block(b)%vars(2,i,j,k) = -real(i*j*k + b, R8P)
        enddo
      enddo
    enddo
  enddo
  orion%vtk%format = fmt
  end subroutine make_field_dims

  subroutine round_trip_dims(kind, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of the given kind in the given format, read it back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: kind, fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, ndir
  !---------------------------------------------------------------------------------------------------------------------------------

  call make_field_dims(kind, fmt, written)
  vtm = 'mesh_dims_'//kind//'_'//fmt
  label = kind//' '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == size(written%block), trim(label)//': number of blocks')
  if (size(back%block) /= size(written%block)) return
  ndir = size(written%block(1)%mesh,1)
  do b = 1, size(written%block)
    associate(w => written%block(b), r => back%block(b))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': block size')
    call check(size(r%mesh,1) == ndir, trim(label)//': mesh read as '//trim(str(.true.,size(r%mesh,1)))// &
                                       '-D instead of '//trim(str(.true.,ndir))//'-D')
    if (size(r%mesh,1) /= ndir) cycle
    call check(all(lbound(r%mesh) == lbound(w%mesh)) .and. all(ubound(r%mesh) == ubound(w%mesh)), &
               trim(label)//': mesh bounds')
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) cycle
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates')
    call check(all(lbound(r%vars) == lbound(w%vars)) .and. all(ubound(r%vars) == ubound(w%vars)), &
               trim(label)//': variable bounds')
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) cycle
    call check(all(r%vars == w%vars), trim(label)//': variables')
    end associate
  enddo
  end subroutine round_trip_dims

  subroutine many_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Any number of blocks, read. For 1, 16 and 17 blocks (3-D, a few cells each, different sizes) in each of the ascii, binary
  !< and raw formats, a field written without time is read back, and the number of blocks, their names and sizes, every
  !< coordinate and every variable are checked. Every value is an integer, so it reads back bit for bit in every format.
  !< Defect: vtk_read_structured_multiblock kept the block names listed in the .vtm in an array of 16, so a field of 17 blocks
  !< was read past its end (a crash or a hang).
  !< Basis: Fortran 2018, 9.5.3.1 (p. 128): the value of a subscript shall be within the bounds of its dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: counts(3) = [1_I4P, 16_I4P, 17_I4P]
  integer :: f, c
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    do c = 1, size(counts)
      call round_trip_blocks(counts(c), trim(formats(f)))
    enddo
  enddo
  end subroutine many_blocks

  subroutine round_trip_blocks(nb, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write nb blocks in the given format, read them back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     intent(in) :: nb
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, i, j, k, ni, nj, nk
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(written%block(1:nb))
  do b = 1, nb
    ni = 1 + mod(b, 3); nj = 1 + mod(b, 2); nk = 1 + mod(b, 4)
    written%block(b)%name = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt//'_B'//trim(str(.true.,b))
    written%block(b)%Ni = ni; written%block(b)%Nj = nj; written%block(b)%Nk = nk
    allocate(written%block(b)%mesh(1:3,0:ni,0:nj,0:nk))
    allocate(written%block(b)%vars(1:2,1:ni,1:nj,1:nk))
    do k = 0, nk
      do j = 0, nj
        do i = 0, ni
          written%block(b)%mesh(1,i,j,k) = real(i + 10*b, R8P)
          written%block(b)%mesh(2,i,j,k) = real(j, R8P)
          written%block(b)%mesh(3,i,j,k) = real(k + 1, R8P)
        enddo
      enddo
    enddo
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          written%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          written%block(b)%vars(2,i,j,k) = -real(b, R8P)
        enddo
      enddo
    enddo
  enddo
  written%vtk%format = fmt
  vtm = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt
  label = trim(str(.true.,nb))//' blocks '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == nb, trim(label)//': '//trim(str(.true.,size(back%block)))//' blocks read')
  if (size(back%block) /= nb) return
  do b = 1, nb
    associate(w => written%block(b), r => back%block(b))
    call check(trim(r%name) == trim(w%name), trim(label)//': name of block '//trim(str(.true.,b)))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': size of block '//trim(str(.true.,b)))
    call check(size(r%mesh,1) == 3, trim(label)//': mesh of block '//trim(str(.true.,b))//' not 3-D')
    if (size(r%mesh,1) /= 3) cycle
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) then
      call check(.false., trim(label)//': mesh bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates of block '//trim(str(.true.,b)))
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) then
      call check(.false., trim(label)//': variable bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%vars == w%vars), trim(label)//': variables of block '//trim(str(.true.,b)))
    end associate
  enddo
  end subroutine round_trip_blocks

  subroutine face_on_z0
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A face on the plane z = 0. vtk_read_structured_multiblock reads a block with one plane of nodes in k whose nodes all lie
  !< on the plane z = 0, such as the face of a 3-D block on a wall at z = 0, as a 3-D block: its nodes keep their z, and the
  !< block has no cell in k, as its extent gives. Only a block with two planes of nodes in k and z = 0 at every node, the form
  !< in which vtk_write_structured_multiblock writes a 2-D mesh, is read as 2-D. In each of the ascii, binary and raw formats a
  !< 3-D field of two blocks, a face on the plane z = 0 (3 x 4 x 0 cells, its cell variables with one layer of cells) and a
  !< volume block (2 x 2 x 2 cells) next to it, is written and read back, and for each block Ni, Nj, Nk, the shape of the mesh
  !< and of the variables, every node, and every cell value of the volume block are checked, bit for bit (every value is an
  !< integer or a half).
  !< Defect: the reader took such a face for a 2-D block, dropped z and gave it one layer of cells in k, filled from past the
  !< end of the values it had read: wrong values or a crash.
  !< Basis: VTK File Formats (p. 13): the cells of a StructuredGrid are given by its extent, and VTK 9.7.1 counts one layer of
  !< cells in a direction with one plane of points (vtkStructuredData::GetNumberOfCells).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: f
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    call write_and_read(trim(formats(f)))
  enddo
  end subroutine face_on_z0

  pure function node(b, i, j, k) result(x)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Coordinates of node (i, j, k) of block b: the face (b = 1) on the plane z = 0, the volume block (b = 2) from z = 0 to z = 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: x(3)
  !---------------------------------------------------------------------------------------------------------------------------------

  x = [real(i + 10*(b-1), R8P), real(j, R8P) + 0.5_R8P*i, real(k, R8P)]
  end function node

  pure function cell(b, i, j, k) result(v)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Values of the two cell variables in cell (i, j, k) of block b.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: b, i, j, k
  real(R8P)                :: v(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  v = [real(i + 10*j + 100*k, R8P), -real(b, R8P) - 0.5_R8P]
  end function cell

  subroutine write_and_read(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field in the format given, read it back and check it.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w, r
  character(len=16)            :: varnames
  character(len=64)            :: label
  integer(I4P)                 :: b, i, j, k, err
  logical                      :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'F'//trim(str(.true., b))
    w%block(b)%Ni = dims(1,b); w%block(b)%Nj = dims(2,b); w%block(b)%Nk = dims(3,b)
    allocate(w%block(b)%mesh(1:3, 0:dims(1,b), 0:dims(2,b), 0:dims(3,b)))
    allocate(w%block(b)%vars(1:2, 1:max(dims(1,b),1), 1:max(dims(2,b),1), 1:max(dims(3,b),1)))
    do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
      w%block(b)%mesh(:,i,j,k) = node(b, i, j, k)
    enddo; enddo; enddo
    do k = 1, ubound(w%block(b)%vars,4); do j = 1, ubound(w%block(b)%vars,3); do i = 1, ubound(w%block(b)%vars,2)
      w%block(b)%vars(:,i,j,k) = cell(b, i, j, k)
    enddo; enddo; enddo
  enddo
  w%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath='face_z0_'//fmt//'_', vtmpath='face_z0_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': written without error')

  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='face_z0_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  call check(size(r%block) == 2, fmt//': 2 blocks read back')
  if (size(r%block) /= 2) return
  do b = 1, 2
    label = fmt//' block '//trim(str(.true., b))
    call check(r%block(b)%Ni == dims(1,b) .and. r%block(b)%Nj == dims(2,b) .and. r%block(b)%Nk == dims(3,b), &
               trim(label)//': Ni, Nj, Nk')
    same = all(shape(r%block(b)%mesh) == [3, dims(1,b)+1, dims(2,b)+1, dims(3,b)+1])
    call check(same, trim(label)//': shape of the mesh, 3 coordinates and every node')
    if (same) then
      do k = 0, dims(3,b); do j = 0, dims(2,b); do i = 0, dims(1,b)
        same = same .and. all(r%block(b)%mesh(:,i,j,k) == node(b, i, j, k))
      enddo; enddo; enddo
      call check(same, trim(label)//': nodes')
    endif
    ! The reader counts the cells of a block from its extent: none in k for the face
    same = all(shape(r%block(b)%vars) == [2, dims(1,b), dims(2,b), dims(3,b)])
    call check(same, trim(label)//': shape of the variables, one cell per extent interval')
    if (same .and. size(r%block(b)%vars) > 0) then
      do k = 1, dims(3,b); do j = 1, dims(2,b); do i = 1, dims(1,b)
        same = same .and. all(r%block(b)%vars(:,i,j,k) == cell(b, i, j, k))
      enddo; enddo; enddo
      call check(same, trim(label)//': cell values')
    endif
  enddo
  end subroutine write_and_read

  subroutine read_errors
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Errors of the reader. vtk_read_structured_multiblock returns an error, and stops reading, at the first thing it cannot
  !< read: a .vtm file that does not exist, that has no block list or that lists no block file, a block file that cannot be
  !< opened, points or variable values that cannot be read; it does not go on with the coordinates or values it has not read.
  !< It closes each block file once read, also when it stops on an error in that file. A field of two 3-D blocks is written in
  !< the ascii, binary and raw formats and read back: as written (no error, every node and value as written: the control), with
  !< both block files closed after the read, and a second time in the same program; with a path to the block files that does
  !< not exist; from a .vtm that lists a third block file that does not exist; from a .vtm whose second block file holds a
  !< StructuredGrid without points, closed after the error. Then, in each format, from a .vtm whose block file is cut in the
  !< middle of the values of its last variable (an error, and the file closed after it), and once from a .vtm that does not
  !< exist, from one without a block list and from one whose block list holds no block file. Last, in each format, a read that
  !< fails on the block files (a path to them that does not exist) and a read of the field into the same object, which must
  !< give no error and the field of a read into a new object. These last reads come last: a reader that stops the program on
  !< them cannot hide the other checks.
  !< Defects: the reader did not look at the return values of the files it opened: it read from a unit it had not opened and
  !< copied coordinates it had not read; it closed only the last block file, so a field of more blocks than the files a process
  !< can keep open was not read, and a raw block file cut short was read without an error; a .vtm that does not exist, has no
  !< block list or lists no block file stopped the program; a read into the object of a read that failed stopped the program,
  !< on the allocation of blocks already allocated.
  !< Basis: Fortran 2018, 12.11.2 (p. 253) and 12.11.5 (p. 254): an input/output error initiates error termination without
  !< IOSTAT=, and defines it with a positive value with IOSTAT=; 9.2 (p. 123): a reference is permitted only if the variable is
  !< defined; 12.5.6.18 (p. 223): OPEN with STATUS='OLD' needs the file; 12.5.7.1 (p. 223): a unit stays connected until CLOSE
  !< or the end of the program; 9.7.1.3 (p. 135): allocating an allocated variable is an error condition.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer :: f
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, size(formats)
    call write_and_read_errors(trim(formats(f)))
  enddo
  do f = 1, size(formats)
    call read_cut(trim(formats(f)))
  enddo
  call broken_vtm
  do f = 1, size(formats)
    call read_again(trim(formats(f)))
  enddo
  end subroutine read_errors

  logical function is_open(name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Whether the file is connected to a unit of this program.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file=name, opened=is_open)
  end function is_open

  logical function same_field(r, w)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Whether r holds the blocks of w, every node and value as in w.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: r, w
  integer                      :: b
  !---------------------------------------------------------------------------------------------------------------------------------

  same_field = allocated(r%block)
  if (same_field) same_field = size(r%block) == size(w%block)
  if (same_field) then
    do b = 1, size(w%block)
      same_field = same_field .and. allocated(r%block(b)%mesh) .and. allocated(r%block(b)%vars)
      if (same_field) same_field = all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
                                   all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
      if (same_field) same_field = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
    enddo
  endif
  end function same_field

  subroutine write_vtm(name, files)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a .vtm file that lists the block files given.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: name, files(:)
  integer                      :: u, n
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=name//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  do n = 1, size(files)
    write(u,'(A,I0,A)') '      <DataSet index="', n - 1, '" file="'//trim(files(n))//'"/>'
  enddo
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  end subroutine write_vtm

  subroutine cut_copy(src, dst, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Copy the block file src, written in the format fmt, to dst cut in the middle of the values of its last variable, b: in the
  !< ascii and binary formats between the start tag and the end tag of its data array, in the raw format 80 bytes before the
  !< end tag of the appended data, inside the 4 + 18*8 bytes of b, the last array of the appended data.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: src, dst, fmt
  character(len=:), allocatable :: buf
  integer                       :: u, n, p1, p2, p3, keep
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=src, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  if (fmt == 'raw') then
    keep = index(buf, '</AppendedData>', back=.true.) - 80
  else
    p1 = index(buf, 'Name="b"', back=.true.)
    p2 = p1 + index(buf(p1:), '>')
    p3 = p2 + index(buf(p2:), '</DataArray>') - 1
    keep = (p2 + p3)/2
  endif
  open(newunit=u, file=dst, access='stream', form='unformatted', status='replace', action='write')
  write(u) buf(1:keep)
  close(u)
  end subroutine cut_copy

  subroutine write_and_read_errors(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the field in the format given, read it back as written, a second time, and from the broken forms.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: w, r1, r2, r3, r4, r5
  character(len=16)            :: varnames
  character(len=64)            :: files(3)
  integer(I4P)                 :: b, i, j, k, err, u
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'E'//trim(str(.true., b))
    w%block(b)%Ni = 2; w%block(b)%Nj = 3; w%block(b)%Nk = 1 + b
    allocate(w%block(b)%mesh(1:3, 0:2, 0:3, 0:1+b), w%block(b)%vars(1:2, 1:2, 1:3, 1:1+b))
    do k = 0, 1 + b; do j = 0, 3; do i = 0, 2
      w%block(b)%mesh(:,i,j,k) = [real(i + 10*b, R8P), real(j, R8P), real(k, R8P) + 0.5_R8P]
    enddo; enddo; enddo
    do k = 1, 1 + b; do j = 1, 3; do i = 1, 2
      w%block(b)%vars(:,i,j,k) = [real(i + 10*j + 100*k, R8P), -real(b, R8P)]
    enddo; enddo; enddo
  enddo
  w%vtk%format = fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=w, vtspath='errors_'//fmt//'_', vtmpath='errors_'//fmt, varnames=varnames)
  call check(err == 0, fmt//': written without error')

  ! The control: the field as written, both block files closed after the read
  r1%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0, fmt//': read back without error')
  call check(same_field(r1, w), fmt//': read back as written')
  call check(.not.is_open('errors_'//fmt//'_E1.vts') .and. .not.is_open('errors_'//fmt//'_E2.vts'), &
             fmt//': block files closed after the read')

  ! The same field read a second time in the same program
  r5%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r5, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0 .and. same_field(r5, w), fmt//': read a second time as written')

  ! A path to the block files that does not exist
  r2%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='errors_'//fmt, vtspath='no_such_folder/')
  call check(err /= 0, fmt//': error for block files that do not exist')

  ! A .vtm that lists a third block file that does not exist
  files(1) = 'errors_'//fmt//'_E1.vts'
  files(2) = 'errors_'//fmt//'_E2.vts'
  files(3) = 'errors_'//fmt//'_E3.vts'
  call write_vtm('errors_missing_'//fmt, files)
  r3%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r3, vtmpath='errors_missing_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file listed in the .vtm that does not exist')

  ! A .vtm whose second block file holds a StructuredGrid without points
  open(newunit=u, file='errors_nopoints_'//fmt//'_E2.vts', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="+0 +2 +0 +3 +0 +3">'
  write(u,'(A)') '    <Piece Extent="+0 +2 +0 +3 +0 +3">'
  write(u,'(A)') '    </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  files(2) = 'errors_nopoints_'//fmt//'_E2.vts'
  call write_vtm('errors_nopoints_'//fmt, files(1:2))
  r4%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r4, vtmpath='errors_nopoints_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file without points')
  call check(.not.is_open('errors_nopoints_'//fmt//'_E2.vts'), fmt//': block file without points closed after the error')
  end subroutine write_and_read_errors

  subroutine read_cut(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read from a .vtm whose only block file is the second block of the field written in the format given, cut in the middle of
  !< the values of its last variable.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: r
  character(len=64)            :: files(1)
  integer(I4P)                 :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  call cut_copy('errors_'//fmt//'_E2.vts', 'errors_cut_'//fmt//'_E2.vts', fmt)
  files(1) = 'errors_cut_'//fmt//'_E2.vts'
  call write_vtm('errors_cut_'//fmt, files)
  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='errors_cut_'//fmt, vtspath='')
  call check(err /= 0, fmt//': error for a block file cut in the values of a variable')
  call check(.not.is_open('errors_cut_'//fmt//'_E2.vts'), fmt//': block file cut in its values closed after the error')
  end subroutine read_cut

  subroutine broken_vtm
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read from a .vtm that does not exist (a file of that name left by an earlier run is removed first), from one without a block
  !< list and from one whose block list holds no block file.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: r1, r2, r3
  character(len=64) :: none(0)
  integer(I4P)      :: err
  integer           :: u
  logical           :: there
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file='errors_no_such_file.vtm', exist=there)
  if (there) then
    open(newunit=u, file='errors_no_such_file.vtm')
    close(u, status='delete')
  endif
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='errors_no_such_file', vtspath='')
  call check(err /= 0, 'error for a .vtm that does not exist')

  open(newunit=u, file='errors_nolist.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='errors_nolist', vtspath='')
  call check(err /= 0, 'error for a .vtm without a block list')

  call write_vtm('errors_noblocks', none)
  err = vtk_read_structured_multiblock(orion=r3, vtmpath='errors_noblocks', vtspath='')
  call check(err /= 0, 'error for a .vtm that lists no block file')
  end subroutine broken_vtm

  subroutine read_again(fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< In the format given, a read that fails on the block files, which leaves blocks in the object, and a read of the field into
  !< the same object: no error, and the field of a read into a new object.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: r, r0
  integer(I4P)                 :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  r0%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r0, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0, fmt//': read into a new object')
  r%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=r, vtmpath='errors_'//fmt, vtspath='no_such_folder/')
  call check(err /= 0 .and. allocated(r%block), fmt//': a read that fails on the block files leaves blocks in the object')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='errors_'//fmt, vtspath='')
  call check(err == 0 .and. same_field(r, r0), fmt//': read again into the object of a read that failed, as into a new one')
  end subroutine read_again

  subroutine point_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Variables at the nodes, read. vtk_read_structured_multiblock reads a block file whose variables are point data with Ni, Nj
  !< and Nk the cells and the values in vars(:,0:Ni,0:Nj,0:Nk), as the Tecplot readers return them, and sets orion%vtk%node:
  !< a block file of one cell, written here in ascii as ORION lists its arrays (Points first), with one variable as point
  !< data, 8 values on 8 nodes (return value 0, orion%vtk%node set, Ni Nj Nk = 1 1 1, vars(1:1,0:1,0:1,0:1), each value at
  !< its node). A block file of the same cell without variables, neither PointData nor CellData, is a mesh: read with
  !< orion%vtk%node unset and Ni Nj Nk = 1 1 1.
  !< Defect: the reader returned the numbers of nodes as Ni, Nj and Nk for such a file, while the Tecplot readers and the VTK
  !< writer take them as numbers of cells, and set orion%vtk%node for a block file without variables.
  !< Basis: VTK File Formats (p. 12): PointData holds the data of each point of a piece, CellData those of each cell; the
  !< Tecplot readers return variables at the nodes in vars(:,0:Ni,0:Nj,0:Nk), with Ni, Nj and Nk the cells.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  call read_point_data
  end subroutine point_data

  subroutine read_point_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Read a block file with one variable as point data, and one without variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: r, m
  integer(I4P)     :: err
  logical          :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  call point_data_file('pointdata', .true.)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='pointdata', vtspath='')
  call check(err == 0, 'a block file with point data read (return value 0)')
  if (allocated(r%block)) then
    call check(r%vtk%node, 'a block file with point data read with orion%vtk%node set')
    call check(r%block(1)%Ni == 1 .and. r%block(1)%Nj == 1 .and. r%block(1)%Nk == 1, &
               'a block file with point data read with Ni, Nj and Nk the cells')
    same = .false.
    if (allocated(r%block(1)%vars)) same = all(lbound(r%block(1)%vars) == [1, 0, 0, 0]) .and. &
                                            all(ubound(r%block(1)%vars) == [1, 1, 1, 1])
    call check(same, 'its values read at the nodes, vars(1:1,0:1,0:1,0:1)')
    if (same) call check(all(reshape(r%block(1)%vars, [8]) == [1, 2, 3, 4, 5, 6, 7, 8]), 'each value read at its node')
  endif
  call point_data_file('nodata', .false.)
  m%vtk%format = 'ascii'
  m%vtk%node = .true.
  err = vtk_read_structured_multiblock(orion=m, vtmpath='nodata', vtspath='')
  call check(err == 0, 'a block file without variables read (return value 0)')
  if (allocated(m%block)) then
    call check(.not.m%vtk%node, 'a block file without variables read with orion%vtk%node unset')
    call check(m%block(1)%Ni == 1 .and. m%block(1)%Nj == 1 .and. m%block(1)%Nk == 1, &
               'a block file without variables read with Ni, Nj and Nk the cells')
  endif
  end subroutine read_point_data

  subroutine point_data_file(stem, values)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write stem.vtm and its block file stem_P1.vts, one cell and its 8 nodes, with the variable a as point data when values is
  !< true, without variables otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: stem
  logical,          intent(in) :: values
  integer                      :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=stem//'_P1.vts', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '    <Piece Extent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '      <Points>'
  write(u,'(A)') '        <DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
  write(u,'(A)') '          0 0 1 1 0 1 0 1 1 1 1 1 0 0 2 1 0 2 0 1 2 1 1 2'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '      </Points>'
  if (values) then
    write(u,'(A)') '      <PointData>'
    write(u,'(A)') '        <DataArray type="Float64" Name="a" NumberOfComponents="1" format="ascii">'
    write(u,'(A)') '          1 2 3 4 5 6 7 8'
    write(u,'(A)') '        </DataArray>'
    write(u,'(A)') '      </PointData>'
  endif
  write(u,'(A)') '    </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  open(newunit=u, file=stem//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="'//stem//'_P1.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  end subroutine point_data_file

  subroutine name_with_celldata
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A variable whose name holds the text CellData is not taken for a CellData element: a field of one block of 2 x 2 x 2 cells
  !< with the variable CellData_x at the nodes, written in the ascii format, is read back at the nodes (return value 0,
  !< orion%vtk%node set), every value as written.
  !< Defect: read_variables_name took any line that holds the text CellData for the CellData element, as it did not for
  !< <PointData, so the variables of such a file were taken for cell data and not found there.
  !< Basis: VTK File Formats (p. 12): the variables of a piece are the arrays of its PointData and CellData elements.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r
  character(len=16)  :: varnames
  integer(I4P)       :: err
  integer            :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'cdname_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 2; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:2,0:2), w%block(1)%vars(1,0:2,0:2,0:2))
  do k = 0, 2; do j = 0, 2; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
  enddo; enddo; enddo
  w%vtk%format = 'ascii'
  w%vtk%node = .true.
  varnames = 'CellData_x'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='cdname', varnames=varnames)
  call check(err == 0, 'cdname: written')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='cdname', vtspath='')
  write(*,'(A,I0,A,L1)') 'cdname: return value ', err, ', orion%vtk%node ', r%vtk%node
  call check(err == 0 .and. r%vtk%node, 'cdname: read at the nodes without error')
  call check_field(r, w)
  end subroutine name_with_celldata

  subroutine name_with_gt
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A variable whose name holds '>' is read back: fields of one block of 2 x 3 x 2 cells with the variables a>b and c at the
  !< cells, written in the ascii, binary and raw formats, are read back with their two variables, every value as written.
  !< Defect: array_name took the start tag to end at the first '>' of its line and looked for the closing quote of the Name value
  !< only before it, so the name a>b was not found and the variable was dropped, with the return value 0.
  !< Basis: XML 1.0 (Fifth Edition), 2.3, production [10]: an attribute value holds any character but '<', '&' and its quote, so
  !< it may hold '>'; vtk_write_structured_multiblock writes the names as given.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), parameter :: fmts(3) = ['ascii ', 'binary', 'raw   ']
  type(orion_data)   :: w, r(3)
  character(len=8)   :: varnames
  integer(I4P)       :: err
  integer            :: f, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, 3
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = 'gtname_'//trim(fmts(f))//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
      w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
    enddo; enddo; enddo
    w%vtk%format = trim(fmts(f))
    varnames = 'a>b c'
    err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='gtname_'//trim(fmts(f)), varnames=varnames)
    call check(err == 0, 'gtname_'//trim(fmts(f))//': written')
    r(f)%vtk%format = trim(fmts(f))
    err = vtk_read_structured_multiblock(orion=r(f), vtmpath='gtname_'//trim(fmts(f)), vtspath='')
    write(*,'(A,I0)') 'gtname_'//trim(fmts(f))//': return value ', err
    call check(err == 0, 'gtname_'//trim(fmts(f))//': read without error')
    call check_field(r(f), w)
  enddo
  end subroutine name_with_gt

  subroutine cut_after_start_tag
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A block file that ends right after a DataArray start tag, as a write cut short there leaves it, is refused: fields of one
  !< block of 2 x 3 x 2 cells with the variables u and p at the cells, written in the ascii format, get their block file cut
  !< after the start tag of the Points, and of u; each read returns -1, the error of a block file that cannot be read.
  !< Defect: the second pass of read_variables_name used the last line again at the end of the file, so the name of that tag
  !< was counted once more than in the first pass and written past the end of the array of the names (a corrupted heap).
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 12.11.3, item (3) (p. 254): at an end-of-file
  !< condition the input list items become undefined.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), parameter :: cuts(2) = ['Points', 'u     ']
  type(orion_data)   :: w, r(2)
  character(len=8)   :: varnames
  character(len=16)  :: field
  integer(I4P)       :: werr, err
  integer            :: c, i, j, k
  logical            :: done
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, 2
    field = 'cuttag'//trim(str(no_sign=.true., n=c))
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = trim(field)//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    w%block(1)%vars = 1._R8P
    w%vtk%format = 'ascii'
    varnames = 'u p'
    werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath=trim(field), varnames=varnames)
    call cut_after_tag(trim(field)//'_B1.vts', trim(cuts(c)), done)
    call check(werr == 0 .and. done, trim(field)//': written, its block file cut after the start tag of '//trim(cuts(c)))
    r(c)%vtk%format = 'ascii'
    err = vtk_read_structured_multiblock(orion=r(c), vtmpath=trim(field), vtspath='')
    write(*,'(A,I0)') trim(field)//': return value ', err
    call check(err == -1, trim(field)//': refused (return value -1)')
  enddo
  end subroutine cut_after_start_tag

  subroutine cut_after_tag(fname, array, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again up to the end of the line that holds the start tag of the data array named array: done tells
  !< that the tag was found.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, array
  logical,          intent(out) :: done
  character(len=:), allocatable :: text
  integer                       :: u, sz, p, q
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=sz)
  allocate(character(len=sz) :: text)
  read(u) text
  close(u)
  p = index(text, 'Name="'//array//'"')
  if (p == 0) return
  q = index(text(p:), achar(10))
  if (q == 0) return
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) text(1:p+q-1)
  close(u)
  done = .true.
  end subroutine cut_after_tag

  subroutine name_in_single_quotes
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A variable whose Name is in single quotes is read: a field of one block of 2 x 3 x 2 cells with the variables u and p at the
  !< cells, written in the ascii format, gets Name="u" made Name='u' in its block file, and is read back as written.
  !< Defect: get_char read a value in double quotes only, and search compared the Name of each DataArray start tag with the name
  !< looked for in an .and. with the status of get_char: when get_char found none, the Name was not set and was referenced
  !< anyway, and the read stopped the program with a segmentation fault in a release build.
  !< Basis: XML 1.0 (Fifth Edition), 2.3, production [10]: an attribute value is in double or in single quotes; the reader of
  !< VTK 9.7.1, measured, reads a Name in single quotes as one in double quotes. Fortran 2018 (ISO/IEC 1539-1:2018, final draft
  !< J3/18-007r1): 9.7.3.2, paragraph 6 (p. 137), an allocatable actual argument of an INTENT(OUT) allocatable dummy argument
  !< is deallocated when the procedure is invoked; 5.4.10, paragraph 2 (p. 42), an unallocated allocatable variable shall not
  !< be referenced; 10.1.7, paragraph 1 (p. 154), a processor need not evaluate every operand of an expression, so it may
  !< evaluate them all.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  integer(I4P)       :: werr, err
  integer            :: i, j, k
  logical            :: done
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'sqname_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
  do k = 0, 2; do j = 0, 3; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo; enddo; enddo
  do k = 1, 2; do j = 1, 3; do i = 1, 2
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
    w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
  enddo; enddo; enddo
  w%vtk%format = 'ascii'
  varnames = 'u p'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='sqname', varnames=varnames)
  call single_quoted_name('sqname_B1.vts', 'u', done)
  call check(werr == 0 .and. done, 'sqname: written, Name="u" made Name=''u'' in its block file')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='sqname', vtspath='')
  write(*,'(A,I0)') 'sqname: return value ', err
  call check(err == 0, 'sqname: read (return value 0)')
  call check_field(r, w)
  end subroutine name_in_single_quotes

  subroutine single_quoted_name(fname, array, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again with Name="array" made Name='array', the value in single quotes: done tells that it was found.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, array
  logical,          intent(out) :: done
  character(len=:), allocatable :: text
  integer                       :: u, sz, p
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=sz)
  allocate(character(len=sz) :: text)
  read(u) text
  close(u)
  p = index(text, 'Name="'//array//'"')
  if (p == 0) return
  text(p:p+len(array)+6) = 'Name='''//array//''''
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) text
  close(u)
  done = .true.
  end subroutine single_quoted_name

  subroutine attributes_in_single_quotes
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Every attribute that the reader reads is read also with its value in single quotes: a field of one block of 2 x 3 x 2 cells
  !< with the variables u and p at the cells and the time 0.5 gets one attribute in single quotes at a time, in its block file
  !< byte_order (raw format), WholeExtent, Extent, the format, the type and the offset (raw format) of the Points, of u and of the
  !< field data TIME, NumberOfComponents of u, NumberOfTuples of TIME, the Name of the Points and of TIME, and in its .vtm file
  !< the file of its DataSet; each field is read back as written, with the time 0.5.
  !< Defect: get_char and get_int read values in double quotes only, and their values were used also when they had read none:
  !< unallocated strings were referenced and integers without a value were used. A release build stopped with a segmentation
  !< fault (byte_order, the format of u, the format and the type of TIME), the Debug build at an index out of bounds
  !< (NumberOfComponents of u), and offsets without a value read the points or the values of u from another place of the file;
  !< with the guard of these values such files were refused.
  !< Basis: XML 1.0 (Fifth Edition), 2.3, production [10]: an attribute value is in double or in single quotes; the reader of
  !< VTK 9.7.1, measured, reads attributes in single quotes as in double quotes. Fortran 2018 (ISO/IEC 1539-1:2018, final draft
  !< J3/18-007r1): 8.5.10, paragraph 3 (p. 102), an INTENT(OUT) dummy argument becomes undefined when the procedure is invoked;
  !< 9.7.3.2, paragraph 6 (p. 137), an allocatable actual argument of an INTENT(OUT) allocatable dummy argument is deallocated
  !< then; 5.4.10, paragraph 2 (p. 42), an unallocated allocatable variable shall not be referenced.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, parameter :: ncase = 17
  character(len=18), parameter :: attr(ncase) = [character(len=18) :: 'byte_order', 'WholeExtent', 'Extent', 'format', &
       'type', 'offset', 'format', 'type', 'NumberOfComponents', 'offset', 'NumberOfTuples', 'type', 'format', 'offset', 'Name', &
       'Name', 'file']
  character(len=15), parameter :: anchor(ncase) = [character(len=15) :: '<VTKFile', '<StructuredGrid', '<Piece', &
       'Name="Points"', 'Name="Points"', 'Name="Points"', 'Name="u"', 'Name="u"', 'Name="u"', 'Name="u"', 'Name="TIME"', &
       'Name="TIME"', 'Name="TIME"', 'Name="TIME"', 'Name="TIME"', 'Name="Points"', '<DataSet']
  character(len=6), parameter :: fmts(ncase) = [character(len=6) :: 'raw', 'ascii', 'ascii', 'ascii', 'ascii', 'raw', &
       'ascii', 'ascii', 'ascii', 'raw', 'ascii', 'ascii', 'ascii', 'raw', 'ascii', 'ascii', 'ascii']
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  character(len=16)  :: field
  character(len=40)  :: what
  integer(I4P)       :: werr, err
  real(R8P)          :: t
  integer            :: c, i, j, k
  logical            :: done
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, ncase
    field = 'sqattr'//trim(str(no_sign=.true., n=c))
    what = trim(attr(c))//' in '//trim(anchor(c))//', '//trim(fmts(c))
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = trim(field)//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
      w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
    enddo; enddo; enddo
    w%vtk%format = trim(fmts(c))
    varnames = 'u p'
    werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath=trim(field), varnames=varnames, time=0.5_R8P)
    if (trim(attr(c)) == 'file') then
      call quoted_attribute(trim(field)//'.vtm', trim(anchor(c)), trim(attr(c)), done)
    else
      call quoted_attribute(trim(field)//'_B1.vts', trim(anchor(c)), trim(attr(c)), done)
    endif
    call check(werr == 0 .and. done, trim(field)//': written, '//trim(what)//' in single quotes')
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = trim(fmts(c))
    t = -7._R8P
    err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(field), vtspath='', time=t)
    write(*,'(A,I0,A,ES12.4)') trim(field)//' ('//trim(what)//'): return value ', err, ', time ', t
    call check(err == 0 .and. t == 0.5_R8P, trim(field)//': read as written, with the time 0.5')
    call check_field(r, w)
  enddo
  end subroutine attributes_in_single_quotes

  subroutine quoted_attribute(fname, anchor, attr, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again with the value of the attribute attr in single quotes in the start tag that holds the text
  !< anchor: done tells that it was found.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, anchor, attr
  logical,          intent(out) :: done
  character(len=:), allocatable :: text
  integer                       :: u, sz, p, s, e, q, a, b
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=sz)
  allocate(character(len=sz) :: text)
  read(u) text
  close(u)
  p = index(text, anchor)
  if (p == 0) return
  s = index(text(1:p), '<', back=.true.)
  e = p - 1 + index(text(p:), '>')
  if (s == 0 .or. e < p) return
  q = index(text(s:e), ' '//attr//'="')
  if (q == 0) return
  a = s + q + len(attr) + 1
  b = a + index(text(a+1:e), '"')
  if (b == a) return
  text(a:a) = "'"
  text(b:b) = "'"
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) text
  close(u)
  done = .true.
  end subroutine quoted_attribute

  subroutine scalars_without_components
  !---------------------------------------------------------------------------------------------------------------------------------
  !< An array without NumberOfComponents has one component: fields of one block of 2 x 3 x 2 cells with the variables u and p at
  !< the cells, written in the ascii, binary and raw formats, get NumberOfComponents="1" taken out of the start tags of u and p in
  !< their block files, as the writer of VTK 9.7.1 writes an array of one component, and are read back as written.
  !< Defect: the reader used the number of components also when the start tag did not give it: the count had no value (the
  !< values were read wrong with the return value 0, or the Debug build stopped at an index out of bounds), and with the guard
  !< of the attributes the file was refused.
  !< Basis: the reader of VTK 9.7.1 (vtkXMLStructuredGridReader), measured: it reads an array without NumberOfComponents as one
  !< component, with its values, in the ascii and raw formats; its writer leaves NumberOfComponents out for an array of one
  !< component.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), parameter :: fmts(3) = ['ascii ', 'binary', 'raw   ']
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  character(len=16)  :: field
  integer(I4P)       :: werr, err
  integer            :: f, i, j, k, n
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, 3
    field = 'nocomp_'//trim(fmts(f))
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = trim(field)//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
      w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
    enddo; enddo; enddo
    w%vtk%format = trim(fmts(f))
    varnames = 'u p'
    werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath=trim(field), varnames=varnames)
    call remove_text(trim(field)//'_B1.vts', ' NumberOfComponents="1"', n)
    call check(werr == 0 .and. n == 2, trim(field)//': written, NumberOfComponents taken out of u and p')
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = trim(fmts(f))
    err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(field), vtspath='')
    write(*,'(A,I0)') trim(field)//': return value ', err
    call check(err == 0, trim(field)//': read without error')
    call check_field(r, w)
  enddo
  end subroutine scalars_without_components

  subroutine remove_text(fname, text, n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again without every occurrence of text: n tells how many were taken out.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, text
  integer,          intent(out) :: n
  character(len=:), allocatable :: s, t
  integer                       :: u, sz, p
  !---------------------------------------------------------------------------------------------------------------------------------

  n = 0
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=sz)
  allocate(character(len=sz) :: s)
  read(u) s
  close(u)
  t = ''
  do
    p = index(s, text)
    if (p == 0) exit
    t = t//s(1:p-1)
    s = s(p+len(text):)
    n = n + 1
  enddo
  t = t//s
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) t
  close(u)
  end subroutine remove_text

  subroutine vector_variables_refused
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A variable of more than one component is refused: block files of one cell, in the layout of ORION, with the variable a of
  !< three components at the cells, its NumberOfComponents in double and in single quotes, and with white space around its
  !< equal sign, and the variable b of one; each read returns -1, the error of a block file that cannot be read.
  !< Defect: the reader put the first values of such an array in the field, one per cell, with the return value 0 (and took
  !< NumberOfComponents in single quotes, or with white space around its equal sign, for a missing one, so 1).
  !< Basis: the field of ORION holds one value per variable and cell (vars(:,i,j,k)); the reader of VTK 9.7.1, measured, reads
  !< such an array as one of three components, with NumberOfComponents in double or in single quotes.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=1), parameter :: quotes(3) = ['"', "'", '"']
  type(orion_data)   :: r
  character(len=16)  :: field
  integer(I4P)       :: err
  integer            :: c
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, 3
    field = 'vector'//trim(str(no_sign=.true., n=c))
    call vector_cell_data_file(trim(field), quotes(c), c == 3)
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = 'ascii'
    err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(field), vtspath='')
    write(*,'(A,I0)') trim(field)//': return value ', err
    call check(err == -1, trim(field)//': a variable of three components refused (return value -1)')
  enddo
  end subroutine vector_variables_refused

  subroutine vector_cell_data_file(stem, quote, blanks)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write stem.vtm and its block file stem_P1.vts, one cell and its 8 nodes, with the variable a of three components at the
  !< cell, its NumberOfComponents in the quotes quote (with white space around its equal sign when blanks is true), and the
  !< variable b of one.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: stem
  character(len=1), intent(in) :: quote
  logical,          intent(in) :: blanks
  character(len=:), allocatable :: eq
  integer                      :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  eq = '='
  if (blanks) eq = ' = '
  open(newunit=u, file=stem//'_P1.vts', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '    <Piece Extent="+0 +1 +0 +1 +0 +1">'
  write(u,'(A)') '      <Points>'
  write(u,'(A)') '        <DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
  write(u,'(A)') '          0 0 1 1 0 1 0 1 1 1 1 1 0 0 2 1 0 2 0 1 2 1 1 2'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '      </Points>'
  write(u,'(A)') '      <CellData>'
  write(u,'(A)') '        <DataArray type="Float64" Name="a" NumberOfComponents'//eq//quote//'3'//quote//' format="ascii">'
  write(u,'(A)') '          1 2 3'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '        <DataArray type="Float64" Name="b" NumberOfComponents="1" format="ascii">'
  write(u,'(A)') '          4'
  write(u,'(A)') '        </DataArray>'
  write(u,'(A)') '      </CellData>'
  write(u,'(A)') '    </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  open(newunit=u, file=stem//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="'//stem//'_P1.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  end subroutine vector_cell_data_file

  subroutine attribute_blanks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< An attribute with white space around its equal sign is read: a field of one block of 2 x 3 x 2 cells with the variables u
  !< and p at the cells, written in the ascii format, gets Name="u" made Name = 'u', and NumberOfComponents="1" made
  !< NumberOfComponents = '1' for u and p, in its block file, and is read back as written.
  !< Defect: get_char and get_int took the value right after the equal sign only: such an attribute was taken as missing.
  !< Basis: XML 1.0 (Fifth Edition), 2.8, production [25]: Eq ::= S? '=' S?; 2.3, production [10]: the value in double or in
  !< single quotes.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  integer(I4P)       :: werr, err
  integer            :: i, j, k, n1, n2
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'blanks_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
  do k = 0, 2; do j = 0, 3; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo; enddo; enddo
  do k = 1, 2; do j = 1, 3; do i = 1, 2
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
    w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
  enddo; enddo; enddo
  w%vtk%format = 'ascii'
  varnames = 'u p'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='blanks', varnames=varnames)
  call replace_text('blanks_B1.vts', 'Name="u"', "Name = 'u'", n1)
  call replace_text('blanks_B1.vts', 'NumberOfComponents="1"', "NumberOfComponents = '1'", n2)
  call check(werr == 0 .and. n1 == 1 .and. n2 == 2, 'blanks: written, with white space around the equal signs of u and p')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='blanks', vtspath='')
  write(*,'(A,I0)') 'blanks: return value ', err
  call check(err == 0, 'blanks: read (return value 0)')
  call check_field(r, w)
  end subroutine attribute_blanks

  subroutine replace_text(fname, old, new, n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again with every occurrence of old made new: n tells how many.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, old, new
  integer,          intent(out) :: n
  character(len=:), allocatable :: s, t
  integer                       :: u, sz, p
  !---------------------------------------------------------------------------------------------------------------------------------

  n = 0
  open(newunit=u, file=fname, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=sz)
  allocate(character(len=sz) :: s)
  read(u) s
  close(u)
  t = ''
  do
    p = index(s, old)
    if (p == 0) exit
    t = t//s(1:p-1)//new
    s = s(p+len(old):)
    n = n + 1
  enddo
  t = t//s
  open(newunit=u, file=fname, access='stream', form='unformatted', status='replace', action='write')
  write(u) t
  close(u)
  end subroutine replace_text

  subroutine names_holding_attributes
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The text of a value in quotes is not taken for attributes, and every array has a Name that can be read: a block file of one
  !< cell with the variable a of three components, its Name 'a NumberOfComponents="1"' before its NumberOfComponents="3", is refused
  !< (return value -1); a field of one block of 2 x 3 x 2 cells with the variables u and p at the cells, written in the raw format,
  !< its Name of u made 'x format="ascii"' before its format="appended", is read back as written, as is the same field in the ascii
  !< format with an attribute whose value 'x Name="p"' stands before the Name="u" of u; the same field with Name="" for u (binary
  !< format), without the Name of u or without that of the Points (ascii format), and a field of two such blocks without the Name of
  !< u or without that of the Points in its second block file, are refused (return value -1).
  !< Defect, measured before this change: a release build stopped with a segmentation fault on the block file of a, on the field
  !< with format="ascii" in the Name of u, on Name="" for u in the binary format and on the fields without the Name of u; the Debug
  !< build refused the block file of a and the field with format="ascii" in the Name of u (return value -1), read the field with
  !< Name="" for u with the return value 0 and values other than those written in the binary format (-1 in the ascii format, 0 and
  !< the values written in the raw format), and the field of one block without the Name of u without u (return value 0). In both
  !< builds the field with Name="p" in the value of another attribute was read with values other than those written (return value
  !< 0), and the fields without the Name of the Points were refused with the return value 1, as files that list an array before
  !< their Points.
  !< Basis: XML 1.0 (Fifth Edition), 3.1, productions [40] and [41]: a start tag is its name and its attributes, each after a white
  !< space, a name, an equal sign and a value in quotes (2.3, production [10]): the text of a value is not an attribute. The reader
  !< finds the arrays of every block file by the names of the arrays of the first one: an array without a Name that can be read
  !< cannot be found.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r, w2
  character(len=8)   :: varnames
  integer(I4P)       :: werr, err
  integer            :: i, j, k, n
  !---------------------------------------------------------------------------------------------------------------------------------

  call vector_cell_data_file('holdcomp', '"', .false.)
  call replace_text('holdcomp_P1.vts', 'Name="a"', "Name='a NumberOfComponents="//'"1"'//"'", n)
  call check(n == 1, 'holdcomp: written, the Name of a holding NumberOfComponents="1"')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='holdcomp', vtspath='')
  write(*,'(A,I0)') 'holdcomp: return value ', err
  call check(err == -1, 'holdcomp: the variable of three components refused (return value -1)')

  allocate(w%block(1))
  w%block(1)%name = 'holdfmt_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
  do k = 0, 2; do j = 0, 3; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo; enddo; enddo
  do k = 1, 2; do j = 1, 3; do i = 1, 2
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
    w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
  enddo; enddo; enddo
  w%vtk%format = 'raw'
  varnames = 'u p'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='holdfmt', varnames=varnames)
  call replace_text('holdfmt_B1.vts', 'Name="u"', "Name='x format="//'"ascii"'//"'", n)
  call check(werr == 0 .and. n == 1, 'holdfmt: written, the Name of u holding format="ascii"')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'raw'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='holdfmt', vtspath='')
  write(*,'(A,I0)') 'holdfmt: return value ', err
  call check(err == 0, 'holdfmt: read (return value 0)')
  call check_field(r, w)

  ! Another attribute whose value holds Name="p", before the Name of u
  w%vtk%format = 'ascii'
  w%block(1)%name = 'holdname_B1'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='holdname', varnames=varnames)
  call replace_text('holdname_B1.vts', 'Name="u"', "info='x Name="//'"p"'//"' Name="//'"u"', n)
  call check(werr == 0 .and. n == 1, 'holdname: written, an attribute holding Name="p" before the Name of u')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='holdname', vtspath='')
  write(*,'(A,I0)') 'holdname: return value ', err
  call check(err == 0, 'holdname: read (return value 0)')
  call check_field(r, w)

  ! A Name of u that is there and cannot be read, in the binary format
  w%vtk%format = 'binary'
  w%block(1)%name = 'emptyname_B1'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='emptyname', varnames=varnames)
  call replace_text('emptyname_B1.vts', 'Name="u"', 'Name=""', n)
  call check(werr == 0 .and. n == 1, 'emptyname: written, Name="" for u')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'binary'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='emptyname', vtspath='')
  write(*,'(A,I0)') 'emptyname: return value ', err
  call check(err == -1, 'emptyname: refused (return value -1)')

  ! Without the Name of u; without that of the Points
  w%vtk%format = 'ascii'
  w%block(1)%name = 'noname_B1'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='noname', varnames=varnames)
  call replace_text('noname_B1.vts', ' Name="u"', '', n)
  call check(werr == 0 .and. n == 1, 'noname: written, u without its Name')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='noname', vtspath='')
  write(*,'(A,I0)') 'noname: return value ', err
  call check(err == -1, 'noname: refused (return value -1)')
  w%block(1)%name = 'nopointsname_B1'
  werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='nopointsname', varnames=varnames)
  call replace_text('nopointsname_B1.vts', ' Name="Points"', '', n)
  call check(werr == 0 .and. n == 1, 'nopointsname: written, the Points without their Name')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='nopointsname', vtspath='')
  write(*,'(A,I0)') 'nopointsname: return value ', err
  call check(err == -1, 'nopointsname: refused (return value -1)')

  ! The same in the second block file of a field of two blocks
  allocate(w2%block(2))
  w2%block(1) = w%block(1)
  w2%block(2) = w%block(1)
  w2%vtk%format = 'ascii'
  w2%block(1)%name = 'noname2_B1'
  w2%block(2)%name = 'noname2_B2'
  werr = vtk_write_structured_multiblock(orion=w2, vtspath='', vtmpath='noname2', varnames=varnames)
  call replace_text('noname2_B2.vts', ' Name="u"', '', n)
  call check(werr == 0 .and. n == 1, 'noname2: written, u without its Name in the second block file')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='noname2', vtspath='')
  write(*,'(A,I0)') 'noname2: return value ', err
  call check(err == -1, 'noname2: refused (return value -1)')
  w2%block(1)%name = 'nopointsname2_B1'
  w2%block(2)%name = 'nopointsname2_B2'
  werr = vtk_write_structured_multiblock(orion=w2, vtspath='', vtmpath='nopointsname2', varnames=varnames)
  call replace_text('nopointsname2_B2.vts', ' Name="Points"', '', n)
  call check(werr == 0 .and. n == 1, 'nopointsname2: written, the Points without their Name in the second block file')
  if (allocated(r%block)) deallocate(r%block)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='nopointsname2', vtspath='')
  write(*,'(A,I0)') 'nopointsname2: return value ', err
  call check(err == -1, 'nopointsname2: refused (return value -1)')
  end subroutine names_holding_attributes

  subroutine tags_holding_gt
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The end of a tag is its first '>' outside the values in quotes of its attributes: a block file of one cell with the variable a
  !< of three components, its start tag over two lines, a value that holds '>' on the first (info="x>y" after its Name, or the
  !< Name "a>c") and NumberOfComponents="3" on the second, is refused as a tag split over lines (return value 1); fields of one
  !< block of 2 x 3 x 2 cells with the variables u and p at the cells, in the ascii, binary and raw formats, with
  !< <FieldData a="x>y"/> before their Piece, are read back as written.
  !< Defect, measured before this change (the release and the Debug build alike): unclosed_tag and empty_element took the first
  !< '>' after the '<' of a tag for its end, also inside a value: the split start tag was not seen as split, and the read was
  !< refused with the return value -1, without the message of a split tag; the empty FieldData element was taken to stay open,
  !< and the field was read without its variables, with the return value 0.
  !< Basis: XML 1.0 (Fifth Edition), 2.3, production [10]: an attribute value may hold '>'; 3.1, productions [40] and [44]: a
  !< start tag and an empty-element tag end with the '>' (or '/>') that follows their name and attributes.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), parameter :: fmts(3) = ['ascii ', 'binary', 'raw   ']
  character(len=*), parameter :: stems(2) = ['gtsplit1', 'gtsplit2']
  character(len=36), parameter :: firsts(2) = [character(len=36) :: 'Name="a" info="x>y" format="ascii"', &
                                               'Name="a>c" format="ascii"']
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  integer(I4P)       :: werr, err
  integer            :: c, f, i, j, k, n
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, 2
    call vector_cell_data_file(trim(stems(c)), '"', .false.)
    call replace_text(trim(stems(c))//'_P1.vts', 'Name="a" NumberOfComponents="3" format="ascii">', &
                      trim(firsts(c))//achar(10)//'          NumberOfComponents="3">', n)
    call check(n == 1, trim(stems(c))//': written, the start tag of a over two lines, with > in a value of the first')
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = 'ascii'
    err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(stems(c)), vtspath='')
    write(*,'(A,I0)') trim(stems(c))//': return value ', err
    call check(err == 1, trim(stems(c))//': refused as a tag split over lines (return value 1)')
  enddo

  do f = 1, 3
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = 'gtfield_'//trim(fmts(f))//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
      w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
    enddo; enddo; enddo
    w%vtk%format = trim(fmts(f))
    varnames = 'u p'
    werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='gtfield_'//trim(fmts(f)), varnames=varnames)
    call replace_text('gtfield_'//trim(fmts(f))//'_B1.vts', '<Piece ', '<FieldData a="x>y"/>'//achar(10)//'<Piece ', n)
    call check(werr == 0 .and. n == 1, 'gtfield_'//trim(fmts(f))//': written, <FieldData a="x>y"/> before the Piece')
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = trim(fmts(f))
    err = vtk_read_structured_multiblock(orion=r, vtmpath='gtfield_'//trim(fmts(f)), vtspath='')
    write(*,'(A,I0)') 'gtfield_'//trim(fmts(f))//': return value ', err
    call check(err == 0, 'gtfield_'//trim(fmts(f))//': read (return value 0)')
    call check_field(r, w)
  enddo
  end subroutine tags_holding_gt

  subroutine byte_order_values
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The byte order of a raw block file is its whole value: a field of one block of 2 x 3 x 2 cells with the variables u and p at
  !< the cells, written in the raw format on a little-endian machine (byte_order="LittleEndian"), with byte_order made
  !< "NotLittleEndian", "LittleEndianX" or "xBigEndianLittleEndian" is refused (return value -1); with "littleendian" it is read
  !< back as written (the case of the letters is not looked at).
  !< Defect, measured before this change (the release and the Debug build alike): the value was matched as a substring, and the
  !< three values were read as little endian, with the return value 0.
  !< Basis: the order of the raw values is known only for the two values that the writers of ORION write (Lib_VTK.f90,
  !< VTK_INI_XML) and that the writer of VTK 9.7.1 writes (measured): LittleEndian and BigEndian.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=22), parameter :: values(4) = [character(len=22) :: 'NotLittleEndian', 'LittleEndianX', &
                                               'xBigEndianLittleEndian', 'littleendian']
  integer(I4P),      parameter :: expected(4) = [-1_I4P, -1_I4P, -1_I4P, 0_I4P]
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  character(len=16)  :: field
  integer(I4P)       :: werr, err
  integer            :: c, i, j, k, n
  !---------------------------------------------------------------------------------------------------------------------------------

  do c = 1, 4
    field = 'byteorder'//trim(str(no_sign=.true., n=c))
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(1))
    w%block(1)%name = trim(field)//'_B1'
    w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
    allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
    do k = 0, 2; do j = 0, 3; do i = 0, 2
      w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
    enddo; enddo; enddo
    do k = 1, 2; do j = 1, 3; do i = 1, 2
      w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
      w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
    enddo; enddo; enddo
    w%vtk%format = 'raw'
    varnames = 'u p'
    werr = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath=trim(field), varnames=varnames)
    call replace_text(trim(field)//'_B1.vts', 'byte_order="LittleEndian"', 'byte_order="'//trim(values(c))//'"', n)
    call check(werr == 0 .and. n == 1, trim(field)//': written, byte_order="'//trim(values(c))//'"')
    if (allocated(r%block)) deallocate(r%block)
    r%vtk%format = 'raw'
    err = vtk_read_structured_multiblock(orion=r, vtmpath=trim(field), vtspath='')
    write(*,'(A,I0)') trim(field)//': return value ', err
    if (expected(c) == 0) then
      call check(err == 0, trim(field)//': read (return value 0)')
      call check_field(r, w)
    else
      call check(err == -1, trim(field)//': refused (return value -1)')
    endif
  enddo
  end subroutine byte_order_values

  subroutine name_in_appended_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The bytes of the raw appended data are not taken for names: a field of one block of 2 x 2 x 2 cells with the variables u and
  !< p at the cells, written in the raw format, one value of u holding the bytes of a line feed and of the text "Name" (so that
  !< a record of the data begins with "Name"), is read back with its two variables, every value as written.
  !< Defect: read_variables_name read a block file to its end, the raw appended data included, as text, and took a record of the
  !< data that begins with "Name" for one more variable, whose values the reader then did not find.
  !< Basis: VTK File Formats (p. 16): the appended data section begins after the underscore inside the AppendedData element,
  !< and "Data in this section is always in binary form".
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  integer(I4P)       :: err
  integer            :: i, j, k
  integer(I1P), parameter :: bytes(8) = [10_I1P, 78_I1P, 97_I1P, 109_I1P, 101_I1P, 32_I1P, 32_I1P, 64_I1P] ! LF "Name  @"
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'appname_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 2; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:2,0:2), w%block(1)%vars(2,1:2,1:2,1:2))
  do k = 0, 2; do j = 0, 2; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo; enddo; enddo
  do k = 1, 2; do j = 1, 2; do i = 1, 2
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
    w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
  enddo; enddo; enddo
  w%block(1)%vars(1,2,1,1) = transfer(bytes, 1._R8P)
  w%vtk%format = 'raw'
  varnames = 'u p'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='appname', varnames=varnames)
  call check(err == 0, 'appname: written')
  r%vtk%format = 'raw'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='appname', vtspath='')
  write(*,'(A,I0)') 'appname: return value ', err
  call check(err == 0, 'appname: read without error')
  call check_field(r, w)
  end subroutine name_in_appended_data

  subroutine name_in_binary_values
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The base64 code of the values of an array (binary format) is not taken for a name: a field of one block of 2 x 3 x 2 cells
  !< with the variables u and p at the cells, written in the binary format, the first value of u holding the bytes 0x35 0xA9
  !< 0x9E where the code turns them into the text "Name" (2.0003026217746083), is read back with its two variables, every value
  !< as written.
  !< Defect: read_variables_name took every line of a block file that holds the four characters Name, outside the FieldData
  !< element, for an array name: the line of the code of u was taken for one more variable, of zeros, between u and p, and the
  !< reader returned 0.
  !< Basis: XML 1.0 (Fifth Edition), 3.1, production [40]: the attributes of an element are in its start tag; the name of a
  !< data array is its Name attribute.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)   :: w, r
  character(len=8)   :: varnames
  character(len=256) :: line
  integer(I4P)       :: err
  integer            :: i, j, k, u, ios
  logical            :: in_data
  integer(I1P), parameter :: bytes(8) = [0_I1P, 0_I1P, 53_I1P, -87_I1P, -98_I1P, 0_I1P, 0_I1P, 64_I1P] ! 0x35 0xA9 0x9E
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'binname_B1'
  w%block(1)%Ni = 2; w%block(1)%Nj = 3; w%block(1)%Nk = 2
  allocate(w%block(1)%mesh(3,0:2,0:3,0:2), w%block(1)%vars(2,1:2,1:3,1:2))
  do k = 0, 2; do j = 0, 3; do i = 0, 2
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo; enddo; enddo
  do k = 1, 2; do j = 1, 3; do i = 1, 2
    w%block(1)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
    w%block(1)%vars(2,i,j,k) = -real(i + j + k, R8P)
  enddo; enddo; enddo
  w%block(1)%vars(1,1,1,1) = transfer(bytes, 1._R8P)
  w%vtk%format = 'binary'
  varnames = 'u p'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='binname', varnames=varnames)
  call check(err == 0, 'binname: written')
  ! The premise: a line of the code of the values, not a tag, holds the text Name
  in_data = .false.
  open(newunit=u, file='binname_B1.vts', status='old', action='read')
  do
    read(u,'(A)',iostat=ios) line
    if (ios /= 0) exit
    if (index(line,'Name') > 0 .and. index(line,'<') == 0) in_data = .true.
  enddo
  close(u)
  call check(in_data, 'binname: the code of the values of u holds the text Name')
  r%vtk%format = 'binary'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='binname', vtspath='')
  write(*,'(A,I0)') 'binname: return value ', err
  call check(err == 0, 'binname: read without error')
  call check_field(r, w)
  end subroutine name_in_binary_values

  subroutine split_start_tag
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A tag split over lines is refused, with a message: two fields of blocks of 2 x 3 x 2 cells with the variables u and p at the
  !< cells, written in the ascii format, get a start tag over two lines (its Name attribute on the second): that of u in the one
  !< block of the first field, and that of p in the second block of the other, past the first array name of that block file.
  !< Each read returns 1; read again in a child run of this program, each gives on the standard error a message with the block
  !< file, the line of the tag and the tag.
  !< Defect: the reader takes the name of a data array from its start tag on one line (array_name): a start tag split over
  !< lines named no array, and the field was read without that array, with the return value 0.
  !< Basis: XML 1.0 (Fifth Edition), W3C Recommendation 26 November 2008, 3.1, production [40], and 2.3, production [3]: the
  !< attributes of a start tag are separated by white space, which holds line breaks; the reader takes each tag on its line.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), parameter :: fields(2) = ['splittag ', 'splittag2']
  character(len=*), parameter :: files(2) = ['splittag_B1.vts ', 'splittag2_B2.vts']
  character(len=1), parameter :: arrays(2) = ['u', 'p']
  type(orion_data)              :: w, r
  character(len=8)              :: varnames
  character(len=:), allocatable :: self
  integer(I4P)                  :: err(2), werr(2)
  integer                       :: f, b, i, j, k, at(2), length, exitstat, cmdstat
  logical                       :: split(2)
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, 2
    if (allocated(w%block)) deallocate(w%block)
    allocate(w%block(f))
    do b = 1, f
      w%block(b)%name = trim(fields(f))//'_B'//trim(str(no_sign=.true., n=b))
      w%block(b)%Ni = 2; w%block(b)%Nj = 3; w%block(b)%Nk = 2
      allocate(w%block(b)%mesh(3,0:2,0:3,0:2), w%block(b)%vars(2,1:2,1:3,1:2))
      do k = 0, 2; do j = 0, 3; do i = 0, 2
        w%block(b)%mesh(:,i,j,k) = [real(i + 2*(b-1), R8P), real(j, R8P), real(k, R8P)]
      enddo; enddo; enddo
      do k = 1, 2; do j = 1, 3; do i = 1, 2
        w%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k, R8P)
        w%block(b)%vars(2,i,j,k) = -real(i + j + k, R8P)
      enddo; enddo; enddo
    enddo
    w%vtk%format = 'ascii'
    varnames = 'u p'
    werr(f) = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath=trim(fields(f)), varnames=varnames)
    call split_tag(trim(files(f)), arrays(f), split(f), at(f))
  enddo
  call check(all(werr == 0), 'splittag: written')
  call check(all(split), 'splittag: the start tag of u, and of p in the second block file, over two lines')
  do f = 1, 2
    r%vtk%format = 'ascii'
    err(f) = vtk_read_structured_multiblock(orion=r, vtmpath=trim(fields(f)), vtspath='')
    write(*,'(A,I0)') trim(fields(f))//': return value ', err(f)
    call check(err(f) == 1, trim(fields(f))//': refused (return value 1)')
  enddo
  ! The messages, from a child run of this program that reads the two fields again
  call get_command_argument(0, length=length)
  allocate(character(len=length) :: self)
  call get_command_argument(0, self)
  call execute_command_line("'"//self//"' split_start_tag > splittag_log.txt 2>&1", exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'splittag: the child run')
  do f = 1, 2
    call check(file_holds('splittag_log.txt', 'VTK READ ERROR: '//trim(files(f))//', line '// &
                          trim(str(no_sign=.true., n=at(f)))//': a tag that does not end on its line') .and. &
               file_holds('splittag_log.txt', trim(files(f))//', line '//trim(str(no_sign=.true., n=at(f)))// &
                          ': a tag that does not end on its line (start tags must be on one line): <DataArray'), &
               trim(fields(f))//': the message gives the block file, the line and the tag')
  enddo
  end subroutine split_start_tag

  subroutine split_tag(fname, array, done, at)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the file fname again with the start tag of the data array named array over two lines, its Name attribute on the
  !< second: done tells that the tag was found, at its line (the first of the two).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: fname, array
  logical,          intent(out) :: done
  integer,          intent(out) :: at
  character(len=512), allocatable :: lines(:)
  character(len=512)              :: line
  integer                         :: u, ios, n, i, p
  !---------------------------------------------------------------------------------------------------------------------------------

  done = .false.; at = 0; n = 0
  open(newunit=u, file=fname, status='old', action='read')
  do
    read(u,'(A)',iostat=ios) line
    if (ios /= 0) exit
    n = n + 1
  enddo
  allocate(lines(n))
  rewind(u)
  do i = 1, n
    read(u,'(A)') lines(i)
  enddo
  close(u)
  open(newunit=u, file=fname, status='replace', action='write')
  do i = 1, n
    p = index(lines(i), ' Name="'//array//'"')
    if (.not.done .and. index(lines(i), '<DataArray') > 0 .and. p > 0) then
      write(u,'(A)') lines(i)(1:p-1)
      write(u,'(A)') '   '//trim(lines(i)(p+1:))
      done = .true.
      at = i
    else
      write(u,'(A)') trim(lines(i))
    endif
  enddo
  close(u)
  end subroutine split_tag

  subroutine split_start_tag_child
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The reads of split_start_tag again, in a child run of this program: the messages of the reader go to its standard error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: r1, r2
  integer(I4P)     :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  r1%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r1, vtmpath='splittag', vtspath='')
  write(*,'(A,I0)') 'splittag (child): return value ', err
  r2%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r2, vtmpath='splittag2', vtspath='')
  write(*,'(A,I0)') 'splittag2 (child): return value ', err
  end subroutine split_start_tag_child

  logical function file_holds(fname, text)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Does a line of the file fname hold text?
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fname, text
  character(len=1024)          :: line
  integer                      :: u, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  file_holds = .false.
  open(newunit=u, file=fname, status='old', action='read', iostat=ios)
  if (ios /= 0) return
  do
    read(u, '(A)', iostat=ios) line
    if (ios /= 0) exit
    if (index(line, text) > 0) then
      file_holds = .true.
      exit
    endif
  enddo
  close(u)
  end function file_holds

  subroutine arrays_before_points
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Block files in the layout of VTK, refused. vtk_read_structured_multiblock takes the arrays of a block file in the order in
  !< which vtk_write_structured_multiblock writes them, the Points first, and refuses a block file that lists its data arrays
  !< before its Points, as VTK writes them, before reading any block: return value 1, the mesh of the block not read. Block
  !< files of one cell written here in ascii as VTK 9.7.1 writes them (PointData, then CellData, then Points; data arrays
  !< without NumberOfComponents): one variable as point data with an empty CellData element; one variable as cell data with
  !< an empty PointData element; one variable as point data and one as cell data. A mesh in the same layout, with empty
  !< PointData and CellData elements, is read as before (return value 0, orion%vtk%node unset, Ni Nj Nk = 1 1 1).
  !< Defect: the reader took the first array of a block file for the Points and the others for its variables: for these
  !< files it read the arrays under the names shifted by one, for an array without NumberOfComponents with an undefined
  !< number of components, and stopped with an error at the last one, with the mesh of the first block read.
  !< Basis: VTK File Formats (p. 13): the structure of a StructuredGrid piece lists PointData, CellData and Points, in this
  !< order, the order in which VTK 9.7.1 writes them; the reader of ORION takes the order of its writer, the Points first.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  call read_vtk_layout('vtklayout_point', .true., .false.)
  call read_vtk_layout('vtklayout_cell', .false., .true.)
  call read_vtk_layout('vtklayout_both', .true., .true.)
  call read_vtk_layout('vtklayout_mesh', .false., .false.)
  call second_block_vtk_layout()
  end subroutine arrays_before_points

  subroutine second_block_vtk_layout
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A field of two blocks whose first block file is in the order of ORION, written by vtk_write_structured_multiblock, and whose
  !< second one is in the layout of VTK, with the same variable c as cell data: refused before any block is read, as a field
  !< whose first block file is in the layout of VTK (return value 1, the mesh of neither block read).
  !< Defect: only the first block file was checked, so the first block was read and the second one with the names of the first.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)  :: w, r
  character(len=8)  :: varnames
  integer(I4P)      :: err
  integer           :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'vtklayout_two_B1'
  w%block(1)%Ni = 1; w%block(1)%Nj = 1; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(3,0:1,0:1,0:1), w%block(1)%vars(1,1:1,1:1,1:1))
  do k = 0, 1; do j = 0, 1; do i = 0, 1
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k + 1, R8P)]
  enddo; enddo; enddo
  w%block(1)%vars = 9._R8P
  w%vtk%format = 'ascii'
  varnames = 'c'
  err = vtk_write_structured_multiblock(orion=w, vtspath='', vtmpath='vtklayout_two_orion', varnames=varnames)
  call check(err == 0, 'vtklayout_two: the first block file written by ORION')
  call write_vtk_layout('vtklayout_two_V2.vts', .false., .true.)
  call write_vtm('vtklayout_two', [character(len=24) :: 'vtklayout_two_B1.vts', 'vtklayout_two_V2.vts'])
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath='vtklayout_two', vtspath='')
  write(*,'(A,I0)') 'vtklayout_two: return value ', err
  call check(err == 1, 'vtklayout_two: a second block file in the layout of VTK refused (return value 1)')
  if (allocated(r%block)) call check(.not.allocated(r%block(1)%mesh) .and. .not.allocated(r%block(2)%mesh), &
                                     'vtklayout_two: no block read')
  end subroutine second_block_vtk_layout

  subroutine read_vtk_layout(stem, point, cell)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write stem.vtm and its block file stem_V1.vts, one cell in the layout of VTK, with a variable p as point data and a variable
  !< c as cell data when point and cell are true, and read it: refused when it has a variable, read when it is a mesh.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: stem
  logical,          intent(in) :: point, cell
  type(orion_data)             :: r
  integer(I4P)                 :: err
  integer                      :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  call write_vtk_layout(stem//'_V1.vts', point, cell)
  open(newunit=u, file=stem//'.vtm', status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="'//stem//'_V1.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath=stem, vtspath='')
  if (point .or. cell) then
    call check(err == 1, stem//': a block file in the layout of VTK refused (return value 1)')
    if (allocated(r%block)) call check(.not.allocated(r%block(1)%mesh), stem//': no block read')
  else
    call check(err == 0, stem//': a mesh in the layout of VTK read (return value 0)')
    if (allocated(r%block)) then
      call check(.not.r%vtk%node .and. r%block(1)%Ni == 1 .and. r%block(1)%Nj == 1 .and. r%block(1)%Nk == 1, &
                 stem//': read with orion%vtk%node unset and Ni, Nj and Nk the cells')
      if (allocated(r%block(1)%mesh)) call check(all(shape(r%block(1)%mesh) == [3, 2, 2, 2]), stem//': its nodes read')
    endif
  endif
  end subroutine read_vtk_layout

  subroutine write_vtk_layout(fname, point, cell, field_line)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write the block file fname, one cell in the layout of VTK, with a variable p as point data and a variable c as cell data
  !< when point and cell are true, and with the line field_line, a FieldData element, after the StructuredGrid start tag when
  !< it is given.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)           :: fname
  logical,          intent(in)           :: point, cell
  character(len=*), intent(in), optional :: field_line
  integer                                :: u
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=fname, status='replace', action='write')
  write(u,'(A)') '<?xml version="1.0"?>'
  write(u,'(A)') '<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian" header_type="UInt32"'// &
                 ' compressor="vtkZLibDataCompressor">'
  write(u,'(A)') '  <StructuredGrid WholeExtent="0 1 0 1 0 1">'
  if (present(field_line)) write(u,'(A)') field_line
  write(u,'(A)') '  <Piece Extent="0 1 0 1 0 1">'
  write(u,'(A)') '    <PointData>'
  if (point) then
    write(u,'(A)') '      <DataArray type="Float64" Name="p" format="ascii" RangeMin="1" RangeMax="8">'
    write(u,'(A)') '        1 2 3 4 5 6 7 8'
    write(u,'(A)') '      </DataArray>'
  endif
  write(u,'(A)') '    </PointData>'
  write(u,'(A)') '    <CellData>'
  if (cell) then
    write(u,'(A)') '      <DataArray type="Float64" Name="c" format="ascii" RangeMin="9" RangeMax="9">'
    write(u,'(A)') '        9'
    write(u,'(A)') '      </DataArray>'
  endif
  write(u,'(A)') '    </CellData>'
  write(u,'(A)') '    <Points>'
  write(u,'(A)') '      <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="ascii" RangeMin="1" RangeMax="2">'
  write(u,'(A)') '        0 0 1 1 0 1 0 1 1 1 1 1 0 0 2 1 0 2 0 1 2 1 1 2'
  write(u,'(A)') '      </DataArray>'
  write(u,'(A)') '    </Points>'
  write(u,'(A)') '  </Piece>'
  write(u,'(A)') '  </StructuredGrid>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  end subroutine write_vtk_layout
  end program vtk_read_multiblock