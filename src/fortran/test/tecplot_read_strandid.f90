!< Solution time and STRANDID of the zones of Tecplot files.
!< tec_write_structured_multiblock writes the solution time with its sign: a negative solutiontime marks a steady
!< solution (minus the iteration count of the run, for instance). The zones of a time-accurate solution belong to the
!< strand of their block: STRANDID = b in the header of zone b of an ASCII file, StrandID b in a binary file. The zones
!< of a steady solution are static: no STRANDID in an ASCII file, StrandID 0 (static) for TecIO. The ASCII reader keeps
!< solutiontime as written, sign included, and stores the STRANDID of the last zone header in orion%strandid, -1 when that
!< header has none; a file whose zone headers hold no SOLUTIONTIME reads with solutiontime = -huge(1._R8P), the most negative
!< real, which no solution has (it gave -10, the time of a steady solution at iteration 10).
!< With TecIO, the .szplt files are read back with the TecIO reader, the solution time and the StrandID of every zone, and
!< with tec_read_structured_multiblock, with and without dims_only: the time and the StrandID of the last zone, -1 for a
!< static zone (StrandID 0 for TecIO), as the ASCII reader gives them.
!< A file without SOLUTIONTIME, read and written again, reads back with the time of the first read and static zones.
!< Defect: the writer wrote the absolute value of the time, with STRANDID = 0 for a steady solution, which the Tecplot
!< Data Format Guide does not allow in an ASCII file (STRANDID >= 1) and which marks a strand, not a static zone, in a
!< binary file; a time-accurate solution had no strand, and the binary files had StrandID 0 for every zone and the
!< absolute value of the time, so they did not mark a steady solution. A file without SOLUTIONTIME, read with -10, was
!< so written again with SOLUTIONTIME = 10 and STRANDID = 0, and read back as a strand at time 10.
!< Basis: Tecplot 360 EX 2023 R1 Data Format Guide: ASCII zone record (p. 124), "StrandID's must be positive integer
!< values greater than or equal to 1. By convention strandID's are successive integer values", SOLUTIONTIME a floating
!< point value; TECZNE142 (p. 61), StrandID "0 = static zone, not associated with a strand. Values greater than 0
!< indicate a zone is assigned to a given strand"; binary .plt file (p. 185), "-1 = static strand ID, >=0 valid strand ID".
!< The step number and the named scalars of a field (field_scalars), as the data set auxiliary data of the file:
!< tec_write_structured_multiblock writes cycle as CYCLE and each of fldnames with its value, and
!< tec_read_structured_multiblock returns them, in an ASCII file and, with TecIO, a .szplt file (also with dims_only); a
!< file written without them reads back with cycle 0 and no scalar found; names and values of different sizes, and blank
!< names, are refused by both functions (blank_name_read for the reader), and the other names that the auxiliary data
!< cannot hold by the writer. An addition, not a defect: cycle, fldnames, fldvalues and fldfound are new optional
!< arguments, the same as those of the VTK multi-block functions.
!< Basis: Tecplot 360 EX 2023 R1 Data Format Guide: data set auxiliary data record (p. 135), "DATASETAUXDATA name =
!< "value"", "name is a unique character string with no spaces"; TECAUXSTR142 (pp. 29-30); reading auxiliary data with
!< tecDataSetAuxDataGetNumItems and tecDataSetAuxDataGetItem (p. 172).
!< Stops with a non-zero code when a check fails.
program tecplot_read_strandid
  use IR_Precision, only: R8P, I4P
  use Lib_Tecplot
  use Lib_ORION_data
#ifdef TECIO
  use, intrinsic :: iso_c_binding
#endif
  implicit none
#ifdef TECIO
  include "tecio.f90"
#endif
  integer :: nfail, ncase

  nfail = 0
  ncase = 0
  call round_trip(-5._R8P,  -1, 'steady_strandid.tec')
  call round_trip(0.25_R8P,  2, 'unsteady_strandid.tec')
  call handwritten(', SOLUTIONTIME=5, STRANDID = 0', '', 5._R8P, 0, 'STRANDID = 0')
  call handwritten(', SOLUTIONTIME=5, STRANDID=1', '', 5._R8P, 1, 'STRANDID = 1')
  call handwritten(', SOLUTIONTIME=-40', '', -40._R8P, -1, 'a negative SOLUTIONTIME, no STRANDID')
  call handwritten(', SOLUTIONTIME=5, STRANDID = 0', ', SOLUTIONTIME=7', 7._R8P, -1, &
                   'two zones, STRANDID only on the first: the last zone counts')
  call handwritten('', '', -huge(1._R8P), -1, 'neither SOLUTIONTIME nor STRANDID: the most negative real')
  call handwritten(', SOLUTIONTIME=-10', '', -10._R8P, -1, 'SOLUTIONTIME = -10, a steady solution at iteration 10')
#ifdef TECIO
  call binary(-5._R8P, 'steady_strandid.szplt')
  call binary(0.25_R8P, 'unsteady_strandid.szplt')
#endif
  call no_time_rewritten(-huge(1._R8P))
  call field_scalars('ascii', 'scalars.tec')
  call no_scalars('ascii', 'no_scalars.tec')
  call scalars_refused()
  call blank_name_read()
#ifdef TECIO
  call field_scalars('binary', 'scalars.szplt')
  call no_scalars('binary', 'no_scalars.szplt')
#endif
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_strandid: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_strandid: all checks passed'

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

  ! Two zones of a two-dimensional field with one variable at the cells and the given solutiontime.
  subroutine field(time, written)
    real(R8P),        intent(in)  :: time
    type(orion_data), intent(out) :: written
    integer :: b, i, j

    written%tec%node = .false.
    written%tec%bc = .false.
    written%solutiontime = time
    allocate(written%block(1:2))
    do b = 1, 2
      written%block(b)%name = 'zone'
      allocate(written%block(b)%mesh(1:2,0:4,0:3,0:0))
      allocate(written%block(b)%vars(1:1,1:4,1:3,1:1))
      do j = 0, 3
        do i = 0, 4
          written%block(b)%mesh(1,i,j,0) = real(i + 4*(b-1), R8P)
          written%block(b)%mesh(2,i,j,0) = real(j, R8P)
        enddo
      enddo
      written%block(b)%vars = real(b, R8P)
    enddo
  end subroutine field

  ! The two zones of field written to an ASCII file with tec_write_structured_multiblock and read back: the zone headers
  ! hold the signed time, and STRANDID = b for a time-accurate solution, none for a steady one; the reader gives back the
  ! time and the STRANDID of the last zone header.
  subroutine round_trip(time, strand_read, filename)
    real(R8P),        intent(in) :: time
    integer,          intent(in) :: strand_read
    character(len=*), intent(in) :: filename
    type(orion_data)    :: written, read_back
    integer             :: err, u, ios, nzones
    character(len=32)   :: t, s
    character(len=1024) :: line
    logical             :: strands_right

    call field(time, written)
    written%tec%format = 'ascii'
    write(t,'(ES12.4)') time
    write(*,'(A)') filename//': two zones written with solutiontime '//trim(adjustl(t))
    err = tec_write_structured_multiblock(orion=written, varnames='v', filename=filename, Nvars=1)
    call check(err == 0, 'written without error')
    if (err /= 0) return
    ! the zone headers: STRANDID = b in zone b of a time-accurate solution, no STRANDID in those of a steady one
    strands_right = .true.
    nzones = 0
    open(newunit=u, file=filename, status='old', action='read')
    do
      read(u,'(A)',iostat=ios) line
      if (ios /= 0) exit
      if (index(line, 'ZONE') == 0) cycle
      nzones = nzones + 1
      write(s,'(A,I0)') 'STRANDID=', nzones
      if (time >= 0._R8P) then
        strands_right = strands_right .and. index(line, trim(s)) > 0
      else
        strands_right = strands_right .and. index(line, 'STRANDID') == 0
      endif
    enddo
    close(u)
    call check(nzones == 2, 'two zone headers')
    if (time >= 0._R8P) then
      call check(strands_right, 'STRANDID = b in the header of zone b (the strand of the block)')
    else
      call check(strands_right, 'no STRANDID in the zone headers (static zones)')
    endif
    read_back%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=read_back, filename=filename)
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(read_back%block) == 2, 'two zones read')
    write(t,'(ES12.4)') read_back%solutiontime
    call check(read_back%solutiontime == time, 'solutiontime read back '//trim(adjustl(t))//', sign included')
    write(t,'(I0)') read_back%strandid
    call check(read_back%strandid == strand_read, 'strandid '//trim(t))
  end subroutine round_trip

  ! A file written by hand: one zone, or two when tail2 is not empty, whose headers end with tail1 and tail2.
  subroutine handwritten(tail1, tail2, time_read, strand_read, what)
    character(len=*), intent(in) :: tail1, tail2, what
    real(R8P),        intent(in) :: time_read
    integer,          intent(in) :: strand_read
    type(orion_data)  :: d
    integer           :: u, err, nzones
    character(len=32) :: filename, t

    ncase = ncase + 1
    write(filename,'(A,I0,A)') 'strandid_case_', ncase, '.tec'
    open(newunit=u, file=trim(filename), status='replace')
    write(u,'(A)') ' VARIABLES = "x" "y" "v"'
    write(u,'(A)') ' ZONE  T = a, I=2, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'//tail1
    write(u,'(A)') ' 0.0 1.0 0.0 1.0'
    write(u,'(A)') ' 0.0 0.0 1.0 1.0'
    write(u,'(A)') ' 1.0'
    nzones = 1
    if (len_trim(tail2) > 0) then
      write(u,'(A)') ' ZONE  T = b, I=2, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'//tail2
      write(u,'(A)') ' 1.0 2.0 1.0 2.0'
      write(u,'(A)') ' 0.0 0.0 1.0 1.0'
      write(u,'(A)') ' 2.0'
      nzones = 2
    endif
    close(u)
    write(*,'(A)') trim(filename)//': '//what
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename=trim(filename))
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block) == nzones, 'every zone read')
    write(t,'(ES12.4)') d%solutiontime
    call check(d%solutiontime == time_read, 'solutiontime '//trim(adjustl(t)))
    write(t,'(I0)') d%strandid
    call check(d%strandid == strand_read, 'strandid '//trim(t))
  end subroutine handwritten
#ifdef TECIO

  ! The two zones of field written to a .szplt file with tec_write_structured_multiblock and read with the TecIO reader:
  ! every zone has the signed time, and StrandID b for a time-accurate solution, 0 (static) for a steady one. Then read with
  ! tec_read_structured_multiblock, with and without dims_only: the time of the last zone and its StrandID, 2, or -1 for a
  ! static zone.
  subroutine binary(time, filename)
    real(R8P),        intent(in) :: time
    character(len=*), intent(in) :: filename
    type(orion_data)   :: written, read_back, headers
    type(c_ptr)        :: h
    integer            :: err
    integer(c_int32_t) :: e, nzones, z, strand
    real(c_double)     :: t
    logical            :: times_right, strands_right
    character(len=32)  :: ts

    call field(time, written)
    written%tec%format = 'binary'
    write(ts,'(ES12.4)') time
    write(*,'(A)') filename//': two zones written with solutiontime '//trim(adjustl(ts))
    err = tec_write_structured_multiblock(orion=written, varnames='v', filename=filename, Nvars=1)
    call check(err == 0, 'written without error')
    h = c_null_ptr
    e = tecFileReaderOpen(filename//c_null_char, h)
    call check(e == 0, 'opened by the TecIO reader')
    if (e /= 0) return
    e = tecDataSetGetNumZones(h, nzones)
    call check(e == 0 .and. nzones == 2, 'two zones')
    times_right = .true.
    strands_right = .true.
    do z = 1, nzones
      e = tecZoneGetSolutionTime(h, z, t)
      times_right = times_right .and. e == 0 .and. t == time
      e = tecZoneGetStrandID(h, z, strand)
      strands_right = strands_right .and. e == 0 .and. strand == merge(z, 0_c_int32_t, time >= 0._R8P)
    enddo
    e = tecFileReaderClose(h)
    call check(times_right, 'the solution time of every zone, sign included')
    if (time >= 0._R8P) then
      call check(strands_right, 'StrandID b for zone b (the strand of the block)')
    else
      call check(strands_right, 'StrandID 0 for every zone (static zones)')
    endif
    read_back%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=read_back, filename=filename)
    call check(err == 0, 'read without error')
    write(ts,'(ES12.4)') read_back%solutiontime
    call check(read_back%solutiontime == time, 'solutiontime read back '//trim(adjustl(ts))//', sign included')
    write(ts,'(I0)') read_back%strandid
    call check(read_back%strandid == merge(2, -1, time >= 0._R8P), 'strandid '//trim(ts))
    headers%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=headers, filename=filename, dims_only=.true.)
    call check(err == 0, 'zone headers read without error (dims_only)')
    call check(headers%solutiontime == time .and. headers%strandid == merge(2, -1, time >= 0._R8P), &
               'solutiontime and strandid from the zone headers (dims_only)')
  end subroutine binary
#endif

  ! A file whose zone header holds no SOLUTIONTIME, read and written again with tec_write_structured_multiblock: the zone
  ! header of the ASCII file written has no STRANDID (a static zone), and the file reads back with the time of the first
  ! read, sign included, and strandid -1; with TecIO the time read is written to a .szplt file too (binary). Before, the
  ! writer wrote the absolute value of the time with STRANDID = 0: the file read with -10 was written with SOLUTIONTIME = 10
  ! and STRANDID = 0, and read back with the time 10 of a strand.
  subroutine no_time_rewritten(time_read)
    real(R8P), intent(in) :: time_read
    type(orion_data)    :: d, again
    integer             :: u, err, ios
    character(len=1024) :: line
    logical             :: no_strand
    character(len=32)   :: t

    write(*,'(A)') 'no_time.tec: a file without SOLUTIONTIME, read and written again to no_time_rewritten.tec'
    open(newunit=u, file='no_time.tec', status='replace')
    write(u,'(A)') ' VARIABLES = "x" "y" "v"'
    write(u,'(A)') ' ZONE  T = a, I=2, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'
    write(u,'(A)') ' 0.0 1.0 0.0 1.0'
    write(u,'(A)') ' 0.0 0.0 1.0 1.0'
    write(u,'(A)') ' 1.0'
    close(u)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='no_time.tec')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    write(t,'(ES12.4)') d%solutiontime
    call check(d%solutiontime == time_read, 'solutiontime '//trim(adjustl(t)))
    err = tec_write_structured_multiblock(orion=d, varnames='v', filename='no_time_rewritten.tec', Nvars=1)
    call check(err == 0, 'written again without error')
    if (err /= 0) return
    no_strand = .true.
    open(newunit=u, file='no_time_rewritten.tec', status='old', action='read')
    do
      read(u,'(A)',iostat=ios) line
      if (ios /= 0) exit
      if (index(line, 'ZONE') > 0) no_strand = no_strand .and. index(line, 'STRANDID') == 0
    enddo
    close(u)
    call check(no_strand, 'no STRANDID in the zone header written (a static zone)')
    again%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=again, filename='no_time_rewritten.tec')
    call check(err == 0, 'read again without error')
    if (err /= 0) return
    write(t,'(ES12.4)') again%solutiontime
    call check(again%solutiontime == d%solutiontime, 'solutiontime read again '//trim(adjustl(t))//', sign included')
    write(t,'(I0)') again%strandid
    call check(again%strandid == -1, 'strandid '//trim(t))
#ifdef TECIO
    call binary(d%solutiontime, 'no_time_rewritten.szplt')
#endif
  end subroutine no_time_rewritten

  ! The field written in the format fmt with cycle 7 and three named scalars, and read back with them, a name asked in another
  ! case and one that the file does not hold; with TecIO the zone headers of the .szplt file are read with dims_only too. In
  ! an ASCII file the scalars are DATASETAUXDATA records after the zones.
  subroutine field_scalars(fmt, filename)
    character(len=*), intent(in) :: fmt, filename
    type(orion_data)    :: w, r, h
    character(len=20)   :: names(3), asked(4)
    real(R8P)           :: values(3), got(4)
    logical             :: found(4)
    integer(I4P)        :: n
    integer             :: err, u, ios, nrec
    character(len=1024) :: line
    logical             :: after_zones

    write(*,'(A)') filename//': written with cycle 7 and three named scalars'
    call field(0.5_R8P, w)
    w%tec%format = fmt
    names = [character(len=20) :: 'ITERATION', 'OUTPUT_NUMBER', 'LAST_OUTPUT_TIME']
    values = [1234._R8P, 56._R8P, 1._R8P/3._R8P]
    err = tec_write_structured_multiblock(orion=w, varnames='v', filename=filename, Nvars=1, cycle=7_I4P, &
                                          fldnames=names, fldvalues=values)
    call check(err == 0, 'written without error')
    if (fmt == 'ascii') then
      ! the records after the last zone: the last lines of the file
      nrec = 0
      after_zones = .true.
      open(newunit=u, file=filename, status='old', action='read')
      do
        read(u,'(A)',iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'DATASETAUXDATA') == 1) then
          nrec = nrec + 1
        elseif (nrec > 0) then
          after_zones = .false.
        endif
      enddo
      close(u)
      call check(nrec == 4 .and. after_zones, 'four DATASETAUXDATA records, after the zones')
    endif
    asked = [character(len=20) :: 'last_output_time', 'NOT_WRITTEN', 'ITERATION', 'OUTPUT_NUMBER']
    n = -1
    got = 12345._R8P
    found = [.false., .true., .false., .false.]
    r%tec%format = fmt
    err = tec_read_structured_multiblock(orion=r, filename=filename, cycle=n, fldnames=asked, fldvalues=got, fldfound=found)
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(r%block) == 2 .and. r%solutiontime == 0.5_R8P, 'the field and its time read back')
    call check(n == 7_I4P, 'the cycle written')
    call check(found(1) .and. (.not.found(2)) .and. found(3) .and. found(4), 'found exactly the names written, in any case')
    call check(got(1) == values(3), 'LAST_OUTPUT_TIME as written, bit for bit')
    call check(got(2) == 0._R8P, 'a name the file does not hold reads as 0')
    call check(got(3) == values(1) .and. got(4) == values(2), 'ITERATION and OUTPUT_NUMBER as written')
    if (fmt == 'binary') then
      n = -1
      got = 12345._R8P
      found = .false.
      h%tec%format = fmt
      err = tec_read_structured_multiblock(orion=h, filename=filename, dims_only=.true., cycle=n, fldnames=asked, &
                                           fldvalues=got, fldfound=found)
      call check(err == 0 .and. n == 7_I4P .and. all(found .eqv. [.true., .false., .true., .true.]) .and. &
                 got(1) == values(3) .and. got(3) == values(1) .and. got(4) == values(2), &
                 'the cycle and the named scalars from the zone headers (dims_only)')
    endif
  end subroutine field_scalars

  ! The field written in the format fmt without cycle and named scalars: read with them, the cycle is 0 and no scalar is found.
  subroutine no_scalars(fmt, filename)
    character(len=*), intent(in) :: fmt, filename
    type(orion_data)  :: w, r
    character(len=20) :: asked(2)
    real(R8P)         :: got(2)
    logical           :: found(2)
    integer(I4P)      :: n
    integer           :: err

    write(*,'(A)') filename//': written without cycle and named scalars'
    call field(0.5_R8P, w)
    w%tec%format = fmt
    err = tec_write_structured_multiblock(orion=w, varnames='v', filename=filename, Nvars=1)
    call check(err == 0, 'written without error')
    asked = [character(len=20) :: 'ITERATION', 'LAST_OUTPUT_TIME']
    n = -1
    got = 12345._R8P
    found = .true.
    r%tec%format = fmt
    err = tec_read_structured_multiblock(orion=r, filename=filename, cycle=n, fldnames=asked, fldvalues=got, fldfound=found)
    call check(err == 0, 'read without error')
    call check(n == 0_I4P .and. .not.any(found) .and. all(got == 0._R8P), 'no cycle and no named scalar: 0, not found')
  end subroutine no_scalars

  ! Names and values that do not go together, and names that the auxiliary data cannot hold (blank, with a blank, an equal
  ! sign or a double quote, given twice, or CYCLE and Time, the items of the writer, in any case), are refused by the
  ! writer, which returns 1 and writes no file; names and values that do not go together by the reader, which returns 1
  ! (a blank name: blank_name_read).
  subroutine scalars_refused()
    type(orion_data)  :: w, r
    real(R8P)         :: got(2)
    logical           :: found(1)
    integer           :: err, u

    write(*,'(A)') 'refused.tec: named scalars that cannot be written'
    ! no file of an earlier run
    open(newunit=u, file='refused.tec')
    close(u, status='delete')
    call field(0.5_R8P, w)
    w%tec%format = 'ascii'
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['a']), 'names without values')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['a', 'b'], fldvalues=[1._R8P]), 'two names, one value')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['a b'], fldvalues=[1._R8P]), 'a name with a blank')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['a=b'], fldvalues=[1._R8P]), 'a name with an equal sign')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['a"b'], fldvalues=[1._R8P]), 'a name with a double quote')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['   '], fldvalues=[1._R8P]), 'a blank name')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['ab', 'AB'], fldvalues=[1._R8P, 2._R8P]), 'a name given twice')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['cycle'], fldvalues=[1._R8P]), 'the name CYCLE')
    call writes_nothing(tec_write_structured_multiblock(orion=w, varnames='v', filename='refused.tec', Nvars=1, &
                        fldnames=['Time'], fldvalues=[1._R8P]), 'the name Time')
    r%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=r, filename='unsteady_strandid.tec', fldnames=['a'])
    call check(err == 1, 'reader: names without values refused')
    err = tec_read_structured_multiblock(orion=r, filename='unsteady_strandid.tec', fldnames=['a', 'b'], fldvalues=got, &
                                         fldfound=found)
    call check(err == 1, 'reader: two names and one fldfound refused')
  end subroutine scalars_refused

  ! A blank name in fldnames, alone or after a name, is refused by the reader as by vtk_read_structured_multiblock: the return
  ! value 1 and nothing read (no block), for an ASCII file and, with TecIO, a .szplt file. No item of the auxiliary data has a
  ! blank name (TECAUXSTR142 refuses one, and the ASCII reader takes a record only with a name before its equal sign): such a
  ! name could never be found.
  ! Basis: docs/user-guide/fortran-api.md, Cycle and named scalars: "no name is blank; otherwise both functions return 1 and
  ! write or read nothing", and the Tecplot functions "take the same optional arguments, with the same rules".
  subroutine blank_name_read()
    type(orion_data)  :: r
    real(R8P)         :: got(2)
    logical           :: found(2)
    integer           :: err, f, nf
    character(len=32) :: files(2)

    write(*,'(A)') 'blank names in fldnames: refused by the reader'
    files(1) = 'unsteady_strandid.tec'
    nf = 1
#ifdef TECIO
    files(2) = 'steady_strandid.szplt'
    nf = 2
#endif
    do f = 1, nf
      err = tec_read_structured_multiblock(orion=r, filename=trim(files(f)), fldnames=['    '], fldvalues=got(1:1))
      call check(err == 1 .and. .not.allocated(r%block), trim(files(f))//': a blank name refused, nothing read')
      err = tec_read_structured_multiblock(orion=r, filename=trim(files(f)), fldnames=['ITERATION', '         '], &
                                           fldvalues=got, fldfound=found)
      call check(err == 1 .and. .not.allocated(r%block), trim(files(f))//': a blank name after a name refused, nothing read')
    enddo
  end subroutine blank_name_read

  ! The writer returned e for refused.tec: it must be 1, with no file written (one written is removed for the next case).
  subroutine writes_nothing(e, what)
    integer,          intent(in) :: e
    character(len=*), intent(in) :: what
    integer :: u
    logical :: there
    inquire(file='refused.tec', exist=there)
    call check(e == 1 .and. .not.there, 'writer: '//what//' refused, no file written')
    if (there) then
      open(newunit=u, file='refused.tec')
      close(u, status='delete')
    endif
  end subroutine writes_nothing

end program tecplot_read_strandid
