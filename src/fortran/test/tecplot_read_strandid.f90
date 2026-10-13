!< Solution time and STRANDID of the zones of Tecplot files.
!< tec_write_structured_multiblock writes the solution time with its sign: a negative solutiontime marks a steady
!< solution (minus the iteration count of the run, for instance). The zones of a time-accurate solution belong to the
!< strand of their block: STRANDID = b in the header of zone b of an ASCII file, StrandID b in a binary file. The zones
!< of a steady solution are static: no STRANDID in an ASCII file, StrandID 0 (static) for TecIO. The ASCII reader keeps
!< solutiontime as written, sign included, and stores the STRANDID of the last zone header in orion%strandid, -1 when that
!< header has none; a file whose zone headers hold no SOLUTIONTIME reads with solutiontime = -huge(1._R8P), the most negative
!< real, which no solution has (it gave -10, the time of a steady solution at iteration 10).
!< With TecIO, the .szplt files are read back with the TecIO reader: the solution time and the StrandID of every zone.
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
!< Stops with a non-zero code when a check fails.
program tecplot_read_strandid
  use IR_Precision, only: R8P
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
  ! every zone has the signed time, and StrandID b for a time-accurate solution, 0 (static) for a steady one.
  subroutine binary(time, filename)
    real(R8P),        intent(in) :: time
    character(len=*), intent(in) :: filename
    type(orion_data)   :: written
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

end program tecplot_read_strandid
