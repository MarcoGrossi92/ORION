!< Tecplot ASCII zones followed by data that their headers do not announce.
!< After the values that a zone header announces, only blank lines, the next ZONE record, DATASETAUXDATA records or the end of
!< the file may follow. A file with text after the last row, with more rows than I gives, or with a row of one value too many
!< is refused, with a message that gives the file, the zone, the line and what was found (refused_files). Well-formed POINT,
!< BLOCK and two-zone files still read, with rows that wrap onto several lines and a last line without a line feed
!< (accepted_files), and the refusals of ORION 2.1 stay: fewer rows than I gives, a token that is not a number, J=2 with too
!< little data (refusals_kept).
!< Defect: the reader read the values of a zone as one stream and stopped after the count of the header; what followed was
!< ignored, so these three files were read without an error, the extra row or value lost.
!< Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-4 (ASCII data): the values of a zone are a stream of numbers
!< separated by blanks, tabs or commas, its line breaks free; I, J, K and the variables give their count.
!< The test runs itself on each refused file, with the name of the file as its argument, to read the message on its standard
!< error. Stops with a non-zero code when a check fails.
program tecplot_read_unannounced_data
  use Lib_Tecplot
  use Lib_ORION_data
  use IR_Precision
  implicit none
  character(len=1), parameter   :: lf = achar(10)
  character(len=*), parameter   :: names = ' VARIABLES = "T" "a" "b" "c"'//lf
  integer, parameter            :: n = 5000      ! rows of the POINT zones
  character(len=:), allocatable :: self
  integer                       :: nfail

  if (command_argument_count() == 1) then
    call read_argument()
    stop
  endif

  call get_argument(0, self)
  nfail = 0
  call refused_files()
  call accepted_files()
  call refusals_kept()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_unannounced_data: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_unannounced_data: all checks passed'

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

  ! Run with the name of a file: read it and write the return value on the standard output.
  subroutine read_argument()
    character(len=:), allocatable :: file
    type(orion_data)              :: r
    integer                       :: err
    call get_argument(1, file)
    err = tec_read_structured_multiblock(orion=r, filename=file)
    write(*,'(I0)') err
  end subroutine read_argument

  subroutine get_argument(i, arg)
    integer,                       intent(in)  :: i
    character(len=:), allocatable, intent(out) :: arg
    integer                                    :: length
    call get_command_argument(i, length=length)
    allocate(character(len=length) :: arg)
    call get_command_argument(i, arg)
  end subroutine get_argument

  pure function itoa(i) result(s)
    integer, intent(in)           :: i
    character(len=:), allocatable :: s
    character(len=16)             :: buffer
    write(buffer,'(I0)') i
    s = trim(buffer)
  end function itoa

  ! The whole of a file, byte by byte (empty when it cannot be read).
  function file_text(file) result(text)
    character(len=*), intent(in)  :: file
    character(len=:), allocatable :: text
    integer                       :: u, size_, ios
    text = ''
    open(newunit=u, file=file, access='stream', form='unformatted', status='old', action='read', iostat=ios)
    if (ios /= 0) return
    inquire(unit=u, size=size_)
    if (size_ > 0) then
      deallocate(text)
      allocate(character(len=size_) :: text)
      read(u, iostat=ios) text
      if (ios /= 0) text = ''
    endif
    close(u)
  end function file_text

  subroutine open_bytes(file, u)
    character(len=*), intent(in)  :: file
    integer,          intent(out) :: u
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
  end subroutine open_bytes

  ! Row r of a POINT zone of T a b c: r 2r 3r 4r. A long row has a fifth value, 9; a wrapped row is on two lines.
  function row(r, long, wrap) result(text)
    integer, intent(in)           :: r
    logical, intent(in)           :: long, wrap
    character(len=:), allocatable :: text
    character(len=1)              :: sep
    sep = ' '
    if (wrap) sep = lf
    text = itoa(r)//' '//itoa(2*r)//sep//itoa(3*r)//' '//itoa(4*r)
    if (long) text = text//' 9'
  end function row

  ! A POINT zone of T a b c on unit u: the header announces I = announced, the zone holds rows 1 to held. Row long_row (0: none)
  ! has a fifth value; with wrap every row is on two lines; without last_lf the last row has no line feed.
  subroutine write_point_zone(u, title, announced, held, long_row, wrap, last_lf)
    integer,          intent(in) :: u, announced, held, long_row
    character(len=*), intent(in) :: title
    logical,          intent(in) :: wrap, last_lf
    integer                      :: r
    write(u) ' ZONE T="'//title//'", I='//itoa(announced)//', DATAPACKING=POINT'//lf
    do r = 1, held
      write(u) row(r, r == long_row, wrap)
      if (r < held .or. last_lf) write(u) lf
    enddo
  end subroutine write_point_zone

  ! A file of one POINT zone (write_point_zone), with the text tail after it.
  subroutine write_point_file(file, announced, held, long_row, tail)
    character(len=*), intent(in) :: file, tail
    integer,          intent(in) :: announced, held, long_row
    integer                      :: u
    call open_bytes(file, u)
    write(u) names
    call write_point_zone(u, 'table', announced, held, long_row, .false., .true.)
    write(u) tail
    close(u)
  end subroutine write_point_file

  ! The file is refused: run on it, the test returns a nonzero value and its message holds every text of expected.
  subroutine check_refused(file, what, expected)
    character(len=*), intent(in) :: file, what
    character(len=*), intent(in) :: expected(:)
    character(len=:), allocatable :: out, msg
    integer                       :: exitstat, cmdstat, err, ios, e
    exitstat = -1
    cmdstat = -1
    call execute_command_line("'"//self//"' "//file//' > '//file//'.out 2> '//file//'.err', &
                              exitstat=exitstat, cmdstat=cmdstat)
    call check(cmdstat == 0 .and. exitstat == 0, what//': the test runs on the file')
    out = file_text(file//'.out')
    read(out, *, iostat=ios) err
    if (ios /= 0) err = 0
    write(*,'(A,I0)') '        return value: ', err
    call check(err /= 0, what//': refused')
    msg = file_text(file//'.err')
    do e = 1, size(expected)
      call check(index(msg, trim(expected(e))) > 0, what//': the message has '//trim(expected(e)))
    enddo
    if (any([(index(msg, trim(expected(e))) == 0, e = 1, size(expected))])) write(*,'(A)') msg
  end subroutine check_refused

  ! The three files of the defect, and two more of the same rule: text between two zones, and after a DATASETAUXDATA record.
  subroutine refused_files()
    integer :: u
    character(len=*), parameter :: what = 'data that the zone header does not announce'
    write(*,'(A)') 'refused: data after the values that a zone header announces'

    ! Text after the last row: a line END at the end of the file (line n+3)
    call write_point_file('tec_unannounced_end.dat', n, n, 0, 'END'//lf)
    call check_refused('tec_unannounced_end.dat', 'a line END after the last row', [character(len=60) :: what, &
      'File               : tec_unannounced_end.dat', 'Zone               : 1', &
      'Physical line      : '//itoa(n+3)//lf, 'Found              : "END"'])

    ! Fewer rows announced than held: I = n-1, n rows; the last row (line n+2) is not announced
    call write_point_file('tec_unannounced_row.dat', n-1, n, 0, '')
    call check_refused('tec_unannounced_row.dat', 'I='//itoa(n-1)//' on a zone of '//itoa(n)//' rows', &
      [character(len=60) :: what, 'File               : tec_unannounced_row.dat', 'Zone               : 1', &
      'Values announced   : '//itoa(4*(n-1)), 'Physical line      : '//itoa(n+2)//lf, &
      'Found              : "'//row(n, .false., .false.)//'"'])

    ! A row of 5 values where the header announces 4 variables (row 100): the last value of the file is one too many
    call write_point_file('tec_unannounced_value.dat', n, n, 100, '')
    call check_refused('tec_unannounced_value.dat', 'a row of 5 values in a POINT zone of 4 variables', &
      [character(len=60) :: what, 'File               : tec_unannounced_value.dat', 'Zone               : 1', &
      'Values announced   : '//itoa(4*n), 'Physical line      : '//itoa(n+2)//lf, 'Found              : "'//itoa(4*n)//'"'])

    ! Text between two zones: refused in zone 1, on its line (2 + 5 + 2 = 9)
    call open_bytes('tec_unannounced_between.dat', u)
    write(u) names
    call write_point_zone(u, 'one', 5, 5, 0, .false., .true.)
    write(u) lf//'END'//lf
    call write_point_zone(u, 'two', 2, 2, 0, .false., .true.)
    close(u)
    call check_refused('tec_unannounced_between.dat', 'a line END between two zones', [character(len=60) :: what, &
      'Zone               : 1', 'Physical line      : 9'//lf, 'Found              : "END"'])

    ! Text after a DATASETAUXDATA record that follows the last zone (line 2 + 5 + 2 = 9)
    call write_point_file('tec_unannounced_after_aux.dat', 5, 5, 0, 'DATASETAUXDATA CYCLE = "3"'//lf//'END'//lf)
    call check_refused('tec_unannounced_after_aux.dat', 'a line END after a DATASETAUXDATA record', &
      [character(len=60) :: what, 'Zone               : 1', 'Physical line      : 9'//lf, 'Found              : "END"'])
  end subroutine refused_files

  ! Block b of r is a POINT zone of T a b c (write_point_zone) of rows rows: one coordinate T, the variables a b c at the nodes.
  logical function point_block(r, b, rows) result(same)
    type(orion_data), intent(in) :: r
    integer,          intent(in) :: b, rows
    integer                      :: i, s
    same = allocated(r%block)
    if (same) same = size(r%block) >= b
    if (.not.same) return
    same = allocated(r%block(b)%mesh) .and. allocated(r%block(b)%vars)
    if (same) same = all(shape(r%block(b)%mesh) == [1, rows, 1, 1]) .and. all(shape(r%block(b)%vars) == [3, rows, 1, 1])
    if (.not.same) return
    do i = 0, rows-1
      same = same .and. r%block(b)%mesh(1,i,0,0) == real(i+1, R8P)
      do s = 1, 3
        same = same .and. r%block(b)%vars(s,i,0,0) == real((s+1)*(i+1), R8P)
      enddo
    enddo
  end function point_block

  ! Well-formed files: they read, with the values written.
  subroutine accepted_files()
    type(orion_data) :: r
    integer          :: u, err, cyc
    character(len=1), parameter :: tab = achar(9)
    write(*,'(A)') 'accepted: well-formed POINT, BLOCK and two-zone files'

    ! POINT: n rows, blank lines and DATASETAUXDATA records after them, the last line blank and without a line feed
    call write_point_file('tec_announced_point.dat', n, n, 0, &
      lf//'DATASETAUXDATA CYCLE = "12"'//lf//'  '//tab//lf//'datasetauxdata Tw = "300"'//lf//'   ')
    err = tec_read_structured_multiblock(orion=r, filename='tec_announced_point.dat', cycle=cyc)
    call check(err == 0, 'POINT zone of '//itoa(n)//' rows, then blank lines and DATASETAUXDATA records: read')
    if (err == 0) call check(point_block(r, 1, n) .and. cyc == 12, 'POINT zone: the values written and CYCLE = 12')

    ! BLOCK: x y at the nodes, p at the cells (I = 3, J = 2), values wrapped anywhere, tabs and commas, a trailing comma, and
    ! the last row without a line feed
    call open_bytes('tec_announced_block.dat', u)
    write(u) ' VARIABLES = "x" "y" "p"'//lf// &
             ' ZONE T="b", I=3, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'//lf// &
             '0 1'//lf//'2'//tab//'0, 1,2 0'//lf//lf//'0 0 1'//lf//'1 1'//lf//'11,'//tab//'12,'
    close(u)
    if (allocated(r%block)) deallocate(r%block)
    if (allocated(r%varnames)) deallocate(r%varnames)
    err = tec_read_structured_multiblock(orion=r, filename='tec_announced_block.dat')
    call check(err == 0, 'BLOCK zone, values wrapped across lines, a trailing comma, no last line feed: read')
    if (err == 0) then
      call check(size(r%block) == 1 .and. size(r%block(1)%mesh,1) == 2 .and. size(r%block(1)%vars,1) == 1, &
                 'BLOCK zone: two coordinates, one variable')
      if (size(r%block(1)%mesh,1) == 2 .and. size(r%block(1)%vars,1) == 1) &
        call check(r%block(1)%mesh(1,2,1,0) == 2._R8P .and. r%block(1)%mesh(2,2,1,0) == 1._R8P .and. &
                   r%block(1)%vars(1,1,1,1) == 11._R8P .and. r%block(1)%vars(1,2,1,1) == 12._R8P, &
                   'BLOCK zone: the values written')
    endif

    ! Two zones: rows wrapped onto two lines, blank lines between the zones, the header of zone 2 on two lines, its last row
    ! without a line feed
    call open_bytes('tec_announced_two_zones.dat', u)
    write(u) names
    call write_point_zone(u, 'one', 7, 7, 0, .true., .true.)
    write(u) lf//lf//' ZONE T="two"'//lf//' I=3, DATAPACKING=POINT'//lf
    write(u) row(1, .false., .false.)//lf//row(2, .false., .false.)//lf//row(3, .false., .false.)
    close(u)
    if (allocated(r%block)) deallocate(r%block)
    if (allocated(r%varnames)) deallocate(r%varnames)
    err = tec_read_structured_multiblock(orion=r, filename='tec_announced_two_zones.dat')
    call check(err == 0, 'two POINT zones, rows wrapped, blank lines between them, no last line feed: read')
    if (err == 0) call check(size(r%block) == 2 .and. point_block(r, 1, 7) .and. point_block(r, 2, 3), &
                             'two zones: the values written')
  end subroutine accepted_files

  ! The refusals of ORION 2.1 stay, their messages with the line of the error.
  subroutine refusals_kept()
    integer :: u
    write(*,'(A)') 'refused as before'

    ! More rows announced than held: the file ends in the zone (last row on line n+2)
    call write_point_file('tec_refused_more_rows.dat', n+1, n, 0, '')
    call check_refused('tec_refused_more_rows.dat', 'I='//itoa(n+1)//' on a zone of '//itoa(n)//' rows', &
      [character(len=60) :: 'unexpected end of file', 'Physical line      : '//itoa(n+2)//lf])

    ! A token that is not a number, on the fourth row (line 6)
    call open_bytes('tec_refused_token.dat', u)
    write(u) names//' ZONE T="t", I=5, DATAPACKING=POINT'//lf//'1 2 3 4'//lf//'2 4 6 8'//lf//'3 6 9 12'//lf// &
             '4 8 x12 16'//lf//'5 10 15 20'//lf
    close(u)
    call check_refused('tec_refused_token.dat', 'a token that is not a number', &
      [character(len=60) :: 'invalid numeric token', 'Physical line      : 6'//lf, 'Bad token          : "x12"'])

    ! J=2 with the data of one node line: 2 x 2 nodes announced, x and y of 2 x 1 held
    call open_bytes('tec_refused_j2.dat', u)
    write(u) ' VARIABLES = "x" "y"'//lf//' ZONE T="j", I=2, J=2, DATAPACKING=BLOCK'//lf//'0 1'//lf//'0 0'//lf
    close(u)
    call check_refused('tec_refused_j2.dat', 'J=2 with too little data', [character(len=60) :: 'unexpected end of file'])
  end subroutine refusals_kept

end program tecplot_read_unannounced_data
