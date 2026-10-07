!< Forms of the variable names in the VARIABLES header of a Tecplot ASCII file.
!< The reader takes each name on its own: in double quotes, in single quotes or bare, mixed in one
!< list, separated by blanks, tabs or commas; a name in quotes may hold blanks and commas. A quote
!< closes the name, so two quoted names with nothing between them ("a""b") are two names. The list
!< may continue over several lines, up to the first ZONE line, and the keyword is read in any case.
!< A quote that is never closed is refused.
!< Stops with a non-zero code when a check fails.
program tecplot_read_variable_names
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  character(len=1), parameter :: nl = new_line('a')
  character(len=1), parameter :: tab = achar(9)
  integer :: nfail

  nfail = 0
  call names_ok('bare names', ' VARIABLES = x y z rho(1) u', [character(len=8) :: 'x', 'y', 'z', 'rho(1)', 'u'])
  call names_ok('names in double quotes', ' VARIABLES ="x" "y" "z" "a" "b"', [character(len=8) :: 'x', 'y', 'z', 'a', 'b'])
  call names_ok('names in single quotes', " VARIABLES = 'x' 'y' 'z' 'a'", [character(len=8) :: 'x', 'y', 'z', 'a'])
  call names_ok('commas and tabs', ' VARIABLES = x,y,'//tab//'z, a ,b', [character(len=8) :: 'x', 'y', 'z', 'a', 'b'])
  call names_ok('adjacent quoted names, one line', ' VARIABLES ="x" "y" "z" "a""b"', [character(len=8) :: 'x', 'y', 'z', 'a', 'b'])
  call names_ok('adjacent quoted names, next line', ' VARIABLES ="x" "y" "z"'//nl//'"a""b""c"', &
                [character(len=8) :: 'x', 'y', 'z', 'a', 'b', 'c'])
  call names_ok('quoted, single-quoted and bare names mixed', &
                ' variables = "x", y'//tab//"'z' "//'"a b" b', [character(len=8) :: 'x', 'y', 'z', 'a b', 'b'])
  call names_ok('a quote of the other kind inside a name', ' VARIABLES = x y z "it'//"'"//'s"', &
                [character(len=8) :: 'x', 'y', 'z', "it's"])
  call names_unclosed('a quote never closed', ' VARIABLES = x y z "a b')
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_variable_names: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_variable_names: all checks passed'

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

  ! One zone of 3 x 2 x 2 nodes (2 cells) after the given header: x y z nodal, the other variables
  ! cell-centred, one value per line.
  subroutine write_file(filename, header, nvar)
    character(len=*), intent(in) :: filename, header
    integer,          intent(in) :: nvar
    character(len=16) :: last
    integer :: u, n
    open(newunit=u, file=filename, status='replace')
    write(u,'(A)') header
    write(last,'(I0)') nvar
    write(u,'(A)') ' ZONE T=blocco, I=3, J=2, K=2, DATAPACKING=BLOCK, '// &
                   'VARLOCATION=([1-3]=NODAL,[4-'//trim(last)//']=CELLCENTERED), SOLUTIONTIME=0.5'
    do n = 1, 3*12 + (nvar - 3)*2
      write(u,'(I0)') n
    enddo
    close(u)
  end subroutine write_file

  ! The header must give exactly these names, and the zone must be read whole.
  subroutine names_ok(what, header, expected)
    character(len=*), intent(in) :: what, header, expected(:)
    type(orion_data) :: d
    integer :: err, k
    logical :: same
    call write_file('variable_names.tec', header, size(expected))
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='variable_names.tec')
    write(*,'(A)') what
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(allocated(d%varnames), 'names stored')
    if (.not.allocated(d%varnames)) return
    call check(size(d%varnames) == size(expected), 'number of names')
    if (size(d%varnames) /= size(expected)) return
    same = .true.
    do k = 1, size(expected)
      if (trim(d%varnames(k)) /= trim(expected(k))) then
        same = .false.
        write(*,'(A)') '        name '//trim(d%varnames(k))//' instead of '//trim(expected(k))
      endif
    enddo
    call check(same, 'names')
    call check(size(d%block(1)%vars,1) == size(expected) - 3, 'one solution band per name after z')
  end subroutine names_ok

  subroutine names_unclosed(what, header)
    character(len=*), intent(in) :: what, header
    type(orion_data) :: d
    integer :: err
    call write_file('variable_names_unclosed.tec', header, 6)
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='variable_names_unclosed.tec')
    write(*,'(A)') what
    call check(err /= 0, 'refused')
  end subroutine names_unclosed

end program tecplot_read_variable_names
