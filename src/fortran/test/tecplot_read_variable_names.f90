!< Forms of the variable names in the VARIABLES header of a Tecplot ASCII file.
!< The reader takes each name on its own: in double quotes, in single quotes or bare, mixed in one
!< list, separated by blanks, tabs or commas; a name in quotes may hold blanks and commas. A quote
!< closes the name, so two quoted names with nothing between them ("a""b") are two names. The list
!< may continue over several lines, up to the first ZONE record (the keyword as a whole word: a name
!< such as zone_velocity is a name), and the keyword is read in any case.
!< A quote that is never closed is refused.
!< The title of each zone as the name of its block, in ASCII and POINT files (block_names).
!< Stops with a non-zero code when a check fails.
program tecplot_read_variable_names
  use Lib_Tecplot
  use Lib_ORION_data
  use IR_Precision
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
  call names_ok('names that begin with zone in a continuation line', &
                ' VARIABLES = x y z'//nl//'zone_velocity Zone2 pressure', &
                [character(len=16) :: 'x', 'y', 'z', 'zone_velocity', 'Zone2', 'pressure'])
  call names_unclosed('a quote never closed', ' VARIABLES = x y z "a b')
  call block_names()
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


  ! Zone titles as block names. The Tecplot ASCII readers give each block the title of its zone (T
  ! =) as its name, as tec_read_szplt does for a .szplt file. Two blocks, named blk_A and with a
  ! name of 600 characters, are written with tec_write_structured_multiblock and their names read
  ! back with tec_read_structured_multiblock. A file written by hand with three zones is read: a
  ! title in double quotes that holds blanks, a comma and the text I=9 J=9, the keywords separated
  ! by blanks; a title without quotes, after the keyword in lower case (t=); a zone without a title,
  ! whose block gets a blank name. Two zones of points, named pt_A and with a name of 600
  ! characters, are written with tec_write_points_multivars and their names read back with
  ! tec_read_points_multivars, which also reads a file of points written by hand, with a title in
  ! quotes that holds blanks and a zone without a title.
  ! Defect: tec_read_ascii and tec_read_points_multivars never set the names of the blocks, which
  ! held what the memory held.
  ! Basis: Fortran 2018, 19.6.6 (p. 514): a component without default initialization becomes
  ! undefined when the object is allocated; Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2
  ! (p. 123): T is the zone title, and section 4-2 (p. 120): double quotes enclose strings with
  ! blanks.
  subroutine block_names()
    call written_and_read
    call written_by_hand
    call points_written_and_read
    call points_by_hand
  end subroutine block_names

  ! A name of n characters, lower-case letters and digits: no blank, comma, quote or equal sign.
  pure function long_name(n) result(name)
    integer, intent(in)          :: n
    character(len=n)             :: name
    character(len=36), parameter :: c = 'abcdefghijklmnopqrstuvwxyz0123456789'
    integer                      :: i
    do i = 1, n
      name(i:i) = c(mod(i - 1, 36) + 1:mod(i - 1, 36) + 1)
    enddo
  end function long_name

  ! One block of 2 x 1 x 1 cells for each name, with one variable at the cells.
  subroutine fill(w, names)
    type(orion_data), intent(out) :: w
    character(len=*), intent(in)  :: names(:)
    integer                       :: b, i, j, k
    allocate(w%block(size(names)))
    do b = 1, size(names)
      w%block(b)%name = names(b)
      w%block(b)%Ni = 2; w%block(b)%Nj = 1; w%block(b)%Nk = 1
      allocate(w%block(b)%mesh(1:3,0:2,0:1,0:1), w%block(b)%vars(1:1,1:2,1:1,1:1))
      do k = 0, 1; do j = 0, 1; do i = 0, 2
        w%block(b)%mesh(:,i,j,k) = [real(i + 3*b, R8P), real(j, R8P), real(k, R8P)]
      enddo; enddo; enddo
      w%block(b)%vars(1,:,1,1) = [real(10*b + 1, R8P), real(10*b + 2, R8P)]
    enddo
    w%tec%node = .false.
  end subroutine fill

  ! Two blocks named blk_A and with a name of 600 characters, written to an ASCII file and read back.
  subroutine written_and_read
    type(orion_data)            :: w, r
    character(len=*), parameter :: file = 'tec_block_names.tec'
    integer                     :: err
    call fill(w, [character(len=600) :: 'blk_A', long_name(600)])
    w%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=w, varnames='u', filename=file)
    call check(err == 0, 'ASCII file of two blocks: written without error')
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, 'ASCII file of two blocks: read back without error')
    if (err /= 0) return
    call check(size(r%block) == 2, 'ASCII file of two blocks: two blocks read back')
    if (size(r%block) /= 2) return
    call check(r%block(1)%name == 'blk_A', 'ASCII file of two blocks: the name blk_A read back')
    call check(r%block(2)%name == long_name(600), 'ASCII file of two blocks: the name of 600 characters read back')
  end subroutine written_and_read

  ! Three zones of 2 x 2 nodes (x, y) written by hand: a title in quotes with blanks, a comma and I=9 J=9; a title without
  ! quotes after t=; no title.
  subroutine written_by_hand
    type(orion_data)            :: r
    character(len=*), parameter :: file = 'tec_block_names_by_hand.tec'
    character(len=1), parameter :: lf = achar(10)
    integer                     :: u, err, b
    logical                     :: dims
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
    write(u) ' VARIABLES = "x" "y"'//lf// &
             ' ZONE T="Zone one, with I=9 J=9" I=2 J=2'//lf//' 0 1 0 1'//lf//' 0 0 1 1'//lf// &
             ' ZONE t=two, I=2, J=2'//lf//' 2 3 2 3'//lf//' 0 0 1 1'//lf// &
             ' ZONE I=2, J=2'//lf//' 4 5 4 5'//lf//' 0 0 1 1'//lf
    close(u)
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, 'ASCII file written by hand: read without error')
    if (err /= 0) return
    call check(size(r%block) == 3, 'ASCII file written by hand: three blocks read')
    if (size(r%block) /= 3) return
    dims = .true.
    do b = 1, 3
      dims = dims .and. r%block(b)%Ni == 1 .and. r%block(b)%Nj == 1
    enddo
    call check(dims, 'ASCII file written by hand: every zone of 2 x 2 nodes, whatever its title')
    call check(r%block(1)%name == 'Zone one, with I=9 J=9', 'ASCII file written by hand: the title in quotes as the name')
    call check(r%block(2)%name == 'two', 'ASCII file written by hand: the title without quotes after t= as the name')
    call check(r%block(3)%name == '', 'ASCII file written by hand: a blank name for the zone without a title')
  end subroutine written_by_hand

  ! Two zones of points named pt_A and with a name of 600 characters, written with tec_write_points_multivars and read back with
  ! tec_read_points_multivars.
  subroutine points_written_and_read
    type(orion_data)            :: w, r
    character(len=*), parameter :: file = 'tec_block_names_points.dat'
    character(len=600)          :: names(2)
    integer                     :: err, b, i
    names = [character(len=600) :: 'pt_A', long_name(600)]
    allocate(w%block(2))
    do b = 1, 2
      w%block(b)%name = names(b)
      w%block(b)%Ni = 3; w%block(b)%Nj = 1; w%block(b)%Nk = 1
      allocate(w%block(b)%mesh(1:1,1:3,1:1,1:1), w%block(b)%vars(1:2,1:3,1:1,1:1))
      do i = 1, 3
        w%block(b)%mesh(1,i,1,1) = 0.5_R8P*i + b
        w%block(b)%vars(:,i,1,1) = [real(10*b + i, R8P), -0.25_R8P*i]
      enddo
    enddo
    err = tec_write_points_multivars(orion=w, varnames='"x" "u" "v"', filename=file)
    call check(err == 0, 'file of points: written without error')
    err = tec_read_points_multivars(orion=r, nvar=2, filename=file)
    call check(err == 0, 'file of points: read back without error')
    if (err /= 0) return
    call check(size(r%block) == 2, 'file of points: two zones read back')
    if (size(r%block) /= 2) return
    call check(r%block(1)%name == 'pt_A', 'file of points: the name pt_A read back')
    call check(r%block(2)%name == long_name(600), 'file of points: the name of 600 characters read back')
  end subroutine points_written_and_read

  ! A file of points written by hand: a zone of 3 points with a title in quotes that holds blanks, a zone of 2 points without a
  ! title.
  subroutine points_by_hand
    type(orion_data)            :: r
    character(len=*), parameter :: file = 'tec_block_names_points_by_hand.dat'
    character(len=1), parameter :: lf = achar(10)
    integer                     :: u, err
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
    write(u) ' VARIABLES = "x" "u" "v"'//lf// &
             ' ZONE T="Line one with blanks", I=3, F=POINT'//lf//' 0.0 1.0 2.0'//lf//' 0.5 1.5 2.5'//lf//' 1.0 2.0 3.0'//lf// &
             ' ZONE I=2, F=POINT'//lf//' 2.0 3.0 4.0'//lf//' 2.5 3.5 4.5'//lf
    close(u)
    err = tec_read_points_multivars(orion=r, nvar=2, filename=file)
    call check(err == 0, 'file of points written by hand: read without error')
    if (err /= 0) return
    call check(size(r%block) == 2, 'file of points written by hand: two zones read')
    if (size(r%block) /= 2) return
    call check(r%block(1)%name == 'Line one with blanks', 'file of points written by hand: the title in quotes as the name')
    call check(r%block(2)%name == '', 'file of points written by hand: a blank name for the zone without a title')
  end subroutine points_by_hand
end program tecplot_read_variable_names
