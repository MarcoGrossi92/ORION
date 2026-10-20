!< Tecplot ASCII files whose VARIABLES header is longer than 1000 characters.
!< Two blocks with x y z and 200 cell-centred variables named as a solver names them (rho(1) ...
!< rho(195), u, v, w, p, T) are written with the ORION writer, so the VARIABLES line has more than
!< 2000 characters. The file must hold the whole line, and the reader must return every name and
!< every value. The reader keeps at most 512 names: 512 names are read, 513 are refused.
!< Zone headers longer than 1000 characters too: block names of 600 and 4096 characters in the zone
!< titles of ASCII and POINT files, and a title of 3000 characters (long_names).
!< A last line without a line feed, whose length is a multiple of the pieces of read_whole_line
!< (last_line_without_line_feed). Data lines longer than 1000 characters, up to the 32,000 of the
!< Tecplot format, in BLOCK and POINT zones (long_data_lines).
!< An error in the coordinates of a BLOCK zone ends the read (block_coordinate_errors).
!< Stops with a non-zero code when a check fails.
program tecplot_read_long_header
  use, intrinsic :: iso_fortran_env, only : iostat_end
  use Lib_Tecplot
  use Lib_ORION_data
  use IR_Precision
  use strings, only : read_whole_line
  implicit none
  integer :: nfail

  nfail = 0
  call variables_200()
  call names_512()
  call names_513()
  call long_names()
  call last_line_without_line_feed()
  call long_data_lines()
  call block_coordinate_errors()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_long_header: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_long_header: all checks passed'

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

  ! The names of nv solution variables: rho(1) ... rho(nv-5), u, v, w, p, T.
  function solver_names(nv) result(names)
    integer, intent(in) :: nv
    character(len=16)   :: names(nv)
    integer :: s
    do s = 1, nv - 5
      write(names(s),'(A,I0,A)') 'rho(', s, ')'
    enddo
    names(nv-4:nv) = ['u', 'v', 'w', 'p', 'T']
  end function solver_names

  ! Two blocks of 3 x 2 x 2 and 2 x 3 x 1 cells; x y z nodal, the nv variables cell-centred.
  ! Every value is exact in the writer's format: 1000 s + 100 i + 10 j + k + b/2.
  subroutine fill(w, nv)
    type(orion_data), intent(inout) :: w
    integer,          intent(in)    :: nv
    integer :: b, i, j, k, s, n(3,2)
    n(:,1) = [3, 2, 2]
    n(:,2) = [2, 3, 1]
    allocate(w%block(2))
    do b = 1, 2
      write(w%block(b)%name,'(A,I0)') 'block', b
      w%block(b)%Ni = n(1,b); w%block(b)%Nj = n(2,b); w%block(b)%Nk = n(3,b)
      allocate(w%block(b)%mesh(1:3,0:n(1,b),0:n(2,b),0:n(3,b)))
      allocate(w%block(b)%vars(1:nv,1:n(1,b),1:n(2,b),1:n(3,b)))
      do k = 0, n(3,b); do j = 0, n(2,b); do i = 0, n(1,b)
        w%block(b)%mesh(:,i,j,k) = [real(i,8), real(j,8), real(k,8) + 10.0d0*b]
      enddo; enddo; enddo
      do k = 1, n(3,b); do j = 1, n(2,b); do i = 1, n(1,b); do s = 1, nv
        w%block(b)%vars(s,i,j,k) = 1000.0d0*s + 100.0d0*i + 10.0d0*j + real(k,8) + 0.5d0*b
      enddo; enddo; enddo; enddo
    enddo
  end subroutine fill

  subroutine check_names(d, names)
    type(orion_data), intent(in) :: d
    character(len=*), intent(in) :: names(:)
    integer :: s
    logical :: same
    call check(allocated(d%varnames), 'names returned')
    if (.not. allocated(d%varnames)) return
    write(*,'(A,I0,A,I0)') '        names returned: ', size(d%varnames), ' of ', size(names) + 3
    call check(size(d%varnames) == size(names) + 3, 'every name returned, coordinates first')
    if (size(d%varnames) /= size(names) + 3) return
    same = d%varnames(1) == 'x' .and. d%varnames(2) == 'y' .and. d%varnames(3) == 'z'
    do s = 1, size(names)
      same = same .and. d%varnames(3+s) == names(s)
    enddo
    call check(same, 'every name as written')
  end subroutine check_names

  subroutine check_values(w, d)
    type(orion_data), intent(in) :: w, d
    integer :: b
    logical :: same
    call check(size(d%block) == 2, 'two blocks')
    if (size(d%block) /= 2) return
    same = .true.
    do b = 1, 2
      same = size(d%block(b)%mesh,1) == 3 .and. allocated(d%block(b)%vars)
      if (.not. same) exit
      same = all(lbound(d%block(b)%mesh) == lbound(w%block(b)%mesh)) .and. &
             all(ubound(d%block(b)%mesh) == ubound(w%block(b)%mesh)) .and. &
             all(lbound(d%block(b)%vars) == lbound(w%block(b)%vars)) .and. &
             all(ubound(d%block(b)%vars) == ubound(w%block(b)%vars))
      if (.not. same) exit
      same = all(d%block(b)%mesh == w%block(b)%mesh) .and. all(d%block(b)%vars == w%block(b)%vars)
      if (.not. same) exit
    enddo
    call check(same, 'every coordinate and every value of the two blocks as written')
  end subroutine check_values

  ! Writes nv variables with the ORION writer and returns the length of the VARIABLES line
  ! it should write and whether the file holds that line whole.
  subroutine write_file(w, names, fname, whole)
    type(orion_data), intent(inout) :: w
    character(len=*), intent(in)    :: names(:), fname
    logical,          intent(out)   :: whole
    character(len=:), allocatable :: list, header
    character(len=65536) :: line
    integer :: u, err, ios, s
    list = trim(names(1))
    do s = 2, size(names)
      list = list//' '//trim(names(s))
    enddo
    call fill(w, size(names))
    w%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=w, varnames=list, filename=fname)
    call check(err == 0, 'written')
    header = ' VARIABLES = x y z '//list
    line = ' '
    open(newunit=u, file=fname, status='old', action='read')
    read(u,'(A)',iostat=ios) line
    close(u)
    write(*,'(A,I0,A,I0)') '        characters in the VARIABLES line of the file: ', len_trim(line), &
                           ' of ', len(header)
    whole = ios == 0 .and. trim(line) == header
  end subroutine write_file

  subroutine variables_200()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '200 variables: x y z nodal, rho(1) ... rho(195) u v w p T cell-centred'
    call write_file(w, solver_names(200), 'long_header_200.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_200.tec')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(200))
    call check_values(w, d)
  end subroutine variables_200

  ! 3 coordinates and 509 variables: 512 names, the most the reader keeps.
  subroutine names_512()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '512 names: x y z and 509 variables'
    call write_file(w, solver_names(509), 'long_header_512.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_512.tec')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(509))
    call check_values(w, d)
  end subroutine names_512

  ! 3 coordinates and 510 variables: 513 names, one more than the reader keeps.
  subroutine names_513()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '513 names: x y z and 510 variables'
    call write_file(w, solver_names(510), 'long_header_513.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_513.tec')
    call check(err /= 0, 'refused with an error (more than 512 names)')
  end subroutine names_513


  ! Block names of any length in the zone headers. A block name holds 4096 characters, and the
  ! writers put it in the zone header as the zone title (T =). A block of 3 x 2 x 1 cells is written
  ! to an ASCII file with tec_write_structured_multiblock, with a name of 600 characters and with
  ! one of 4096: the zone header must hold the whole name and the I, J and K of the zone, and
  ! tec_read_structured_multiblock must read the block back with its dimensions, coordinates and
  ! values. A file written by hand, whose zone header has a title of 3000 characters, in quotes, on
  ! its first line and I, J and K on the next one, and whose last line has no line feed, is read.
  ! Last, as a reader that loses the dimensions of a zone can stop the program, a zone of 5 points
  ! (F=POINT, x u v on each line) with a name of 600 and of 4096 characters is written by hand, with
  ! the header that the writer of points had (ZONE  T = name, I=5, F=POINT), and read with
  ! tec_read_structured_multiblock. Every coordinate and value is an integer, a half or a quarter,
  ! exact in every format.
  ! Defect: the writers built the zone header in 500 characters and the ASCII readers read each line
  ! in 1000, so a longer zone header lost I, J and K and the file was refused; the reader of points
  ! that tec_read_structured_multiblock replaced split the zone header into pieces of 100 characters
  ! and wrote past the end of one.
  ! Basis: Fortran 2018, 10.2.1.3 (p. 162): an assignment to a character variable of fixed length
  ! truncates the value; Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2 (p. 123): T is the
  ! zone title.
  subroutine long_names()
    call ascii_block(600)
    call ascii_block(4096)
    call ascii_by_hand
    call points(600)
    call points(4096)
  end subroutine long_names

  ! The digits of n.
  pure function itoa(n) result(s)
    integer, intent(in)           :: n
    character(len=:), allocatable :: s
    character(len=16)             :: buffer
    write(buffer,'(I0)') n
    s = trim(buffer)
  end function itoa

  ! A name of n characters, lower-case letters and digits from the character first on: no blank, comma, quote or equal sign.
  pure function long_name(n, first) result(name)
    integer,   intent(in)          :: n
    character, intent(in)          :: first
    character(len=n)               :: name
    character(len=36), parameter   :: c = 'abcdefghijklmnopqrstuvwxyz0123456789'
    integer                        :: i, k
    k = index(c, first)
    do i = 1, n
      name(i:i) = c(mod(k + i - 2, 36) + 1:mod(k + i - 2, 36) + 1)
    enddo
  end function long_name

  ! The whole content of a file, line feeds included.
  function file_text(file) result(text)
    character(len=*), intent(in)  :: file
    character(len=:), allocatable :: text
    integer                       :: u, n
    open(newunit=u, file=file, access='stream', form='unformatted', status='old', action='read')
    inquire(unit=u, size=n)
    allocate(character(len=n) :: text)
    if (n > 0) read(u) text
    close(u)
  end function file_text

  ! One block of 3 x 2 x 1 cells for each name: x, y and z at the nodes, two variables at the cells.
  subroutine fill_names(w, names)
    type(orion_data), intent(out) :: w
    character(len=*), intent(in)  :: names(:)
    integer                       :: b, i, j, k
    allocate(w%block(size(names)))
    do b = 1, size(names)
      w%block(b)%name = names(b)
      w%block(b)%Ni = 3; w%block(b)%Nj = 2; w%block(b)%Nk = 1
      allocate(w%block(b)%mesh(1:3,0:3,0:2,0:1), w%block(b)%vars(1:2,1:3,1:2,1:1))
      do k = 0, 1; do j = 0, 2; do i = 0, 3
        w%block(b)%mesh(:,i,j,k) = [real(i + 4*b, R8P), 0.5_R8P*j, 0.25_R8P*k]
      enddo; enddo; enddo
      do k = 1, 1; do j = 1, 2; do i = 1, 3
        w%block(b)%vars(:,i,j,k) = [real(1000*b + 100*i + 10*j + k, R8P), -0.5_R8P*(i + b)]
      enddo; enddo; enddo
    enddo
    w%tec%node = .false.
  end subroutine fill_names

  ! r holds the blocks of w: as many, and each with the same dimensions, coordinates and values.
  logical function same_field(w, r) result(same)
    type(orion_data), intent(in) :: w, r
    integer                      :: b
    same = allocated(r%block)
    if (same) same = size(r%block) == size(w%block)
    if (.not.same) return
    do b = 1, size(w%block)
      same = r%block(b)%Ni == w%block(b)%Ni .and. r%block(b)%Nj == w%block(b)%Nj .and. r%block(b)%Nk == w%block(b)%Nk
      if (same) same = allocated(r%block(b)%mesh) .and. allocated(r%block(b)%vars)
      if (same) same = all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
                       all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
      if (same) same = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
      if (.not.same) return
    enddo
  end function same_field

  ! A block with a name of n characters, written to an ASCII file with tec_write_structured_multiblock and read back.
  subroutine ascii_block(n)
    integer, intent(in)           :: n
    type(orion_data)              :: w, r
    character(len=:), allocatable :: name, file, what
    integer                       :: err
    name = long_name(n, 'a')
    file = 'tec_long_name_'//itoa(n)//'.tec'
    what = 'ASCII file, block name of '//itoa(n)//' characters'
    call fill_names(w, [name])
    w%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=w, varnames='u v', filename=file)
    call check(err == 0, what//': written without error')
    call check(index(file_text(file), ' ZONE  T = '//name//', I=4, J=3, K=2, DATAPACKING=BLOCK') > 0, &
               what//': the zone header holds the whole name and I, J, K')
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read back without error')
    if (err /= 0) return
    call check(same_field(w, r), what//': read back with its dimensions, coordinates and values')
  end subroutine ascii_block

  ! A file written by hand: the zone header has a title of 3000 characters, in quotes, on its first line and I, J and K on
  ! the next one; the values follow one per line, in the order of a BLOCK zone, and the last line has no line feed.
  subroutine ascii_by_hand
    type(orion_data)              :: w, r
    character(len=*), parameter   :: file = 'tec_long_title_by_hand.tec'
    character(len=*), parameter   :: what = 'ASCII file written by hand, title of 3000 characters'
    character(len=1), parameter   :: lf = achar(10)
    character(len=:), allocatable :: text
    character(len=32)             :: value
    integer                       :: u, err, d, s, i, j, k
    call fill_names(w, ['hand'])
    text = ' VARIABLES = "x" "y" "z" "u" "v"'//lf//' ZONE T="'//long_name(3000, 'k')//'"'//lf// &
           ' I=4, J=3, K=2, DATAPACKING=BLOCK, VARLOCATION=([4-5]=CELLCENTERED)'
    do d = 1, 3
      do k = 0, 1; do j = 0, 2; do i = 0, 3
        write(value,'(ES24.16)') w%block(1)%mesh(d,i,j,k)
        text = text//lf//trim(value)
      enddo; enddo; enddo
    enddo
    do s = 1, 2
      do k = 1, 1; do j = 1, 2; do i = 1, 3
        write(value,'(ES24.16)') w%block(1)%vars(s,i,j,k)
        text = text//lf//trim(value)
      enddo; enddo; enddo
    enddo
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
    write(u) text
    close(u)
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read without error')
    if (err /= 0) return
    call check(same_field(w, r), what//': read with its dimensions, coordinates and values')
  end subroutine ascii_by_hand

  ! A zone of 5 points with a name of n characters, in a POINT file written by hand (one point per line: x u v) and read with
  ! tec_read_structured_multiblock: a line of nodes, one coordinate and two variables at the nodes,
  ! mesh(1:1,0:4,0:0,0:0) and vars(1:2,0:4,0:0,0:0).
  subroutine points(n)
    integer, intent(in)           :: n
    type(orion_data)              :: w, r
    character(len=:), allocatable :: name, file, what, text
    character(len=1), parameter   :: lf = achar(10)
    character(len=80)             :: line
    integer                       :: err, i, u
    name = long_name(n, 'p')
    file = 'tec_long_name_points_'//itoa(n)//'.dat'
    what = 'POINT file, zone name of '//itoa(n)//' characters'
    allocate(w%block(1))
    w%block(1)%name = name
    w%block(1)%Ni = 4; w%block(1)%Nj = 0; w%block(1)%Nk = 0
    allocate(w%block(1)%mesh(1:1,0:4,0:0,0:0), w%block(1)%vars(1:2,0:4,0:0,0:0))
    text = ' VARIABLES = "x" "u" "v"'//lf//' ZONE  T = '//name//', I=5, F=POINT'
    do i = 0, 4
      w%block(1)%mesh(1,i,0,0) = 0.25_R8P*(i + 1)
      w%block(1)%vars(:,i,0,0) = [10._R8P*(i + 1), -0.5_R8P*(i + 1)]
      write(line,'(3ES24.16)') w%block(1)%mesh(1,i,0,0), w%block(1)%vars(:,i,0,0)
      text = text//lf//trim(line)
    enddo
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
    write(u) text//lf
    close(u)
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read without error')
    if (err /= 0) return
    call check(allocated(r%block), what//': blocks read')
    if (.not.allocated(r%block)) return
    call check(size(r%block) == 1, what//': read as one zone')
    if (size(r%block) /= 1) return
    call check(r%block(1)%name == name, what//': the whole name read back')
    call check(r%tec%node, what//': the variables at the nodes')
    call check(same_field(w, r), what//': read as one zone of 5 points, with its coordinates and values')
  end subroutine points

  ! A new file, written byte by byte (stream access): the lines are ended by the line feeds that the caller writes.
  subroutine open_bytes(file, u)
    character(len=*), intent(in)  :: file
    integer,          intent(out) :: u
    open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write')
  end subroutine open_bytes

  ! Block b of r is a line of nodes with one coordinate, x, and the variables v(1:nv,1:n) at its n nodes, as written.
  logical function line_block(r, b, x, v) result(same)
    type(orion_data), intent(in) :: r
    integer,          intent(in) :: b
    real(R8P),        intent(in) :: x(:), v(:,:)
    same = allocated(r%block)
    if (same) same = size(r%block) >= b
    if (.not.same) return
    same = allocated(r%block(b)%mesh) .and. allocated(r%block(b)%vars)
    if (same) same = all(shape(r%block(b)%mesh) == [1, size(x), 1, 1]) .and. &
                     all(shape(r%block(b)%vars) == [size(v,1), size(x), 1, 1])
    if (same) same = all(reshape(r%block(b)%mesh, [size(x)]) == x) .and. &
                     all(reshape(r%block(b)%vars, [size(v,1), size(x)]) == v)
  end function line_block

  ! The last line of a file, without a line feed. read_whole_line reads a line 512 characters at a time; with gfortran the
  ! read after the last whole piece of a last line whose length is a multiple of 512 gives the end of the file, not the end
  ! of the record. Files written byte by byte: a line of 512 characters with a line feed, then a last line of 1024 without
  ! one; a last line of 512 characters with its line feed. read_whole_line must return each line whole with iostat 0, then
  ! iostat_end. On a unit connected for formatted stream access, a last line of 512 characters without a line feed:
  ! read_whole_line must give iostat_end, after that line, whole, at most once. A Tecplot ASCII file whose only data line,
  ! x and p of a zone of 4 nodes, is its last line, of 512 characters without a line feed, must be read with every value
  ! as written.
  ! Defect: with gfortran read_whole_line returned the end of the file instead of the last line of 1024 characters, and
  ! tec_read_structured_multiblock, which reads the first data line of a zone with read_whole_line, refused the Tecplot file.
  ! Basis: Fortran 2018, 12.3.4.3 (p. 214): after the end of the file, a BACKSPACE statement may reposition the file;
  ! 12.8.2 (p. 244): it is positioned before the endfile record. A stream file has no endfile record (12.2.4, p. 211), and
  ! BACKSPACE would put it back before its last line (12.8.2), which every next read would return again.
  subroutine last_line_without_line_feed()
    character(len=1), parameter   :: lf = achar(10)
    character(len=:), allocatable :: a, b, line
    type(orion_data)              :: r
    integer                       :: u, ios, err
    logical                       :: ok
    write(*,'(A)') 'last line without a line feed'
    a = long_name(512, 'a')
    b = long_name(1024, 'b')
    ! A line of 512 characters with its line feed, then the last line, of 1024 characters, without one
    call open_bytes('rwl_512_1024_no_lf.txt', u)
    write(u) a//lf//b
    close(u)
    open(newunit=u, file='rwl_512_1024_no_lf.txt', status='old', action='read')
    call read_whole_line(u, line, ios)
    call check(ios == 0 .and. len(line) == 512 .and. line == a, 'read_whole_line: a line of 512 characters, whole')
    call read_whole_line(u, line, ios)
    call check(ios == 0 .and. len(line) == 1024 .and. line == b, &
               'read_whole_line: the last line, 1024 characters without a line feed, whole')
    if (ios == 0) then
      call read_whole_line(u, line, ios)
      call check(ios == iostat_end, 'read_whole_line: then the end of the file')
    endif
    close(u)
    ! The last line, of 512 characters, with its line feed: one line, then the end of the file
    call open_bytes('rwl_512_lf.txt', u)
    write(u) a//lf
    close(u)
    open(newunit=u, file='rwl_512_lf.txt', status='old', action='read')
    call read_whole_line(u, line, ios)
    call check(ios == 0 .and. len(line) == 512 .and. line == a, &
               'read_whole_line: the last line, 512 characters with a line feed, whole')
    if (ios == 0) then
      call read_whole_line(u, line, ios)
      call check(ios == iostat_end, 'read_whole_line: then the end of the file')
    endif
    close(u)
    ! The last line, of 512 characters, without a line feed, on a unit connected for formatted stream access: the end of the
    ! file, after that line, whole, at most once
    call open_bytes('rwl_512_no_lf_stream.txt', u)
    write(u) a
    close(u)
    open(newunit=u, file='rwl_512_no_lf_stream.txt', access='stream', form='formatted', status='old', action='read')
    call read_whole_line(u, line, ios)
    ok = ios == iostat_end
    if (ios == 0) then
      ok = len(line) == 512 .and. line == a
      call read_whole_line(u, line, ios)
      ok = ok .and. ios == iostat_end
    endif
    close(u)
    call check(ok, 'read_whole_line, formatted stream access, the last line, 512 characters without a line feed: '// &
                   'the end of the file, after the line at most once')
    ! A Tecplot file whose only data line is the last one, of 512 characters, without a line feed
    call open_bytes('tec_last_line_512_no_lf.dat', u)
    write(u) ' VARIABLES = "x" "p"'//lf//' ZONE T="u", I=4, J=1, DATAPACKING=BLOCK'//lf//repeat(' ', 497)//'1 2 3 4 5 6 7 8'
    close(u)
    err = tec_read_structured_multiblock(orion=r, filename='tec_last_line_512_no_lf.dat')
    call check(err == 0, 'Tecplot file whose only data line is the last, 512 characters without a line feed: read')
    if (err /= 0) return
    call check(line_block(r, 1, [1._R8P, 2._R8P, 3._R8P, 4._R8P], reshape([5._R8P, 6._R8P, 7._R8P, 8._R8P], [1, 4])), &
               'Tecplot file whose only data line is the last, 512 characters without a line feed: every value as written')
  end subroutine last_line_without_line_feed

  ! A token of 25 characters: ES25.16E3, two blanks before a positive value.
  function tok25(v) result(s)
    real(R8P), intent(in) :: v
    character(len=25)     :: s
    write(s,'(ES25.16E3)') v
  end function tok25

  ! A token of 23 characters: ES23.16E2, a blank before a positive value below 1E+100.
  function tok23(v) result(s)
    real(R8P), intent(in) :: v
    character(len=23)     :: s
    write(s,'(ES23.16E2)') v
  end function tok23

  ! Reads file with tec_read_structured_multiblock: it must give size(x,2) blocks, block b a line of nodes with the coordinate
  ! x(:,b) and the variables v(:,:,b) at its nodes, as written.
  subroutine check_lines_file(file, what, x, v)
    character(len=*), intent(in) :: file, what
    real(R8P),        intent(in) :: x(:,:), v(:,:,:)
    type(orion_data)             :: r
    integer                      :: err, b
    logical                      :: same
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read')
    if (err /= 0) return
    same = size(r%block) == size(x,2)
    do b = 1, size(x,2)
      if (same) same = line_block(r, b, x(:,b), v(:,:,b))
    enddo
    call check(same, what//': every value as written')
  end subroutine check_lines_file

  ! Data lines longer than 1000 characters, which the Tecplot format allows up to 32,000. Every file is written a token at a
  ! time (stream access), its values exact in the format (whole numbers, halves and quarters), and every value read must be
  ! the value written:
  ! - two BLOCK zones of 100 nodes, x and p each on one line of 100 values of 25 characters (2500 characters);
  ! - a BLOCK zone of 43 nodes whose x line has 1001 characters, its last value, 123456.75, ending at column 1001;
  ! - a BLOCK zone of 1391 nodes, x on a line of 31,998 characters and p on one of 32,000;
  ! - a POINT zone of 3 nodes, x and 49 variables, each node on one line of 1150 characters;
  ! - two zones of 4 nodes, x and p, whose last data line, not the first one of the zone, has no line feed and 512 or 1024
  !   characters.
  ! Defect: the reader kept the first 1000 characters of each data line, so it refused these files, or read the value that
  ! crosses column 1000 as 1.2345675, with the return value 0.
  ! Basis: Tecplot 360 EX 2023 R1 Data Format Guide, section 4-2 (p. 119): "The maximum length of a line in a data file is
  ! 32,000 characters"; Fortran 2018, 13.7.4 (p. 269): an A edit descriptor without a width reads as many characters as the
  ! variable holds, and 12.3.4.2 (p. 214): an advancing read leaves the file after the record, so the rest of a longer line
  ! was lost.
  subroutine long_data_lines()
    character(len=1), parameter   :: lf = achar(10)
    real(R8P), allocatable        :: x(:,:), v(:,:,:)
    character(len=:), allocatable :: names
    integer                       :: u, i, s, b
    write(*,'(A)') 'data lines longer than 1000 characters'

    ! Two BLOCK zones of 100 nodes, x and p each on one line of 2500 characters
    allocate(x(100,2), v(1,100,2))
    do b = 1, 2
      do i = 1, 100
        x(i,b) = real(i + 1000*(b - 1), R8P) + 0.25_R8P
        v(1,i,b) = real(10*i + 5000*b, R8P) + 0.5_R8P
      enddo
    enddo
    call open_bytes('tec_data_lines_2500.dat', u)
    write(u) ' VARIABLES = "x" "p"'//lf
    do b = 1, 2
      write(u) ' ZONE T="z'//itoa(b)//'", I=100, J=1, DATAPACKING=BLOCK'//lf
      do i = 1, 100
        write(u) tok25(x(i,b))
      enddo
      write(u) lf
      do i = 1, 100
        write(u) tok25(v(1,i,b))
      enddo
      write(u) lf
    enddo
    close(u)
    call check_lines_file('tec_data_lines_2500.dat', 'two BLOCK zones, x and p on lines of 2500 characters', x, v)
    deallocate(x, v)

    ! A BLOCK zone of 43 nodes: x on one line of 12 blanks and 43 values of 23 characters (1001), the last one,
    ! 1.2345675000000000E+05, ending at column 1001; p one value per line
    allocate(x(43,1), v(1,43,1))
    do i = 1, 43
      x(i,1) = real(i, R8P) + 0.5_R8P
      v(1,i,1) = real(100 + i, R8P)
    enddo
    x(43,1) = 123456.75_R8P
    call open_bytes('tec_data_line_1001.dat', u)
    write(u) ' VARIABLES = "x" "p"'//lf//' ZONE T="c", I=43, J=1, DATAPACKING=BLOCK'//lf//repeat(' ', 12)
    do i = 1, 43
      write(u) tok23(x(i,1))
    enddo
    write(u) lf
    do i = 1, 43
      write(u) itoa(100 + i)//lf
    enddo
    close(u)
    call check_lines_file('tec_data_line_1001.dat', 'BLOCK zone, a value of the x line ending at column 1001', x, v)
    deallocate(x, v)

    ! A BLOCK zone of 1391 nodes: x on one line of 5 blanks and 1391 values of 23 characters (31,998), p on one of 7 blanks
    ! and 1391 values (32,000, the longest line of the Data Format Guide)
    allocate(x(1391,1), v(1,1391,1))
    do i = 1, 1391
      x(i,1) = real(i, R8P) + 0.25_R8P
      v(1,i,1) = real(1000*i, R8P) + 0.5_R8P
    enddo
    call open_bytes('tec_data_lines_32000.dat', u)
    write(u) ' VARIABLES = "x" "p"'//lf//' ZONE T="d", I=1391, J=1, DATAPACKING=BLOCK'//lf//repeat(' ', 5)
    do i = 1, 1391
      write(u) tok23(x(i,1))
    enddo
    write(u) lf//repeat(' ', 7)
    do i = 1, 1391
      write(u) tok23(v(1,i,1))
    enddo
    write(u) lf
    close(u)
    call check_lines_file('tec_data_lines_32000.dat', 'BLOCK zone, x and p on lines of 31,998 and 32,000 characters', x, v)
    deallocate(x, v)

    ! A POINT zone of 3 nodes, x and 49 variables, each node on one line of 50 values of 23 characters (1150)
    allocate(x(3,1), v(49,3,1))
    names = ' VARIABLES = "x"'
    do s = 1, 49
      names = names//' "v'//itoa(s)//'"'
    enddo
    do i = 1, 3
      x(i,1) = real(i, R8P) + 0.75_R8P
      do s = 1, 49
        v(s,i,1) = real(100*s + i, R8P) + 0.25_R8P
      enddo
    enddo
    call open_bytes('tec_data_lines_point_1150.dat', u)
    write(u) names//lf//' ZONE T="e", I=3, J=1, DATAPACKING=POINT'//lf
    do i = 1, 3
      write(u) tok23(x(i,1))
      do s = 1, 49
        write(u) tok23(v(s,i,1))
      enddo
      write(u) lf
    enddo
    close(u)
    call check_lines_file('tec_data_lines_point_1150.dat', 'POINT zone, x and 49 variables on lines of 1150 characters', x, v)
    deallocate(x, v)

    ! A zone of 4 nodes, x and p: x on the first data line, p on the last line, without a line feed, of 512 and of 1024
    ! characters
    allocate(x(4,1), v(1,4,1))
    x(:,1) = [1._R8P, 2._R8P, 3._R8P, 4._R8P]
    v(1,:,1) = [5._R8P, 6._R8P, 7._R8P, 8._R8P]
    do i = 1, 2
      call open_bytes('tec_data_last_line_'//itoa(512*i)//'_no_lf.dat', u)
      write(u) ' VARIABLES = "x" "p"'//lf//' ZONE T="u", I=4, J=1, DATAPACKING=BLOCK'//lf//'1 2 3 4'//lf// &
               repeat(' ', 512*i - 7)//'5 6 7 8'
      close(u)
      call check_lines_file('tec_data_last_line_'//itoa(512*i)//'_no_lf.dat', &
                            'BLOCK zone, its last data line of '//itoa(512*i)//' characters without a line feed', x, v)
    enddo
    deallocate(x, v)
  end subroutine long_data_lines

  ! Writes text to file byte by byte and reads it with tec_read_structured_multiblock: the read must end with the return
  ! value 1.
  subroutine check_refused(file, text, what)
    character(len=*), intent(in) :: file, text, what
    type(orion_data)             :: r
    integer                      :: u, err
    call open_bytes(file, u)
    write(u) text
    close(u)
    err = tec_read_structured_multiblock(orion=r, filename=file)
    write(*,'(A,I0)') '        return value: ', err
    call check(err == 1, what//': refused with the return value 1')
  end subroutine check_refused

  ! An error in the coordinates of a BLOCK zone ends the read, as in a POINT zone: the return value must be 1. Files written
  ! byte by byte, a BLOCK zone of 4 nodes with x and p: a token that is not a number among the coordinates (1 2 x3 4, then
  ! 5 6 7 8); a file that ends among the coordinates (1 2 3).
  ! Defect: the reader went on to read the variables of the zone. With the token that is not a number it took them from the
  ! values that follow (4 5 6 7) and returned 0; at the end of the file it read past the end of the file, with a second
  ! message, and with gfortran returned the status of that read (5001).
  ! Basis: Fortran 2018, 12.3.4.3 (p. 214): the file shall not be positioned after the endfile record prior to data transfer.
  subroutine block_coordinate_errors()
    character(len=1), parameter :: lf = achar(10)
    character(len=*), parameter :: head = ' VARIABLES = "x" "p"'//lf//' ZONE T="w", I=4, J=1, DATAPACKING=BLOCK'//lf
    write(*,'(A)') 'an error in the coordinates of a BLOCK zone'
    call check_refused('tec_block_coordinate_not_a_number.dat', head//'1 2 x3 4'//lf//'5 6 7 8'//lf, &
                       'BLOCK zone, a token that is not a number among the coordinates')
    call check_refused('tec_block_coordinates_end_of_file.dat', head//'1 2 3'//lf, &
                       'BLOCK zone, the file ends among the coordinates')
  end subroutine block_coordinate_errors
end program tecplot_read_long_header
