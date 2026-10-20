!< Tecplot ASCII files whose VARIABLES header is longer than 1000 characters.
!< Two blocks with x y z and 200 cell-centred variables named as a solver names them (rho(1) ...
!< rho(195), u, v, w, p, T) are written with the ORION writer, so the VARIABLES line has more than
!< 2000 characters. The file must hold the whole line, and the reader must return every name and
!< every value. The reader keeps at most 512 names: 512 names are read, 513 are refused.
!< Zone headers longer than 1000 characters too: block names of 600 and 4096 characters in the zone
!< titles of ASCII and POINT files, and a title of 3000 characters (long_names).
!< Stops with a non-zero code when a check fails.
program tecplot_read_long_header
  use Lib_Tecplot
  use Lib_ORION_data
  use IR_Precision
  implicit none
  integer :: nfail

  nfail = 0
  call variables_200()
  call names_512()
  call names_513()
  call long_names()
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
end program tecplot_read_long_header
