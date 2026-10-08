!< Tecplot binary (.szplt) file whose list of variable names is longer than 1000 characters.
!< Two blocks with x y z and 200 cell-centred variables named as a solver names them (rho(1) ...
!< rho(195), u, v, w, p, T) are written with the ORION writer and read back: the reader must
!< return every name and every value, also in the header-only pass and when only the second zone
!< is read (zone_mask), as a parallel caller does. TecIO build.
!< Zone titles longer than 128 characters and paths of 256 characters or more (long_names).
!< Stops with a non-zero code when a check fails.
program tecplot_read_szplt_long_header
  use Lib_Tecplot
  use Lib_ORION_data
  use IR_Precision
  implicit none
  integer :: nfail

  nfail = 0
  call variables_200()
  call long_names()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_szplt_long_header: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_szplt_long_header: all checks passed'

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

  ! The names as one string, each between double quotes, as a solver passes them to the writer.
  function quoted_list(names) result(list)
    character(len=*), intent(in)  :: names(:)
    character(len=:), allocatable :: list
    integer :: s
    list = '"'//trim(names(1))//'"'
    do s = 2, size(names)
      list = list//' "'//trim(names(s))//'"'
    enddo
  end function quoted_list

  ! Two blocks of 3 x 2 x 2 and 2 x 3 x 1 cells; x y z nodal, the nv variables cell-centred.
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

  ! Compares the blocks listed in blocks with what was written.
  subroutine check_values(w, d, blocks, what)
    type(orion_data), intent(in) :: w, d
    integer,          intent(in) :: blocks(:)
    character(len=*), intent(in) :: what
    integer :: n, b
    logical :: same
    call check(size(d%block) == 2, 'two blocks')
    if (size(d%block) /= 2) return
    same = .true.
    do n = 1, size(blocks)
      b = blocks(n)
      same = allocated(d%block(b)%mesh) .and. allocated(d%block(b)%vars)
      if (.not. same) exit
      same = size(d%block(b)%mesh,1) == 3
      if (.not. same) exit
      same = all(lbound(d%block(b)%mesh) == lbound(w%block(b)%mesh)) .and. &
             all(ubound(d%block(b)%mesh) == ubound(w%block(b)%mesh)) .and. &
             all(lbound(d%block(b)%vars) == lbound(w%block(b)%vars)) .and. &
             all(ubound(d%block(b)%vars) == ubound(w%block(b)%vars))
      if (.not. same) exit
      same = all(d%block(b)%mesh == w%block(b)%mesh) .and. all(d%block(b)%vars == w%block(b)%vars)
      if (.not. same) exit
    enddo
    call check(same, what)
  end subroutine check_values

  subroutine variables_200()
    type(orion_data) :: w, d, h, z
    integer :: err, b
    logical :: counts
    write(*,'(A)') '200 variables: x y z nodal, rho(1) ... rho(195) u v w p T cell-centred (ORION writer)'
    call fill(w, 200)
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames=quoted_list(solver_names(200)), &
                                          filename='long_header_200.szplt')
    call check(err == 0, 'written')
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_200.szplt')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(200))
    call check_values(w, d, [1, 2], 'every coordinate and every value of the two blocks as written')

    write(*,'(A)') 'header-only pass'
    h%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=h, filename='long_header_200.szplt', dims_only=.true.)
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(h, solver_names(200))
    counts = size(h%block) == 2
    if (counts) then
      do b = 1, 2
        counts = counts .and. h%block(b)%Ni == w%block(b)%Ni .and. h%block(b)%Nj == w%block(b)%Nj .and. &
                 h%block(b)%Nk == w%block(b)%Nk
      enddo
    endif
    call check(counts, 'the cell counts of the two blocks')

    write(*,'(A)') 'second zone only (zone_mask)'
    z%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=z, filename='long_header_200.szplt', zone_mask=[.false., .true.])
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(z, solver_names(200))
    call check_values(w, z, [2], 'every coordinate and every value of the second block as written')
  end subroutine variables_200


  ! Block names and paths of any length in .szplt files. Two blocks named a1 and with a name of 600
  ! characters are written to a .szplt file and read back with tec_read_structured_multiblock: the
  ! names of the blocks are the zone titles as written, with no character after them, and the name
  ! of 600 characters comes back as its first 128, the part of a zone title that Tecplot keeps
  ! (Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2, p. 123) and that TecIO writes. A field
  ! written under a path of 277 characters, two directories made under the current one, is read
  ! back. Every coordinate and value is an integer, a half or a quarter.
  ! Defect: tec_read_szplt handed the path to TecIO in 256 characters, so a longer path lost its
  ! C_NULL_CHAR and the file was not read, and it ended every zone title with char(0).
  ! Basis: TecIO takes the file name as a string ended by C_NULL_CHAR, as its example builds it
  ! (lib/TecIO/examples/rewriteszl/rewriteszlf90.f90, lines 111-117); Tecplot 360 EX 2023 R1 Data
  ! Format Guide, section 4-3.2 (p. 123): a zone title of up to 128 characters.
  subroutine long_names()
    call szplt_names
    call szplt_long_path
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

  ! Two blocks named a1 and with a name of 600 characters, written to a .szplt file and read back: the names are the zone
  ! titles as written, with no character after them, and the name of 600 characters comes back as its first 128.
  subroutine szplt_names
    type(orion_data)            :: w, r
    character(len=*), parameter :: file = 'tec_long_names.szplt', what = '.szplt file'
    character(len=600)          :: long
    integer                     :: err
    long = long_name(600, 's')
    call fill_names(w, [character(len=600) :: 'a1', long])
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='u v', filename=file)
    call check(err == 0, what//': written without error')
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read back without error')
    if (err /= 0) return
    call check(same_field(w, r), what//': read back with its dimensions, coordinates and values')
    if (.not.same_field(w, r)) return
    call check(r%block(1)%name == 'a1' .and. len_trim(r%block(1)%name) == 2, &
               what//': the name a1 read back as a1, with no character after it')
    call check(r%block(2)%name == long(1:128) .and. len_trim(r%block(2)%name) == 128, &
               what//': the name of 600 characters read back as its first 128')
  end subroutine szplt_names

  ! A field written to a .szplt file under a path of 277 characters, two directories made under the current one (a name in
  ! a path holds at most 255 bytes), and read back.
  subroutine szplt_long_path
    type(orion_data)              :: w, r
    character(len=:), allocatable :: dir, file, what
    integer                       :: err, exitstat, cmdstat
    logical                       :: exists
    dir = 'tec_long_path_'//long_name(120, 'c')//'/'//long_name(130, 'd')
    file = dir//'/field.szplt'
    what = '.szplt file under a path of '//itoa(len(file))//' characters'
    call execute_command_line('mkdir -p '//dir, exitstat=exitstat, cmdstat=cmdstat)
    call check(cmdstat == 0 .and. exitstat == 0, what//': directories made')
    call fill_names(w, ['z1'])
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames='u v', filename=file)
    inquire(file=file, exist=exists)
    call check(err == 0 .and. exists, what//': written without error')
    err = tec_read_structured_multiblock(orion=r, filename=file)
    call check(err == 0, what//': read back without error')
    if (err /= 0) return
    call check(same_field(w, r), what//': read back with its dimensions, coordinates and values')
  end subroutine szplt_long_path
end program tecplot_read_szplt_long_header
