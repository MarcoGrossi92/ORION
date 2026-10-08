  program tecplot_long_names
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Block names and file paths of any length in the Tecplot writers and readers. A block name (orion%block(b)%name) holds 4096
  !> characters, and the writers put it in the zone header as the zone title (T =). The program writes a block of 3 x 2 x 1
  !> cells to an ASCII file with tec_write_structured_multiblock, with a name of 600 characters and with one of 4096: the zone
  !> header must hold the whole name and the I, J and K of the zone, and tec_read_structured_multiblock must read the block back
  !> with its dimensions, coordinates and values. It reads a file written by hand whose zone header has a title of 3000
  !> characters, in quotes, on its first line and I, J and K on the next one, and whose last line has no line feed. With TecIO
  !> it writes .szplt files and reads them back with tec_read_structured_multiblock: the names of the blocks are the zone
  !> titles as written, with no character after them, and a name of 600 characters comes back as its first 128, the part of a
  !> zone title that Tecplot keeps (Tecplot 360 EX 2023 R1 Data Format Guide, section 4-3.2, p. 123) and that TecIO writes;
  !> and a field written under a path of 277 characters, two directories made under the current one, is read back. Last, as
  !> a reader that loses the dimensions of a zone can stop the program, it writes a zone of 5 points with
  !> tec_write_points_multivars, with the two names, and reads it back with tec_read_points_multivars. Every coordinate and
  !> value is an integer, a half or a quarter, exact in every format. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_Tecplot, only: tec_write_structured_multiblock, tec_read_structured_multiblock, tec_write_points_multivars, &
                         tec_read_points_multivars
  implicit none
  integer :: checks = 0, failures = 0

  call ascii_block(600)
  call ascii_block(4096)
  call ascii_by_hand
#ifdef TECIO
  call szplt_names
  call szplt_long_path
#endif
  call points(600)
  call points(4096)

  write(*,'(A,I0,A,I0,A)') 'tecplot_long_names: ', checks, ' checks, ', failures, ' failed'
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

  pure function itoa(n) result(s)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The digits of n.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in)           :: n
  character(len=:), allocatable :: s
  character(len=16)             :: buffer
  !---------------------------------------------------------------------------------------------------------------------------------

  write(buffer,'(I0)') n
  s = trim(buffer)
  end function itoa

  pure function long_name(n, first) result(name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A name of n characters, lower-case letters and digits from the character first on: no blank, comma, quote or equal sign.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer,   intent(in)          :: n
  character, intent(in)          :: first
  character(len=n)               :: name
  character(len=36), parameter   :: c = 'abcdefghijklmnopqrstuvwxyz0123456789'
  integer                        :: i, k
  !---------------------------------------------------------------------------------------------------------------------------------

  k = index(c, first)
  do i = 1, n
    name(i:i) = c(mod(k + i - 2, 36) + 1:mod(k + i - 2, 36) + 1)
  enddo
  end function long_name

  function file_text(file) result(text)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The whole content of a file, line feeds included.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: file
  character(len=:), allocatable :: text
  integer                       :: u, n
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=file, access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: text)
  if (n > 0) read(u) text
  close(u)
  end function file_text

  subroutine fill(w, names)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< One block of 3 x 2 x 1 cells for each name: x, y and z at the nodes, two variables at the cells.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(out) :: w
  character(len=*), intent(in)  :: names(:)
  integer                       :: b, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

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
  end subroutine fill

  logical function same_field(w, r) result(same)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< r holds the blocks of w: as many, and each with the same dimensions, coordinates and values.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: w, r
  integer                      :: b
  !---------------------------------------------------------------------------------------------------------------------------------

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

  subroutine ascii_block(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A block with a name of n characters, written to an ASCII file with tec_write_structured_multiblock and read back.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in)           :: n
  type(orion_data)              :: w, r
  character(len=:), allocatable :: name, file, what
  integer                       :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  name = long_name(n, 'a')
  file = 'tec_long_name_'//itoa(n)//'.tec'
  what = 'ASCII file, block name of '//itoa(n)//' characters'
  call fill(w, [name])
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

  subroutine ascii_by_hand
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A file written by hand: the zone header has a title of 3000 characters, in quotes, on its first line and I, J and K on
  !< the next one; the values follow one per line, in the order of a BLOCK zone, and the last line has no line feed.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w, r
  character(len=*), parameter   :: file = 'tec_long_title_by_hand.tec'
  character(len=*), parameter   :: what = 'ASCII file written by hand, title of 3000 characters'
  character(len=1), parameter   :: lf = achar(10)
  character(len=:), allocatable :: text
  character(len=32)             :: value
  integer                       :: u, err, d, s, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  call fill(w, ['hand'])
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

  subroutine points(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A zone of 5 points with a name of n characters, written with tec_write_points_multivars and read back with
  !< tec_read_points_multivars.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, intent(in)           :: n
  type(orion_data)              :: w, r
  character(len=:), allocatable :: name, file, what
  integer                       :: err, i
  logical                       :: one_zone
  !---------------------------------------------------------------------------------------------------------------------------------

  name = long_name(n, 'p')
  file = 'tec_long_name_points_'//itoa(n)//'.dat'
  what = 'POINT file, zone name of '//itoa(n)//' characters'
  allocate(w%block(1))
  w%block(1)%name = name
  w%block(1)%Ni = 5; w%block(1)%Nj = 1; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(1:1,1:5,1:1,1:1), w%block(1)%vars(1:2,1:5,1:1,1:1))
  do i = 1, 5
    w%block(1)%mesh(1,i,1,1) = 0.25_R8P*i
    w%block(1)%vars(:,i,1,1) = [10._R8P*i, -0.5_R8P*i]
  enddo
  err = tec_write_points_multivars(orion=w, varnames='"x" "u" "v"', filename=file)
  call check(err == 0, what//': written without error')
  call check(index(file_text(file), ' ZONE  T = '//name//', I=5, F=POINT') > 0, &
             what//': the zone header holds the whole name and I')
  err = tec_read_points_multivars(orion=r, nvar=2, filename=file)
  call check(err == 0, what//': read back without error')
  one_zone = allocated(r%block)
  if (one_zone) one_zone = size(r%block) == 1
  if (one_zone) one_zone = r%block(1)%Ni == 5 .and. r%block(1)%Nj == 1 .and. r%block(1)%Nk == 1
  call check(one_zone, what//': read back as one zone of 5 points')
  if (.not.one_zone) return
  call check(all(r%block(1)%mesh(1,:,1,1) == w%block(1)%mesh(1,:,1,1)) .and. &
             all(r%block(1)%vars(:,:,1,1) == w%block(1)%vars(:,:,1,1)), what//': read back with its coordinates and values')
  end subroutine points

#ifdef TECIO
  subroutine szplt_names
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Two blocks named a1 and with a name of 600 characters, written to a .szplt file and read back: the names are the zone
  !< titles as written, with no character after them, and the name of 600 characters comes back as its first 128.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)            :: w, r
  character(len=*), parameter :: file = 'tec_long_names.szplt', what = '.szplt file'
  character(len=600)          :: long
  integer                     :: err
  !---------------------------------------------------------------------------------------------------------------------------------

  long = long_name(600, 's')
  call fill(w, [character(len=600) :: 'a1', long])
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

  subroutine szplt_long_path
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A field written to a .szplt file under a path of 277 characters, two directories made under the current one (a name in
  !< a path holds at most 255 bytes), and read back.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: w, r
  character(len=:), allocatable :: dir, file, what
  integer                       :: err, exitstat, cmdstat
  logical                       :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  dir = 'tec_long_path_'//long_name(120, 'c')//'/'//long_name(130, 'd')
  file = dir//'/field.szplt'
  what = '.szplt file under a path of '//itoa(len(file))//' characters'
  call execute_command_line('mkdir -p '//dir, exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, what//': directories made')
  call fill(w, ['z1'])
  w%tec%format = 'binary'
  err = tec_write_structured_multiblock(orion=w, varnames='u v', filename=file)
  inquire(file=file, exist=exists)
  call check(err == 0 .and. exists, what//': written without error')
  err = tec_read_structured_multiblock(orion=r, filename=file)
  call check(err == 0, what//': read back without error')
  if (err /= 0) return
  call check(same_field(w, r), what//': read back with its dimensions, coordinates and values')
  end subroutine szplt_long_path
#endif
  endprogram tecplot_long_names
