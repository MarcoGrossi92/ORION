module functions
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK
  use Lib_Tecplot
  use Lib_PLOT3D
  implicit none


contains


  pure function extract_path(inpath) result(outpath)
    implicit none
    integer :: index_start
    ! Paths of any length
    character(len=*), intent(in)      :: inpath
    character(len=:), allocatable     :: outpath

    ! Find the last occurrence of '/' in the string
    index_start = len_trim(inpath)
    do while (index_start > 0)
      if (inpath(index_start:index_start) == '/') exit
      index_start = index_start - 1
    end do
    ! Extract the folder path
    outpath = inpath(1:index_start)
    if (outpath=='') outpath = './'

  end function extract_path


  !> The extension of a file name: the text after the last dot of its last component, '' when it has none. The converter takes
  !> the format of a file from it: a dot in a directory of the path (`cases.dat/out.vtm`) is not an extension.
  pure function file_extension(file) result(ext)
    implicit none
    character(len=*), intent(in)  :: file
    character(len=:), allocatable :: ext
    integer :: n, slash, dot

    n = len_trim(file)
    slash = index(file(1:n), '/', back=.true.)
    dot = index(file(1:n), '.', back=.true.)
    if (dot > slash) then
      ext = file(dot+1:n)
    else
      ext = ''
    endif

  end function file_extension


  pure function lower(text) result(out)
    implicit none
    character(len=*), intent(in) :: text
    character(len=len(text))     :: out
    integer :: i, c

    out = text
    do i = 1, len(text)
      c = iachar(text(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
    enddo

  end function lower


  !> The value of the attribute `key` (name with its `=`) of an XML line, in double or in single quotes; '' when absent.
  function attribute_of(line, key) result(value)
    implicit none
    character(len=*), intent(in)  :: line, key
    character(len=:), allocatable :: value
    integer :: p, q

    value = ''
    p = index(line, key)
    if (p == 0) return
    p = p + len(key)
    if (p > len(line)) return
    if (line(p:p) /= '"' .and. line(p:p) /= "'") return
    q = index(line(p+1:), line(p:p))
    if (q == 0) return
    value = line(p+1:p+q-1)

  end function attribute_of


  !> The format of the block files of a .vtm file, as the VTK reader names it ('ascii', 'binary' or 'raw'): the format of the
  !> first array of the first block file listed in the .vtm file (ORION writes every array of its files in the same format).
  !> '' when the .vtm file or the block file cannot be read, or the block file holds no array.
  function vtk_block_format(vtmfile, vtspath) result(format)
    use strings, only: read_whole_line
    implicit none
    character(len=*), intent(in)  :: vtmfile, vtspath
    character(len=:), allocatable :: format
    character(len=:), allocatable :: line, name, value
    integer :: u, ios

    format = ''
    name = ''
    open(newunit=u, file=vtmfile, action='read', status='old', iostat=ios)
    if (ios /= 0) return
    do
      call read_whole_line(u, line, ios)
      if (ios /= 0) exit
      name = attribute_of(line, 'file=')
      if (len(name) > 0) exit
    enddo
    close(u)
    if (len(name) == 0) return

    open(newunit=u, file=vtspath//name, action='read', status='old', iostat=ios)
    if (ios /= 0) return
    do
      call read_whole_line(u, line, ios)
      if (ios /= 0) exit
      if (index(line, '<DataArray') == 0) cycle
      value = attribute_of(line, 'format=')
      if (len(value) == 0) cycle
      select case (lower(value))
      case ('ascii', 'binary')
        format = lower(value)
      case ('appended')
        format = 'raw'
      end select
      exit
    enddo
    close(u)

  end function vtk_block_format


  !> Read the .vtm file and the block files it lists, which are looked for from the directory of the .vtm file. `format` is
  !> the format of the block files ('ascii', 'binary' or 'raw'); '' takes it from the files (vtk_block_format).
  subroutine read_vtm(orion,file,format)
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file, format
    integer :: E_IO, n
    character(len=:), allocatable :: path

    n = len_trim(file)
    if (n < 4) then
      E_IO = 1
    elseif (file(n-3:n) /= '.vtm') then
      E_IO = 1
    else
      ! vtk_read_structured_multiblock takes the name of the .vtm file without its extension, and the directory of the block files
      path = extract_path(file)
      orion%vtk%format = format
      if (len_trim(format) == 0) then
        orion%vtk%format = vtk_block_format(file, path)
        if (len_trim(orion%vtk%format) == 0) orion%vtk%format = 'binary'
      endif
      E_IO = vtk_read_structured_multiblock(orion=orion,vtmpath=file(1:n-4),vtspath=path)
    endif
    if (E_IO/=0) then
      write(*,'(A)')' ERROR read VTK files'
      stop 1
    endif
    ! The reader tells where the variables are in orion%vtk%node; the Tecplot writer takes it from orion%tec%node
    orion%tec%node = orion%vtk%node

  end subroutine read_vtm


  subroutine write_vtm(orion,file,varname_scalar,format)
    use strings, only: parse
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file, format
    character(len=*), intent(inout) :: varname_scalar
    integer :: E_IO
    ! Of any length: the output path without its extension, and its directory
    character(len=:), allocatable :: name
    character(len=:), allocatable :: path

    if (len_trim(file) < 4) then
      E_IO = 1
    elseif (file(len_trim(file)-3:len_trim(file)) /= '.vtm') then
      E_IO = 1
    else
      orion%vtk%format = format
      name = file(1:len_trim(file)-4)
      path = extract_path(file)
      E_IO = vtk_write_structured_multiblock(orion=orion,vtspath=trim(path),vtmpath=trim(name),varnames=varname_scalar)
    endif
    if (E_IO/=0) then
      write(*,'(A)')' ERROR write VTK files'
      stop 1
    endif

  end subroutine write_vtm


  subroutine read_tec(orion,file)
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file
    integer :: E_IO

    E_IO = tec_read_structured_multiblock(orion=orion,filename=trim(file))
    if (E_IO/=0) then
      write(*,'(A)')' ERROR read Tecplot file'
      stop 1
    endif
    ! The reader tells where the variables are in orion%tec%node; the VTK writer takes it from orion%vtk%node
    orion%vtk%node = orion%tec%node

  end subroutine read_tec


  subroutine write_tec(orion,file,varname_scalar)
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file,varname_scalar
    integer :: E_IO

    E_IO = tec_write_structured_multiblock(orion=orion,varnames=varname_scalar,filename=trim(file))
    if (E_IO/=0) then
      write(*,'(A)')' ERROR write Tecplot file'
      stop 1
    endif

  end subroutine write_tec


  subroutine read_p3d(orion,file)
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file
    character(len=:), allocatable   :: msg
    integer :: E_IO

    E_IO = p3d_read_multiblock(orion=orion,filename=trim(file),errmsg=msg)
    if (E_IO/=0) then
      write(*,'(A)')' ERROR read PLOT3D file: '//msg
      stop 1
    endif

  end subroutine read_p3d


  subroutine write_p3d(orion,file)
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: file
    integer :: E_IO

    ! A PLOT3D grid file holds the grid only
    if (allocated(orion%block(1)%vars)) then
      if (size(orion%block(1)%vars,1) > 0) &
        write(*,'(A)')' WARNING the variables are not written: a PLOT3D grid file holds the grid only'
    endif
    E_IO = p3d_write_multiblock(orion=orion,filename=trim(file))
    if (E_IO/=0) then
      write(*,'(A)')' ERROR write PLOT3D file'
      stop 1
    endif

  end subroutine write_p3d


  !> The names of the variables of the first block, for the writers: those of the input, with the characters that a list of
  !> names cannot hold (blanks, commas, tabs, quotes) replaced by underscores, or var1, var2, ... when the input has no names
  !> (a VTK file written by another program) or a blank one. None for a mesh only (a PLOT3D file, for instance).
  function variable_names(orion) result(names)
    implicit none
    type(orion_data), intent(in)  :: orion
    character(len=:), allocatable :: names
    character(len=:), allocatable :: name
    integer :: b, nv, ndir
    logical :: stored

    names = ''
    if (.not.allocated(orion%block(1)%vars)) return
    nv = size(orion%block(1)%vars,1)
    ndir = size(orion%block(1)%mesh,1)
    ! varnames holds the names of the coordinates, then those of the variables
    stored = .false.
    if (allocated(orion%varnames)) stored = size(orion%varnames) == ndir + nv
    do b = 1, nv
      name = ''
      if (stored) name = trim(adjustl(orion%varnames(ndir+b)))
      if (len(name) == 0) then
        name = 'var'//trim(str(.true.,b))
      else
        name = list_safe(name)
      endif
      names = names//' '//name
    enddo

  end function variable_names


  !> The text with its blanks, commas, tabs and quotes replaced by underscores.
  pure function list_safe(text) result(out)
    implicit none
    character(len=*), intent(in) :: text
    character(len=len(text))     :: out
    integer :: i

    out = text
    do i = 1, len(text)
      if (scan(text(i:i), ' ,"'''//achar(9)) > 0) out(i:i) = '_'
    enddo

  end function list_safe

end module functions



program main
  use functions
  use Lib_ORION_data
  implicit none
  type(orion_data)              :: data
  integer                       :: b
  ! Deferred length so any path fits, and initialised so the extension tests
  ! below never read an undefined string.
  character(len=:), allocatable :: infile, outfile
  ! The formats asked for with --in-format= and --out-format=, in lower case; '' when not given
  character(len=:), allocatable :: informat, outformat
  character(len=:), allocatable :: varname_scalar

  infile    = ''
  outfile   = ''
  informat  = ''
  outformat = ''

  data%tec%format = 'binary'
  data%vtk%format = 'binary'
  data%p3d%format = 'ascii'

  call command_line_argument()

  ! Fail loudly rather than reading the input and writing nothing.
  if (len_trim(infile)==0) then
    write(*,'(A)')' ERROR no input file given (--in-file=<file>)'
    stop 1
  endif
  if (len_trim(outfile)==0) then
    write(*,'(A)')' ERROR no output file given (--out-file=<file>)'
    stop 1
  endif
  ! An unrecognised extension used to fall through every branch: the input was
  ! never read and the first size() query below then dereferenced an
  ! unallocated block.
  if (.not.known_format(infile)) then
    write(*,'(2A)')' ERROR unsupported input format: ',trim(infile)
    write(*,'(A)') ' Supported extensions: .dat .tec .szplt .p3d .vtm'
    stop 1
  endif
  if (.not.known_format(outfile)) then
    write(*,'(2A)')' ERROR unsupported output format: ',trim(outfile)
    write(*,'(A)') ' Supported extensions: .dat .tec .szplt .p3d .vtm'
    stop 1
  endif
  call check_formats()

  write(*,*)
  write(*,'(2A)')' - Input file  : ',trim(infile)
  write(*,'(2A)')' - Output file : ',trim(outfile)
  write(*,*)

  select case (file_extension(infile))
  case ('dat', 'tec', 'szplt')
    call read_tec(data,infile)
  case ('p3d')
    call read_p3d(data,infile)
  case ('vtm')
    call read_vtm(data,infile,informat)
  end select

  varname_scalar = variable_names(data)
  do b = 1, size(data%block)
    data%block(b)%name = 'Block'//trim(str(.true.,b))
  enddo

  select case (file_extension(outfile))
  case ('dat', 'tec', 'szplt')
    call write_tec(data,outfile,varname_scalar)
  case ('p3d')
    call write_p3d(data,outfile)
  case ('vtm')
    call write_vtm(data,outfile,varname_scalar,outformat)
  end select

  write(*,*)
  write(*,'(A)')' Done!'


contains


  !> Does the file name carry an extension the converter handles?
  logical function known_format(file)
    implicit none
    character(len=*), intent(in) :: file

    select case (file_extension(file))
    case ('dat', 'tec', 'szplt', 'p3d', 'vtm')
      known_format = .true.
    case default
      known_format = .false.
    end select

  end function known_format


  !> The formats asked for with --in-format= and --out-format=, which only some files have: the block files of a .vtm file
  !> (ascii, binary or raw: an input takes it from the files when it is not given, an output is binary) and a PLOT3D output
  !> (ascii only). The other formats follow the extension of the file. An option the file cannot use is refused, not ignored.
  subroutine check_formats()
    implicit none

    if (len(informat) > 0) then
      if (file_extension(infile) == 'vtm') then
        if (informat /= 'ascii' .and. informat /= 'binary' .and. informat /= 'raw') then
          write(*,'(A)')' ERROR unsupported input format: '//informat//' (a .vtm file: ascii, binary or raw)'
          stop 1
        endif
      endif
    endif
    if (len(outformat) > 0) then
      if (file_extension(outfile) == 'vtm') then
        if (outformat /= 'ascii' .and. outformat /= 'binary' .and. outformat /= 'raw') then
          write(*,'(A)')' ERROR unsupported output format: '//outformat//' (a .vtm file: ascii, binary or raw)'
          stop 1
        endif
      elseif (file_extension(outfile) == 'p3d') then
        if (outformat /= 'ascii') then
          write(*,'(A)')' ERROR unsupported output format: '//outformat//' (a .p3d file: ascii only)'
          stop 1
        endif
      endif
    endif
    if (len(outformat) == 0 .and. file_extension(outfile) == 'vtm') outformat = 'binary'
    ! The Tecplot writer takes its format from the extension of the output file; the readers, from that of the input file
    select case (file_extension(outfile))
    case ('dat', 'tec')
      data%tec%format = 'ascii'
    case ('szplt')
      data%tec%format = 'binary'
    end select

  end subroutine check_formats


  subroutine command_line_argument()
    implicit none
    ! Deferred length: a fixed-size buffer silently truncated any argument
    ! longer than itself, so a long --out-file= path lost its extension, no
    ! write branch matched, and the run read the whole input and reported
    ! "Done!" having written nothing.
    character(len=:), allocatable :: arg
    integer :: arg_count, i, arglen

    ! Get the number of command-line arguments
    arg_count = COMMAND_ARGUMENT_COUNT()

    if (arg_count==0) then
      call print_help
      stop
    endif

    ! Loop through each command-line argument
    do i = 1, arg_count
      ! Get the i-th command-line argument, sized to fit
      call GET_COMMAND_ARGUMENT(i, length=arglen)
      if (allocated(arg)) deallocate(arg)
      allocate(character(len=arglen) :: arg)
      call GET_COMMAND_ARGUMENT(i, arg)

      ! Check for different command-line options
      if (arg == '-h' .or. arg == '--help') then
        ! Display help information
        call print_help()
        stop

      elseif (index(arg, '--out-file=') == 1) then
        outfile = trim(adjustl(arg(12:)))

      elseif (index(arg, '--in-file=') == 1) then
        infile = trim(adjustl(arg(11:)))

      elseif (index(arg, '--out-format=') == 1) then
        outformat = lower(trim(adjustl(arg(14:))))

      elseif (index(arg, '--in-format=') == 1) then
        informat = lower(trim(adjustl(arg(13:))))

      end if

    end do

  end subroutine


  subroutine print_help
    implicit none
    write(*,*) "File format converter. Supported formats: PLOT3D, Tecplot, VTK"
    write(*,*)
    write(*,*) "Usage: ORION --in-file=<file> --out-file=<file> [options]"
    write(*,*) "  The format of a file is that of its extension: .dat .tec .szplt (Tecplot), .p3d (PLOT3D), .vtm (VTK)"
    write(*,*)
    write(*,*) "Options:"
    write(*,*) "  -h, --help              Display this help message"
    write(*,*) "  --in-file=<file>        Choose the input file path"
    write(*,*) "  --in-format=<format>    Format of the block files of a .vtm input: ascii, binary or raw"
    write(*,*) "                          (default: taken from the files)"
    write(*,*) "  --out-file=<file>       Choose the output file path"
    write(*,*) "  --out-format=<format>   Format of the block files of a .vtm output: ascii, binary or raw"
    write(*,*) "                          (default: binary); of a .p3d output: ascii (the default)"
  end subroutine print_help

endprogram main
