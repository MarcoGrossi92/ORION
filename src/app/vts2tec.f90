program vts2tec
  use IR_Precision
  use Lib_VTK
  use Lib_Tecplot
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P), allocatable          :: x(:),y(:),z(:) ! Input geo arrays
  real(R8P), allocatable          :: v(:)           ! Input var arrays
  type(obj_tecblock), allocatable :: block(:)       ! Output tecblock
  character(256), allocatable     :: file(:)
  character(256), allocatable     :: varnames(:), varname_scalar
  character(6)                    :: input_format, output_format
  integer(I4P)                    :: Nblocks,nx1,nx2,ny1,ny2,nz1,nz2,nn,nc
  integer(I4P)                    :: b, i, j, k, n, p, err

  tec_format%binary = .true.
  input_format = 'binary'

  call command_line_argument()

  ! Find and count VTS files
  call FindFiles('vts')
  write(*,'(A,I4)')' Number of VTS files = ',Nblocks
  if (Nblocks==0) then
    write(*,'(A)')' No file to be converted'
    stop
  endif
  do i = 1, Nblocks
    write(*,'(2A)') "  ", trim(adjustl(file(i)))
  end do

  write(*,*)
  write(*,'(2A)')' - Input format  : ',trim(input_format)
  write(*,'(2A)')' - Output format : ',trim(output_format)
  write(*,*)

  allocate(block(1:Nblocks))

  ! Read VTS file
  do b = 1, Nblocks
    
    call read_variables_name(file(b),varnames)
    ! Geometry
    err = VTK_INI_XML_READ(input_format=trim(input_format),filename=trim(file(b)),mesh_topology='StructuredGrid',&
                            nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
    err = VTK_GEO_XML_READ(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=x,Y=y,Z=z)
    allocate(block(b)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
    n = 0
    do k = nz1, nz2; do j = ny1, ny2; do i = nx1, nx2
          n = n + 1
          block(b)%mesh(1,i,j,k) = x(n)
          block(b)%mesh(2,i,j,k) = y(n)
          block(b)%mesh(3,i,j,k) = z(n)
    enddo; enddo; enddo
    ! Variables field
    allocate(block(b)%vars(1:size(varnames)-1,nx1:nx2,ny1:ny2,nz1:nz2))
    do p = 1, size(varnames)-1
      if (allocated(v)) deallocate(v)
      err = VTK_VAR_XML_READ(var_location='cell', varname=trim(varnames(p+1)), NC_NN=nn, NCOMP=nc, var=v)
      if (err==0) then
        write(*,'(A)') ' - Data location : cell centers'
      else
        err = VTK_VAR_XML_READ(var_location='node', varname=trim(varnames(p+1)), NC_NN=nn, NCOMP=nc, var=v)
        if (err==0) then
          write(*,'(A)') ' - Data location : cell nodes'
          tec_format%node = .true.
        endif
      endif
      n = 0
      do k = nz1+1, nz2; do j = ny1+1, ny2; do i = nx1+1, nx2
            n = n + 1
            block(b)%vars(p,i,j,k) = v(n)
      enddo; enddo; enddo
    enddo
  enddo
  err = VTK_END_XML_READ()

  ! Write in Tecplot format
  varname_scalar = ''
  do p = 2, size(varnames)
    varname_scalar = trim(varname_scalar)//' '//trim(varnames(p))
  enddo
  err = tec_output(block_=block,time=0.d0,varnames=varname_scalar,filename='tecfile')
  if (err/=0) write(*,'(A)') 'Error during writing of tecplot file'
  
!-----------------------------------------------------------------------------------------------------------------------------------
contains

  subroutine command_line_argument()
    implicit none
    character(99):: arg
    integer(I4P) :: arg_count

    ! Get the number of command-line arguments
    arg_count = COMMAND_ARGUMENT_COUNT()

    ! Loop through each command-line argument
    do i = 1, arg_count
      ! Get the i-th command-line argument
      call GET_COMMAND_ARGUMENT(i, arg)

      ! Check for different command-line options
      if (arg == '-h' .or. arg == '--help') then
        ! Display help information
        call print_help()
        stop
      elseif (index(arg, '--out-format=') > 0) then
        ! Extract the output format from the argument
        output_format = trim(adjustl(arg(14:)))
        if (index(output_format,'ascii')>0) tec_format%binary = .false.
      elseif (index(arg, '--in-format=') > 0) then
        ! Extract the input format from the argument
        input_format = trim(adjustl(arg(13:)))
      end if
    end do

  end subroutine


  subroutine print_help
    implicit none
    write(*,*) "Usage: vts2tec [options]"
    write(*,*) "Options:"
    write(*,*) "  -h, --help              Display this help message"
    write(*,*) "  --in-format=<format>    Set the input format (e.g., binary,ascii)"
    write(*,*) "  --out-format=<format>   Set the output format (e.g., binary,ascii)"
  end subroutine print_help


  subroutine FindFiles(file_extension)
    implicit none
    character(*), intent(in)    :: file_extension
    character(256) :: command
    character(256) :: file_list_filename
    integer :: status

    ! Create a temporary file to store the list of files
    file_list_filename = "file_list.txt"

    ! Build the system command to list files with the given extension
    command = "ls *." // trim(file_extension) // " > " // trim(file_list_filename)

    ! Execute the system command
    call execute_command_line(command, wait=.true.)

    ! Open the file and read the list of files
    Nblocks = 0
    open(313, file=file_list_filename, status='old', action='read')
    do
      read(313, *, iostat=status)
      if (status /= 0) exit
      Nblocks = Nblocks + 1
    end do
    allocate(file(Nblocks))
    rewind(313)
    do i = 1, Nblocks
      read(313, *, iostat=status) file(i)
    end do
    close(313)

    ! Remove the temporary file
    call execute_command_line("rm " // trim(file_list_filename), wait=.true.)

  endsubroutine FindFiles
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram vts2tec
