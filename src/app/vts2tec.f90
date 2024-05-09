program vts2tec
  use IR_Precision
  use Lib_VTK
  use Lib_Tecplot
  use Lib_ORION_data
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P), allocatable          :: x(:),y(:),z(:) ! Input geo arrays
  real(R8P), allocatable          :: v(:)           ! Input var arrays
  type(orion_data)                :: data
  character(256), allocatable     :: file(:)
  character(256), allocatable     :: varnames(:), varname_scalar
  integer(I4P)                    :: Nblocks,nx1,nx2,ny1,ny2,nz1,nz2,nn,nc
  integer(I4P)                    :: b, i, j, k, n, p, err, start

  data%tec%format = 'binary'
  data%vtk%format = 'binary'

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
  write(*,'(2A)')' - Input format  : ',trim(data%tec%format)
  write(*,'(2A)')' - Output format : ',trim(data%vtk%format)
  write(*,*)

  allocate(data%block(1:Nblocks))

  ! Read VTS file
  do b = 1, Nblocks
    
    call read_variables_name(file(b),varnames,data%tec%node)
    ! Geometry
    err = VTK_INI_XML_READ(input_format=trim(data%vtk%format),filename=trim(file(b)),mesh_topology='StructuredGrid',&
                            nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
    err = VTK_GEO_XML_READ(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=x,Y=y,Z=z)
    allocate(data%block(b)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
    n = 0
    do k = nz1, nz2; do j = ny1, ny2; do i = nx1, nx2
          n = n + 1
          data%block(b)%mesh(1,i,j,k) = x(n)
          data%block(b)%mesh(2,i,j,k) = y(n)
          data%block(b)%mesh(3,i,j,k) = z(n)
    enddo; enddo; enddo
    ! Variables field
    do p = 1, size(varnames)-1
      if (allocated(v)) deallocate(v)
      if (data%tec%node) then
        err = VTK_VAR_XML_READ(var_location='node', varname=trim(varnames(p+1)), NC_NN=nn, NCOMP=nc, var=v)
        write(*,'(A)') ' - Data location : cell nodes'
        start = 0
      else
        err = VTK_VAR_XML_READ(var_location='cell', varname=trim(varnames(p+1)), NC_NN=nn, NCOMP=nc, var=v)
        write(*,'(A)') ' - Data location : cell centers'
        start = 1
      endif
      allocate(data%block(b)%vars(1:size(varnames)-1,nx1+start:nx2,ny1+start:ny2,nz1+start:nz2))
      n = 0
      do k = nz1+start, nz2; do j = ny1+start, ny2; do i = nx1+start, nx2
            n = n + 1
            data%block(b)%vars(p,i,j,k) = v(n)
      enddo; enddo; enddo
    enddo
  enddo
  err = VTK_END_XML_READ()

  ! Write in Tecplot format
  varname_scalar = ''
  do p = 2, size(varnames)
    varname_scalar = trim(varname_scalar)//' '//trim(varnames(p))
  enddo
  err = tec_write_structured_multiblock(data_=data,varnames=varname_scalar,filename='tecfile')
  if (err/=0) write(*,'(A)') 'Error during writing of tecplot file'

  write(*,*)
  write(*,'(A)')' Done!'
  
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
        data%tec%format = trim(adjustl(arg(14:)))
      elseif (index(arg, '--in-format=') > 0) then
        ! Extract the input format from the argument
        data%vtk%format = trim(adjustl(arg(13:)))
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
