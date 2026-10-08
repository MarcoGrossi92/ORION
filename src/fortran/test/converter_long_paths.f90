  program converter_long_paths
  !---------------------------------------------------------------------------------------------------------------------------------
  !> The converter (the program converter, src/fortran/app/conversion.f90) writes a .vtm file at the path given with --out-file,
  !> whatever its length. The program writes a field of one block of 3 x 2 x 1 cells with two variables at the cells to an ASCII
  !> Tecplot file and runs the converter on it, its path taken from the environment variable ORION_CONVERTER (set by ctest),
  !> with three output paths relative to the current directory: a short one in a subdirectory, one of more than 32 characters
  !> and one of more than 256, under two directories (a name in a path holds at most 255 bytes). Each time the converter must
  !> end without error, the .vtm file must be at the path given, and vtk_read_structured_multiblock must read the field back
  !> from it with the dimensions, coordinates and values of the input. Every coordinate and value is an integer, a half or a
  !> quarter, exact in every format. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_Tecplot, only: tec_write_structured_multiblock
  use Lib_VTK, only: vtk_read_structured_multiblock
  implicit none
  integer                       :: checks = 0, failures = 0
  integer                       :: n, status
  character(len=:), allocatable :: converter, dir
  type(orion_data)              :: w

  call get_environment_variable('ORION_CONVERTER', length=n, status=status)
  if (status /= 0 .or. n == 0) then
    write(*,'(A)') 'converter_long_paths: ORION_CONVERTER, the path of the converter, is not set (ctest sets it)'
    stop 1
  endif
  allocate(character(len=n) :: converter)
  call get_environment_variable('ORION_CONVERTER', value=converter)

  call fill(w)
  w%tec%format = 'ascii'
  call check(tec_write_structured_multiblock(orion=w, varnames='u v', filename='converter_in.tec') == 0, 'input file written')
  call convert('converter_out/', 'field', 'output path of 23 characters, in a subdirectory')
  dir = 'converter_out_'//repeat('d', 30)//'/'
  call convert(dir, 'field', 'output path of '//itoa(len(dir) + 9)//' characters')
  dir = 'converter_out_'//repeat('e', 120)//'/'//repeat('f', 130)//'/'
  call convert(dir, 'field', 'output path of '//itoa(len(dir) + 9)//' characters')

  write(*,'(A,I0,A,I0,A)') 'converter_long_paths: ', checks, ' checks, ', failures, ' failed'
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

  subroutine fill(w)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< One block of 3 x 2 x 1 cells: x, y and z at the nodes, two variables at the cells.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(out) :: w
  integer                       :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(1))
  w%block(1)%name = 'b1'
  w%block(1)%Ni = 3; w%block(1)%Nj = 2; w%block(1)%Nk = 1
  allocate(w%block(1)%mesh(1:3,0:3,0:2,0:1), w%block(1)%vars(1:2,1:3,1:2,1:1))
  do k = 0, 1; do j = 0, 2; do i = 0, 3
    w%block(1)%mesh(:,i,j,k) = [real(i, R8P), 0.5_R8P*j, 0.25_R8P*k + 1._R8P]
  enddo; enddo; enddo
  do k = 1, 1; do j = 1, 2; do i = 1, 3
    w%block(1)%vars(:,i,j,k) = [real(100*i + 10*j + k, R8P), -0.5_R8P*i]
  enddo; enddo; enddo
  w%tec%node = .false.
  end subroutine fill

  subroutine convert(dir, base, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Make the directory dir, run the converter from converter_in.tec to dir//base//'.vtm' and read the field back.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: dir, base, what
  type(orion_data)             :: r
  integer                      :: exitstat, cmdstat, err
  logical                      :: exists, same
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p '//dir, exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, what//': directory made')
  call execute_command_line("'"//converter//"' --in-file=converter_in.tec --out-file="//dir//base//'.vtm > '// &
                            dir//'converter_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, what//': the converter ends without error')
  inquire(file=dir//base//'.vtm', exist=exists)
  call check(exists, what//': the .vtm file is at the path given')
  if (.not.exists) return
  err = vtk_read_structured_multiblock(orion=r, vtmpath=dir//base, vtspath=dir)
  call check(err == 0, what//': the field read back without error')
  if (err /= 0) return
  same = size(r%block) == 1
  if (same) same = r%block(1)%Ni == 3 .and. r%block(1)%Nj == 2 .and. r%block(1)%Nk == 1
  if (same) same = allocated(r%block(1)%mesh) .and. allocated(r%block(1)%vars)
  if (same) same = all(shape(r%block(1)%mesh) == shape(w%block(1)%mesh)) .and. &
                   all(shape(r%block(1)%vars) == shape(w%block(1)%vars))
  if (same) same = all(r%block(1)%mesh == w%block(1)%mesh) .and. all(r%block(1)%vars == w%block(1)%vars)
  call check(same, what//': the field read back with the dimensions, coordinates and values of the input')
  end subroutine convert
  endprogram converter_long_paths
