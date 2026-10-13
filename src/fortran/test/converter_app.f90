  program converter_app
  !---------------------------------------------------------------------------------------------------------------------------------
  !> The converter (the program converter, src/fortran/app/conversion.f90) writes a .vtm file at the path given with --out-file,
  !> whatever its length. The program writes a field of one block of 3 x 2 x 1 cells with two variables at the cells to an ASCII
  !> Tecplot file and runs the converter on it, its path taken from the environment variable ORION_CONVERTER (set by ctest),
  !> with three output paths relative to the current directory: a short one in a subdirectory, one of more than 32 characters
  !> and one of more than 256, under two directories (a name in a path holds at most 255 bytes). Each time the converter must
  !> end without error, the .vtm file must be at the path given, and vtk_read_structured_multiblock must read the field back
  !> from it with the dimensions, coordinates and values of the input. Every coordinate and value is an integer, a half or a
  !> quarter, exact in every format. The same block with its two variables at the nodes, converted from an ASCII Tecplot file to
  !> a .vtm file, must be read back with its variables at the nodes. An input that the converter cannot read must end it with a
  !> nonzero exit status. Then a binary Tecplot file (binary_files): with TecIO the converter writes the input to a .szplt file
  !> and converts that file to a .vtm one, as above; without TecIO it must end with a nonzero exit status. Exit status 0 when
  !> every check passes, 1 otherwise.
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
    write(*,'(A)') 'converter_app: ORION_CONVERTER, the path of the converter, is not set (ctest sets it)'
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
  call node_location
  call error_status
  call binary_files

  write(*,'(A,I0,A,I0,A)') 'converter_app: ', checks, ' checks, ', failures, ' failed'
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

  subroutine convert(dir, base, what, infile)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Make the directory dir, run the converter from infile (converter_in.tec when absent) to dir//base//'.vtm' and read the field
  !< back.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)           :: dir, base, what
  character(len=*), intent(in), optional :: infile
  type(orion_data)                       :: r
  integer                                :: exitstat, cmdstat, err
  logical                                :: exists, same
  character(len=:), allocatable          :: input
  !---------------------------------------------------------------------------------------------------------------------------------

  input = 'converter_in.tec'
  if (present(infile)) input = infile
  call execute_command_line('mkdir -p '//dir, exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, what//': directory made')
  call execute_command_line("'"//converter//"' --in-file="//input//" --out-file="//dir//base//'.vtm > '// &
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

  subroutine node_location
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Variables at the nodes stay at the nodes: the block of fill with its two variables at the nodes instead of the cells,
  !< written to an ASCII Tecplot file and converted to a .vtm file in a subdirectory, is read back from it with its variables
  !< at the nodes (orion%vtk%node set) and the dimensions, coordinates and values of the input.
  !< Defect: the Tecplot reader tells where the variables are in orion%tec%node and the VTK writer takes orion%vtk%node, which
  !< the converter left unset: the values at the nodes were written as values at the cells, without an error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)             :: wn, r
  integer                      :: i, j, k, exitstat, cmdstat, err
  logical                      :: same
  !---------------------------------------------------------------------------------------------------------------------------------

  call fill(wn)
  deallocate(wn%block(1)%vars)
  allocate(wn%block(1)%vars(1:2,0:3,0:2,0:1))
  do k = 0, 1; do j = 0, 2; do i = 0, 3
    wn%block(1)%vars(:,i,j,k) = [real(100*i + 10*j + k, R8P), -0.5_R8P*i]
  enddo; enddo; enddo
  wn%tec%node = .true.
  wn%tec%format = 'ascii'
  call check(tec_write_structured_multiblock(orion=wn, varnames='u v', filename='converter_nodes.tec') == 0, &
             'nodes: input file written')
  call execute_command_line('mkdir -p converter_nodes', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'nodes: directory made')
  call execute_command_line("'"//converter//"' --in-file=converter_nodes.tec --out-file=converter_nodes/field.vtm > "// &
                            'converter_nodes/converter_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'nodes: the converter ends without error')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='converter_nodes/field', vtspath='converter_nodes/')
  call check(err == 0 .and. r%vtk%node, 'nodes: the field read back without error, its variables at the nodes')
  if (err /= 0) return
  same = size(r%block) == 1
  if (same) same = r%block(1)%Ni == 3 .and. r%block(1)%Nj == 2 .and. r%block(1)%Nk == 1
  if (same) same = allocated(r%block(1)%mesh) .and. allocated(r%block(1)%vars)
  if (same) same = all(shape(r%block(1)%mesh) == shape(wn%block(1)%mesh)) .and. &
                   all(shape(r%block(1)%vars) == shape(wn%block(1)%vars)) .and. &
                   all(lbound(r%block(1)%vars) == lbound(wn%block(1)%vars))
  if (same) same = all(r%block(1)%mesh == wn%block(1)%mesh) .and. all(r%block(1)%vars == wn%block(1)%vars)
  call check(same, 'nodes: the field read back with the dimensions, coordinates and values of the input')
  end subroutine node_location

  subroutine error_status
  !---------------------------------------------------------------------------------------------------------------------------------
  !< An input that the converter cannot read ends it with its message, a nonzero exit status and no output file (in a
  !< directory made for the outputs, without the files of an earlier run, so that the check measures the converter): an empty
  !< Tecplot file, and the .vtm file written by the first conversion (the converter does not read .vtm files, a known limit).
  !< Defect: every error of the converter ended it with a STOP statement without a stop code, so with the exit status 0, as a
  !< success; with the .vtm input the VTK reader of before stopped it with a runtime error, a nonzero exit status.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer                      :: u, exitstat, cmdstat
  logical                      :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p converter_errors && rm -f converter_errors/empty.vtm converter_errors/field.tec', &
                            exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'errors: directory made')
  open(newunit=u, file='converter_empty.tec', status='replace', action='write')
  close(u)
  call execute_command_line("'"//converter//"' --in-file=converter_empty.tec --out-file=converter_errors/empty.vtm > "// &
                            'converter_errors_empty.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'errors: an empty Tecplot input ends the converter with a nonzero exit status')
  call check(log_holds('converter_errors_empty.txt', 'ERROR read Tecplot file'), 'errors: the message of the empty Tecplot input')
  inquire(file='converter_errors/empty.vtm', exist=exists)
  call check(.not.exists, 'errors: no output file for the empty Tecplot input')
  call execute_command_line("'"//converter//"' --in-file=converter_out/field.vtm --out-file=converter_errors/field.tec > "// &
                            'converter_errors_vtm.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'errors: a .vtm input ends the converter with a nonzero exit status')
  call check(log_holds('converter_errors_vtm.txt', 'ERROR read VTK files'), 'errors: the message of the .vtm input')
  inquire(file='converter_errors/field.tec', exist=exists)
  call check(.not.exists, 'errors: no output file for the .vtm input')
  end subroutine error_status

  logical function log_holds(fname, text)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< True when a line of the text file fname holds text.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: fname, text
  character(len=512)           :: line
  integer                      :: u, ios
  !---------------------------------------------------------------------------------------------------------------------------------

  log_holds = .false.
  open(newunit=u, file=fname, status='old', action='read', iostat=ios)
  if (ios /= 0) return
  do
    read(u, '(A)', iostat=ios) line
    if (ios /= 0) exit
    if (index(line, text) > 0) log_holds = .true.
  enddo
  close(u)
  end function log_holds

  subroutine binary_files
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A binary Tecplot file, written and read by the Tecplot library with TecIO only. With TecIO the converter writes the input
  !< to converter_out.szplt, ending without error, and converts that file to a .vtm file, which reads back as the input.
  !< Without TecIO the converter asked to write a .szplt file, or to read one, must end with a nonzero exit status and the
  !< message of the library.
  !< Defect: without TecIO the Tecplot library stopped the program with a STOP statement, normal termination, so the converter
  !< ended with exit status 0 having written nothing, and a script or a test that ran it took it for a success.
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 11.4, paragraph 2 (p. 197): the recommended exit
  !< status is zero for a STOP statement with a character stop code and a processor-dependent nonzero value for an ERROR STOP
  !< statement with one.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer :: exitstat, cmdstat
  logical :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

#ifdef TECIO
  call execute_command_line("'"//converter//"' --in-file=converter_in.tec --out-file=converter_out.szplt > "// &
                            'converter_szplt_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'binary output: the converter ends without error')
  inquire(file='converter_out.szplt', exist=exists)
  call check(exists, 'binary output: the .szplt file is written')
  if (exists) call convert('converter_from_szplt/', 'field', 'binary input', infile='converter_out.szplt')
#else
  call execute_command_line("'"//converter//"' --in-file=converter_in.tec --out-file=converter_out.szplt > "// &
                            'converter_szplt_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'binary output without TecIO: the converter ends with a nonzero exit status')
  call check(log_holds('converter_szplt_log.txt', 'without compiling against TecIO'), &
             'binary output without TecIO: the message of the library')
  inquire(file='converter_out.szplt', exist=exists)
  call check(.not.exists, 'binary output without TecIO: no .szplt file')
  call execute_command_line("'"//converter//"' --in-file=converter_in.szplt --out-file=converter_from_szplt.vtm > "// &
                            'converter_szplt_in_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'binary input without TecIO: the converter ends with a nonzero exit status')
  call check(log_holds('converter_szplt_in_log.txt', 'without compiling against TecIO'), &
             'binary input without TecIO: the message of the library')
#endif
  end subroutine binary_files
  endprogram converter_app
