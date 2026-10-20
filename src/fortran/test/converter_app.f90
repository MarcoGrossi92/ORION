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
  !> a .vtm file, must be read back with its variables at the nodes. The same for point data: Tecplot files in POINT packing,
  !> written by hand, of a line (x u v), a plane (x y u) and a volume (x y z u) of nodes, converted to a .vtm file (read back
  !> with its variables at the nodes) and to a .dat file (read back as written): every node keeps its coordinates and its values.
  !> VTK and PLOT3D files, as input and as output: the .vtm files of the conversions above (variables at the cells and at the
  !> nodes; block files in binary, ascii and raw, the format taken from the files or given with --in-format) are converted to
  !> Tecplot and to .vtm files in another format and read back as they were; a grid (a mesh without variables) is converted from
  !> a PLOT3D file to Tecplot, VTK and PLOT3D files, and a three-dimensional and a two-dimensional field, a .vtm file included,
  !> to a PLOT3D file; a mesh that a PLOT3D file cannot hold, and the output formats that a file cannot have, are refused.
  !> The names of the variables come through every conversion (a name with a blank has it replaced by an underscore); the
  !> format of a file is that of its extension, not of any dot of its path (directories named like extensions); and an output
  !> that cannot be written (a directory that does not exist) ends the converter with a nonzero exit status, in every
  !> format, with no output file. An input that the converter cannot read must end it with a nonzero exit status. Then a binary Tecplot file (binary_files): with TecIO the converter writes the input to a .szplt file
  !> and converts that file to a .vtm one, as above, and a .szplt file with a FEPOLYGON zone, which ORION does not read, must
  !> end it with a nonzero exit status; without TecIO it must end with a nonzero exit status. Exit status 0 when every check
  !> passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_Tecplot, only: tec_write_structured_multiblock, tec_read_structured_multiblock
  use Lib_VTK, only: vtk_read_structured_multiblock
  use Lib_PLOT3D, only: p3d_read_multiblock, p3d_write_multiblock
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
  call point_data
  call vtk_input
  call plot3d_files
  call names_and_paths
  call output_errors
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

  subroutine point_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Point data: variables at the points of a Tecplot file in POINT packing (F=POINT, one point per line), written by hand, for
  !< a line of 5 points (x u v), a plane of 3 x 2 (x y u) and a volume of 3 x 2 x 2 (x y z u). Each is converted by the converter
  !< to a .vtm file, read back with vtk_read_structured_multiblock, and to a .dat file, read back with
  !< tec_read_structured_multiblock; in both the variables are at the nodes (orion%vtk%node, orion%tec%node set) and every node
  !< has the coordinates (i, j, k) and the values u = 100 i + 10 j + k (and v = -i for the line) it had in the input.
  !< Defect: the .dat files of the converter were read and written by tec_read_points_multivars and tec_write_points_multivars,
  !< which took two variables and one coordinate whatever the file, and wrote only the first line of a plane or a volume.
  !< Basis: P, by construction: the values are those of the formulas above.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer, parameter :: shapes(3,3) = reshape([5, 1, 1,  3, 2, 1,  3, 2, 2], [3, 3])
  character(len=3)   :: names(3) = ['p1d', 'p2d', 'p3d']
  type(orion_data)   :: r
  integer            :: c, nd, exitstat, cmdstat, err
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p converter_points', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'points: directory made')
  do c = 1, 3
    nd = c
    call write_point_file('converter_points/'//names(c)//'.dat', shapes(:,c), nd)
    ! to VTK
    call execute_command_line("'"//converter//"' --in-file=converter_points/"//names(c)//'.dat --out-file=converter_points/'// &
                              names(c)//'.vtm > converter_points/'//names(c)//'_vtm.txt 2>&1', &
                              exitstat=exitstat, cmdstat=cmdstat)
    call check(cmdstat == 0 .and. exitstat == 0, 'points '//names(c)//' to .vtm: the converter ends without error')
    err = vtk_read_structured_multiblock(orion=r, vtmpath='converter_points/'//names(c), vtspath='converter_points/')
    call check(err == 0 .and. r%vtk%node, 'points '//names(c)//' to .vtm: read back, variables at the nodes')
    if (err == 0) then
      r%tec%node = r%vtk%node
      call check(nodes_ok(r, nd, shapes(:,c)), 'points '//names(c)//' to .vtm: every node with its coordinates and values')
    endif
    ! to Tecplot ASCII (BLOCK packing)
    call execute_command_line("'"//converter//"' --in-file=converter_points/"//names(c)//'.dat --out-file=converter_points/'// &
                              names(c)//'_out.dat > converter_points/'//names(c)//'_dat.txt 2>&1', &
                              exitstat=exitstat, cmdstat=cmdstat)
    call check(cmdstat == 0 .and. exitstat == 0, 'points '//names(c)//' to .dat: the converter ends without error')
    err = tec_read_structured_multiblock(orion=r, filename='converter_points/'//names(c)//'_out.dat')
    call check(err == 0 .and. r%tec%node, 'points '//names(c)//' to .dat: read back, variables at the nodes')
    if (err == 0) call check(nodes_ok(r, nd, shapes(:,c)), &
                             'points '//names(c)//' to .dat: every node with its coordinates and values')
  enddo
  end subroutine point_data

  subroutine write_point_file(file, dims, nd)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A Tecplot ASCII file in POINT packing of one zone of dims(1) x dims(2) x dims(3) points (nd coordinates, i fastest): the
  !< line of a point is its coordinates (i, j, k), then u = 100 i + 10 j + k and, for a line (nd = 1), v = -i.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: file
  integer,          intent(in) :: dims(3), nd
  character(len=128)           :: line
  integer                      :: u, i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  open(newunit=u, file=file, status='replace', action='write')
  select case(nd)
  case(1)
    write(u,'(A)') ' VARIABLES = "x" "u" "v"'
    write(u,'(A,I0,A)') ' ZONE T="pts", I=', dims(1), ', F=POINT'
  case(2)
    write(u,'(A)') ' VARIABLES = "x" "y" "u"'
    write(u,'(A,I0,A,I0,A)') ' ZONE T="pts", I=', dims(1), ', J=', dims(2), ', F=POINT'
  case default
    write(u,'(A)') ' VARIABLES = "x" "y" "z" "u"'
    write(u,'(A,I0,A,I0,A,I0,A)') ' ZONE T="pts", I=', dims(1), ', J=', dims(2), ', K=', dims(3), ', F=POINT'
  end select
  do k = 0, dims(3) - 1; do j = 0, dims(2) - 1; do i = 0, dims(1) - 1
    select case(nd)
    case(1)
      write(line,'(3(F12.1,1X))') real(i, R8P), real(100*i + 10*j + k, R8P), real(-i, R8P)
    case(2)
      write(line,'(3(F12.1,1X))') real(i, R8P), real(j, R8P), real(100*i + 10*j + k, R8P)
    case default
      write(line,'(4(F12.1,1X))') real(i, R8P), real(j, R8P), real(k, R8P), real(100*i + 10*j + k, R8P)
    end select
    write(u,'(A)') trim(line)
  enddo; enddo; enddo
  close(u)
  end subroutine write_point_file

  logical function nodes_ok(o, nd, dims) result(ok)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< o holds one block of dims(1) x dims(2) x dims(3) nodes, nd coordinates, with the coordinates (i, j, k) and the values of
  !< write_point_file at the nodes: u = vars(1,...), and v = vars(2,...) for a line.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: o
  integer,          intent(in) :: nd, dims(3)
  integer                      :: i, j, k, nv
  !---------------------------------------------------------------------------------------------------------------------------------

  ok = allocated(o%block)
  if (ok) ok = size(o%block) == 1
  if (.not.ok) return
  nv = merge(2, 1, nd == 1)
  ok = allocated(o%block(1)%mesh) .and. allocated(o%block(1)%vars)
  if (.not.ok) return
  ok = size(o%block(1)%mesh, 1) == nd .and. size(o%block(1)%vars, 1) == nv .and. &
       all(lbound(o%block(1)%mesh) == [1, 0, 0, 0]) .and. all(lbound(o%block(1)%vars) == [1, 0, 0, 0]) .and. &
       ubound(o%block(1)%mesh, 2) == dims(1) - 1 .and. ubound(o%block(1)%vars, 2) == dims(1) - 1
  if (ok .and. nd >= 2) ok = ubound(o%block(1)%mesh, 3) == dims(2) - 1 .and. ubound(o%block(1)%vars, 3) == dims(2) - 1
  if (ok .and. nd == 3) ok = ubound(o%block(1)%mesh, 4) == dims(3) - 1 .and. ubound(o%block(1)%vars, 4) == dims(3) - 1
  if (.not.ok) return
  do k = 0, ubound(o%block(1)%mesh, 4); do j = 0, ubound(o%block(1)%mesh, 3); do i = 0, ubound(o%block(1)%mesh, 2)
    ok = ok .and. o%block(1)%mesh(1,i,j,k) == real(i, R8P)
    if (nd >= 2) ok = ok .and. o%block(1)%mesh(2,i,j,k) == real(j, R8P)
    if (nd >= 3) ok = ok .and. o%block(1)%mesh(3,i,j,k) == real(k, R8P)
    ok = ok .and. o%block(1)%vars(1,i,j,k) == real(100*i + 10*j + k, R8P)
    if (nd == 1) ok = ok .and. o%block(1)%vars(2,i,j,k) == real(-i, R8P)
  enddo; enddo; enddo
  end function nodes_ok

  integer function run_converter(infile, outfile, options, log) result(exitstat)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Run the converter from infile to outfile, with the options (a blank when none), its output in the file log; the exit status
  !< of the converter, -1 when it could not be run.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: infile, outfile, options, log
  integer                      :: cmdstat
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line("'"//converter//"' --in-file="//infile//' --out-file='//outfile//' '//options//' > '//log//' 2>&1', &
                            exitstat=exitstat, cmdstat=cmdstat)
  if (cmdstat /= 0) exitstat = -1
  end function run_converter

  logical function same_field(a, b) result(same)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The blocks of a and b have the same bounds of mesh and vars, and the same coordinates and values.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: a, b
  integer                      :: k
  !---------------------------------------------------------------------------------------------------------------------------------

  same = same_meshes(a, b)
  if (.not.same) return
  do k = 1, size(a%block)
    same = allocated(a%block(k)%vars) .eqv. allocated(b%block(k)%vars)
    if (same .and. allocated(a%block(k)%vars)) then
      same = all(lbound(a%block(k)%vars) == lbound(b%block(k)%vars)) .and. same_array(a%block(k)%vars, b%block(k)%vars)
    endif
    if (.not.same) return
  enddo
  end function same_field

  logical function same_meshes(a, b) result(same)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< a and b have the same blocks, each with the same bounds of mesh and the same coordinates (their variables are not compared).
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(in) :: a, b
  integer                      :: k
  !---------------------------------------------------------------------------------------------------------------------------------

  same = allocated(a%block) .and. allocated(b%block)
  if (same) same = size(a%block) == size(b%block)
  if (.not.same) return
  do k = 1, size(a%block)
    same = allocated(a%block(k)%mesh) .and. allocated(b%block(k)%mesh)
    if (same) same = all(lbound(a%block(k)%mesh) == lbound(b%block(k)%mesh)) .and. same_array(a%block(k)%mesh, b%block(k)%mesh)
    if (.not.same) return
  enddo
  end function same_meshes

  logical function same_array(a, b) result(same)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The arrays a and b have the same shape and values.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in) :: a(:,:,:,:), b(:,:,:,:)
  !---------------------------------------------------------------------------------------------------------------------------------

  same = all(shape(a) == shape(b))
  if (same) same = all(a == b)
  end function same_array

  subroutine vtk_input
  !---------------------------------------------------------------------------------------------------------------------------------
  !< VTK files as input. The .vtm file of the first conversion (variables at the cells, block files in binary) and that of the
  !< nodes (node_location) are converted to Tecplot ASCII files and read back: the field of fill and the field at the nodes,
  !< with the variables where the .vtm file holds them (orion%tec%node), bit for bit. The input in ascii and raw (a .vtm file
  !< written by the converter with --out-format) is converted to Tecplot with the format taken from the files, and with it given
  !< (--in-format, in any case); a .vtm file in binary is converted to a .vtm file in ascii, that one to raw, that one to
  !< Tecplot, and the field is the same. A format that is not the one of the files, given with --in-format, is refused.
  !< Defect: the converter did not read .vtm files: it handed the name with its extension to the reader, which adds it, so that
  !< no .vtm file was found.
  !< Basis: P, by construction: the converter writes the input of the conversions it reads back.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=5), parameter :: formats(2) = ['ascii', 'raw  ']
  type(orion_data)            :: r, wn
  integer                     :: f, err, exitstat
  logical                     :: exists
  character(len=:), allocatable :: d
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p converter_vtk', exitstat=exitstat)
  call check(exitstat == 0, 'vtk input: directory made')
  ! variables at the cells, then at the nodes
  call check(run_converter('converter_out/field.vtm', 'converter_vtk/cells.tec', ' ', 'converter_vtk/cells.txt') == 0, &
             'vtk input, cells: the converter ends without error')
  err = tec_read_structured_multiblock(orion=r, filename='converter_vtk/cells.tec')
  call check(err == 0, 'vtk input, cells: the Tecplot file read back without error')
  if (err == 0) call check(same_field(r, w) .and. .not.r%tec%node, &
                           'vtk input, cells: the field of the input, with its variables at the cells')
  call check(run_converter('converter_nodes/field.vtm', 'converter_vtk/nodes.tec', ' ', 'converter_vtk/nodes.txt') == 0, &
             'vtk input, nodes: the converter ends without error')
  err = tec_read_structured_multiblock(orion=r, filename='converter_vtk/nodes.tec')
  call check(err == 0, 'vtk input, nodes: the Tecplot file read back without error')
  if (err == 0) then
    call fill_nodes(wn)
    call check(same_field(r, wn) .and. r%tec%node, 'vtk input, nodes: the field of the input, with its variables at the nodes')
  endif
  ! the other formats of the block files
  do f = 1, 2
    d = 'converter_vtk/'//trim(formats(f))
    call execute_command_line('mkdir -p '//d, exitstat=exitstat)
    call check(run_converter('converter_in.tec', d//'/field.vtm', '--out-format='//trim(formats(f)), d//'_out.txt') == 0, &
               'vtk input '//trim(formats(f))//': the .vtm file written')
    call check(run_converter(d//'/field.vtm', d//'.tec', ' ', d//'_in.txt') == 0, &
               'vtk input '//trim(formats(f))//': the converter ends without error, the format taken from the files')
    err = tec_read_structured_multiblock(orion=r, filename=d//'.tec')
    call check(err == 0 .and. same_field(r, w), 'vtk input '//trim(formats(f))//': the field of the input')
    call check(run_converter(d//'/field.vtm', d//'_given.tec', '--in-format='//upper(trim(formats(f))), d//'_given.txt') == 0, &
               'vtk input '//trim(formats(f))//': the converter ends without error, the format given')
    err = tec_read_structured_multiblock(orion=r, filename=d//'_given.tec')
    call check(err == 0 .and. same_field(r, w), 'vtk input '//trim(formats(f))//': the field of the input, the format given')
  enddo
  ! .vtm to .vtm in another format: binary to ascii, ascii to raw, raw to Tecplot
  call execute_command_line('mkdir -p converter_vtk/chain_a converter_vtk/chain_r', exitstat=exitstat)
  call check(run_converter('converter_out/field.vtm', 'converter_vtk/chain_a/field.vtm', '--out-format=ascii', &
                           'converter_vtk/chain_a.txt') == 0, 'vtk input, binary to ascii: the converter ends without error')
  call check(run_converter('converter_vtk/chain_a/field.vtm', 'converter_vtk/chain_r/field.vtm', '--out-format=raw', &
                           'converter_vtk/chain_r.txt') == 0, 'vtk input, ascii to raw: the converter ends without error')
  call check(run_converter('converter_vtk/chain_r/field.vtm', 'converter_vtk/chain.tec', ' ', 'converter_vtk/chain_t.txt') == 0, &
             'vtk input, raw to Tecplot: the converter ends without error')
  err = tec_read_structured_multiblock(orion=r, filename='converter_vtk/chain.tec')
  call check(err == 0 .and. same_field(r, w), 'vtk input, binary to ascii to raw to Tecplot: the field of the input')
  ! a format that is not the one of the files, and one that does not exist
  call check(run_converter('converter_vtk/ascii/field.vtm', 'converter_vtk/wrong.tec', '--in-format=binary', &
                           'converter_vtk/wrong.txt') /= 0, 'vtk input: a format that is not that of the files is refused')
  inquire(file='converter_vtk/wrong.tec', exist=exists)
  call check(.not.exists, 'vtk input: no output file for the format that is not that of the files')
  call check(run_converter('converter_out/field.vtm', 'converter_vtk/unknown.tec', '--in-format=hdf', &
                           'converter_vtk/unknown.txt') /= 0, 'vtk input: an input format that does not exist is refused')
  call check(log_holds('converter_vtk/unknown.txt', 'ERROR unsupported input format'), &
             'vtk input: the message of the input format that does not exist')
  end subroutine vtk_input

  subroutine fill_nodes(wn)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The field of node_location, the block of fill with its two variables at the nodes.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(out) :: wn
  integer                       :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  call fill(wn)
  deallocate(wn%block(1)%vars)
  allocate(wn%block(1)%vars(1:2,0:3,0:2,0:1))
  do k = 0, 1; do j = 0, 2; do i = 0, 3
    wn%block(1)%vars(:,i,j,k) = [real(100*i + 10*j + k, R8P), -0.5_R8P*i]
  enddo; enddo; enddo
  wn%tec%node = .true.
  end subroutine fill_nodes

  pure function upper(text) result(out)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The text in upper case.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: text
  character(len=len(text))     :: out
  integer                      :: i
  !---------------------------------------------------------------------------------------------------------------------------------

  out = text
  do i = 1, len(text)
    if (text(i:i) >= 'a' .and. text(i:i) <= 'z') out(i:i) = achar(iachar(text(i:i)) - 32)
  enddo
  end function upper

  subroutine plot3d_files
  !---------------------------------------------------------------------------------------------------------------------------------
  !< PLOT3D files as input and as output. A grid (two blocks, the nodes of fill and a second one moved) written by
  !< p3d_write_multiblock is converted to Tecplot, VTK and PLOT3D files and read back with its blocks and coordinates; a field
  !< with variables is converted to a PLOT3D file, which holds the grid (the converter says that it leaves the variables out).
  !< A two-dimensional field (two coordinates, one layer of cells in k) is converted to a .vtm file and that one to a PLOT3D
  !< file, and the grid read back is the two-dimensional one. A mesh of one coordinate (a line of points) is refused, with no
  !< output file, and so is the output format binary.
  !< Defect: the converter built the names of the variables from the variables of the first block, which a grid has not (a size
  !< of an unallocated array): a PLOT3D file could not be converted to Tecplot; p3d_write_multiblock took a block of Nk /= 0 for
  !< a three-dimensional one, and a mesh of two coordinates read from a .vtm file (Nk = 1) was written past its bounds.
  !< Basis: P, by construction: the grids read back are those written.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: g, r, f2
  integer          :: err, exitstat
  logical          :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p converter_p3d/vtk converter_p3d/vtk2', exitstat=exitstat)
  call check(exitstat == 0, 'plot3d: directory made')
  call fill(g)
  deallocate(g%block(1)%vars)
  call add_second_block(g)
  g%p3d%format = 'ascii'
  call check(p3d_write_multiblock(orion=g, filename='converter_p3d/grid.p3d') == 0, 'plot3d: the grid written')
  ! to Tecplot
  call check(run_converter('converter_p3d/grid.p3d', 'converter_p3d/grid.tec', ' ', 'converter_p3d/grid_tec.txt') == 0, &
             'plot3d to Tecplot: the converter ends without error')
  err = tec_read_structured_multiblock(orion=r, filename='converter_p3d/grid.tec')
  call check(err == 0, 'plot3d to Tecplot: the Tecplot file read back without error')
  if (err == 0) call check(same_meshes(r, g), 'plot3d to Tecplot: the blocks and coordinates of the grid')
  ! to VTK
  call check(run_converter('converter_p3d/grid.p3d', 'converter_p3d/vtk/grid.vtm', ' ', 'converter_p3d/grid_vtm.txt') == 0, &
             'plot3d to VTK: the converter ends without error')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='converter_p3d/vtk/grid', vtspath='converter_p3d/vtk/')
  call check(err == 0, 'plot3d to VTK: the .vtm file read back without error')
  if (err == 0) call check(same_meshes(r, g), 'plot3d to VTK: the blocks and coordinates of the grid')
  ! from the .vtm file just written to PLOT3D
  call check(run_converter('converter_p3d/vtk/grid.vtm', 'converter_p3d/again.p3d', ' ', 'converter_p3d/again.txt') == 0, &
             'VTK to plot3d: the converter ends without error')
  err = p3d_read_multiblock(orion=r, filename='converter_p3d/again.p3d')
  call check(err == 0, 'VTK to plot3d: the PLOT3D file read back without error')
  if (err == 0) call check(same_meshes(r, g), 'VTK to plot3d: the blocks and coordinates of the grid')
  ! a field with variables to PLOT3D
  call check(run_converter('converter_in.tec', 'converter_p3d/field.p3d', ' ', 'converter_p3d/field.txt') == 0, &
             'Tecplot to plot3d: the converter ends without error')
  call check(log_holds('converter_p3d/field.txt', 'WARNING the variables are not written'), &
             'Tecplot to plot3d: the converter says that it leaves the variables out')
  err = p3d_read_multiblock(orion=r, filename='converter_p3d/field.p3d')
  call check(err == 0, 'Tecplot to plot3d: the PLOT3D file read back without error')
  if (err == 0) call check(size(r%block) == 1, 'Tecplot to plot3d: one block')
  if (err == 0 .and. size(r%block) == 1) &
    call check(same_array(r%block(1)%mesh, w%block(1)%mesh), 'Tecplot to plot3d: the coordinates of the field')
  ! a two-dimensional field: Tecplot to VTK to PLOT3D
  call fill_2d(f2)
  f2%tec%format = 'ascii'
  call check(tec_write_structured_multiblock(orion=f2, varnames='u v', filename='converter_p3d/field2d.tec') == 0, &
             'plot3d 2-D: the Tecplot file written')
  call check(run_converter('converter_p3d/field2d.tec', 'converter_p3d/vtk2/field2d.vtm', ' ', 'converter_p3d/f2d_vtm.txt') == 0, &
             'plot3d 2-D, Tecplot to VTK: the converter ends without error')
  call check(run_converter('converter_p3d/vtk2/field2d.vtm', 'converter_p3d/field2d.p3d', ' ', 'converter_p3d/f2d_p3d.txt') == 0, &
             'plot3d 2-D, VTK to PLOT3D: the converter ends without error')
  err = p3d_read_multiblock(orion=r, filename='converter_p3d/field2d.p3d')
  call check(err == 0, 'plot3d 2-D: the PLOT3D file read back without error')
  if (err == 0) then
    call check(size(r%block) == 1, 'plot3d 2-D: one block')
    if (size(r%block) == 1) then
      call check(size(r%block(1)%mesh, 1) == 2, 'plot3d 2-D: two coordinates')
      if (size(r%block(1)%mesh, 1) == 2) &
        call check(same_array(r%block(1)%mesh, f2%block(1)%mesh), 'plot3d 2-D: the coordinates of the field')
    endif
  endif
  ! a line of points cannot be a PLOT3D grid
  call write_point_file('converter_p3d/line.dat', [5, 1, 1], 1)
  call check(run_converter('converter_p3d/line.dat', 'converter_p3d/line.p3d', ' ', 'converter_p3d/line.txt') /= 0, &
             'plot3d: a line of points is refused')
  inquire(file='converter_p3d/line.p3d', exist=exists)
  call check(.not.exists, 'plot3d: no PLOT3D file for a line of points')
  ! the output format binary
  call check(run_converter('converter_in.tec', 'converter_p3d/binary.p3d', '--out-format=binary', &
                           'converter_p3d/binary.txt') /= 0, 'plot3d: the output format binary is refused')
  inquire(file='converter_p3d/binary.p3d', exist=exists)
  call check(.not.exists, 'plot3d: no PLOT3D file for the output format binary')
  end subroutine plot3d_files

  subroutine add_second_block(o)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< A second block in o, the first 3 x 3 x 2 nodes of the mesh of the first one moved by 10 in x.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(inout) :: o
  type(orion_data)                :: t
  !---------------------------------------------------------------------------------------------------------------------------------

  call move_alloc(o%block, t%block)
  allocate(o%block(2))
  call move_alloc(t%block(1)%mesh, o%block(1)%mesh)
  o%block(1)%name = 'b1'; o%block(1)%Ni = 3; o%block(1)%Nj = 2; o%block(1)%Nk = 1
  o%block(2)%name = 'b2'; o%block(2)%Ni = 2; o%block(2)%Nj = 2; o%block(2)%Nk = 1
  allocate(o%block(2)%mesh(1:3,0:2,0:2,0:1))
  o%block(2)%mesh = o%block(1)%mesh(:,0:2,0:2,0:1)
  o%block(2)%mesh(1,:,:,:) = o%block(2)%mesh(1,:,:,:) + 10._R8P
  end subroutine add_second_block

  subroutine fill_2d(o)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< One block of 3 x 2 cells, x and y at the nodes, two variables at the cells (vars(:,1:3,1:2,1:1)).
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data), intent(out) :: o
  integer                       :: i, j
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(o%block(1))
  o%block(1)%name = 'b2d'
  o%block(1)%Ni = 3; o%block(1)%Nj = 2; o%block(1)%Nk = 0
  allocate(o%block(1)%mesh(1:2,0:3,0:2,0:0), o%block(1)%vars(1:2,1:3,1:2,1:1))
  do j = 0, 2; do i = 0, 3
    o%block(1)%mesh(:,i,j,0) = [real(i, R8P) + 0.25_R8P*j, 0.5_R8P*j]
  enddo; enddo
  do j = 1, 2; do i = 1, 3
    o%block(1)%vars(:,i,j,1) = [real(10*i + j, R8P), -0.5_R8P*i]
  enddo; enddo
  o%tec%node = .false.
  end subroutine fill_2d

  subroutine names_and_paths
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The names of the variables, and paths that look like formats. A Tecplot file with the variables "Mass fraction" and
  !< Pressure is converted to a .vtm file and that one to a Tecplot file: the names of the arrays of the block files and
  !< those read back from the Tecplot file are Mass_fraction and Pressure (a blank is not part of a name of a list of names),
  !< and the VTK reader returns x y z with them. A .vtm file in a directory named converter_dots.dat is converted to a
  !< Tecplot file in a directory named converter_dots.tec, and to a PLOT3D file in a directory named converter_dots.vtm,
  !< each time the one file asked for.
  !< Defect: the converter named its variables var1, var2, ...; and it took the format of a file from any ".dat", ".tec",
  !< ".vtm", ".p3d" or ".szplt" of its path, so that a directory named converter_dots.dat sent a .vtm file to the reader of
  !< Tecplot files, and a path with two of them to two writers.
  !< Basis: P, by construction.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: r
  integer          :: u, err, i, es
  character(len=1), parameter :: lf = achar(10)
  logical          :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  call execute_command_line('mkdir -p converter_names converter_dots.dat converter_dots.tec converter_dots.vtm', exitstat=es)
  call check(es == 0, 'names: directories made')
  open(newunit=u, file='converter_names/named.tec', status='replace', action='write')
  write(u,'(A)') ' VARIABLES = "x" "y" "z" "Mass fraction" "Pressure"'
  write(u,'(A)') ' ZONE T="b", I=2, J=2, K=2, DATAPACKING=BLOCK, VARLOCATION=([1-3]=NODAL,[4-5]=CELLCENTERED)'
  write(u,'(8(F4.1,1X))') (real(mod(i,2), R8P), i = 0, 7)
  write(u,'(8(F4.1,1X))') (real(mod(i/2,2), R8P), i = 0, 7)
  write(u,'(8(F4.1,1X))') (real(i/4, R8P), i = 0, 7)
  write(u,'(A)') ' 5.0'//lf//' 6.0'
  close(u)
  ! the values on one line are read by the Fortran reader
  call check(run_converter('converter_names/named.tec', 'converter_names/n.vtm', ' ', 'converter_names/to_vtm.txt') == 0, &
             'names, Tecplot to VTK: the converter ends without error')
  err = vtk_read_structured_multiblock(orion=r, vtmpath='converter_names/n', vtspath='converter_names/')
  call check(err == 0, 'names, Tecplot to VTK: the .vtm file read back')
  if (err == 0 .and. allocated(r%varnames)) then
    call check(size(r%varnames) == 5, 'names, Tecplot to VTK: three coordinates and two variables')
    if (size(r%varnames) == 5) call check(all(r%varnames == [character(len=32) :: 'x', 'y', 'z', 'Mass_fraction', 'Pressure']), &
                                          'names, Tecplot to VTK: x y z Mass_fraction Pressure')
  else
    call check(.false., 'names, Tecplot to VTK: varnames set')
  endif
  call check(run_converter('converter_names/n.vtm', 'converter_names/back.tec', ' ', 'converter_names/to_tec.txt') == 0, &
             'names, VTK to Tecplot: the converter ends without error')
  err = tec_read_structured_multiblock(orion=r, filename='converter_names/back.tec')
  call check(err == 0, 'names, VTK to Tecplot: the Tecplot file read back')
  if (err == 0 .and. allocated(r%varnames)) then
    call check(size(r%varnames) == 5, 'names, VTK to Tecplot: three coordinates and two variables')
    if (size(r%varnames) == 5) call check(all(r%varnames == [character(len=32) :: 'x', 'y', 'z', 'Mass_fraction', 'Pressure']), &
                                          'names, VTK to Tecplot: x y z Mass_fraction Pressure')
  else
    call check(.false., 'names, VTK to Tecplot: varnames set')
  endif
  ! directories named like formats
  call check(run_converter('converter_names/named.tec', 'converter_dots.dat/in.vtm', ' ', 'converter_names/dots_vtm.txt') == 0, &
             'paths: Tecplot to a .vtm file in converter_dots.dat')
  call execute_command_line('test "$(ls converter_dots.dat | tr "\n" " ")" = "Block1.vts in.vtm "', exitstat=es)
  call check(es == 0, 'paths: converter_dots.dat holds the .vtm file and its block file, and nothing else')
  call check(run_converter('converter_dots.dat/in.vtm', 'converter_dots.tec/out.tec', ' ', 'converter_names/dots_tec.txt') == 0, &
             'paths: a .vtm file in converter_dots.dat to a Tecplot file in converter_dots.tec')
  err = tec_read_structured_multiblock(orion=r, filename='converter_dots.tec/out.tec')
  call check(err == 0, 'paths: the Tecplot file read back')
  call check(run_converter('converter_dots.dat/in.vtm', 'converter_dots.vtm/grid.p3d', ' ', 'converter_names/dots_p3d.txt') == 0, &
             'paths: a .vtm file in converter_dots.dat to a PLOT3D file in converter_dots.vtm')
  call execute_command_line('test "$(ls converter_dots.vtm | tr "\n" " ")" = "grid.p3d "', exitstat=es)
  call check(es == 0, 'paths: converter_dots.vtm holds the PLOT3D file, and nothing else')
  call execute_command_line('test "$(ls converter_dots.tec | tr "\n" " ")" = "out.tec "', exitstat=es)
  call check(es == 0, 'paths: converter_dots.tec holds the Tecplot file, and nothing else')
  inquire(file='converter_dots.vtm/grid.p3d', exist=exists)
  call check(exists, 'paths: the PLOT3D file is there')
  end subroutine names_and_paths

  subroutine output_errors
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Output that cannot be written. A conversion to a file in a directory that does not exist ends the converter with a
  !< nonzero exit status and its ERROR line, in every output format (.tec, .dat, .vtm, .p3d), and leaves no file, nor a file
  !< fort.N.
  !< Defect: a .vtm file ended with "Done!" and the exit status 0 having written nothing (the status of the last call of the
  !< writer was the one returned), a Tecplot file ended the converter with a runtime error and a backtrace, and the writers
  !< wrote to the unit that was not connected, which creates fort.N.
  !< Basis: P, by construction.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=4), parameter :: ext(4) = ['tec ', 'dat ', 'vtm ', 'p3d ']
  integer                     :: f, es
  logical                     :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  do f = 1, 4
    call check(run_converter('converter_in.tec', 'converter_no_such_dir/out.'//trim(ext(f)), ' ', &
                             'converter_errors_dir_'//trim(ext(f))//'.txt') /= 0, &
               'output errors: .'//trim(ext(f))//' in a directory that does not exist: a nonzero exit status')
    call check(log_holds('converter_errors_dir_'//trim(ext(f))//'.txt', 'ERROR write'), &
               'output errors: .'//trim(ext(f))//' in a directory that does not exist: the ERROR line')
    inquire(file='converter_no_such_dir', exist=exists)
    call check(.not.exists, 'output errors: .'//trim(ext(f))//' in a directory that does not exist: nothing created')
    call execute_command_line('ls fort.* > /dev/null 2>&1', exitstat=es)
    call check(es /= 0, 'output errors: .'//trim(ext(f))//' in a directory that does not exist: no file fort.N')
  enddo
  end subroutine output_errors

  subroutine error_status
  !---------------------------------------------------------------------------------------------------------------------------------
  !< An input that the converter cannot read ends it with its message, a nonzero exit status and no output file (in a
  !< directory made for the outputs, without the files of an earlier run, so that the check measures the converter): an empty
  !< Tecplot file, a .vtm file that does not exist and one that lists a block file that does not exist.
  !< Defect: every error of the converter ended it with a STOP statement without a stop code, so with the exit status 0, as a
  !< success.
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
  call execute_command_line("'"//converter//"' --in-file=converter_errors/none.vtm --out-file=converter_errors/field.tec > "// &
                            'converter_errors_vtm.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, &
             'errors: a .vtm input that does not exist ends the converter with a nonzero exit status')
  call check(log_holds('converter_errors_vtm.txt', 'ERROR read VTK files'), 'errors: the message of the .vtm input')
  inquire(file='converter_errors/field.tec', exist=exists)
  call check(.not.exists, 'errors: no output file for the .vtm input that does not exist')
  open(newunit=u, file='converter_errors/nofile.vtm', status='replace', action='write')
  write(u,'(A)') '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  write(u,'(A)') '  <vtkMultiBlockDataSet>'
  write(u,'(A)') '    <Block index="0">'
  write(u,'(A)') '      <DataSet index="0" file="./nofile.vts"/>'
  write(u,'(A)') '    </Block>'
  write(u,'(A)') '  </vtkMultiBlockDataSet>'
  write(u,'(A)') '</VTKFile>'
  close(u)
  call execute_command_line("'"//converter//"' --in-file=converter_errors/nofile.vtm --out-file=converter_errors/field.tec > "// &
                            'converter_errors_nofile.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, &
             'errors: a .vtm input that lists a block file that does not exist ends the converter with a nonzero exit status')
  inquire(file='converter_errors/field.tec', exist=exists)
  call check(.not.exists, 'errors: no output file for the .vtm input that lists a block file that does not exist')
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
  !< With TecIO, a .szplt file of one zone that is not ordered, a FEPOLYGON zone (a file of one ordered zone written by the
  !< Tecplot library, with the type of the zone in its header changed, as tecplot_read_szplt_plane_xyz makes it), must end
  !< the converter with a nonzero exit status, the message of the library and no output file.
  !< Defect: without TecIO the Tecplot library stopped the program with a STOP statement, normal termination, so the converter
  !< ended with exit status 0 having written nothing, and a script or a test that ran it took it for a success; the .szplt
  !< reader did the same for a FEPOLYGON or FEPOLYHEDRON zone.
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
  call not_ordered_input
  call missing_input
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

#ifdef TECIO
  subroutine not_ordered_input
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The converter on a .szplt file with a FEPOLYGON zone: a nonzero exit status, the message of the library, no output file.
  !< The file is a copy of a .szplt file of one ordered zone, polyzone, written by the Tecplot library, with the type of the
  !< zone, the byte after the zone name and the separator char(13) in the header of the file, changed from o (ORDERED) to g
  !< (FEPOLYGON), the characters of lib/TecIO/teciosrc/fileStuff.h.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data)              :: p
  integer                       :: i, j, u, sz, k, exitstat, cmdstat
  logical                       :: exists
  character(len=:), allocatable :: bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(p%block(1))
  p%block(1)%name = 'polyzone'
  p%block(1)%Ni = 2; p%block(1)%Nj = 1; p%block(1)%Nk = 0
  allocate(p%block(1)%mesh(1:2,0:2,0:1,0:0), p%block(1)%vars(1:1,1:2,1:1,1:1))
  do j = 0, 1; do i = 0, 2
    p%block(1)%mesh(:,i,j,0) = [real(i, R8P), real(j, R8P)]
  enddo; enddo
  p%block(1)%vars(1,:,1,1) = [1._R8P, 2._R8P]
  p%tec%node = .false.
  p%tec%format = 'binary'
  call check(tec_write_structured_multiblock(orion=p, varnames='a', filename='converter_ordered.szplt') == 0, &
             'not ordered: the file of one ordered zone written')
  inquire(file='converter_ordered.szplt', size=sz)
  call check(sz > 0, 'not ordered: the file of one ordered zone is there')
  if (sz <= 0) return
  allocate(character(len=sz) :: bytes)
  open(newunit=u, file='converter_ordered.szplt', access='stream', form='unformatted', status='old', action='read')
  read(u) bytes
  close(u)
  k = index(bytes, 'polyzone'//achar(13)//'o')
  call check(k > 0 .and. index(bytes, 'polyzone'//achar(13)//'o', back=.true.) == k, &
             'not ordered: the type of the zone found once in the header')
  if (k == 0) return
  bytes(k+9:k+9) = 'g'
  open(newunit=u, file='converter_polygon.szplt', access='stream', form='unformatted', status='replace', action='write')
  write(u) bytes
  close(u)
  call execute_command_line('mkdir -p converter_errors && rm -f converter_errors/polygon.vtm', exitstat=exitstat, &
                            cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat == 0, 'not ordered: directory made')
  call execute_command_line("'"//converter//"' --in-file=converter_polygon.szplt --out-file=converter_errors/polygon.vtm > "// &
                            'converter_polygon_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'not ordered: a FEPOLYGON zone ends the converter with a nonzero exit status')
  call check(log_holds('converter_polygon_log.txt', 'FEPOLYGON zone; ORION reads ordered zones only') .and. &
             log_holds('converter_polygon_log.txt', 'ERROR read Tecplot file'), 'not ordered: the messages of the library and the converter')
  inquire(file='converter_errors/polygon.vtm', exist=exists)
  call check(.not.exists, 'not ordered: no output file')
  end subroutine not_ordered_input

  subroutine missing_input
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The converter on a .szplt file that is not there: a nonzero exit status, the message of the library, which names the TecIO
  !< call that fails and the file, and the message of the converter.
  !< Defect: the .szplt reader returned the status of tecFileReaderOpen without a message of its own (TecIO printed its own),
  !< while it gives one for every other TecIO call that fails.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer :: exitstat, cmdstat
  logical :: exists
  !---------------------------------------------------------------------------------------------------------------------------------

  inquire(file='converter_missing.szplt', exist=exists)
  call check(.not.exists, 'missing input: no file converter_missing.szplt')
  call execute_command_line("'"//converter//"' --in-file=converter_missing.szplt --out-file=converter_errors/missing.vtm > "// &
                            'converter_missing_log.txt 2>&1', exitstat=exitstat, cmdstat=cmdstat)
  call check(cmdstat == 0 .and. exitstat /= 0, 'missing input: the converter ends with a nonzero exit status')
  call check(log_holds('converter_missing_log.txt', 'TECPLOT SZPLT READ ERROR: tecFileReaderOpen failed') .and. &
             log_holds('converter_missing_log.txt', 'File: converter_missing.szplt') .and. &
             log_holds('converter_missing_log.txt', 'ERROR read Tecplot file'), &
             'missing input: the messages of the library, with the call and the file, and of the converter')
  end subroutine missing_input
#endif
  endprogram converter_app
