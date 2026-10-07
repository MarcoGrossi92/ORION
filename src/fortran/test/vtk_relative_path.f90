  program vtk_relative_path
  !---------------------------------------------------------------------------------------------------------------------------------
  !> simplified_relative_path(path1, path2) gives the prefix with which vtk_write_structured_multiblock lists the block files in
  !> the .vtm file: path1 is the path of the .vtm file without its extension, path2 the prefix of the block files (vtspath), and
  !> the result R is such that dir(path1)//R//name names the file path2//name, dir(path1) being path1 up to its last '/' (empty
  !> when it has none), since a reader takes the file of a DataSet relative to the directory of the .vtm file. Both paths are
  !> relative to the same directory, or both absolute; an absolute path2 is also taken with a relative path1, and is then the
  !> answer itself. The program checks the result for every form of the two paths: no common prefix, an empty path on one side
  !> or on both, a common directory, a common prefix that is not a directory, equal paths, a prefix of file names without a
  !> trailing '/', paths that start with './' or '../', absolute paths. The answer of each case is written from the rule above
  !> before the program was run. It also calls the function on the forms it cannot answer, which need the current directory
  !> (an absolute path1 with a relative path2, the same directory written in two forms, a path1 that climbs out of the common
  !> directory): only that it returns. Then it writes a field of two blocks with vtk_write_structured_multiblock three ways,
  !> the .vtm file and the block files sharing no prefix: block files with a name prefix next to the .vtm (blk_relpath_,
  !> fld_relpath); the .vtm in a subdirectory and the block files in the current directory (vtspath = '', a listed path,
  !> '../', longer than vtspath); the block files in a subdirectory and the .vtm in the current directory. Each time the .vtm
  !> lists the block files as they are on disk, seen from its directory, and vtk_read_structured_multiblock, given the directory
  !> of the .vtm as vtspath, reads the field back as written. The subdirectory is made with execute_command_line('mkdir ...').
  !> Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use strings, only: simplified_relative_path
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  integer :: checks = 0, failures = 0
  integer :: exitstat, cmdstat

  ! No common prefix
  call case('fld',          '',             '',               'no directory, block files next to the .vtm')
  call case('',             '',             '',               'both empty')
  call case('',             'vtk/',         'vtk/',           'empty path1')
  call case('fld',          'vtk/',         'vtk/',           'block files in a subdirectory')
  call case('out/fld',      '',             '../',            '.vtm in a subdirectory, block files in the current one')
  call case('a/b/fld',      '',             '../../',         '.vtm two levels down')
  call case('out/fld',      'vtk/',         '../vtk/',        'two subdirectories')
  call case('fld',          'pre',          'pre',            'block files with a name prefix')
  call case('fld',          './',           './',             'block files in ./')
  call case('out/fld',      '../vtk/',      '../../vtk/',     'block files above the current directory')
  ! Common directory, common prefix
  call case('out/fld',      'out/vtk/',     'vtk/',           'common directory')
  call case('out/fld',      'out/',         '',               'block files in the directory of the .vtm')
  call case('a/b/fld',      'a/c/',         '../c/',          'common directory, sibling subdirectories')
  call case('a/fld',        'a/b/c/',       'b/c/',           'block files two levels below the .vtm')
  call case('a/b/c/fld',    'a/',           '../../',         'block files two levels above the .vtm')
  call case('out1/fld',     'out2/vtk/',    '../out2/vtk/',   'common prefix that is not a directory')
  call case('out/field',    'out/fieldvtk/','fieldvtk/',      'common prefix past the common directory')
  call case('out/fld',      'out/fld',      'fld',            'equal paths')
  call case('out/fld',      'out/vtk',      'vtk',            'block files with a name prefix, no trailing /')
  call case('./out/fld',    './vtk/',       '../vtk/',        'both paths starting with ./')
  call case('../out/fld',   '../out/vtk/',  'vtk/',           'both paths starting with ../')
  ! Absolute paths
  call case('/d/out/fld',   '/d/out/vtk/',  'vtk/',           'absolute, common directory')
  call case('/a/fld',       '/b/vtk/',      '../b/vtk/',      'absolute, common root only')
  call case('/fld',         '/vtk/',        'vtk/',           'absolute, at the root')
  call case('fld',          '/d/vtk/',      '/d/vtk/',        'relative path1 without a directory, absolute path2')
  call case('out/fld',      '/d/vtk/',      '/d/vtk/',        'relative path1 with a directory, absolute path2')
  ! Forms that need the current directory: only that the function returns
  call returns('/d/out/fld', 'vtk/',     'absolute path1, relative path2')
  call returns('./out/fld',  'out/vtk/', 'the same directory written as ./out and as out')
  call returns('../out/fld', 'vtk/',     'path1 climbing out of the common directory')

  call execute_command_line('mkdir relpath_sub', exitstat=exitstat, cmdstat=cmdstat)  ! it may already be there
  call write_and_read('blk_relpath_', 'fld_relpath', '', 'block files with a name prefix')
  call write_and_read('', 'relpath_sub/fld', 'relpath_sub/', '.vtm in a subdirectory, vtspath empty')
  call write_and_read('relpath_sub/blk_', 'fld2_relpath', '', 'block files in a subdirectory')

  write(*,'(A,I0,A,I0,A)') 'vtk_relative_path: ', checks, ' checks, ', failures, ' failed'
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

  subroutine case(path1, path2, expected, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The relative path of path2 seen from the directory of path1 is the one expected.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: path1, path2, expected, what
  character(len=256)           :: r
  !---------------------------------------------------------------------------------------------------------------------------------

  r = simplified_relative_path(path1, path2)
  call check(trim(r) == expected, what//': "'//path1//'", "'//path2//'" -> "'//trim(r)//'", "'//expected//'" expected')
  end subroutine case

  subroutine returns(path1, path2, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The function returns on a form it cannot answer.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in) :: path1, path2, what
  character(len=256)           :: r
  !---------------------------------------------------------------------------------------------------------------------------------

  r = simplified_relative_path(path1, path2)
  call check(len_trim(r) <= 256, what//': returns')
  end subroutine returns

  subroutine write_and_read(vtspath, vtmpath, vtmdir, what)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a field of two blocks with the block files vtspath//<block>.vts and the .vtm file vtmpath.vtm, whose directory is
  !< vtmdir; check that the .vtm lists the block files as they are on disk, seen from vtmdir, and read the field back, with
  !< vtmdir as vtspath.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: vtspath, vtmpath, vtmdir, what
  type(orion_data)              :: w, r
  character(len=16)             :: varnames
  character(len=:), allocatable :: buf
  integer(I4P)                  :: b, i, j, k, err
  integer                       :: u, n, p, q, nfiles
  logical                       :: there, same
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(w%block(2))
  do b = 1, 2
    w%block(b)%name = 'RP'//trim(str(.true., b))
    w%block(b)%Ni = 2; w%block(b)%Nj = 1 + b; w%block(b)%Nk = 1
    allocate(w%block(b)%mesh(1:3, 0:2, 0:1+b, 0:1), w%block(b)%vars(1:1, 1:2, 1:1+b, 1:1))
    do k = 0, 1; do j = 0, 1 + b; do i = 0, 2
      w%block(b)%mesh(:,i,j,k) = [real(i + 10*b, R8P), real(j, R8P), real(k, R8P) + 0.5_R8P]
    enddo; enddo; enddo
    w%block(b)%vars(1,:,:,1) = real(b, R8P)
  enddo
  w%vtk%format = 'ascii'
  varnames = 'a'
  err = vtk_write_structured_multiblock(orion=w, vtspath=vtspath, vtmpath=vtmpath, varnames=varnames)
  call check(err == 0, what//': field written without error')
  open(newunit=u, file=vtmpath//'.vtm', access='stream', form='unformatted', status='old', action='read')
  inquire(unit=u, size=n)
  allocate(character(len=n) :: buf)
  read(u) buf
  close(u)
  nfiles = 0
  p = index(buf, 'file="')
  do while (p > 0)
    p = p + len('file="')
    q = index(buf(p:), '"') + p - 2
    inquire(file=vtmdir//buf(p:q), exist=there)
    call check(there, what//': the .vtm lists a block file that is on disk: '//buf(p:q))
    nfiles = nfiles + 1
    n = index(buf(q+1:), 'file="')
    p = 0; if (n > 0) p = q + n
  enddo
  call check(nfiles == 2, what//': the .vtm lists two block files')
  r%vtk%format = 'ascii'
  err = vtk_read_structured_multiblock(orion=r, vtmpath=vtmpath, vtspath=vtmdir)
  call check(err == 0, what//': field read back without error')
  same = .false.
  if (err == 0 .and. allocated(r%block)) then
    same = size(r%block) == 2
    if (same) then
      do b = 1, 2
        same = same .and. all(shape(r%block(b)%mesh) == shape(w%block(b)%mesh)) .and. &
               all(shape(r%block(b)%vars) == shape(w%block(b)%vars))
        if (same) same = all(r%block(b)%mesh == w%block(b)%mesh) .and. all(r%block(b)%vars == w%block(b)%vars)
      enddo
    endif
  endif
  call check(same, what//': field read back as written')
  end subroutine write_and_read
  endprogram vtk_relative_path
