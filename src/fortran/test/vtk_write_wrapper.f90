  program vtk_write_multiblock
  USE IR_Precision
  USE Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  use Lib_ORION_data
  USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
  use strings, only: simplified_relative_path
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks writing.
  !> It writes the fields that vtk_read_wrapper reads back, then checks vtk_write_structured_multiblock, one subroutine per
  !> case, each with what it checks, the defect it closes and its basis. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data)       :: orion
  integer(I4P), parameter:: nx1=0_I4P,nx2=90_I4P,ny1=0_I4P,ny2=50_I4P
  integer(I4P)           :: nz1 ,nz2
  integer(I4P)           :: i,j,k,b,E_IO
  character(len=10)      :: varnames
  integer :: checks = 0, failures = 0
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! 3D test

  nz1 = 0_I4P; nz2 = 50_I4P
  allocate(orion%block(1:4))
  do b = 1, 4
    orion%block(b)%name = 'B3D'//trim(str(.true.,b))
    orion%block(b)%Ni = nx2
    orion%block(b)%Nj = ny2
    orion%block(b)%Nk = nz2
    allocate(orion%block(b)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
    allocate(orion%block(b)%vars(1:2,nx1+1:nx2,ny1+1:ny2,nz1+1:nz2))
    do k=nz1,nz2
      do j=ny1,ny2
        do i=nx1,nx2
        orion%block(b)%mesh(1,i,j,k) = i*1._R8P
        orion%block(b)%mesh(2,i,j,k) = j*1._R8P
        orion%block(b)%mesh(3,i,j,k) = k*1._R8P
        enddo
      enddo
    enddo
    do k=nz1+1,nz2
      do j=ny1+1,ny2
        do i=nx1+1,nx2
        orion%block(b)%vars(1,i,j,k) = (i*j*k)*1._R8P
        orion%block(b)%vars(2,i,j,k) = -(i*j*k)*1._R8P
        enddo
      enddo
    enddo
    if (b>1) then
      orion%block(b)%mesh(1,:,:,:) = orion%block(b-1)%mesh(1,:,:,:) + nx2*1._R8P
    endif
  enddo

  varnames='var1 var2'
  orion%vtk%format='raw'
  E_IO = vtk_write_structured_multiblock(orion=orion,vtspath='',vtmpath='field3D',varnames=varnames)

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Pure 2D test
  
  deallocate(orion%block)
  allocate(orion%block(1:4))
  do b = 1, 4
    orion%block(b)%name = 'B2D'//trim(str(.true.,b))
    orion%block(b)%Ni = nx2
    orion%block(b)%Nj = ny2
    orion%block(b)%Nk = 1_I4P
    allocate(orion%block(b)%mesh(1:2,nx1:nx2,ny1:ny2,0:0))
    allocate(orion%block(b)%vars(1:2,nx1+1:nx2,ny1+1:ny2,1:1))
    do j=ny1,ny2
      do i=nx1,nx2
      orion%block(b)%mesh(1,i,j,0) = i*1._R8P
      orion%block(b)%mesh(2,i,j,0) = j*1._R8P
      enddo
    enddo
    do j=ny1+1,ny2
      do i=nx1+1,nx2
      orion%block(b)%vars(1,i,j,1) = (i*j*k)*1._R8P
      orion%block(b)%vars(2,i,j,1) = -(i*j*k)*1._R8P
      enddo
    enddo
    if (b>1) then
      orion%block(b)%mesh(1,:,:,:) = orion%block(b-1)%mesh(1,:,:,:) + nx2*1._R8P
    endif
  enddo

  varnames='var1 var2'
  orion%vtk%format='raw'
  E_IO = vtk_write_structured_multiblock(orion=orion,vtspath='',vtmpath='field2D',varnames=varnames)

  call many_blocks
  call relative_paths

  write(*,'(A,I0,A,I0,A)') 'vtk_write_wrapper: ', checks, ' checks, ', failures, ' failed'
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

  subroutine many_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Any number of blocks, written. A field of 130 blocks (3-D, a few cells each, different sizes) is written in the ascii
  !< format without time and read back, and the number of blocks, their names and sizes, every coordinate and every variable
  !< are checked, bit for bit. One format is enough: VTK_INI_XML stores the index of a block file before it looks at the
  !< format. 130 rather than 100: past the array, a gfortran release build of the old writer went on for a few blocks without
  !< any sign, then crashed (from 110 blocks in our runs).
  !< Defect: vtk_write_structured_multiblock kept the file index of each block in an array of 99, so a field of more blocks was
  !< written past its end.
  !< Basis: Fortran 2018, 9.5.3.1 (p. 128): the value of a subscript shall be within the bounds of its dimension.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  call round_trip(130_I4P, 'ascii')
  end subroutine many_blocks

  subroutine round_trip(nb, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write nb blocks in the given format, read them back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     intent(in) :: nb
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, i, j, k, ni, nj, nk
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(written%block(1:nb))
  do b = 1, nb
    ni = 1 + mod(b, 3); nj = 1 + mod(b, 2); nk = 1 + mod(b, 4)
    written%block(b)%name = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt//'_B'//trim(str(.true.,b))
    written%block(b)%Ni = ni; written%block(b)%Nj = nj; written%block(b)%Nk = nk
    allocate(written%block(b)%mesh(1:3,0:ni,0:nj,0:nk))
    allocate(written%block(b)%vars(1:2,1:ni,1:nj,1:nk))
    do k = 0, nk
      do j = 0, nj
        do i = 0, ni
          written%block(b)%mesh(1,i,j,k) = real(i + 10*b, R8P)
          written%block(b)%mesh(2,i,j,k) = real(j, R8P)
          written%block(b)%mesh(3,i,j,k) = real(k + 1, R8P)
        enddo
      enddo
    enddo
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          written%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          written%block(b)%vars(2,i,j,k) = -real(b, R8P)
        enddo
      enddo
    enddo
  enddo
  written%vtk%format = fmt
  vtm = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt
  label = trim(str(.true.,nb))//' blocks '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == nb, trim(label)//': '//trim(str(.true.,size(back%block)))//' blocks read')
  if (size(back%block) /= nb) return
  do b = 1, nb
    associate(w => written%block(b), r => back%block(b))
    call check(trim(r%name) == trim(w%name), trim(label)//': name of block '//trim(str(.true.,b)))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': size of block '//trim(str(.true.,b)))
    call check(size(r%mesh,1) == 3, trim(label)//': mesh of block '//trim(str(.true.,b))//' not 3-D')
    if (size(r%mesh,1) /= 3) cycle
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) then
      call check(.false., trim(label)//': mesh bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates of block '//trim(str(.true.,b)))
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) then
      call check(.false., trim(label)//': variable bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%vars == w%vars), trim(label)//': variables of block '//trim(str(.true.,b)))
    end associate
  enddo
  end subroutine round_trip

  subroutine relative_paths
  !---------------------------------------------------------------------------------------------------------------------------------
  !< The paths of the block files in the .vtm. simplified_relative_path(path1, path2) gives the prefix with which
  !< vtk_write_structured_multiblock lists the block files in the .vtm file: path1 is the path of the .vtm file without its
  !< extension, path2 the prefix of the block files (vtspath), and the result R is such that dir(path1)//R//name names the file
  !< path2//name, dir(path1) being path1 up to its last '/' (empty when it has none), since a reader takes the file of a
  !< DataSet relative to the directory of the .vtm file. Both paths are relative to the same directory, or both absolute; an
  !< absolute path2 is also taken with a relative path1, and is then the answer itself. The result is checked for every form of
  !< the two paths: no common prefix, an empty path on one side or on both, a common directory, a common prefix that is not a
  !< directory, equal paths, a prefix of file names without a trailing '/', paths that start with './' or '../', absolute
  !< paths. The answer of each case was written from the rule above before the checks were run. The function is also called on
  !< the forms it cannot answer, which need the current directory (an absolute path1 with a relative path2, the same directory
  !< written in two forms, a path1 that climbs out of the common directory): it returns, and the result is written to the
  !< output, not checked. Then a field of two blocks
  !< is written with vtk_write_structured_multiblock three ways, the .vtm file and the block files sharing no prefix: block
  !< files with a name prefix next to the .vtm (blk_relpath_, fld_relpath); the .vtm in a subdirectory and the block files in
  !< the current directory (vtspath = '', a listed path, '../', longer than vtspath); the block files in a subdirectory and the
  !< .vtm in the current directory. Each time the .vtm lists the block files as they are on disk, seen from its directory, and
  !< vtk_read_structured_multiblock, given the directory of the .vtm as vtspath, reads the field back as written. The
  !< subdirectory is made with execute_command_line('mkdir ...').
  !< Defect: simplified_relative_path used last_slash, which it set only when the paths share a prefix, so for paths without a
  !< common prefix the .vtm listed what the memory held; the writer also kept the prefix in a string as long as vtspath, so a
  !< longer one was cut ('../' became '').
  !< Basis: Fortran 2018, 9.2 (p. 123) and 19.6.4 (p. 512): a variable that is not initially defined is undefined, and a
  !< reference to it is not permitted; the reader of VTK 9.7.1 takes the file of a DataSet relative to the directory of the
  !< .vtm file (vtkXMLCompositeDataReader::GetFilePath).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer :: exitstat, cmdstat
  !---------------------------------------------------------------------------------------------------------------------------------

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
  call write_and_read_paths('blk_relpath_', 'fld_relpath', '', 'block files with a name prefix')
  call write_and_read_paths('', 'relpath_sub/fld', 'relpath_sub/', '.vtm in a subdirectory, vtspath empty')
  call write_and_read_paths('relpath_sub/blk_', 'fld2_relpath', '', 'block files in a subdirectory')
  end subroutine relative_paths

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
  !< The function returns on a form it cannot answer: the result is written to the output, not checked (it would need the
  !< current directory).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(in)  :: path1, path2, what
  character(len=:), allocatable :: r
  !---------------------------------------------------------------------------------------------------------------------------------

  r = simplified_relative_path(path1, path2)
  write(*,'(A)') what//': returns "'//r//'"'
  end subroutine returns

  subroutine write_and_read_paths(vtspath, vtmpath, vtmdir, what)
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
  end subroutine write_and_read_paths
  end program vtk_write_multiblock