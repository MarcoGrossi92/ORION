  program orion_data_copy
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing the type orion_data and its copy, copyORION. Each subroutine says what it checks, the defect it
  !> closes and its basis. Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  implicit none
  integer :: checks = 0, failures = 0
  !---------------------------------------------------------------------------------------------------------------------------------

  call copy_every_member

  write(*,'(A,I0,A,I0,A)') 'orion_data_copy: ', checks, ' checks, ', failures, ' failed'
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
  else
    write(*,'(A)') '  ok    '//message
  endif
  end subroutine check

  subroutine copy_every_member
  !---------------------------------------------------------------------------------------------------------------------------------
  !< copyORION(a, b) gives b every member of a: the time, the strand, every format option, the names of the variables and the
  !< blocks, set in a to values other than the defaults of the type.
  !< Defect: copyORION copied tec%format, tec%double, vtk%format and p3d%format only; b, intent(out), came back with the
  !< defaults of the others: solutiontime 0, strandid -1, tec%node, tec%bc, tec%extension and vtk%node as declared.
  !< Basis: Fortran 2018 (ISO/IEC 1539-1:2018, final draft J3/18-007r1), 8.5.10, paragraph 3 (p. 102), and 19.6.5, item (24)
  !< (p. 513): an INTENT (OUT) dummy argument becomes undefined on invocation, but for its default-initialized subcomponents,
  !< which become defined; a copy holds what its source holds.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(orion_data) :: a, b
  integer :: i, j, k
  !---------------------------------------------------------------------------------------------------------------------------------

  a%solutiontime = -5._R8P
  a%strandid = 3
  a%tec%extension = '.dat'; a%tec%format = 'ascii'; a%tec%node = .true.; a%tec%bc = .true.; a%tec%double = .false.
  a%vtk%format = 'raw'; a%vtk%node = .true.
  a%p3d%format = 'ascii'
  allocate(a%varnames(3))
  a%varnames = ['x', 'y', 'u']
  allocate(a%block(1))
  a%block(1)%name = 'B1'
  a%block(1)%Ni = 2; a%block(1)%Nj = 1; a%block(1)%Nk = 0
  allocate(a%block(1)%mesh(2,0:2,0:1,0:0), a%block(1)%vars(1,0:2,0:1,0:0))
  do k = 0, 0; do j = 0, 1; do i = 0, 2
    a%block(1)%mesh(:,i,j,k) = [real(i, R8P), real(j, R8P)]
    a%block(1)%vars(1,i,j,k) = real(10*i + j, R8P) + 0.25_R8P
  enddo; enddo; enddo
  call copyORION(a, b)
  call check(b%solutiontime == a%solutiontime, 'copy: solutiontime')
  call check(b%strandid == a%strandid, 'copy: strandid')
  call check(b%tec%extension == a%tec%extension .and. b%tec%format == a%tec%format, 'copy: tec%extension and tec%format')
  call check((b%tec%node .eqv. a%tec%node) .and. (b%tec%bc .eqv. a%tec%bc) .and. (b%tec%double .eqv. a%tec%double), &
             'copy: tec%node, tec%bc and tec%double')
  call check(b%vtk%format == a%vtk%format .and. (b%vtk%node .eqv. a%vtk%node), 'copy: vtk%format and vtk%node')
  call check(b%p3d%format == a%p3d%format, 'copy: p3d%format')
  call check(allocated(b%varnames), 'copy: the names of the variables')
  if (allocated(b%varnames)) call check(all(b%varnames == a%varnames), 'copy: the names as in the source')
  call check(allocated(b%block), 'copy: the blocks')
  if (.not.allocated(b%block)) return
  call check(b%block(1)%name == a%block(1)%name .and. b%block(1)%Ni == 2 .and. b%block(1)%Nj == 1 .and. b%block(1)%Nk == 0, &
             'copy: the name and the dimensions of the block')
  call check(all(lbound(b%block(1)%vars) == lbound(a%block(1)%vars)) .and. all(b%block(1)%mesh == a%block(1)%mesh) .and. &
             all(b%block(1)%vars == a%block(1)%vars), 'copy: the mesh and the variables, with their bounds')
  end subroutine copy_every_member
  end program orion_data_copy
