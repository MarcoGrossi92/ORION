!< Tecplot ASCII files whose VARIABLES header is longer than 1000 characters.
!< Two blocks with x y z and 200 cell-centred variables named as a solver names them (rho(1) ...
!< rho(195), u, v, w, p, T) are written with the ORION writer, so the VARIABLES line has more than
!< 2000 characters. The file must hold the whole line, and the reader must return every name and
!< every value. The reader keeps at most 512 names: 512 names are read, 513 are refused.
!< Stops with a non-zero code when a check fails.
program tecplot_read_long_header
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer :: nfail

  nfail = 0
  call variables_200()
  call names_512()
  call names_513()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_long_header: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_long_header: all checks passed'

contains

  subroutine check(ok, what)
    logical,          intent(in) :: ok
    character(len=*), intent(in) :: what
    if (ok) then
      write(*,'(A)') '  ok    '//what
    else
      write(*,'(A)') '  FAIL  '//what
      nfail = nfail + 1
    endif
  end subroutine check

  ! The names of nv solution variables: rho(1) ... rho(nv-5), u, v, w, p, T.
  function solver_names(nv) result(names)
    integer, intent(in) :: nv
    character(len=16)   :: names(nv)
    integer :: s
    do s = 1, nv - 5
      write(names(s),'(A,I0,A)') 'rho(', s, ')'
    enddo
    names(nv-4:nv) = ['u', 'v', 'w', 'p', 'T']
  end function solver_names

  ! The names as one string, each between double quotes, as a solver passes them to the writer.
  function quoted_list(names) result(list)
    character(len=*), intent(in)  :: names(:)
    character(len=:), allocatable :: list
    integer :: s
    list = '"'//trim(names(1))//'"'
    do s = 2, size(names)
      list = list//' "'//trim(names(s))//'"'
    enddo
  end function quoted_list

  ! Two blocks of 3 x 2 x 2 and 2 x 3 x 1 cells; x y z nodal, the nv variables cell-centred.
  ! Every value is exact in the writer's format: 1000 s + 100 i + 10 j + k + b/2.
  subroutine fill(w, nv)
    type(orion_data), intent(inout) :: w
    integer,          intent(in)    :: nv
    integer :: b, i, j, k, s, n(3,2)
    n(:,1) = [3, 2, 2]
    n(:,2) = [2, 3, 1]
    allocate(w%block(2))
    do b = 1, 2
      write(w%block(b)%name,'(A,I0)') 'block', b
      w%block(b)%Ni = n(1,b); w%block(b)%Nj = n(2,b); w%block(b)%Nk = n(3,b)
      allocate(w%block(b)%mesh(1:3,0:n(1,b),0:n(2,b),0:n(3,b)))
      allocate(w%block(b)%vars(1:nv,1:n(1,b),1:n(2,b),1:n(3,b)))
      do k = 0, n(3,b); do j = 0, n(2,b); do i = 0, n(1,b)
        w%block(b)%mesh(:,i,j,k) = [real(i,8), real(j,8), real(k,8) + 10.0d0*b]
      enddo; enddo; enddo
      do k = 1, n(3,b); do j = 1, n(2,b); do i = 1, n(1,b); do s = 1, nv
        w%block(b)%vars(s,i,j,k) = 1000.0d0*s + 100.0d0*i + 10.0d0*j + real(k,8) + 0.5d0*b
      enddo; enddo; enddo; enddo
    enddo
  end subroutine fill

  subroutine check_names(d, names)
    type(orion_data), intent(in) :: d
    character(len=*), intent(in) :: names(:)
    integer :: s
    logical :: same
    call check(allocated(d%varnames), 'names returned')
    if (.not. allocated(d%varnames)) return
    write(*,'(A,I0,A,I0)') '        names returned: ', size(d%varnames), ' of ', size(names) + 3
    call check(size(d%varnames) == size(names) + 3, 'every name returned, coordinates first')
    if (size(d%varnames) /= size(names) + 3) return
    same = d%varnames(1) == 'x' .and. d%varnames(2) == 'y' .and. d%varnames(3) == 'z'
    do s = 1, size(names)
      same = same .and. d%varnames(3+s) == names(s)
    enddo
    call check(same, 'every name as written')
  end subroutine check_names

  subroutine check_values(w, d)
    type(orion_data), intent(in) :: w, d
    integer :: b
    logical :: same
    call check(size(d%block) == 2, 'two blocks')
    if (size(d%block) /= 2) return
    same = .true.
    do b = 1, 2
      same = size(d%block(b)%mesh,1) == 3 .and. allocated(d%block(b)%vars)
      if (.not. same) exit
      same = all(lbound(d%block(b)%mesh) == lbound(w%block(b)%mesh)) .and. &
             all(ubound(d%block(b)%mesh) == ubound(w%block(b)%mesh)) .and. &
             all(lbound(d%block(b)%vars) == lbound(w%block(b)%vars)) .and. &
             all(ubound(d%block(b)%vars) == ubound(w%block(b)%vars))
      if (.not. same) exit
      same = all(d%block(b)%mesh == w%block(b)%mesh) .and. all(d%block(b)%vars == w%block(b)%vars)
      if (.not. same) exit
    enddo
    call check(same, 'every coordinate and every value of the two blocks as written')
  end subroutine check_values

  ! Writes nv variables with the ORION writer and returns the length of the VARIABLES line
  ! it should write and whether the file holds that line whole.
  subroutine write_file(w, names, fname, whole)
    type(orion_data), intent(inout) :: w
    character(len=*), intent(in)    :: names(:), fname
    logical,          intent(out)   :: whole
    character(len=:), allocatable :: list, header
    character(len=65536) :: line
    integer :: u, err, ios
    list = quoted_list(names)
    call fill(w, size(names))
    w%tec%format = 'ascii'
    err = tec_write_structured_multiblock(orion=w, varnames=list, filename=fname)
    call check(err == 0, 'written')
    header = ' VARIABLES ="x" "y" "z" '//list
    line = ' '
    open(newunit=u, file=fname, status='old', action='read')
    read(u,'(A)',iostat=ios) line
    close(u)
    write(*,'(A,I0,A,I0)') '        characters in the VARIABLES line of the file: ', len_trim(line), &
                           ' of ', len(header)
    whole = ios == 0 .and. trim(line) == header
  end subroutine write_file

  subroutine variables_200()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '200 variables: x y z nodal, rho(1) ... rho(195) u v w p T cell-centred'
    call write_file(w, solver_names(200), 'long_header_200.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_200.tec')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(200))
    call check_values(w, d)
  end subroutine variables_200

  ! 3 coordinates and 509 variables: 512 names, the most the reader keeps.
  subroutine names_512()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '512 names: x y z and 509 variables'
    call write_file(w, solver_names(509), 'long_header_512.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_512.tec')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(509))
    call check_values(w, d)
  end subroutine names_512

  ! 3 coordinates and 510 variables: 513 names, one more than the reader keeps.
  subroutine names_513()
    type(orion_data) :: w, d
    logical :: whole
    integer :: err
    write(*,'(A)') '513 names: x y z and 510 variables'
    call write_file(w, solver_names(510), 'long_header_513.tec', whole)
    call check(whole, 'the VARIABLES line of the file holds every name')
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_513.tec')
    call check(err /= 0, 'refused with an error (more than 512 names)')
  end subroutine names_513

end program tecplot_read_long_header
