!< Tecplot binary (.szplt) file whose list of variable names is longer than 1000 characters.
!< Two blocks with x y z and 200 cell-centred variables named as a solver names them (rho(1) ...
!< rho(195), u, v, w, p, T) are written with the ORION writer and read back: the reader must
!< return every name and every value, also in the header-only pass and when only the second zone
!< is read (zone_mask), as a parallel caller does. TecIO build.
!< Stops with a non-zero code when a check fails.
program tecplot_read_szplt_long_header
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer :: nfail

  nfail = 0
  call variables_200()
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_szplt_long_header: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_szplt_long_header: all checks passed'

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

  ! Compares the blocks listed in blocks with what was written.
  subroutine check_values(w, d, blocks, what)
    type(orion_data), intent(in) :: w, d
    integer,          intent(in) :: blocks(:)
    character(len=*), intent(in) :: what
    integer :: n, b
    logical :: same
    call check(size(d%block) == 2, 'two blocks')
    if (size(d%block) /= 2) return
    same = .true.
    do n = 1, size(blocks)
      b = blocks(n)
      same = allocated(d%block(b)%mesh) .and. allocated(d%block(b)%vars)
      if (.not. same) exit
      same = size(d%block(b)%mesh,1) == 3
      if (.not. same) exit
      same = all(lbound(d%block(b)%mesh) == lbound(w%block(b)%mesh)) .and. &
             all(ubound(d%block(b)%mesh) == ubound(w%block(b)%mesh)) .and. &
             all(lbound(d%block(b)%vars) == lbound(w%block(b)%vars)) .and. &
             all(ubound(d%block(b)%vars) == ubound(w%block(b)%vars))
      if (.not. same) exit
      same = all(d%block(b)%mesh == w%block(b)%mesh) .and. all(d%block(b)%vars == w%block(b)%vars)
      if (.not. same) exit
    enddo
    call check(same, what)
  end subroutine check_values

  subroutine variables_200()
    type(orion_data) :: w, d, h, z
    integer :: err, b
    logical :: counts
    write(*,'(A)') '200 variables: x y z nodal, rho(1) ... rho(195) u v w p T cell-centred (ORION writer)'
    call fill(w, 200)
    w%tec%format = 'binary'
    err = tec_write_structured_multiblock(orion=w, varnames=quoted_list(solver_names(200)), &
                                          filename='long_header_200.szplt')
    call check(err == 0, 'written')
    d%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=d, filename='long_header_200.szplt')
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(d, solver_names(200))
    call check_values(w, d, [1, 2], 'every coordinate and every value of the two blocks as written')

    write(*,'(A)') 'header-only pass'
    h%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=h, filename='long_header_200.szplt', dims_only=.true.)
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(h, solver_names(200))
    counts = size(h%block) == 2
    if (counts) then
      do b = 1, 2
        counts = counts .and. h%block(b)%Ni == w%block(b)%Ni .and. h%block(b)%Nj == w%block(b)%Nj .and. &
                 h%block(b)%Nk == w%block(b)%Nk
      enddo
    endif
    call check(counts, 'the cell counts of the two blocks')

    write(*,'(A)') 'second zone only (zone_mask)'
    z%tec%format = 'binary'
    err = tec_read_structured_multiblock(orion=z, filename='long_header_200.szplt', zone_mask=[.false., .true.])
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check_names(z, solver_names(200))
    call check_values(w, z, [2], 'every coordinate and every value of the second block as written')
  end subroutine variables_200

end program tecplot_read_szplt_long_header
