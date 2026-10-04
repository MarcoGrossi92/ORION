!< STRANDID of the zones of a Tecplot ASCII file.
!< tec_write_structured_multiblock marks a steady solution (a negative solutiontime) by writing the
!< absolute value of the time and STRANDID = 0 in every zone header. The ASCII reader keeps solutiontime as
!< written (+|t|) and stores the STRANDID of the last zone header in orion%strandid, -1 when that header has
!< none.
!< Stops with a non-zero code when a check fails.
program tecplot_read_strandid
  use IR_Precision, only: R8P
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer :: nfail, ncase

  nfail = 0
  ncase = 0
  call round_trip(-5._R8P,  5._R8P,    0, 'steady_strandid.tec')
  call round_trip(0.25_R8P, 0.25_R8P, -1, 'unsteady_strandid.tec')
  call handwritten(', SOLUTIONTIME=5, STRANDID = 0', '', 5._R8P, 0, 'STRANDID = 0')
  call handwritten(', SOLUTIONTIME=5, STRANDID=1', '', 5._R8P, 1, 'STRANDID = 1')
  call handwritten(', SOLUTIONTIME=5, STRANDID = 0', ', SOLUTIONTIME=7', 7._R8P, -1, &
                   'two zones, STRANDID only on the first: the last zone counts')
  call handwritten('', '', -10._R8P, -1, 'neither SOLUTIONTIME nor STRANDID')
  if (nfail > 0) then
    write(*,'(A,I0,A)') 'tecplot_read_strandid: ', nfail, ' check(s) FAILED'
    error stop 1
  endif
  write(*,'(A)') 'tecplot_read_strandid: all checks passed'

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

  ! The file holds 15 significant digits: the times used here come back exactly. The tolerance only
  ! spares an equality test of reals.
  logical function same(a, b)
    real(R8P), intent(in) :: a, b
    same = abs(a - b) <= 1.e-13_R8P*abs(b)
  end function same

  ! Two zones written by tec_write_structured_multiblock with the given solutiontime, read back.
  subroutine round_trip(time, time_read, strand_read, filename)
    real(R8P),        intent(in) :: time, time_read
    integer,          intent(in) :: strand_read
    character(len=*), intent(in) :: filename
    type(orion_data)  :: written, read_back
    integer           :: err, b, i, j
    character(len=32) :: t

    written%tec%format = 'ascii'
    written%tec%node = .false.
    written%tec%bc = .false.
    written%solutiontime = time
    allocate(written%block(1:2))
    do b = 1, 2
      written%block(b)%name = 'zone'
      allocate(written%block(b)%mesh(1:2,0:4,0:3,0:0))
      allocate(written%block(b)%vars(1:1,1:4,1:3,1:1))
      do j = 0, 3
        do i = 0, 4
          written%block(b)%mesh(1,i,j,0) = real(i + 4*(b-1), R8P)
          written%block(b)%mesh(2,i,j,0) = real(j, R8P)
        enddo
      enddo
      written%block(b)%vars = real(b, R8P)
    enddo
    write(t,'(ES12.4)') time
    write(*,'(A)') filename//': two zones written with solutiontime '//trim(adjustl(t))
    err = tec_write_structured_multiblock(orion=written, varnames='v', filename=filename, Nvars=1)
    call check(err == 0, 'written without error')
    if (err /= 0) return
    read_back%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=read_back, filename=filename)
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(read_back%block) == 2, 'two zones read')
    write(t,'(ES12.4)') read_back%solutiontime
    call check(same(read_back%solutiontime, time_read), 'solutiontime read back '//trim(adjustl(t)))
    write(t,'(I0)') read_back%strandid
    call check(read_back%strandid == strand_read, 'strandid '//trim(t))
  end subroutine round_trip

  ! A file written by hand: one zone, or two when tail2 is not empty, whose headers end with tail1 and tail2.
  subroutine handwritten(tail1, tail2, time_read, strand_read, what)
    character(len=*), intent(in) :: tail1, tail2, what
    real(R8P),        intent(in) :: time_read
    integer,          intent(in) :: strand_read
    type(orion_data)  :: d
    integer           :: u, err, nzones
    character(len=32) :: filename, t

    ncase = ncase + 1
    write(filename,'(A,I0,A)') 'strandid_case_', ncase, '.tec'
    open(newunit=u, file=trim(filename), status='replace')
    write(u,'(A)') ' VARIABLES = "x" "y" "v"'
    write(u,'(A)') ' ZONE  T = a, I=2, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'//tail1
    write(u,'(A)') ' 0.0 1.0 0.0 1.0'
    write(u,'(A)') ' 0.0 0.0 1.0 1.0'
    write(u,'(A)') ' 1.0'
    nzones = 1
    if (len_trim(tail2) > 0) then
      write(u,'(A)') ' ZONE  T = b, I=2, J=2, K=1, DATAPACKING=BLOCK, VARLOCATION=([1-2]=NODAL,[3]=CELLCENTERED)'//tail2
      write(u,'(A)') ' 1.0 2.0 1.0 2.0'
      write(u,'(A)') ' 0.0 0.0 1.0 1.0'
      write(u,'(A)') ' 2.0'
      nzones = 2
    endif
    close(u)
    write(*,'(A)') trim(filename)//': '//what
    d%tec%format = 'ascii'
    err = tec_read_structured_multiblock(orion=d, filename=trim(filename))
    call check(err == 0, 'read without error')
    if (err /= 0) return
    call check(size(d%block) == nzones, 'every zone read')
    write(t,'(ES12.4)') d%solutiontime
    call check(same(d%solutiontime, time_read), 'solutiontime '//trim(adjustl(t)))
    write(t,'(I0)') d%strandid
    call check(d%strandid == strand_read, 'strandid '//trim(t))
  end subroutine handwritten

end program tecplot_read_strandid
