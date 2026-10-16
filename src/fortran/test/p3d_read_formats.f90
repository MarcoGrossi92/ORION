! p3d_read_multiblock detects the layout of a PLOT3D grid file. Every layout is written
! here byte by byte (so that the byte order opposite to the one of this machine can be
! produced too) and read back; files that are not PLOT3D grids must be refused.
module p3d_read_formats_cases
  use IR_Precision
  use Lib_ORION_data
  use Lib_PLOT3D
  implicit none

  ! layout of a test file
  type :: layout_t
    character(len=7) :: enc  = 'fortran' ! 'ascii', 'fortran' (unformatted records) or 'stream' (C binary)
    logical :: multi         = .true.    ! multi-grid (3 blocks) or single grid (block 1)
    integer :: ndir          = 3         ! 3D or 2D
    logical :: iblank        = .false.
    logical :: swap          = .false.   ! binary: byte order opposite to the one of this machine
    integer :: isize         = 4         ! binary: bytes of an integer
    integer :: rsize         = 8         ! binary: bytes of a real
    integer :: mk            = 4         ! Fortran: bytes of a record marker
    logical :: planes        = .false.   ! Fortran: one record per k plane
    integer :: hdr           = 0         ! Fortran header records: 0 N | dims, 1 N and dims in one, 2 N | dims of each block
    integer :: per_line      = 6         ! ASCII: values per line, 0 the whole file on one line
    logical :: dims_one_line = .false.   ! ASCII: the dimensions of all blocks on one line
    logical :: fancy         = .false.   ! ASCII: repeat counts r*c, D exponents, commas, CRLF line ends
    logical :: drop_last     = .false.   ! ASCII: the last value is missing
    integer :: truncate      = 0         ! binary: bytes missing at the end
  endtype layout_t

  integer, parameter :: dims(3,3) = reshape([5,2,2, 3,3,2, 4,1,3],[3,3]) ! nodes of the blocks
  integer :: nfail = 0


contains

  ! value of coordinate d at node (i,j,k) of block b; z is constant on a k plane, so
  ! that the ASCII writer has runs of equal values for repeat counts
  pure real(R8P) function coord(b,d,i,j,k)
    integer, intent(in) :: b, d, i, j, k
    if (d == 3) then
      coord = 1000._R8P*b + 300._R8P + 10._R8P*k
    else
      coord = 1000._R8P*b + 100._R8P*d + 10._R8P*k + 1._R8P*j + 0.25_R8P*i
    endif
  end function coord


  integer function nblocks(lay)
    type(layout_t), intent(in) :: lay
    nblocks = merge(3,1,lay%multi)
  end function nblocks


  ! nodes along k of block b (1 for a 2D file)
  integer function nk(lay,b)
    type(layout_t), intent(in) :: lay
    integer, intent(in) :: b
    nk = merge(dims(3,b),1,lay%ndir == 3)
  end function nk


  ! the blocks read from filename are those of the layout
  logical function same_blocks(data,lay)
    type(orion_data), intent(in) :: data
    type(layout_t), intent(in)   :: lay
    integer :: b, d, i, j, k

    same_blocks = .false.
    if (.not.allocated(data%block)) return
    if (size(data%block) /= nblocks(lay)) return
    do b = 1, nblocks(lay)
      if (data%block(b)%Ni /= dims(1,b)-1 .or. data%block(b)%Nj /= dims(2,b)-1 .or. &
          data%block(b)%Nk /= nk(lay,b)-1) return
      if (size(data%block(b)%mesh,1) /= lay%ndir) return
      do k = 0, nk(lay,b)-1
        do j = 0, dims(2,b)-1
          do i = 0, dims(1,b)-1
            do d = 1, lay%ndir
              if (data%block(b)%mesh(d,i,j,k) /= coord(b,d,i,j,k)) return
            enddo
          enddo
        enddo
      enddo
    enddo
    same_blocks = .true.
  end function same_blocks


  subroutine check(name,lay)
    character(len=*), intent(in) :: name
    type(layout_t), intent(in)   :: lay
    type(orion_data) :: data
    character(len=:), allocatable :: msg
    integer :: err

    call write_case('case.p3d',lay)
    err = p3d_read_multiblock(orion=data,filename='case.p3d',errmsg=msg)
    if (err == 0 .and. same_blocks(data,lay)) then
      write(*,'(A)') '  ok    '//name
    else
      nfail = nfail + 1
      write(*,'(A,I0,A)') '  FAIL  '//name//': err = ', err, ' '//msg
    endif
  end subroutine check


  ! a file of p3d_write_multiblock (one value per line, list-directed)
  subroutine check_writer(ndir)
    integer, intent(in) :: ndir
    type(orion_data) :: out, in
    type(layout_t) :: lay
    integer :: b, d, i, j, k, err

    lay%ndir = ndir
    allocate(out%block(3))
    do b = 1, 3
      out%block(b)%Ni = dims(1,b)-1
      out%block(b)%Nj = dims(2,b)-1
      out%block(b)%Nk = nk(lay,b)-1
      if (ndir == 2) out%block(b)%Nk = 0
      allocate(out%block(b)%mesh(ndir,0:dims(1,b)-1,0:dims(2,b)-1,0:nk(lay,b)-1))
      do k = 0, nk(lay,b)-1
        do j = 0, dims(2,b)-1
          do i = 0, dims(1,b)-1
            do d = 1, ndir
              out%block(b)%mesh(d,i,j,k) = coord(b,d,i,j,k)
            enddo
          enddo
        enddo
      enddo
    enddo
    out%p3d%format = 'ascii'
    err = p3d_write_multiblock(orion=out,filename='case_writer.p3d')
    if (err == 0) err = p3d_read_multiblock(orion=in,filename='case_writer.p3d')
    if (err == 0 .and. same_blocks(in,lay)) then
      write(*,'(A,I0,A)') '  ok    ', ndir, 'D file of p3d_write_multiblock'
    else
      nfail = nfail + 1
      write(*,'(A,I0,A,I0)') '  FAIL  ', ndir, 'D file of p3d_write_multiblock: err = ', err
    endif
  end subroutine check_writer


  subroutine check_refused(name,lay,expected)
    character(len=*), intent(in) :: name
    type(layout_t), intent(in)   :: lay
    integer, intent(in)          :: expected
    call write_case('case.p3d',lay)
    call refused(name,'case.p3d',expected)
  end subroutine check_refused


  subroutine check_refused_text(name,text,expected)
    character(len=*), intent(in) :: name, text
    integer, intent(in)          :: expected
    integer :: u
    open(newunit=u,file='case.p3d',access='stream',form='unformatted',status='replace')
    write(u) text
    close(u)
    call refused(name,'case.p3d',expected)
  end subroutine check_refused_text


  subroutine check_refused_missing()
    integer :: u
    logical :: exists
    inquire(file='case_missing.p3d',exist=exists)
    if (exists) then
      open(newunit=u,file='case_missing.p3d')
      close(u,status='delete')
    endif
    call refused('missing file','case_missing.p3d',-1)
  end subroutine check_refused_missing


  ! filename is refused with err = expected (any nonzero err when expected < 0), with a
  ! message and no block left
  subroutine refused(name,filename,expected)
    character(len=*), intent(in) :: name, filename
    integer, intent(in)          :: expected
    type(orion_data) :: data
    character(len=:), allocatable :: msg
    integer :: err
    logical :: ok

    err = p3d_read_multiblock(orion=data,filename=filename,errmsg=msg)
    if (expected < 0) then
      ok = err /= 0
    else
      ok = err == expected
    endif
    ok = ok .and. len(msg) > 0 .and. .not.allocated(data%block)
    if (ok) then
      write(*,'(A)') '  ok    '//name//' ('//msg//')'
    else
      nfail = nfail + 1
      write(*,'(A,I0,A)') '  FAIL  '//name//': err = ', err, ' '//msg
    endif
  end subroutine refused


  subroutine write_case(filename,lay)
    character(len=*), intent(in) :: filename
    type(layout_t), intent(in)   :: lay
    if (lay%enc == 'ascii') then
      call write_ascii(filename,lay)
    else
      call write_binary(filename,lay)
    endif
  end subroutine write_case


  subroutine write_ascii(filename,lay)
    character(len=*), intent(in) :: filename
    type(layout_t), intent(in)   :: lay
    character(len=:), allocatable :: s, eol, sep, nl
    real(R8P), allocatable :: v(:)
    integer :: b, d, i, j, k, n, u

    sep = ' '
    eol = achar(10)
    if (lay%fancy) then
      sep = ', '
      eol = achar(13)//achar(10)
    endif
    nl = eol
    if (lay%per_line == 0) nl = sep

    s = ''
    if (lay%multi) s = s//itoa(nblocks(lay))//nl
    do b = 1, nblocks(lay)
      s = s//itoa(dims(1,b))//sep//itoa(dims(2,b))
      if (lay%ndir == 3) s = s//sep//itoa(dims(3,b))
      if (lay%dims_one_line .and. b < nblocks(lay)) then
        s = s//sep
      else
        s = s//nl
      endif
    enddo
    do b = 1, nblocks(lay)
      n = dims(1,b)*dims(2,b)*nk(lay,b)
      do d = 1, lay%ndir
        if (allocated(v)) deallocate(v)
        allocate(v(n))
        v = [(((coord(b,d,i,j,k), i=0,dims(1,b)-1), j=0,dims(2,b)-1), k=0,nk(lay,b)-1)]
        if (lay%drop_last .and. b == nblocks(lay) .and. d == lay%ndir .and. .not.lay%iblank) v = v(1:n-1)
        call put_values(v)
      enddo
      if (lay%iblank) then
        if (allocated(v)) deallocate(v)
        allocate(v(n))
        v = 1
        if (lay%drop_last .and. b == nblocks(lay)) v = v(1:n-1)
        call put_values(v,int=.true.)
      endif
    enddo
    s = s//eol

    open(newunit=u,file=filename,access='stream',form='unformatted',status='replace')
    write(u) s
    close(u)

  contains

    ! the values of a component (or iblank), per_line on a line; with fancy, a run of
    ! equal values is written r*c
    subroutine put_values(v,int)
      real(R8P), intent(in)         :: v(:)
      logical, intent(in), optional :: int
      integer :: q, r, online

      online = 0
      q = 1
      do while (q <= size(v))
        r = 1
        if (lay%fancy) then
          do while (q+r <= size(v))
            if (v(q+r) /= v(q)) exit
            r = r + 1
          enddo
        endif
        if (online > 0) s = s//sep
        if (r > 1) s = s//itoa(r)//'*'
        if (present(int)) then
          s = s//itoa(nint(v(q)))
        else
          s = s//rtoa(v(q))
        endif
        q = q + r
        online = online + 1
        if (lay%per_line > 0 .and. online == lay%per_line .and. q <= size(v)) then
          s = s//eol
          online = 0
        endif
      enddo
      s = s//nl
    end subroutine put_values

    function rtoa(x) result(t)
      real(R8P), intent(in) :: x
      character(len=:), allocatable :: t
      character(len=32) :: buf
      integer :: e
      write(buf,'(ES25.17)') x
      t = trim(adjustl(buf))
      if (lay%fancy) then
        e = index(t,'E')
        t(e:e) = 'D'
      endif
    end function rtoa

  end subroutine write_ascii


  subroutine write_binary(filename,lay)
    character(len=*), intent(in) :: filename
    type(layout_t), intent(in)   :: lay
    integer(I1P), allocatable :: f(:), r(:)
    integer :: b, d, i, j, k, u

    allocate(f(0))
    ! header
    allocate(r(0))
    if (lay%multi) then
      r = ibytes(nblocks(lay))
      if (lay%hdr /= 1) call record(r)
    endif
    do b = 1, nblocks(lay)
      r = [r, ibytes(dims(1,b)), ibytes(dims(2,b))]
      if (lay%ndir == 3) r = [r, ibytes(dims(3,b))]
      if (lay%hdr == 2) call record(r)
    enddo
    if (lay%hdr /= 2) call record(r)
    ! coordinates, then iblank, of each block or of each k plane
    do b = 1, nblocks(lay)
      if (lay%planes) then
        do k = 0, nk(lay,b)-1
          do d = 1, lay%ndir
            do j = 0, dims(2,b)-1
              do i = 0, dims(1,b)-1
                r = [r, rbytes(coord(b,d,i,j,k))]
              enddo
            enddo
          enddo
          if (lay%iblank) then
            do i = 1, dims(1,b)*dims(2,b)
              r = [r, ibytes(1)]
            enddo
          endif
          call record(r)
        enddo
      else
        do d = 1, lay%ndir
          do k = 0, nk(lay,b)-1
            do j = 0, dims(2,b)-1
              do i = 0, dims(1,b)-1
                r = [r, rbytes(coord(b,d,i,j,k))]
              enddo
            enddo
          enddo
        enddo
        if (lay%iblank) then
          do i = 1, dims(1,b)*dims(2,b)*nk(lay,b)
            r = [r, ibytes(1)]
          enddo
        endif
        call record(r)
      endif
    enddo

    open(newunit=u,file=filename,access='stream',form='unformatted',status='replace')
    write(u) f(1:size(f)-lay%truncate)
    close(u)

  contains

    ! r appended to the file as a record (with markers for a Fortran file), then emptied
    subroutine record(r)
      integer(I1P), allocatable, intent(inout) :: r(:)
      integer(I1P), allocatable :: m(:)
      if (lay%enc == 'fortran') then
        if (lay%mk == 4) then
          m = order(transfer(int(size(r),I4P),0_I1P,4))
        else
          m = order(transfer(int(size(r),I8P),0_I1P,8))
        endif
        f = [f, m, r, m]
      else
        f = [f, r]
      endif
      deallocate(r)
      allocate(r(0))
    end subroutine record

    function ibytes(v) result(o)
      integer, intent(in) :: v
      integer(I1P), allocatable :: o(:)
      if (lay%isize == 4) then
        o = order(transfer(int(v,I4P),0_I1P,4))
      else
        o = order(transfer(int(v,I8P),0_I1P,8))
      endif
    end function ibytes

    function rbytes(x) result(o)
      real(R8P), intent(in) :: x
      integer(I1P), allocatable :: o(:)
      if (lay%rsize == 4) then
        o = order(transfer(real(x,R4P),0_I1P,4))
      else
        o = order(transfer(x,0_I1P,8))
      endif
    end function rbytes

    ! the bytes of a value in the byte order of the file
    function order(a) result(o)
      integer(I1P), intent(in) :: a(:)
      integer(I1P), allocatable :: o(:)
      if (lay%swap) then
        o = a(size(a):1:-1)
      else
        o = a
      endif
    end function order

  end subroutine write_binary


  function itoa(i) result(t)
    integer, intent(in) :: i
    character(len=:), allocatable :: t
    character(len=16) :: buf
    write(buf,'(I0)') i
    t = trim(buf)
  end function itoa

end module p3d_read_formats_cases


program p3d_read_formats
  use p3d_read_formats_cases
  implicit none

  write(*,'(A)') 'ASCII'
  call check('6 values per line, a line per component',   layout_t(enc='ascii'))
  call check('one value per line',                         layout_t(enc='ascii',per_line=1))
  call check('the whole file on one line',                 layout_t(enc='ascii',per_line=0))
  call check('repeat counts, D exponents, commas, CRLF',   layout_t(enc='ascii',fancy=.true.,iblank=.true.))
  call check('iblank',                                     layout_t(enc='ascii',iblank=.true.))
  call check('2D, dimensions on one line',                 layout_t(enc='ascii',ndir=2,dims_one_line=.true.))
  call check('3D, dimensions on one line',                 layout_t(enc='ascii',dims_one_line=.true.))
  call check('single grid 3D',                             layout_t(enc='ascii',multi=.false.))
  call check('single grid 2D with iblank',                 layout_t(enc='ascii',multi=.false.,ndir=2,iblank=.true.))

  write(*,'(A)') 'Fortran unformatted'
  call check('8-byte reals',                               layout_t())
  call check('8-byte reals, opposite byte order',          layout_t(swap=.true.))
  call check('4-byte reals, iblank',                       layout_t(rsize=4,iblank=.true.))
  call check('4-byte reals, iblank, opposite byte order',  layout_t(rsize=4,iblank=.true.,swap=.true.))
  call check('8-byte integers',                            layout_t(isize=8))
  call check('8-byte record markers',                      layout_t(mk=8))
  call check('planes',                                     layout_t(planes=.true.))
  call check('planes, iblank, opposite byte order',        layout_t(planes=.true.,iblank=.true.,swap=.true.))
  call check('N and dimensions in one record',             layout_t(hdr=1))
  call check('dimensions of each block in a record',       layout_t(hdr=2))
  call check('2D',                                         layout_t(ndir=2))
  call check('single grid 3D',                             layout_t(multi=.false.))
  call check('single grid 2D, 4-byte reals',               layout_t(multi=.false.,ndir=2,rsize=4))

  write(*,'(A)') 'C binary (stream)'
  call check('8-byte reals',                               layout_t(enc='stream'))
  call check('8-byte reals, opposite byte order',          layout_t(enc='stream',swap=.true.))
  call check('4-byte reals, iblank',                       layout_t(enc='stream',rsize=4,iblank=.true.))
  call check('2D',                                         layout_t(enc='stream',ndir=2))
  call check('single grid 3D, opposite byte order',        layout_t(enc='stream',multi=.false.,swap=.true.))

  write(*,'(A)') 'ORION writer'
  call check_writer(3)
  call check_writer(2)

  write(*,'(A)') 'Refused'
  call check_refused('ASCII, a value missing',            layout_t(enc='ascii',drop_last=.true.), 1)
  call check_refused('Fortran, the last bytes missing',   layout_t(truncate=8), 1)
  call check_refused('C binary, the last bytes missing',  layout_t(enc='stream',truncate=8), 1)
  call check_refused_text('not numbers',                  'mesh 1 2 3'//achar(10), 1)
  call check_refused_text('a slash',                      '1 1 1 1 0 0 0 /'//achar(10), 1)
  call check_refused_text('null values',                  '1 1 1 1 3*'//achar(10), 1)
  ! multi-grid 1x1x1 at (0,0,0), or single grid 1x1x1 at (1,0,0) with iblank 0
  call check_refused_text('more than one layout',         '1 1 1 1 0 0 0'//achar(10), 2)
  call check_refused_text('empty',                        '', 1)
  call check_refused_missing()

  if (nfail /= 0) then
    write(*,'(I0,A)') nfail, ' case(s) failed'
    error stop 1
  endif
  write(*,'(A)') 'p3d_read_formats: OK'

end program p3d_read_formats
