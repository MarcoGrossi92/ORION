! Module for reading and writing PLOT3D (P3D) multiblock files.
! Reads PLOT3D grid files of every common layout, detected from the file itself, and
! writes multiblock mesh data in PLOT3D ASCII format, for ORION data structures.
module Lib_PLOT3D
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT
  use IR_Precision
  use Lib_ORION_data
  implicit none
  private

  public:: p3d_read_multiblock
  public:: p3d_write_multiblock

  ! Layout of a PLOT3D grid file, as detected by p3d_read_multiblock.
  type :: p3d_layout
    logical      :: multi  = .true.   ! the file begins with the number of blocks (multi-grid)
    integer      :: ndir   = 3        ! coordinates per node: 3 (x, y, z) or 2 (x, y)
    logical      :: iblank = .false.  ! an iblank array follows the coordinates of each block
    integer(I8P) :: nhead  = 0        ! header length: values (ASCII) or bytes (binary)
    logical      :: swap   = .false.  ! binary: byte order opposite to the one of this machine
    integer      :: isize  = 4        ! binary: bytes of an integer (4 or 8)
    integer      :: rsize  = 8        ! binary: bytes of a real (4 or 8)
    logical      :: planes = .false.  ! binary: one Fortran record per k plane (PLOT3D "planes")
  endtype p3d_layout

  ! Payload of a binary file: its data bytes without the record markers of a Fortran
  ! unformatted file, as segments of the file. A C binary (stream) file is one segment.
  type :: p3d_payload
    logical      :: records = .false.       ! Fortran unformatted (records) or C binary (stream)
    logical      :: swap    = .false.       ! record markers in the byte order opposite to this machine's
    integer(I8P) :: size    = 0             ! payload bytes
    integer(I8P) :: broken  = 0             ! file position where a chain of Fortran records breaks (0: none)
    integer(I8P), allocatable :: pos(:)     ! file position of the first byte of each segment
    integer(I8P), allocatable :: len(:)     ! bytes of each segment
    integer(I8P), allocatable :: start(:)   ! payload offset (from 0) of each segment
    integer(I8P), allocatable :: recend(:)  ! payload offset of the end of each record
  endtype p3d_payload

  integer, parameter :: CHUNK = 1048576     ! characters scanned, or values decoded, at a time

contains

  ! Write ORION mesh data to a PLOT3D multiblock ASCII file.
  ! [in]  orion    ORION data structure containing mesh blocks to write.
  ! [in]  filename Name of the output PLOT3D file.
  ! [out] err         Error code (0 if successful, nonzero otherwise).
  function p3d_write_multiblock(orion,filename) result(err)
    implicit none
    type(orion_data), intent(in)              :: orion
    character(len=*), intent(in)              :: filename
    integer :: err
    integer :: unit, b, d, k, j, i, Nblocks, ndir
    
    Nblocks = size(orion%block)

    ! initializing file
    select case(orion%p3d%format)
    case('binary')
      stop "You can not write in binary P3D format"
    case('ascii')
      if (index(filename,'.')==0) then
        open(newunit=unit,file=trim(filename)//".p3d",iostat=err)
      else
        open(newunit=unit,file=trim(filename),iostat=err)
      endif
    end select

    if (orion%block(1)%Nk==0) then
      ndir = 2
    else
      ndir = 3
    endif

    ! writing data blocks
    write(unit,*,iostat=err) Nblocks
    
    do b = 1, Nblocks
      if (orion%block(b)%Nk==0) then
        write(unit,*,iostat=err) orion%block(b)%Ni+1, orion%block(b)%Nj+1
      else
        write(unit,*,iostat=err) orion%block(b)%Ni+1, orion%block(b)%Nj+1, orion%block(b)%Nk+1
      endif
    enddo

    do b = 1, Nblocks
      do d = 1, ndir
        do k = 0, orion%block(b)%Nk
          do j = 0, orion%block(b)%Nj
            do i = 0, orion%block(b)%Ni
              write(unit,*,iostat=err) orion%block(b)%mesh(d,i,j,k)
            enddo
          enddo
        enddo
      enddo
    enddo

    close(unit)

  endfunction p3d_write_multiblock


  ! Read a PLOT3D grid file into the ORION data structure. The layout is detected from
  ! the file itself (orion%p3d%format is not used):
  ! - encoding: ASCII (free format: any number of values per line, Fortran repeat counts
  !   r*c, D exponents, commas, CRLF line ends), Fortran unformatted (4- or 8-byte record
  !   markers; records over 2 GiB split in subrecords) or C binary (stream, no markers);
  ! - binary: either byte order, 4- or 8-byte integers, 4- or 8-byte reals;
  ! - multi-grid (the file begins with the number of blocks) or single grid;
  ! - 3D (x, y, z) or 2D (x, y: the blocks get Nk = 0);
  ! - with or without iblank (read past: ORION blocks have no blanking);
  ! - whole or planes (one Fortran record per k plane).
  ! Every layout is checked against the count of the values (ASCII) or of the bytes
  ! (binary) of the file, and a Fortran file against its records: the file is read only
  ! when exactly one layout matches, never guessed.
  ! [inout] orion     ORION data structure to fill with mesh blocks (blocks already present are replaced).
  ! [in]    filename  Name of the input PLOT3D file.
  ! [out]   errmsg    Optional: the reason of a failure, '' on success.
  ! [out]   err       0 if successful; the iostat of a failed open or read; 1 when no PLOT3D
  !                   layout matches the file; 2 when more than one does.
  function p3d_read_multiblock(orion,filename,errmsg) result(err)
    implicit none
    type(orion_data), intent(inout)                      :: orion
    character(len=*), intent(in)                         :: filename
    character(len=:), allocatable, intent(out), optional :: errmsg
    integer :: err
    character(len=:), allocatable :: msg
    integer :: unit
    integer(I8P) :: fsize
    logical :: text

    msg = ''
    if (allocated(orion%block)) deallocate(orion%block)

    open(newunit=unit,file=trim(filename),access='stream',form='unformatted',action='read',status='old',iostat=err)
    if (err /= 0) then
      msg = 'cannot open the file'
    else
      inquire(unit=unit,size=fsize)
      if (fsize <= 0) then
        err = 1
        msg = 'the file is empty'
      else
        err = probe_text(unit,fsize,text)
        if (err == 0) then
          if (text) then
            err = read_ascii(orion,unit,trim(filename),fsize,msg)
          else
            err = read_binary(orion,unit,fsize,msg)
          endif
        endif
      endif
      close(unit)
    endif

    if (err /= 0) then
      if (allocated(orion%block)) deallocate(orion%block)
      if (len(msg) == 0) msg = 'read error, iostat = '//str_i8(int(err,I8P))
    endif
    if (present(errmsg)) errmsg = msg

  endfunction p3d_read_multiblock


  ! text: the first bytes of the file are printable ASCII, tabs and line ends (an ASCII
  ! file); a binary file holds other bytes in its first integers and record markers.
  function probe_text(unit,fsize,text) result(err)
    implicit none
    integer, intent(in)      :: unit
    integer(I8P), intent(in) :: fsize
    logical, intent(out)     :: text
    integer :: err, i, c
    integer(I1P), allocatable :: raw(:)

    text = .false.
    allocate(raw(int(min(fsize,4096_I8P))))
    read(unit,pos=1,iostat=err) raw
    if (err /= 0) return
    text = .true.
    do i = 1, size(raw)
      c = iand(int(raw(i)),255)
      if ((c < 32 .and. c /= 9 .and. c /= 10 .and. c /= 13) .or. c > 126) then
        text = .false.
        exit
      endif
    enddo

  endfunction probe_text


  ! ASCII file. A first pass counts the values and keeps the integers the file begins
  ! with; once the layout is known, a single list-directed READ takes the header and the
  ! coordinates whatever the number of values per line: a READ consumes whole records,
  ! so a READ per value or per block would drop the values left on a line.
  function read_ascii(orion,unit,filename,fsize,msg) result(err)
    implicit none
    type(orion_data), intent(inout)              :: orion
    integer, intent(in)                          :: unit
    character(len=*), intent(in)                 :: filename
    integer(I8P), intent(in)                     :: fsize
    character(len=:), allocatable, intent(inout) :: msg
    integer :: err
    integer(I8P), allocatable :: head(:), dims(:,:), npts(:)
    integer(I8P) :: ntok, nlead, idum, m
    type(p3d_layout) :: lay
    integer :: u2, b, d, i, j, k, nib

    err = ascii_scan(unit,fsize,ntok,head,nlead,msg)
    if (err /= 0) return
    err = ascii_layout(ntok,head(1:nlead),lay,dims,msg)
    if (err /= 0) return
    call allocate_blocks(orion,lay%ndir,dims)
    allocate(npts(size(dims,2)))
    npts = product(dims,dim=1)
    nib = merge(1,0,lay%iblank)

    open(newunit=u2,file=filename,form='formatted',access='sequential',action='read',status='old',iostat=err)
    if (err /= 0) return
    read(u2,*,iostat=err) (idum, m=1,lay%nhead), &
      (((((orion%block(b)%mesh(d,i,j,k), i=0,orion%block(b)%Ni), j=0,orion%block(b)%Nj), &
                                         k=0,orion%block(b)%Nk), d=1,lay%ndir), &
       (idum, m=1,nib*npts(b)), b=1,size(orion%block))
    close(u2)

  endfunction read_ascii


  ! First pass on an ASCII file: count the values (r*c counts r) and keep the leading
  ! integers, up to the longest header possible (1 + 3*N values when the first is N).
  ! Only the characters of numbers and separators are accepted: a '/' would end the
  ! list-directed READ and leave coordinates unread, a null value would leave them unset.
  ! The bytes are classified through a table: character comparisons cost a library call
  ! each with some compilers, ten times the time of the READ pass.
  function ascii_scan(unit,fsize,ntok,head,nlead,msg) result(err)
    implicit none
    integer, intent(in)                          :: unit
    integer(I8P), intent(in)                     :: fsize
    integer(I8P), intent(out)                    :: ntok, nlead
    integer(I8P), allocatable, intent(out)       :: head(:)
    character(len=:), allocatable, intent(inout) :: msg
    integer :: err
    integer(I1P), allocatable :: raw(:)
    integer :: cls(0:255), tok(64), n, i, ic, tl, star
    integer(I8P) :: p, hcap
    logical :: inhead

    ! byte classes: 0 not allowed, 1 separator, 2 character of a number, 3 '*' (repeat)
    cls = 0
    cls([9,10,13,32,44]) = 1
    cls(iachar('0'):iachar('9')) = 2
    cls(iachar(['+','-','.','e','E','d','D'])) = 2
    cls(iachar('*')) = 3

    ntok = 0; nlead = 0; hcap = 3; inhead = .true.
    allocate(head(64))
    allocate(raw(CHUNK))
    tl = 0; star = 0; err = 0
    p = 1
    do while (p <= fsize)
      n = int(min(int(CHUNK,I8P),fsize-p+1))
      read(unit,pos=p,iostat=err) raw(1:n)
      if (err /= 0) return
      do i = 1, n
        ic = iand(int(raw(i)),255)
        select case (cls(ic))
        case (1)
          if (tl > 0) then
            if (inhead .or. star > 0) then
              call end_token
              if (err /= 0) return
            else
              ntok = ntok + 1
              tl = 0
            endif
          endif
        case (2,3)
          if (tl == size(tok)) then
            err = 1
            msg = 'a value longer than 64 characters at byte '//str_i8(p+i-1)
            return
          endif
          tl = tl + 1
          tok(tl) = ic
          if (cls(ic) == 3 .and. star == 0) star = tl
        case default
          err = 1
          msg = 'the character "'//achar(ic)//'" at byte '//str_i8(p+i-1)//' cannot be part of a PLOT3D ASCII file'
          return
        end select
      enddo
      p = p + n
    enddo
    if (tl > 0) call end_token

  contains

    ! a value (r values for r*c) of the header, or a repeat, ends
    subroutine end_token
      integer(I8P) :: r, v, q
      integer :: v0
      logical :: ok

      if (star > 0) then
        call parse_int(tok(1:star-1),r,ok)
        if (.not.ok .or. r < 1 .or. star == tl) then
          err = 1
          msg = 'the repeat "'//codes_str(tok(1:tl))//'" is not of the form r*c (null values are not accepted)'
          return
        endif
        v0 = star + 1
      else
        r = 1
        v0 = 1
      endif
      if (inhead) then
        call parse_int(tok(v0:tl),v,ok)
        if (.not.ok) then
          inhead = .false.
        else
          do q = 1, r
            if (nlead >= hcap) then
              inhead = .false.
              exit
            endif
            if (nlead == size(head,kind=I8P)) call grow(head)
            nlead = nlead + 1
            head(nlead) = v
            if (nlead == 1 .and. v >= 1) hcap = max(3_I8P, 1 + 3*min(v,fsize))
          enddo
        endif
      endif
      ntok = ntok + r
      tl = 0
      star = 0

    end subroutine end_token

  endfunction ascii_scan


  ! The layouts of an ASCII file, tried against its count of values: multi-grid or single
  ! grid, 3D or 2D, with or without iblank. Exactly one must match.
  function ascii_layout(ntok,head,lay,dims,msg) result(err)
    implicit none
    integer(I8P), intent(in)                     :: ntok, head(:)
    type(p3d_layout), intent(out)                :: lay
    integer(I8P), allocatable, intent(out)       :: dims(:,:)
    character(len=:), allocatable, intent(inout) :: msg
    integer :: err, im, nd, ib, nmatch
    integer(I8P) :: nb, off, total, np, b
    logical :: ok
    type(p3d_layout) :: t
    character(len=:), allocatable :: found

    nmatch = 0
    found = ''
    do im = 1, 2
      do nd = 3, 2, -1
        do ib = 0, 1
          t%multi = im == 1
          t%ndir = nd
          t%iblank = ib == 1
          if (t%multi) then
            if (size(head) < 1) cycle
            nb = head(1)
            off = 1
          else
            nb = 1
            off = 0
          endif
          if (nb < 1 .or. nb > ntok) cycle
          if (off + nd*nb > size(head)) cycle
          total = off + nd*nb
          ok = .true.
          do b = 1, nb
            np = npoints(head(off+(b-1)*nd+1:off+b*nd),ntok)
            if (np < 0) then
              ok = .false.
            elseif (np*(nd+ib) > ntok-total) then
              ok = .false.
            endif
            if (.not.ok) exit
            total = total + np*(nd+ib)
          enddo
          if (.not.ok .or. total /= ntok) cycle
          t%nhead = off + nd*nb
          nmatch = nmatch + 1
          found = found//'; '//layout_name(t,.false.)
          if (nmatch == 1) then
            lay = t
            dims = reshape(head(off+1:off+nd*nb),[nd,int(nb)])
          endif
        enddo
      enddo
    enddo

    err = 0
    if (nmatch == 0) then
      err = 1
      msg = 'no PLOT3D layout (multi-grid or single grid, 3D or 2D, with or without iblank) matches the '// &
            str_i8(ntok)//' values of the file'
    elseif (nmatch > 1) then
      err = 2
      msg = 'the '//str_i8(ntok)//' values of the file match more than one PLOT3D layout: '//found(3:)
    endif

  endfunction ascii_layout


  ! Binary file: its payload (Fortran records or C stream), its layout, its coordinates.
  function read_binary(orion,unit,fsize,msg) result(err)
    implicit none
    type(orion_data), intent(inout)              :: orion
    integer, intent(in)                          :: unit
    integer(I8P), intent(in)                     :: fsize
    character(len=:), allocatable, intent(inout) :: msg
    integer :: err
    type(p3d_payload) :: pl
    type(p3d_layout) :: lay
    integer(I8P), allocatable :: dims(:,:)

    call find_payload(unit,fsize,pl)
    err = binary_layout(unit,pl,lay,dims,msg)
    if (err /= 0) return
    call allocate_blocks(orion,lay%ndir,dims)
    err = binary_coordinates(unit,pl,lay,orion)

  endfunction read_binary


  ! A Fortran unformatted file is a chain of records, each between two markers holding
  ! its byte length (a negative leading marker continues the record in a further
  ! subrecord: gfortran and ifx split records over 2 GiB so). The chain must end exactly
  ! at the end of the file; markers of 4 and 8 bytes, in either byte order, are tried.
  ! Otherwise the file is C binary (stream): one segment, no records.
  subroutine find_payload(unit,fsize,pl)
    implicit none
    integer, intent(in)              :: unit
    integer(I8P), intent(in)         :: fsize
    type(p3d_payload), intent(inout) :: pl
    integer :: im

    do im = 1, 4
      if (fortran_records(unit,fsize,merge(4,8,im<=2),mod(im,2)==0,pl)) return
    enddo
    pl%records = .false.
    pl%swap = .false.
    pl%size = fsize
    pl%pos = [1_I8P]
    pl%len = [fsize]
    pl%start = [0_I8P]
    pl%recend = [fsize]

  endsubroutine find_payload


  ! .true. (and pl filled) when the file is a chain of Fortran records with markers of
  ! mk bytes in the byte order given by swap
  function fortran_records(unit,fsize,mk,swap,pl) result(ok)
    implicit none
    integer, intent(in)              :: unit, mk
    integer(I8P), intent(in)         :: fsize
    logical, intent(in)              :: swap
    type(p3d_payload), intent(inout) :: pl
    logical :: ok
    integer(I8P), allocatable :: pos(:), seglen(:), start(:), recend(:)
    integer(I8P) :: p, lead, tail, l, nseg, nrec, total
    integer :: ios

    ok = .false.
    allocate(pos(64), seglen(64), start(64), recend(64))
    nseg = 0; nrec = 0; total = 0; lead = 0
    p = 1
    do while (p <= fsize)
      ok = .false.
      if (fsize-p+1 < 2*mk) exit
      lead = marker(unit,p,mk,swap,ios)
      if (ios /= 0) exit
      l = abs(lead)
      if (l > fsize-p+1-2*mk) exit
      tail = marker(unit,p+mk+l,mk,swap,ios)
      if (ios /= 0 .or. abs(tail) /= l) exit
      ok = .true.
      if (nseg == size(pos,kind=I8P)) then
        call grow(pos); call grow(seglen); call grow(start)
      endif
      nseg = nseg + 1
      pos(nseg) = p + mk
      seglen(nseg) = l
      start(nseg) = total
      total = total + l
      if (lead >= 0) then
        if (nrec == size(recend,kind=I8P)) call grow(recend)
        nrec = nrec + 1
        recend(nrec) = total
      endif
      p = p + 2*mk + l
    enddo
    ! a chain of two records or more that breaks before the end: a truncated Fortran file?
    if (.not.ok .and. nseg >= 2) pl%broken = max(pl%broken,p)
    ok = ok .and. nrec > 0 .and. lead >= 0
    if (.not.ok) return

    pl%records = .true.
    pl%swap = swap
    pl%size = total
    pl%pos = pos(1:nseg)
    pl%len = seglen(1:nseg)
    pl%start = start(1:nseg)
    pl%recend = recend(1:nrec)

  endfunction fortran_records


  ! record marker of mk bytes at file position p (a marker out of range is made invalid)
  function marker(unit,p,mk,swap,ios) result(m)
    implicit none
    integer, intent(in)      :: unit, mk
    integer(I8P), intent(in) :: p
    logical, intent(in)      :: swap
    integer, intent(out)     :: ios
    integer(I8P) :: m
    integer(I4P) :: m4

    if (mk == 4) then
      read(unit,pos=p,iostat=ios) m4
      if (swap) m4 = bswap4(m4)
      m = int(m4,I8P)
    else
      read(unit,pos=p,iostat=ios) m
      if (swap) m = bswap8(m)
    endif
    if (m == -huge(m)-1) m = huge(m)

  endfunction marker


  ! The layouts of a binary file, tried against its payload bytes: byte order (that of the
  ! record markers for a Fortran file), 4- or 8-byte integers, multi-grid or single grid,
  ! 3D or 2D, 8- or 4-byte reals, with or without iblank. Exactly one must match. A 3D
  ! Fortran file of one record per k plane is then read as planes.
  function binary_layout(unit,pl,lay,dims,msg) result(err)
    implicit none
    integer, intent(in)                          :: unit
    type(p3d_payload), intent(in)                :: pl
    type(p3d_layout), intent(out)                :: lay
    integer(I8P), allocatable, intent(out)       :: dims(:,:)
    character(len=:), allocatable, intent(inout) :: msg
    integer :: err, isw, is, im, nd, rs, ib, nmatch
    integer(I8P) :: hb, ptot
    integer(I8P), allocatable :: d(:,:)
    type(p3d_layout) :: t
    character(len=:), allocatable :: found

    nmatch = 0
    found = ''
    do isw = 1, merge(1,2,pl%records)
      t%swap = merge(pl%swap,isw==2,pl%records)
      do is = 4, 8, 4
        t%isize = is
        do im = 1, 2
          t%multi = im == 1
          do nd = 3, 2, -1
            t%ndir = nd
            if (.not.binary_header(unit,pl,t,d,hb,ptot)) cycle
            t%nhead = hb
            do rs = 8, 4, -4
              t%rsize = rs
              do ib = 0, 1
                t%iblank = ib == 1
                if (hb + ptot*(nd*rs+ib*is) /= pl%size) cycle
                nmatch = nmatch + 1
                found = found//'; '//layout_name(t,.true.)
                if (nmatch == 1) then
                  lay = t
                  dims = d
                endif
              enddo
            enddo
          enddo
        enddo
      enddo
    enddo

    err = 0
    if (nmatch == 0) then
      err = 1
      if (pl%records) then
        msg = 'Fortran unformatted file, but no PLOT3D layout matches its records'
      else
        if (pl%broken > 0) then
          msg = 'no PLOT3D layout matches the file: its Fortran records break at byte '//str_i8(pl%broken)// &
                ' (a truncated file?)'
        else
          msg = 'no PLOT3D layout matches the '//str_i8(pl%size)//' bytes of the file (read as C binary: no Fortran records found)'
        endif
      endif
    elseif (nmatch > 1) then
      err = 2
      msg = 'the file matches more than one PLOT3D layout: '//found(3:)
    elseif (pl%records .and. lay%ndir == 3) then
      lay%planes = is_planes(pl,lay,dims)
    endif

  endfunction binary_layout


  ! Header of a binary file read with the integers and byte order of t: the number of
  ! blocks (multi-grid) and their dimensions d, the header bytes hb and the nodes ptot.
  ! .false. as soon as it cannot be one: a dimension below 1, more nodes than the payload
  ! can hold, a header of a Fortran file that does not end at the end of a record.
  function binary_header(unit,pl,t,d,hb,ptot) result(ok)
    implicit none
    integer, intent(in)                    :: unit
    type(p3d_payload), intent(in)          :: pl
    type(p3d_layout), intent(in)           :: t
    integer(I8P), allocatable, intent(out) :: d(:,:)
    integer(I8P), intent(out)              :: hb, ptot
    logical :: ok
    integer(I8P), allocatable :: v(:)
    integer(I8P) :: nb, off, lim, b, b0, nc, np
    integer :: nd, is

    ok = .false.
    hb = 0
    ptot = 0
    nd = t%ndir
    is = t%isize
    if (t%multi) then
      if (pl%size < is) return
      allocate(v(1))
      if (get_ints(unit,pl,0_I8P,is,t%swap,v) /= 0) return
      nb = v(1)
      off = is
    else
      nb = 1
      off = 0
    endif
    ! every block takes nd integers and at least one node of nd 4-byte reals
    if (nb < 1 .or. nb > (pl%size-off)/(nd*is+nd*4)) return
    hb = off + nd*is*nb
    if (pl%records) then
      if (.not.any(pl%recend == hb)) return
    endif
    lim = (pl%size-hb)/(nd*4)
    allocate(d(nd,nb))
    do b0 = 1, nb, 4096
      nc = min(4096_I8P,nb-b0+1)
      if (allocated(v)) deallocate(v)
      allocate(v(nd*nc))
      if (get_ints(unit,pl,off+(b0-1)*nd*is,is,t%swap,v) /= 0) return
      d(:,b0:b0+nc-1) = reshape(v,[nd,int(nc)])
      do b = b0, b0+nc-1
        np = npoints(d(:,b),lim-ptot)
        if (np < 0) return
        ptot = ptot + np
      enddo
    enddo
    ok = .true.

  endfunction binary_header


  ! A 3D Fortran file holds the coordinates of a block in one record (whole) or in one
  ! record per k plane, x, y, z (and iblank) of the plane (planes). Planes when the
  ! records after the header are those of the planes and not those of the blocks; any
  ! other split of the records is read as whole.
  function is_planes(pl,lay,dims) result(planes)
    implicit none
    type(p3d_payload), intent(in) :: pl
    type(p3d_layout), intent(in)  :: lay
    integer(I8P), intent(in)      :: dims(:,:)
    logical :: planes
    integer(I8P) :: perpt, h0, r, b, k, nb
    logical :: whole

    planes = .false.
    perpt = lay%ndir*lay%rsize + merge(lay%isize,0,lay%iblank)
    nb = size(dims,2)
    h0 = 0
    do r = 1, size(pl%recend)
      if (pl%recend(r) == lay%nhead) then
        h0 = r
        exit
      endif
    enddo
    whole = size(pl%recend)-h0 == nb
    if (whole) then
      do b = 1, nb
        if (reclen(h0+b) /= product(dims(:,b))*perpt) then
          whole = .false.
          exit
        endif
      enddo
    endif
    if (whole .or. size(pl%recend)-h0 /= sum(dims(3,:))) return
    r = h0
    do b = 1, nb
      do k = 1, dims(3,b)
        r = r + 1
        if (reclen(r) /= dims(1,b)*dims(2,b)*perpt) return
      enddo
    enddo
    planes = .true.

  contains

    integer(I8P) function reclen(r)
      integer(I8P), intent(in) :: r
      if (r == 1) then
        reclen = pl%recend(1)
      else
        reclen = pl%recend(r) - pl%recend(r-1)
      endif
    end function reclen

  endfunction is_planes


  ! Coordinates of every block from the payload. Whole: the nodes of the block for x,
  ! then y (then z), then iblank (skipped). Planes: the same for each k plane.
  function binary_coordinates(unit,pl,lay,orion) result(err)
    implicit none
    integer, intent(in)             :: unit
    type(p3d_payload), intent(in)   :: pl
    type(p3d_layout), intent(in)    :: lay
    type(orion_data), intent(inout) :: orion
    integer :: err
    integer(I8P) :: off, perpt, nplane, np, k
    integer :: b, d

    err = 0
    perpt = lay%ndir*lay%rsize + merge(lay%isize,0,lay%iblank)
    off = lay%nhead
    do b = 1, size(orion%block)
      nplane = int(orion%block(b)%Ni+1,I8P)*int(orion%block(b)%Nj+1,I8P)
      if (lay%planes) then
        do k = 0, orion%block(b)%Nk
          do d = 1, lay%ndir
            err = get_coords(unit,pl,lay,off+(d-1)*nplane*lay%rsize,nplane,k*nplane,d,orion%block(b)%mesh)
            if (err /= 0) return
          enddo
          off = off + nplane*perpt
        enddo
      else
        np = nplane*(orion%block(b)%Nk+1)
        do d = 1, lay%ndir
          err = get_coords(unit,pl,lay,off+(d-1)*np*lay%rsize,np,0_I8P,d,orion%block(b)%mesh)
          if (err /= 0) return
        enddo
        off = off + np*perpt
      endif
    enddo

  endfunction binary_coordinates


  ! n coordinates d from payload offset lo into the nodes q0, q0+1, ... of mesh (i fastest)
  function get_coords(unit,pl,lay,lo,n,q0,d,mesh) result(err)
    implicit none
    integer, intent(in)             :: unit, d
    type(p3d_payload), intent(in)   :: pl
    type(p3d_layout), intent(in)    :: lay
    integer(I8P), intent(in)        :: lo, n, q0
    real(R8P), intent(inout)        :: mesh(:,0:,0:,0:)
    integer :: err
    real(R8P), allocatable :: v(:)
    integer(I8P) :: c, m, q, g, t, ni1, nj1

    err = 0
    ni1 = size(mesh,2)
    nj1 = size(mesh,3)
    allocate(v(min(n,int(CHUNK,I8P))))
    do c = 0, n-1, int(CHUNK,I8P)
      m = min(int(CHUNK,I8P),n-c)
      err = get_reals(unit,pl,lo+c*lay%rsize,lay%rsize,lay%swap,v(1:m))
      if (err /= 0) return
      do q = 0, m-1
        g = q0 + c + q
        t = g/ni1
        mesh(d,mod(g,ni1),mod(t,nj1),t/nj1) = v(q+1)
      enddo
    enddo

  endfunction get_coords


  ! size(v) reals of rs bytes from payload offset lo. Converted value by value: an
  ! array expression (transfer of the whole buffer) makes ifx place temporaries of the
  ! buffer size on the stack, past its default limit of 8 MB.
  function get_reals(unit,pl,lo,rs,swap,v) result(err)
    implicit none
    integer, intent(in)           :: unit, rs
    type(p3d_payload), intent(in) :: pl
    integer(I8P), intent(in)      :: lo
    logical, intent(in)           :: swap
    real(R8P), intent(out)        :: v(:)
    integer :: err
    integer(I1P), allocatable :: raw(:)
    integer(I8P) :: n, q
    integer(I8P) :: w8
    integer(I4P) :: w4

    n = size(v,kind=I8P)
    allocate(raw(n*rs))
    err = get_bytes(unit,pl,lo,raw)
    if (err /= 0) return
    if (rs == 8) then
      do q = 1, n
        w8 = transfer(raw(8*q-7:8*q),w8)
        if (swap) w8 = bswap8(w8)
        v(q) = transfer(w8,0._R8P)
      enddo
    else
      do q = 1, n
        w4 = transfer(raw(4*q-3:4*q),w4)
        if (swap) w4 = bswap4(w4)
        v(q) = real(transfer(w4,0._R4P),R8P)
      enddo
    endif

  endfunction get_reals


  ! size(v) integers of is bytes from payload offset lo
  function get_ints(unit,pl,lo,is,swap,v) result(err)
    implicit none
    integer, intent(in)           :: unit, is
    type(p3d_payload), intent(in) :: pl
    integer(I8P), intent(in)      :: lo
    logical, intent(in)           :: swap
    integer(I8P), intent(out)     :: v(:)
    integer :: err
    integer(I1P), allocatable :: raw(:)
    integer(I8P) :: n, q
    integer(I4P) :: w4

    n = size(v,kind=I8P)
    allocate(raw(n*is))
    err = get_bytes(unit,pl,lo,raw)
    if (err /= 0) return
    if (is == 8) then
      do q = 1, n
        v(q) = transfer(raw(8*q-7:8*q),v(q))
        if (swap) v(q) = bswap8(v(q))
      enddo
    else
      do q = 1, n
        w4 = transfer(raw(4*q-3:4*q),w4)
        if (swap) w4 = bswap4(w4)
        v(q) = int(w4,I8P)
      enddo
    endif

  endfunction get_ints


  ! size(raw) payload bytes from payload offset lo, across the segments
  function get_bytes(unit,pl,lo,raw) result(err)
    implicit none
    integer, intent(in)           :: unit
    type(p3d_payload), intent(in) :: pl
    integer(I8P), intent(in)      :: lo
    integer(I1P), intent(out)     :: raw(:)
    integer :: err
    integer(I8P) :: nb, got, s, s1, s2, mid, inseg, take

    err = 0
    nb = size(raw,kind=I8P)
    if (lo < 0 .or. lo+nb > pl%size) then
      err = -1
      return
    endif
    ! last segment that begins at or before lo
    s1 = 1
    s2 = size(pl%start)
    do while (s1 < s2)
      mid = (s1+s2+1)/2
      if (pl%start(mid) <= lo) then
        s1 = mid
      else
        s2 = mid - 1
      endif
    enddo
    s = s1
    got = 0
    do while (got < nb)
      inseg = lo + got - pl%start(s)
      take = min(nb-got,pl%len(s)-inseg)
      if (take > 0) then
        read(unit,pos=pl%pos(s)+inseg,iostat=err) raw(got+1:got+take)
        if (err /= 0) return
        got = got + take
      endif
      s = s + 1
    enddo

  endfunction get_bytes


  ! blocks of the dimensions dims (nodes per direction, one column per block)
  subroutine allocate_blocks(orion,ndir,dims)
    implicit none
    type(orion_data), intent(inout) :: orion
    integer, intent(in)             :: ndir
    integer(I8P), intent(in)        :: dims(:,:)
    integer :: b

    allocate(orion%block(size(dims,2)))
    do b = 1, size(dims,2)
      orion%block(b)%Ni = int(dims(1,b)) - 1
      orion%block(b)%Nj = int(dims(2,b)) - 1
      if (ndir == 3) then
        orion%block(b)%Nk = int(dims(3,b)) - 1
      else
        orion%block(b)%Nk = 0
      endif
      allocate(orion%block(b)%mesh(1:ndir,0:orion%block(b)%Ni,0:orion%block(b)%Nj,0:orion%block(b)%Nk))
    enddo

  endsubroutine allocate_blocks


  ! nodes of a block of dimensions d; -1 when a dimension is below 1 or beyond the default
  ! integer (Ni, Nj, Nk), or when the nodes exceed lim
  pure function npoints(d,lim) result(np)
    implicit none
    integer(I8P), intent(in) :: d(:), lim
    integer(I8P) :: np
    integer :: q

    np = -1
    if (any(d < 1) .or. any(d > huge(1))) return
    np = 1
    do q = 1, size(d)
      if (d(q) > lim/np) then
        np = -1
        return
      endif
      np = np*d(q)
    enddo

  endfunction npoints


  ! c (character codes) is an integer (optional sign, digits only): ok, and its value in v
  pure subroutine parse_int(c,v,ok)
    implicit none
    integer, intent(in)       :: c(:)
    integer(I8P), intent(out) :: v
    logical, intent(out)      :: ok
    integer :: i, i0, dgt

    v = 0
    ok = .false.
    i0 = 1
    if (size(c) > 0) then
      if (c(1) == iachar('+') .or. c(1) == iachar('-')) i0 = 2
    endif
    if (i0 > size(c)) return
    do i = i0, size(c)
      dgt = c(i) - iachar('0')
      if (dgt < 0 .or. dgt > 9) return
      if (v > (huge(v)-dgt)/10) return
      v = 10*v + dgt
    enddo
    if (c(1) == iachar('-')) v = -v
    ok = .true.

  endsubroutine parse_int


  ! the characters of the codes c
  pure function codes_str(c) result(s)
    implicit none
    integer, intent(in) :: c(:)
    character(len=size(c)) :: s
    integer :: i

    do i = 1, size(c)
      s(i:i) = achar(c(i))
    enddo

  endfunction codes_str


  ! description of a layout, for the messages
  function layout_name(lay,binary) result(s)
    implicit none
    type(p3d_layout), intent(in) :: lay
    logical, intent(in)          :: binary
    character(len=:), allocatable :: s

    if (lay%multi) then
      s = 'multi-grid '
    else
      s = 'single grid '
    endif
    s = s//achar(48+lay%ndir)//'D'
    if (lay%iblank) s = s//' with iblank'
    if (binary) then
      s = s//', '//achar(48+lay%rsize)//'-byte reals, '//achar(48+lay%isize)//'-byte integers'
      if (lay%swap) s = s//', opposite byte order'
    endif

  endfunction layout_name


  subroutine grow(a)
    implicit none
    integer(I8P), allocatable, intent(inout) :: a(:)
    integer(I8P), allocatable :: t(:)

    allocate(t(2*size(a)))
    t(1:size(a)) = a
    call move_alloc(t,a)

  endsubroutine grow


  elemental function bswap4(x) result(y)
    implicit none
    integer(I4P), intent(in) :: x
    integer(I4P) :: y
    integer :: b

    y = 0_I4P
    do b = 0, 3
      call mvbits(x,8*b,8,y,24-8*b)
    enddo

  endfunction bswap4


  elemental function bswap8(x) result(y)
    implicit none
    integer(I8P), intent(in) :: x
    integer(I8P) :: y
    integer :: b

    y = 0_I8P
    do b = 0, 7
      call mvbits(x,8*b,8,y,56-8*b)
    enddo

  endfunction bswap8


  function str_i8(i) result(s)
    implicit none
    integer(I8P), intent(in) :: i
    character(len=:), allocatable :: s
    character(len=24) :: buf

    write(buf,'(I0)') i
    s = trim(buf)

  endfunction str_i8


  end module Lib_PLOT3D
