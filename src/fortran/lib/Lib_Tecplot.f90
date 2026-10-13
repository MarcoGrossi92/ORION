!> \addtogroup Library
!> @{
!> \defgroup Lib_TecplotLibrary Lib_Tecplot
!> @}

!> \brief Pure Fortran (2003+) library to write data conforming the Tecplot standard.
!> \details Provides comprehensive routines for reading and writing Tecplot structured multiblock and point data files.
!> Supports both ASCII and binary (with TecIO library) formats for ORION data structures.
!> \ingroup Lib_TecplotLibrary
module Lib_Tecplot
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
  use IR_Precision
  use Lib_ORION_data
  ! The Tecplot ASCII readers read every line whole, whatever its length: a zone header holds the block name
  use strings, only: read_whole_line
  implicit none
# if defined(TECIO)
  include "tecio.f90"
# endif
  private

  public:: tec_write_structured_multiblock
  public:: tec_read_structured_multiblock
  public:: tec_write_points_multivars
  public:: tec_read_points_multivars


contains

  !> \brief Compute domain dimensions for Tecplot output.
  !> \details Calculates dimension bounds for node-centered and cell-centered data based on block size and boundary conditions.
  !> \param[in] node Whether data is node-centered (.true.) or cell-centered (.false.)
  !> \param[in] bc Whether boundary conditions are included
  !> \param[in] Nx, Ny, Nz Block dimensions in each direction
  !> \param[in] gc Ghost cell width
  !> \param[out] ni1,ni2,nj1,nj2,nk1,nk2 Bounds for node-centered data
  !> \param[out] ci1,ci2,cj1,cj2,ck1,ck2 Bounds for cell-centered data
  subroutine compute_dimensions(node,bc,Nx,Ny,Nz,gc,ni1,ni2,nj1,nj2,nk1,nk2,ci1,ci2,cj1,cj2,ck1,ck2)
    implicit none
    logical, intent(in) :: node 
    logical, intent(in) :: bc 
    integer, intent(IN) :: Nx, Ny, Nz, gc          ! Block-level data.
    integer, intent(OUT):: ni1,ni2,nj1,nj2,nk1,nk2 ! Bounds of dimensions of node-centered data.
    integer, intent(OUT):: ci1,ci2,cj1,cj2,ck1,ck2 ! Bounds of dimensions of cell-centered data.
    if (node) then
      ni1 = 0 ; ni2 = Nx
      nj1 = 0 ; nj2 = Ny
      nk1 = 0 ; nk2 = Nz

      ci1 = 0 ; ci2 = Nx
      cj1 = 0 ; cj2 = Ny
      ck1 = 0 ; ck2 = Nz
    else
      if (bc) then
        ni1 = 0 - gc ; ni2 = Nx + gc
        nj1 = 0 - gc ; nj2 = Ny + gc
        nk1 = 0 - gc ; nk2 = Nz + gc

        ci1 = 1 - gc ; ci2 = Nz + gc
        cj1 = 1 - gc ; cj2 = Ny + gc
        ck1 = 1 - gc ; ck2 = Nz + gc
      else
        ni1 = 0      ; ni2 = Nx
        nj1 = 0      ; nj2 = Ny
        nk1 = 0      ; nk2 = Nz

        ci1 = 1      ; ci2 = Nx
        cj1 = 1      ; cj2 = Ny
        ck1 = 1      ; ck2 = Nz
      endif
    endif
    ! If 2D domain -> nk = 0; ck = 1 
    if (nk1+nk2==0) then
      ck2 = 1; ck1 = 1
    endif
  endsubroutine compute_dimensions
  !> @}

  !> \ingroup Lib_PostProcessingPublicProcedure
  !> @{
  !> \brief Write ORION structured multiblock data to Tecplot file.
  !> \details Exports ORION block data in Tecplot structured format (ASCII or binary).
  !> Supports both node-centered and cell-centered data with optional boundary conditions.
  !> \param[in] orion ORION data structure containing blocks to write
  !> \param[in] varnames Variable names string (optional)
  !> \param[in] filename Output file name (must end with .dat, .tec, .plt, or .szplt)
  !> \param[in] Nvars Number of variables to write (optional, default: all)
  !> \return err Error code (0 if successful)
  function tec_write_structured_multiblock(orion,varnames,filename,Nvars) result(err)
    implicit none
    type(orion_data), intent(in)              :: orion
    character(len=*), intent(in), optional    :: varnames
    character(len=*), intent(in)              :: filename !< File name of the output file.
    integer, intent(in), optional             :: Nvars    !< Input number of variables saved.
    logical :: meshonly, time_accurate
    integer :: err
# if defined(TECIO)
    integer, external::               tecini142,    &     ! |
                                      tecauxstr142, &     ! |
                                      teczne142,    &     ! | Tecplot external functions.
                                      tecdatd142,   &     ! |
                                      tecend142           ! |
# endif
    character(1), parameter:: tecendrec = char(0) !< End-character for binary-record end.
    character(32768)::        tecvarname          !< Variables name for tecplot header file.
    character(len=:), allocatable:: teczoneheader !< Tecplot string of zone header, of any length: the block name is in it.
    character(500)::          tecvarform          !< Format for variables for tecplot file.
    integer, allocatable::    tecvarloc(:)        !< Tecplot array of variables location.
    character(500)::          tecvarlocstr        !< Tecplot string of variables location.
    integer, allocatable::    tecnull(:)          !< Tecplot null array.
    integer::                 tecunit             !< Free logic unit of tecplot file.
    integer::                 Debug      = 0
    integer::                 VIsDouble  = 0
    integer::                 FileType   = 0
    integer::                 fileFormat ! 0 == PLT, 1 == SZPLT
    integer::                 Nvar                !< Internal number of variables saved.
    integer::                 ndir                !< Internal number of dimensions.
    integer::                 Nblocks             !< Number of blocks.
    integer::                 b                   !< Counter.
    integer::                 NvarTot
    integer::                 gc
    real(R8P)::               time_
  
    ! Preliminary operations
    FileType   = 0
    Debug      = 0
    VIsDouble  = 0
    if (orion%tec%double) VIsDouble = 1   ! 64-bit storage in .plt/.szplt (the data are handed to TecIO as double anyway)
    if (allocated(orion%block(1)%vars) .and. .not.present(Nvars)) then
      meshonly = .false.
      Nvar = size(orion%block(1)%vars,1)
    elseif (allocated(orion%block(1)%vars) .and. present(Nvars)) then
      meshonly = .false.
      Nvar = Nvars
    elseif (.not.allocated(orion%block(1)%vars)) then
      meshonly = .true.
      Nvar = 0
    endif
    Nblocks = size(orion%block)
    ndir = size(orion%block(1)%mesh, 1)
    gc = 1
    ! allocating dynamic arrays
    allocate(tecvarloc(1:ndir+Nvar))
    allocate(tecnull(1:ndir+Nvar))
    ! initializing tecplot variables
    call tec_init()
    ! time
    time_accurate = .true.
    time_ = orion%solutiontime
    if (time_<0) then
      time_ = abs(time_)
      time_accurate = .false.
    endif
    ! initializing tecplot file
    select case(orion%tec%format)
    case('binary')
#   if defined(TECIO)
      if (index(filename,'.plt')>0) then
        fileFormat = 0
      elseif (index(filename,'.szplt')>0) then
        fileFormat = 1
      else
        write(stderr,'(A)')'File name to be written = "'//trim(filename)//'"'
        write(stderr,'(A)')'Tecplot binary file must end with ".plt" or ".szplt"'
        return
      endif
      err = tecini142(tecendrec,trim(tecvarname)//tecendrec,trim(filename)//tecendrec,'.'//tecendrec,fileFormat,FileType,Debug,VIsDouble)
      err = tecauxstr142("Time"//tecendrec,trim(str(n=time_))//tecendrec)
#   else
      stop "You can not write in binary format without compiling against TecIO"
#   endif
    case('ascii')
      open(newunit=tecunit,file=trim(filename))
      write(tecunit,'(A)',iostat=err)trim(tecvarname)
    end select
    ! writing data blocks
    do b=1,Nblocks
      err = tec_blk_data(b = b)
    enddo
    ! finalizing tecplot file
    select case(orion%tec%format)
    case('binary')
# if defined(TECIO)
      err = tecend142()
# endif
    case('ascii')
      close(tecunit)
    end select
    ! deallocating dynamic arrays
    deallocate(tecvarloc)
    deallocate(tecnull)

  contains
    subroutine tec_init()
      implicit none
      integer  :: s ! Counter.
      if (.not.meshonly) then
        NvarTot = ndir + Nvar
      else
        NvarTot = ndir
      endif
      select case(orion%tec%format)
      case('binary')
#     if defined(TECIO)
        ! header variables names
        tecvarname = 'x'
        if (ndir>=2) tecvarname = trim(tecvarname)//' y'
        if (ndir==3) tecvarname = trim(tecvarname)//' z'
        if (.not.meshonly) then
          if (present(varnames)) then
            tecvarname = trim(tecvarname)//' '//unquote(trim(varnames))
          else
            do s = 1, Nvar
              tecvarname = trim(tecvarname)//' var'//trim(str(.true.,s))
            enddo
          endif
        endif
        ! variables location
        if (orion%tec%node) then
          tecvarloc = 1
        else
          tecvarloc(1:ndir) = 1 ; tecvarloc(ndir+1:ndir+Nvar)= 0
        endif
        ! null array
        tecnull = 0
#     else
        stop "You can not write in binary format without compiling against TecIO"
#     endif
      case('ascii')
        ! header variables names
        tecvarname = ' VARIABLES = x'
        if (ndir>=2) tecvarname = trim(tecvarname)//' y'
        if (ndir==3) tecvarname = trim(tecvarname)//' z'
        if (.not.meshonly) then
          if (present(varnames)) then
            tecvarname = trim(tecvarname)//' '//unquote(trim(varnames))
          else
            do s = 1, Nvar
              tecvarname = trim(tecvarname)//' var'//trim(str(.true.,s))
            enddo
          endif
        endif
        ! variables output format
        if (.not.meshonly) then
          write(tecvarform,'(A)')'('//trim(str(no_sign=.true.,n=Nvar))//'('//FR_P//',1X))'
        else
          write(tecvarform,'(A)')'('//trim(str(no_sign=.true.,n=3))//'('//FR_P//',1X))'
        endif
        ! variables location
        if (.not.meshonly) then
          if (orion%tec%node) then
            tecvarlocstr = ', VARLOCATION=([1-'//trim(str(.true.,NvarTot))//']=NODAL)'
          else
            if (Nvar==1) then
              tecvarlocstr = ', VARLOCATION=([1-'//trim(str(.true.,ndir))//']=NODAL,['//trim(str(.true.,ndir+1))//']=CELLCENTERED)'
            else
              tecvarlocstr = ', VARLOCATION=([1-'//trim(str(.true.,ndir))//']=NODAL,['//trim(str(.true.,ndir+1))//'-'//trim(str(.true.,NvarTot))//']=CELLCENTERED)'
            endif
          endif
        else
          tecvarlocstr = ', VARLOCATION=([1-'//trim(str(.true.,ndir))//']=NODAL)'
        endif
      end select
    endsubroutine tec_init

    function tec_blk_data(b) result(err)
      implicit none
      integer, intent(IN):: b           ! Block number.
      integer:: err                     ! Error trapping flag: 0 no errors, >0 error occurs.
      integer:: ni1,ni2,nj1,nj2,nk1,nk2 ! Bounds of dimensions of node-centered data.
      integer:: ci1,ci2,cj1,cj2,ck1,ck2 ! Bounds of dimensions of cell-centered data.
      integer:: nnode,ncell             ! Number of nodes and cells.
      integer:: i,j,k,s                 ! Counters.
      integer:: Nx, Ny, Nz
      integer:: start

      Nx = size(orion%block(b)%mesh,2)-1; Ny = size(orion%block(b)%mesh,3)-1; Nz = size(orion%block(b)%mesh,4)-1
      ! initialize the zone dimensions
      call compute_dimensions(node=orion%tec%node,bc=orion%tec%bc,             &
                              Nx=Nx,Ny=Ny,Nz=Nz,gc=gc,                         &
                              ni1=ni1,ni2=ni2,nj1=nj1,nj2=nj2,nk1=nk1,nk2=nk2, &
                              ci1=ci1,ci2=ci2,cj1=cj1,cj2=cj2,ck1=ck1,ck2=ck2)
      nnode = (ni2-ni1+1)*(nj2-nj1+1)*(nk2-nk1+1)
      ! Cell data: a direction with one plane of nodes (a surface block, such as a face of a volume block) has one layer of
      ! cells, as the variables are written below and as an ordered zone stores its cell-centered values
      ncell = max(ci2-ci1+1,1)*max(cj2-cj1+1,1)*max(ck2-ck1+1,1)
      ! writing the block data
      select case(orion%tec%format)
      case('binary')
#     if defined(TECIO)
        err = teczne142(trim(orion%block(b)%name)//tecendrec,         &
                        0,                                            &
                        ni2-ni1+1,                                    &
                        nj2-nj1+1,                                    &
                        nk2-nk1+1,                                    &
                        0,                                            &
                        0,                                            &
                        0,                                            &
                        time_,                                        &
                        0,                                            &
                        0,                                            &
                        1,                                            & !1=>block,0=>point
                        0,                                            &
                        0,                                            &
                        0,                                            &
                        0,                                            &
                        0,                                            &
                        tecnull(1:NvarTot),                           &
                        tecvarloc(1:NvarTot),                         &
                        tecnull(1:NvarTot),                           &
                        0)
        err=tec_dat(N=nnode,dat=orion%block(b)%mesh(1,ni1:ni2,nj1:nj2,nk1:nk2))
        if (ndir>=2) &
          err=tec_dat(N=nnode,dat=orion%block(b)%mesh(2,ni1:ni2,nj1:nj2,nk1:nk2))
        if (ndir==3) &
          err=tec_dat(N=nnode,dat=orion%block(b)%mesh(3,ni1:ni2,nj1:nj2,nk1:nk2))
        if (.not.meshonly) then
          start = 1
          if (orion%tec%node) start = 0
          ! Cell data: one layer of cells where a direction has one plane of nodes. Data at the nodes keep that plane
          if (.not.orion%tec%node) then
            if (ni2==0) ni2 = 1
            if (nj2==0) nj2 = 1
            if (nk2==0) nk2 = 1
          endif
          ! Force values if 2D
          if (ndir==2) then
            nk1 = 1-start; nk2 = 1
          endif
          ! Variables whose index range in a direction has the length of the values there but other bounds are taken where
          ! they are, such as the node plane vars(:,:,:,0:0) of a two-dimensional block as the Tecplot readers return it
          call var_range(b, 2, start, ni1, ni2)
          call var_range(b, 3, start, nj1, nj2)
          call var_range(b, 4, start, nk1, nk2)
          do s=1,Nvar
            err=tec_dat(N=ncell,dat=orion%block(b)%vars(s,ni1+start:ni2,nj1+start:nj2,nk1+start:nk2))
          enddo
        endif
#     endif
      case('ascii')
        ! tecplot zone header
        teczoneheader = ' ZONE  T = '//trim(orion%block(b)%name)//            &
                        ', I='//trim(str(no_sign=.true.,n=ni2-ni1+1))//       &
                        ', J='//trim(str(no_sign=.true.,n=nj2-nj1+1))//       &
                        ', K='//trim(str(no_sign=.true.,n=nk2-nk1+1))//       &
                        ', DATAPACKING=BLOCK'//adjustl(trim(tecvarlocstr))
        
        teczoneheader = trim(teczoneheader)//', SOLUTIONTIME='//trim(str(no_sign=.true.,n=time_))
        if (.not. time_accurate) teczoneheader = trim(teczoneheader)//', STRANDID = 0'
        write(tecunit,'(A)',iostat=err)trim(teczoneheader)
        write(tecunit,FR_P,iostat=err)(((orion%block(b)%mesh(1,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
        if (ndir>1) &
          write(tecunit,FR_P,iostat=err)(((orion%block(b)%mesh(2,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
        if (ndir>2) &
          write(tecunit,FR_P,iostat=err)(((orion%block(b)%mesh(3,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
        if (.not.meshonly) then
          start = 1
          if (orion%tec%node) start = 0
          ! Cell data: one layer of cells where a direction has one plane of nodes. Data at the nodes keep that plane
          if (.not.orion%tec%node) then
            if (ni2==0) ni2 = 1
            if (nj2==0) nj2 = 1
            if (nk2==0) nk2 = 1
          endif
          ! Force values if 2D
          if (ndir==2) then
            nk1 = 1-start; nk2 = 1
          endif
          ! Variables whose index range in a direction has the length of the values there but other bounds are taken where
          ! they are, such as the node plane vars(:,:,:,0:0) of a two-dimensional block as the Tecplot readers return it
          call var_range(b, 2, start, ni1, ni2)
          call var_range(b, 3, start, nj1, nj2)
          call var_range(b, 4, start, nk1, nk2)
          do s=1,Nvar
            write(tecunit,FR_P,iostat=err)(((orion%block(b)%vars(s,i,j,k),i=ni1+start,ni2),j=nj1+start,nj2),k=nk1+start,nk2)
          enddo
        endif
      end select
    endfunction tec_blk_data

    !> The index range n1+start:n2 of the variables of block b in direction d (2 for i, 3 for j, 4 for k), as tec_blk_data
    !> computes it, becomes the range the variables have in that direction when it has the same length: the node plane of a
    !> two-dimensional block may be vars(:,:,:,0:0), as the Tecplot readers return it, or vars(:,:,:,1:1). A range of another
    !> length, such as that of variables with ghost cells, is kept: the values taken from variables that the range fitted are
    !> those taken before.
    subroutine var_range(b, d, start, n1, n2)
      implicit none
      integer, intent(in)    :: b, d, start ! Block, dimension of vars, first index of the values (0 nodes, 1 cells).
      integer, intent(inout) :: n1, n2      ! The range n1+start:n2.
      if (size(orion%block(b)%vars,d) == n2-n1-start+1) then
        n1 = lbound(orion%block(b)%vars,d) - start
        n2 = ubound(orion%block(b)%vars,d)
      endif
    endsubroutine var_range

    !> `"a""b"` -> ` a  b `: the binary header's separator convention.
    pure function unquote(sin) result(sout)
      implicit none
      character(len=*), intent(in) :: sin
      character(len=len(sin))      :: sout
      integer :: i
      do i = 1, len(sin)
        if (sin(i:i) == '"') then
          sout(i:i) = ' '
        else
          sout(i:i) = sin(i:i)
        endif
      enddo
    endfunction unquote

#   if defined(TECIO)
    ! Function interface for using "tecdat" function.
    !
    ! The callers hand over a section with a FIXED first index of a rank-4 array
    ! (mesh(1,...), vars(s,...)), which is strided, not contiguous. Declaring
    ! `dat` with an explicit shape would therefore make the compiler pack it
    ! into a temporary at the call -- and ifort puts array temporaries on the
    ! STACK, so any block above ~350k points blew the default 8 MB limit and the
    ! writer died with SIGSEGV. The dummy is assumed-shape (and deliberately NOT
    ! contiguous) so the section arrives as a descriptor with no copy, and the
    ! contiguous buffer TecIO needs is built here, on the heap.
    function tec_dat(N,dat) result(err)
      implicit none
      integer,   intent(IN)  :: N          ! Number of data to save.
      real(R8P), intent(IN)  :: dat(:,:,:) ! Data to save.
      real(R8P), allocatable :: buff(:)    ! Contiguous copy handed to TecIO.
      integer :: err                       ! Error trapping flag: 0 no errors, >0 error occurs.
      integer :: i,j,k,n_                  ! Counters.
      allocate(buff(1:N))
      ! Array element order: i fastest, then j, then k -- the order tecdat expects.
      n_ = 0
      do k=1,size(dat,3); do j=1,size(dat,2); do i=1,size(dat,1)
        n_ = n_ + 1
        buff(n_) = dat(i,j,k)
      enddo; enddo; enddo
      err = tecdatd142(N,buff)
      deallocate(buff)
    endfunction tec_dat
#   endif

  endfunction tec_write_structured_multiblock


  !> \brief Write ORION point data to Tecplot file.
  !> \details Exports ORION point data in Tecplot point format.
  !> Each block is written as a separate zone with point values.
  !> \param[in] orion ORION data structure containing blocks to write
  !> \param[in] varnames Variable names string (optional)
  !> \param[in] filename Output file name
  !> \param[in] Nvars Number of variables to write (optional)
  !> \return err Error code (0 if successful)
  function tec_write_points_multivars(orion,varnames,filename,Nvars) result(err)
    implicit none
    type(orion_data), intent(in)              :: orion
    character(len=*), intent(in), optional    :: varnames
    character(len=*), intent(in)              :: filename
    integer, intent(in), optional             :: Nvars
    integer :: err
    character(32768)::        tecvarname          !< Variables name for tecplot header file.
    character(len=:), allocatable:: teczoneheader !< Tecplot string of zone header, of any length: the block name is in it.
    character(500)::          tecvarform          !< Format for variables for tecplot file.
    integer::                 tecunit             !< Free logic unit of tecplot file.
    integer::                 Nvar                !< Internal number of variables saved.
    integer::                 Nblocks             !< Number of blocks.
    integer::                 b                   !< Counter.
  
    ! Preliminary operations
    if (allocated(orion%block(1)%vars) .and. .not.present(Nvars)) then
      Nvar = size(orion%block(1)%vars,1)
    elseif (allocated(orion%block(1)%vars) .and. present(Nvars)) then
      Nvar = Nvars
    elseif (.not.allocated(orion%block(1)%vars)) then
      err = 1
      return
    endif
    Nblocks = size(orion%block)

    ! initializing tecplot variables
    call tec_init()

    ! initializing tecplot file
    open(newunit=tecunit,file=trim(filename))
    write(tecunit,'(A)',iostat=err)trim(tecvarname)
    ! writing data blocks
    do b=1,Nblocks
      err = tec_blk_data(b = b)
    enddo
    ! finalizing tecplot file
    close(tecunit)

  contains
    subroutine tec_init()
      implicit none
      integer  :: s ! Counter.
      ! header variables names
      tecvarname = ' VARIABLES ='
      if (present(varnames)) then
        tecvarname = trim(tecvarname)//' '//trim(varnames)
      else
        do s = 1, Nvar
          tecvarname = trim(tecvarname)//' "var'//trim(str(.true.,s))//'"'
        enddo
      endif
      ! variables output format
      write(tecvarform,'(A)')'('//trim(str(no_sign=.true.,n=Nvar))//'('//FR_P//',1X))'
    endsubroutine tec_init

    function tec_blk_data(b) result(err)
      implicit none
      integer, intent(IN):: b           ! Block number.
      integer:: err                     ! Error trapping flag: 0 no errors, >0 error occurs.
      integer:: i
      integer:: Nx

      Nx = size(orion%block(b)%mesh,2)
      ! tecplot zone header
      teczoneheader = ' ZONE  T = '//trim(orion%block(b)%name)//     &
                      ', I='//trim(str(no_sign=.true.,n=Nx))//       &
                      ', F=POINT'
      write(tecunit,'(A)',iostat=err)trim(teczoneheader)
      do i = 1, Nx
        write(tecunit,*,iostat=err) orion%block(b)%mesh(1,i,1,1), orion%block(b)%vars(1:nvar,i,1,1)
      enddo
    endfunction tec_blk_data

  endfunction tec_write_points_multivars


  !> \brief Read ORION structured multiblock data from Tecplot file.
  !> \details Reads Tecplot structured format files (ASCII or binary) into ORION data structure.
  !> Automatically detects file format based on extension.
  !> \param[inout] orion ORION data structure to fill with data
  !> \param[in] filename Input file name (.dat, .tec, or .szplt)
  !> \param[in] zone_mask Optional per-zone selector: variable data is read only
  !>            for zones whose entry is .true.  Coordinates are always read.
  !> \param[in] dims_only Optional; if .true. read only the zone headers.
  !> \return err Error code (0 if successful)
  !> \note Only the .szplt path honours zone_mask and dims_only -- the ASCII
  !>       format has no per-zone index, so a caller cannot learn a zone's
  !>       dimensions without reading it.  The ASCII reader therefore ignores
  !>       both arguments and performs a full read, which is correct but not
  !>       lean; callers that care must check the extension first.
  function tec_read_structured_multiblock(orion,filename,zone_mask,dims_only) result(err)
    implicit none
    type(orion_data), intent(inout)                        :: orion
    character(len=*), intent(in)                           :: filename
    logical,          intent(in), optional                 :: zone_mask(:)
    logical,          intent(in), optional                 :: dims_only
    integer :: err

    if (index(filename,'.dat')>0 .or. index(filename,'.tec')>0) then
      err = tec_read_ascii(orion,filename)
    elseif (index(filename,'.szplt')>0) then
#     if defined(TECIO)
      err = tec_read_szplt(orion,filename,zone_mask,dims_only)
#     else
      stop "You can not read in binary format without compiling against TecIO"
#     endif
    endif

  end function tec_read_structured_multiblock


  !> \brief Read ORION structured multiblock data from Tecplot ASCII file.
  !> \details Parses Tecplot ASCII format files and populates ORION data structure.
  !> Handles variable extraction, block dimensions, and solution time.
  !> \param[inout] orion ORION data structure to fill with data
  !> \param[in] filename Input file name (.dat or .tec)
  !> \return err Error code (0 if successful)
  !> \brief Read ORION structured multiblock data from Tecplot ASCII file.
  !> \details Parses Tecplot ASCII structured zones using whitespace-separated numeric tokens.
  !> Supports both BLOCK and POINT data packing; physical line breaks are irrelevant to the
  !> numerical data stream. Nodal and cell-centered variables are supported for BLOCK data.
  !> POINT data is supported when all variables are nodal.
  !> Every zone is read with the number of coordinates of the file: three when a zone has
  !> more than one node plane in K, or when the header declares a third coordinate, the first
  !> three variables named x, y, z and z nodal in every zone (single node planes or lines of
  !> a 3-D field, such as a slice); otherwise two when a zone has more than one node line in
  !> J, or when the first two variables are named x and y and y is nodal in every zone (lines
  !> of a 2-D field); otherwise one (a file of lines).
  !> The coordinates are at the nodes and the solution variables all at the nodes or all at the
  !> cells (orion%tec%node): a file with a coordinate at the cells, or with solution variables
  !> at the nodes and at the cells, in a zone or between zones, is refused (return value 1, with
  !> a message).
  function tec_read_ascii(orion,filename) result(err)
    use, intrinsic :: iso_fortran_env, only : iostat_end
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: filename

    real(R8P) :: solutiontime, value
    logical   :: meshonly, zone_node
    integer   :: err
    integer   :: strandid                    ! STRANDID of the last zone header, -1 when absent
    integer   :: tecunit, ios
    integer   :: i, j, k, d, b, s
    integer   :: Nblocks, nvar, ndir
    integer   :: Imax, Jmax, Kmax
    integer   :: start
    logical, allocatable :: zone_point(:), zone_node_arr(:)
    logical, allocatable :: zone_z_cell(:)   ! variable 3 listed as CELLCENTERED in the zone header
    logical, allocatable :: zone_cc(:,:)     ! variable v listed as CELLCENTERED in the header of zone b
    logical   :: plane_xyz                   ! the header declares x, y and z as coordinates (a slice)
    logical   :: line_xy                     ! the header declares x and y as coordinates (lines of a 2-D field)
    integer, allocatable :: Ni(:), Nj(:), Nk(:)
    ! A line and a zone header of any length (read with read_whole_line): the title of a zone is as long as its block name
    character(len=:), allocatable :: line
    character(len=:), allocatable :: header
    character(32768) :: hline               ! header lines only (VARIABLES and what precedes it)
    character(32768) :: variables_header
    character(32768) :: vline
    logical :: found_variables

    ! Persistent tokenizer state. This is deliberately line-based only at the lexical
    ! level: next_value() returns the next numeric token, irrespective of line breaks.
    character(1000) :: data_line
    integer :: data_pos
    integer :: physical_line_number
    integer :: data_line_number
    integer(kind=8) :: data_value_number
    logical :: have_data_line

    meshonly = .false.
    orion%tec%node = .true.
    solutiontime = -10._R8P
    strandid = -1
    err = 0

    ! Open file
    open(newunit=tecunit,file=trim(filename),status='old',action='read',iostat=err)
    if (err/=0) return

    ! -----------------------------------------------------------------------------
    ! Read variable names from the VARIABLES header. Tecplot permits the
    ! variable declarations to span multiple physical lines, e.g.
    !
    !   VARIABLES = "x"
    !   "y"
    !   "z"
    !
    ! Collect the complete header up to the first ZONE line before parsing it.
    ! -----------------------------------------------------------------------------
    variables_header = ''
    found_variables = .false.
    ios = 0
    do while (ios==0)
      read(tecunit,'(A)',iostat=ios) hline
      if (ios/=0) exit

      if (index(upper_case(hline),'VARIABLES')>0) then
        variables_header = trim(hline)
        found_variables = .true.

        ! Continue through subsequent header lines until the first ZONE.
        do
          read(tecunit,'(A)',iostat=ios) vline
          if (ios/=0) exit
          if (is_zone_header(vline)) exit
          if (len_trim(vline)>0) then
            variables_header = trim(variables_header)//' '//trim(vline)
          endif
        enddo
        exit
      endif
    enddo

    if (.not.found_variables) then
      write(stderr,'(A)') 'TECPLOT ASCII READ ERROR: VARIABLES header not found.'
      err = 1
      close(tecunit)
      return
    endif

    call read_variables(variables_header,orion%varnames)
    if (.not.allocated(orion%varnames)) then
      write(stderr,'(A)') 'TECPLOT ASCII READ ERROR: no variables found in VARIABLES header.'
      write(stderr,'(A)') 'Header collected as:'
      write(stderr,'(A)') trim(variables_header)
      err = 1
      close(tecunit)
      return
    endif

    rewind(tecunit)

    ! -----------------------------------------------------------------------------
    ! First pass: locate all structured zones and read their dimensions/packing.
    ! A zone header may span several physical lines. We regard the first subsequent
    ! line whose first token is numeric as the beginning of the data section.
    ! -----------------------------------------------------------------------------
    Nblocks = 0
    ios = 0
    do while (ios==0)
      call read_whole_line(tecunit,line,ios)
      if (ios/=0) exit
      if (is_zone_header(line)) Nblocks = Nblocks + 1
    enddo

    if (Nblocks<=0) then
      err = 1
      close(tecunit)
      return
    endif

    allocate(Ni(Nblocks),Nj(Nblocks),Nk(Nblocks),zone_point(Nblocks),zone_node_arr(Nblocks))
    allocate(zone_z_cell(Nblocks))
    allocate(zone_cc(size(orion%varnames),Nblocks))
    Ni = 0; Nj = 0; Nk = 1
    zone_point = .false.
    zone_node_arr = .true.
    zone_z_cell = .false.
    zone_cc = .false.

    rewind(tecunit)
    b = 0
    ios = 0
    do while (ios==0)
      call read_whole_line(tecunit,line,ios)
      if (ios/=0) exit
      physical_line_number = physical_line_number + 1
      if (.not.is_zone_header(line)) cycle

      b = b + 1
      header = trim(line)
      data_value_number = 0_8
      data_line_number = 0

      ! Gather continuation header lines until the first numeric data line.
      do
        call read_whole_line(tecunit,line,ios)
        if (ios/=0) exit
        physical_line_number = physical_line_number + 1
        if (line_is_numeric_start(line)) then
          data_line = line
          data_pos = 1
          have_data_line = .true.
          data_line_number = physical_line_number
          exit
        endif
        header = trim(header)//' '//trim(line)
      enddo

      call parse_zone_header(header,Ni(b),Nj(b),Nk(b),zone_point(b),zone_node_arr(b),solutiontime,strandid)
      zone_z_cell(b) = var_cellcentered(upper_case(header),3)
      do s = 1, size(orion%varnames)
        zone_cc(s,b) = var_cellcentered(upper_case(header),s)
      enddo
      if (Ni(b)<=0 .or. Nj(b)<=0 .or. Nk(b)<=0) then
        write(stderr,'(/,A)') 'TECPLOT ASCII READ ERROR: invalid zone dimensions.'
        write(stderr,'(A,I0)') 'Zone : ', b
        write(stderr,'(A,I0,A,I0,A,I0)') 'I=',Ni(b),' J=',Nj(b),' K=',Nk(b)
        write(stderr,'(A)') 'Zone header:'
        write(stderr,'(A)') trim(header)
        err = 1
        exit
      endif
      if (err/=0) exit

      ! We have already consumed the first data line. Consume its tokens below
      ! only through the second pass; the first pass does not need the values.
      have_data_line = .false.
    enddo

    if (err/=0 .or. b/=Nblocks) then
      if (err==0) err = 1
      close(tecunit)
      return
    endif

    orion%solutiontime = solutiontime
    orion%strandid = strandid

    ! The number of coordinates is that of the file, the same for every zone (the variables are those
    ! of the file): three when a zone has more than one node plane in K, or when the header declares a
    ! third coordinate: the first three variables are named x, y and z (any case) and z is NODAL in
    ! every zone (not listed as CELLCENTERED in VARLOCATION), so that a file of single node planes or
    ! lines is part of a 3-D field (a slice): z is read as the third coordinate, not as the first
    ! solution variable, so every variable keeps its own band. Otherwise two when a zone has more than
    ! one node line in J (a pure 2-D file, x and y); otherwise one (a file of lines, I-ordered zones).
    ! A zone with one node plane or line in a direction holds one layer of cells there.
    plane_xyz = .false.
    if (size(orion%varnames)>=3) then
      plane_xyz = is_coordinate_name(orion%varnames(1),'X') .and. &
                  is_coordinate_name(orion%varnames(2),'Y') .and. &
                  is_coordinate_name(orion%varnames(3),'Z') .and. .not.any(zone_z_cell)
    endif
    ! The same rule for two coordinates: the first two variables named x and y (any case), y NODAL in every zone, are the
    ! coordinates also of a file of single node lines, the lines of a 2-D field
    line_xy = .false.
    if (size(orion%varnames)>=2) then
      line_xy = is_coordinate_name(orion%varnames(1),'X') .and. &
                is_coordinate_name(orion%varnames(2),'Y') .and. .not.any(zone_cc(2,:))
    endif
    if (any(Nk>1) .or. plane_xyz) then
      ndir = 3
    elseif (any(Nj>1) .or. line_xy) then
      ndir = 2
    else
      ndir = 1
      ! A file of lines only does not tell a 1-D field from lines of a 2-D or 3-D one
      if (size(orion%varnames)>=2) then
        if (is_coordinate_name(orion%varnames(2),'Y')) write(stderr,'(A)') &
          'TECPLOT ASCII READ WARNING: every zone is a line of nodes (J = K = 1); the file is read with one coordinate '// &
          'and its variable '//trim(orion%varnames(2))//' as a solution variable (x and y are the coordinates of a file of '// &
          'lines when they are its first two variables and y is NODAL).'
      endif
    endif

    if (size(orion%varnames)<ndir) then
      err = 1
      close(tecunit)
      return
    endif
    nvar = size(orion%varnames) - ndir
    if (nvar==0) meshonly = .true.

    ! ORION holds the coordinates at the nodes and one location for all solution variables of a file (orion%tec%node): at the
    ! nodes when the zone headers list no variable as CELLCENTERED, at the cells otherwise. A file with a coordinate at the
    ! cells, or with solution variables at the nodes and at the cells, in a zone or between zones, cannot be held and is
    ! refused with a message: a zone of both was read as if every solution variable were at the cells, its values shifted.
    do b = 1, Nblocks
      if (any(zone_cc(1:ndir,b))) then
        write(stderr,'(/,A)') 'TECPLOT ASCII READ ERROR: a coordinate at the cells; ORION reads the coordinates at the nodes.'
        write(stderr,'(A,I0)') 'Zone : ', b
        err = 1
        close(tecunit)
        return
      endif
      if (any(zone_cc(ndir+1:,b)) .and. .not.all(zone_cc(ndir+1:,b))) then
        write(stderr,'(/,A)') 'TECPLOT ASCII READ ERROR: solution variables at the nodes and at the cells in one zone; '// &
          'ORION reads files whose solution variables are all at the nodes or all at the cells.'
        write(stderr,'(A,I0)') 'Zone : ', b
        err = 1
        close(tecunit)
        return
      endif
    enddo
    if (any(zone_node_arr .neqv. zone_node_arr(1))) then
      write(stderr,'(/,A)') 'TECPLOT ASCII READ ERROR: solution variables at the nodes in some zones and at the cells in '// &
        'others; ORION reads files whose solution variables are all at the nodes or all at the cells.'
      err = 1
      close(tecunit)
      return
    endif
    zone_node = zone_node_arr(1)
    orion%tec%node = zone_node
    if (orion%tec%node) then
      start = 0
    else
      start = 1
    endif

    ! POINT packing with mixed nodal/cell-centered variables has no single common
    ! point count for all variables. The current ORION representation assumes a
    ! common storage location, so require nodal data for POINT zones.
    if (any(zone_point) .and. .not.orion%tec%node) then
      write(stderr,'(/,A)') 'TECPLOT ASCII READ ERROR: variables at the cells in POINT data packing; cell-centered '// &
        'variables are written in BLOCK packing.'
      err = 1
      close(tecunit)
      return
    endif

    ! Allocate ORION blocks once the zone geometry is known.
    allocate(orion%block(1:Nblocks))
    do b = 1, Nblocks
      orion%block(b)%Ni = Ni(b)-1
      orion%block(b)%Nj = Nj(b)-1
      orion%block(b)%Nk = Nk(b)-1
    enddo

    ! -----------------------------------------------------------------------------
    ! Second pass: read the numerical stream. No line counting is used here.
    ! next_value() lexes whitespace/comma-separated values from arbitrary physical
    ! lines, so both of these are equivalent:
    !
    !   X1 X2 X3 X4 ...
    !
    ! and
    !
    !   X1
    !   X2
    !   X3
    !   X4
    !
    ! -----------------------------------------------------------------------------
    rewind(tecunit)
    have_data_line = .false.
    data_pos = 1
    physical_line_number = 0
    data_line_number = 0
    data_value_number = 0_8
    b = 0
    ios = 0

    do while (ios==0 .and. b<Nblocks)
      call read_whole_line(tecunit,line,ios)
      if (ios/=0) exit
      if (.not.is_zone_header(line)) cycle

      b = b + 1
      header = trim(line)

      ! Find the first data line for this zone while collecting continuation header lines.
      do
        call read_whole_line(tecunit,line,ios)
        if (ios/=0) exit
        if (line_is_numeric_start(line)) then
          data_line = line
          data_pos = 1
          have_data_line = .true.
          exit
        endif
        header = trim(header)//' '//trim(line)
      enddo

      ! Header was already parsed in the first pass. Use the stored dimensions/packing.
      Imax = Ni(b)
      Jmax = Nj(b)
      Kmax = Nk(b)
      ! The block takes the title of its zone as its name, as in tec_read_szplt
      orion%block(b)%name = zone_title(header)

      allocate(orion%block(b)%mesh(1:ndir,0:Imax-1,0:Jmax-1,0:Kmax-1))
      if (.not.meshonly) then
        allocate(orion%block(b)%vars(1:nvar,start:max(start,Imax-1), &
                                     start:max(start,Jmax-1),start:max(start,Kmax-1)))
      endif

      if (zone_point(b)) then
        ! Tecplot POINT: one complete variable tuple per node.
        do k = 0, Kmax-1
          do j = 0, Jmax-1
            do i = 0, Imax-1
              do d = 1, ndir
                call next_value(tecunit,value,err)
                if (err/=0) exit
                orion%block(b)%mesh(d,i,j,k) = value
              enddo
              if (err/=0) exit
              d = ndir + 1
              do s = 1, nvar
                call next_value(tecunit,value,err)
                if (err/=0) exit
                orion%block(b)%vars(s,i,j,k) = value
              enddo
              if (err/=0) exit
            enddo
            if (err/=0) exit
          enddo
          if (err/=0) exit
        enddo
      else
        ! Tecplot BLOCK: one complete field after another.
        do d = 1, ndir
          do k = 0, Kmax-1
            do j = 0, Jmax-1
              do i = 0, Imax-1
                call next_value(tecunit,value,err)
                if (err/=0) exit
                orion%block(b)%mesh(d,i,j,k) = value
              enddo
              if (err/=0) exit
            enddo
            if (err/=0) exit
          enddo
          if (err/=0) exit
        enddo

        if (.not.meshonly) then
          d = ndir + 1
          do s = 1, nvar
            do k = start, max(start,Kmax-1)
              do j = start, max(start,Jmax-1)
                do i = start, max(start,Imax-1)
                  call next_value(tecunit,value,err)
                  if (err/=0) exit
                  orion%block(b)%vars(s,i,j,k) = value
                enddo
                if (err/=0) exit
              enddo
              if (err/=0) exit
            enddo
            if (err/=0) exit
          enddo
        endif
      endif

      if (err/=0) exit

      ! Reset lexical state before searching for the next ZONE header.
      have_data_line = .false.
      data_pos = 1
    enddo

    if (err==0 .and. b/=Nblocks) err = 1

    close(tecunit)

  contains

    ! A zone record starts with the keyword ZONE as a whole word: a name that merely begins with
    ! it (zone_velocity, in a continuation line of the VARIABLES list) is not one.
    logical function is_zone_header(text)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: u
      u = adjustl(upper_case(text))
      is_zone_header = (index(u,'ZONE')==1)
      if (is_zone_header .and. len(u)>4) &
        is_zone_header = .not.(scan(u(5:5),'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_')>0)
    endfunction is_zone_header

    logical function line_is_numeric_start(text)
      character(len=*), intent(in) :: text
      character(len=:), allocatable :: t   ! the line, of any length
      character(100) :: tok
      integer :: p, q, ios_
      t = adjustl(text)
      if (len_trim(t)==0) then
        line_is_numeric_start = .false.
        return
      endif
      p = 1
      do while (p<=len_trim(t))
        if (t(p:p)/=' ' .and. t(p:p)/=char(9) .and. t(p:p)/=',') exit
        p = p+1
      enddo
      if (p>len_trim(t)) then
        line_is_numeric_start = .false.
        return
      endif
      q = p
      do while (q<=len_trim(t))
        if (t(q:q)==' ' .or. t(q:q)==char(9) .or. t(q:q)==',') exit
        q = q+1
      enddo
      tok = ' '
      tok(1:min(len(tok),q-p)) = t(p:q-1)
      read(tok,*,iostat=ios_) value
      line_is_numeric_start = (ios_==0)
    endfunction line_is_numeric_start

    subroutine parse_zone_header(text,I_,J_,K_,point_,node_,time_,strand_)
      character(len=*), intent(in) :: text
      integer, intent(out) :: I_,J_,K_
      logical, intent(out) :: point_,node_
      real(R8P), intent(inout) :: time_
      integer, intent(out) :: strand_   ! STRANDID of this zone, -1 when absent
      character(len=:), allocatable :: work   ! the zone header, of any length
      character(100) :: token
      integer :: ios_
      real(R8P) :: t_
      logical :: found

      I_ = 0; J_ = 0; K_ = 1
      point_ = .false.
      node_ = .true.

      work = upper_case(text)

      call get_integer_keyword(work,'I',I_)
      call get_integer_keyword(work,'J',J_)
      ! J omitted: one node line, an I-ordered zone (I only, as in the examples of Tecplot); a J that is not a whole number
      ! stays 0, and the zone is refused
      if (keyword_value_pos(work,'J')==0) J_ = 1
      call get_integer_keyword(work,'K',K_,found)
      if (.not.found) K_ = 1   ! K omitted: one node plane (Tecplot's default K = 1)

      ! DATAPACKING=POINT|BLOCK, or the older F=POINT|BLOCK
      call get_word_keyword(work,'DATAPACKING',token,found)
      if (.not.found) call get_word_keyword(work,'F',token,found)
      ! Tecplot's default is POINT for some contexts; for structured zones the
      ! writer in this library emits BLOCK, so retain BLOCK as the safe default.
      point_ = found .and. trim(token)=='POINT'

      ! Cell-centred data can only be declared inside VARLOCATION=(...)
      node_ = (index(varlocation_value(work),'CELLCENTERED')==0)

      call get_word_keyword(work,'SOLUTIONTIME',token,found)
      if (found) then
        read(token,*,iostat=ios_) t_
        if (ios_==0) time_ = t_
      endif

      ! STRANDID = 0 marks a static zone: tec_write_structured_multiblock writes it for a steady solution
      call get_integer_keyword(work,'STRANDID',strand_,found)
      if (.not.found) strand_ = -1
    endsubroutine parse_zone_header

    ! Value of `key` up to the next separator (comma, blank, tab or ')').
    subroutine get_word_keyword(text,key,out,found)
      character(len=*), intent(in) :: text,key
      character(len=*), intent(out) :: out
      logical, intent(out) :: found
      integer :: q, r, n, lt

      out = ' '
      q = keyword_value_pos(text,key)
      found = (q>0)
      if (.not.found) return

      lt = len_trim(text)
      n = 0
      do r = q, lt
        if (index(', )'//char(9),text(r:r))>0 .or. n>=len(out)) exit
        n = n + 1
        out(n:n) = text(r:r)
      enddo
      found = (n>0)
    endsubroutine get_word_keyword

    ! Integer value of `key`. `found` is true only if the key is present and its
    ! value parses.
    subroutine get_integer_keyword(text,key,out,found)
      character(len=*), intent(in) :: text,key
      integer, intent(out) :: out
      logical, intent(out), optional :: found
      character(100) :: token
      integer :: ios_
      logical :: ok

      out = 0
      call get_word_keyword(text,key,token,ok)
      if (ok) then
        read(token,*,iostat=ios_) out
        ok = (ios_==0)
        if (.not.ok) out = 0
      endif
      if (present(found)) found = ok
    endsubroutine get_integer_keyword

    ! The parenthesised value of VARLOCATION=(...), or blank if absent.
    function varlocation_value(text) result(out)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: out
      integer :: q, r

      out = ' '
      q = keyword_value_pos(text,'VARLOCATION')
      if (q<=0) return
      if (text(q:q)/='(') return
      r = index(text(q:),')')
      if (r<=0) then
        r = len_trim(text)
      else
        r = q + r - 1
      endif
      out = text(q:r)
    endfunction varlocation_value

    subroutine next_value(unit,x,istat)
      use, intrinsic :: iso_fortran_env, only : iostat_end
      integer, intent(in) :: unit
      real(R8P), intent(out) :: x
      integer, intent(out) :: istat

      character(1000) :: t
      character(100)  :: tok
      integer :: p, q, L, ios_, token_len

      istat = 0

      do
        ! -----------------------------------------------------------------------
        ! Fetch another physical line only when the current line has no more
        ! numeric tokens. Tecplot ASCII data are whitespace-separated; line
        ! breaks have no semantic meaning for the numerical data stream.
        ! -----------------------------------------------------------------------
        if (.not.have_data_line .or. data_pos > len_trim(data_line)) then
          read(unit,'(A)',iostat=ios_) t

          if (ios_ == iostat_end) then
            write(stderr,'(/,A)') &
              '=============================================================='
            write(stderr,'(A)') &
              'TECPLOT ASCII READ ERROR: unexpected end of file.'
            write(stderr,'(A,I0)') 'Zone               : ', b
            write(stderr,'(A,A)') 'Packing            : ', &
              merge('POINT','BLOCK',zone_point(b))
            write(stderr,'(A,I0)') 'Physical line      : ', data_line_number
            write(stderr,'(A,I0)') 'Numeric value #    : ', data_value_number + 1_8
            if (d <= ndir) then
              write(stderr,'(A,I0)') 'Coordinate variable: ', d
            else
              write(stderr,'(A,I0)') 'Solution variable  : ', s
            endif
            write(stderr,'(A,I0)') 'i                  : ', i
            write(stderr,'(A,I0)') 'j                  : ', j
            write(stderr,'(A,I0)') 'k                  : ', k
            write(stderr,'(A)') &
              'Expected another numeric value, but EOF was reached.'
            write(stderr,'(A)') &
              '=============================================================='
            istat = 1
            return

          elseif (ios_ /= 0) then
            write(stderr,'(/,A)') &
              '=============================================================='
            write(stderr,'(A)') &
              'TECPLOT ASCII READ ERROR: failure reading a data line.'
            write(stderr,'(A,I0)') 'Zone               : ', b
            write(stderr,'(A,I0)') 'Physical line      : ', physical_line_number + 1
            write(stderr,'(A,I0)') 'Fortran IOSTAT     : ', ios_
            write(stderr,'(A,I0)') 'Numeric value #    : ', data_value_number + 1_8
            write(stderr,'(A,I0)') 'i                  : ', i
            write(stderr,'(A,I0)') 'j                  : ', j
            write(stderr,'(A,I0)') 'k                  : ', k
            write(stderr,'(A)') &
              '=============================================================='
            istat = ios_
            return
          endif

          physical_line_number = physical_line_number + 1
          data_line_number = physical_line_number
          data_line = t
          data_pos = 1
          have_data_line = .true.
        endif

        ! -----------------------------------------------------------------------
        ! Skip whitespace and commas.
        ! -----------------------------------------------------------------------
        L = len_trim(data_line)
        do while (data_pos <= L)
          if (data_line(data_pos:data_pos) /= ' ' .and. &
              data_line(data_pos:data_pos) /= char(9) .and. &
              data_line(data_pos:data_pos) /= ',') exit
          data_pos = data_pos + 1
        enddo

        if (data_pos > L) then
          have_data_line = .false.
          cycle
        endif

        ! -----------------------------------------------------------------------
        ! Extract one whitespace/comma-delimited token.
        ! -----------------------------------------------------------------------
        p = data_pos
        q = p
        do while (q <= L)
          if (data_line(q:q) == ' ' .or. &
              data_line(q:q) == char(9) .or. &
              data_line(q:q) == ',') exit
          q = q + 1
        enddo

        token_len = q - p
        if (token_len <= 0) then
          data_pos = q + 1
          cycle
        endif

        if (token_len > len(tok)) then
          write(stderr,'(/,A)') &
            '=============================================================='
          write(stderr,'(A)') &
            'TECPLOT ASCII READ ERROR: numeric token is too long.'
          write(stderr,'(A,I0)') 'Zone               : ', b
          write(stderr,'(A,A)') 'Packing            : ', &
            merge('POINT','BLOCK',zone_point(b))
          write(stderr,'(A,I0)') 'Physical line      : ', data_line_number
          write(stderr,'(A,I0)') 'Numeric value #    : ', data_value_number + 1_8
          write(stderr,'(A,I0)') 'i                  : ', i
          write(stderr,'(A,I0)') 'j                  : ', j
          write(stderr,'(A,I0)') 'k                  : ', k
          if (d <= ndir) then
            write(stderr,'(A,I0)') 'Coordinate variable: ', d
          else
            write(stderr,'(A,I0)') 'Solution variable  : ', s
          endif
          write(stderr,'(A,I0)') 'Token length       : ', token_len
          write(stderr,'(A)') 'Complete line      : '//trim(data_line)
          write(stderr,'(A)') &
            '=============================================================='
          istat = 1
          return
        endif

        tok = ' '
        tok(1:token_len) = data_line(p:q-1)
        data_pos = q

        read(tok,*,iostat=ios_) x
        if (ios_ == 0) then
          data_value_number = data_value_number + 1_8
          return
        endif

        ! -----------------------------------------------------------------------
        ! Non-numeric token in the data stream.
        ! -----------------------------------------------------------------------
        write(stderr,'(/,A)') &
          '=============================================================='
        write(stderr,'(A)') &
          'TECPLOT ASCII READ ERROR: invalid numeric token.'
        write(stderr,'(A,I0)') 'Zone               : ', b
        write(stderr,'(A,A)') 'Packing            : ', &
          merge('POINT','BLOCK',zone_point(b))
        write(stderr,'(A,I0)') 'Physical line      : ', data_line_number
        write(stderr,'(A,I0)') 'Numeric value #    : ', data_value_number + 1_8
        write(stderr,'(A,I0)') 'i                  : ', i
        write(stderr,'(A,I0)') 'j                  : ', j
        write(stderr,'(A,I0)') 'k                  : ', k
        if (d <= ndir) then
          write(stderr,'(A,I0)') 'Coordinate variable: ', d
        else
          write(stderr,'(A,I0)') 'Solution variable  : ', s
        endif
        write(stderr,'(A)') 'Bad token          : "'//trim(tok)//'"'
        write(stderr,'(A,I0)') 'Column             : ', p
        write(stderr,'(A)') 'Complete line      : '//trim(data_line)
        write(stderr,'(A)') &
          '=============================================================='
        istat = 1
        return
      enddo
    endsubroutine next_value

    ! Is text (a variable name) the coordinate name c ('X', 'Y' or 'Z'), in any case?
    logical function is_coordinate_name(text,c)
      character(len=*), intent(in) :: text, c
      is_coordinate_name = (trim(adjustl(upper_case(text)))==c)
    endfunction is_coordinate_name

    ! Is variable iv listed in a [list]=CELLCENTERED group of the zone header text
    ! (upper case)? A list holds numbers and ranges: [4-7], [4,5,6,7], [4-5,7].
    logical function var_cellcentered(text,iv)
      character(len=*), intent(in) :: text
      integer,          intent(in) :: iv
      integer :: p, q, r, c
      character(len=len(text)) :: vl
      var_cellcentered = .false.
      vl = varlocation_value(text)
      p = 1
      do
        q = index(vl(p:),'[')
        if (q<=0) return
        q = p + q - 1
        r = index(vl(q:),']')
        if (r<=0) return
        r = q + r - 1
        p = r + 1
        if (p>len(vl)) return
        c = verify(vl(p:),' =')
        if (c<=0) return
        c = p + c - 1
        if (c+11<=len(vl)) then
          if (vl(c:c+11)=='CELLCENTERED') then
            if (in_var_list(vl(q+1:r-1),iv)) then
              var_cellcentered = .true.
              return
            endif
          endif
        endif
      enddo
    endfunction var_cellcentered

    ! Is iv in a comma-separated list of numbers and ranges (a-b)?
    logical function in_var_list(list,iv)
      character(len=*), intent(in) :: list
      integer,          intent(in) :: iv
      integer :: s, e, d, lo, hi, ios1, ios2
      in_var_list = .false.
      s = 1
      do while (s<=len(list))
        e = index(list(s:),',')
        if (e==0) then
          e = len(list) + 1
        else
          e = s + e - 1
        endif
        if (e>s) then
          d = index(list(s:e-1),'-')
          if (d>0) then
            read(list(s:s+d-2),*,iostat=ios1) lo
            read(list(s+d:e-1),*,iostat=ios2) hi
          else
            read(list(s:e-1),*,iostat=ios1) lo
            ios2 = ios1
            hi = lo
          endif
          if (ios1==0 .and. ios2==0) then
            if (iv>=lo .and. iv<=hi) then
              in_var_list = .true.
              return
            endif
          endif
        endif
        s = e + 1
      enddo
    endfunction in_var_list

  end function tec_read_ascii

  ! Keywords and titles of a zone header: tec_read_ascii and tec_read_points_multivars

  ! Position of the value of `key` (name without '=') in a header line, or 0 if
  ! absent. The key must be a whole word outside quoted strings, followed by
  ! '='; whitespace is allowed on both sides of '=', e.g.
  !   K=2
  !   K=  2
  !   K = 2
  ! so 'K' does not match inside 'BLOCK=' nor in a title such as T="K=99".
  integer function keyword_value_pos(text,key) result(pos)
    character(len=*), intent(in) :: text,key
    integer :: p, q, lt, lk
    logical :: in_quotes

    pos = 0
    lt = len_trim(text)
    lk = len_trim(key)
    in_quotes = .false.

    do p = 1, lt-lk+1
      if (text(p:p)=='"') then
        in_quotes = .not.in_quotes
        cycle
      endif
      if (in_quotes .or. text(p:p+lk-1)/=key(1:lk)) cycle
      if (p>1) then
        if (is_word_char(text(p-1:p-1))) cycle
      endif
      q = skip_blanks(text,p+lk,lt)
      if (q>lt) return
      if (text(q:q)/='=') cycle
      q = skip_blanks(text,q+1,lt)
      if (q<=lt) pos = q
      return
    enddo
  endfunction keyword_value_pos

  pure integer function skip_blanks(text,start,last) result(q)
    character(len=*), intent(in) :: text
    integer, intent(in) :: start, last
    q = start
    do while (q<=last)
      if (text(q:q)/=' ' .and. text(q:q)/=char(9)) exit
      q = q + 1
    enddo
  endfunction skip_blanks

  pure logical function is_word_char(c)
    character(1), intent(in) :: c
    is_word_char = (c>='A' .and. c<='Z') .or. (c>='a' .and. c<='z') .or. &
                   (c>='0' .and. c<='9') .or. c=='_'
  endfunction is_word_char

  function upper_case(text) result(out)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: out
    integer :: q, code
    out = text
    do q = 1, len(text)
      code = iachar(out(q:q))
      if (code>=iachar('a') .and. code<=iachar('z')) &
        out(q:q) = achar(code-iachar('a')+iachar('A'))
    enddo
  endfunction upper_case

  ! The title of a zone, the value of T in its header (the keyword in any case, found as the others): the text between double
  ! quotes, which the next quote closes, as it closes a variable name; or, without quotes, a word up to the next blank, comma,
  ! tab or ')' (Tecplot puts a string with blanks in quotes). Blank when the header has no T.
  function zone_title(text) result(title)
    character(len=*), intent(in)  :: text
    character(len=:), allocatable :: title
    integer :: q, r

    title = ''
    q = keyword_value_pos(upper_case(text),'T')
    if (q<=0) return
    if (text(q:q)=='"') then
      r = index(text(q+1:),'"')
      if (r==0) then
        title = trim(text(q+1:))
      else
        title = text(q+1:q+r-1)
      endif
    else
      r = scan(text(q:),', )'//char(9))
      if (r==0) then
        title = trim(text(q:))
      else
        title = text(q:q+r-2)
      endif
    endif
  endfunction zone_title


  !> \brief Read ORION point data from Tecplot file.
  !> \details Reads Tecplot point format files into ORION data structure.
  !> Each zone is treated as a separate block.
  !> \param[inout] orion ORION data structure to fill with data
  !> \param[in] nvar Number of variables in file
  !> \param[in] filename Input file name
  !> \return err Error code (0 if successful)
  function tec_read_points_multivars(orion,nvar,filename) result(err)
    use, intrinsic :: iso_fortran_env, only : iostat_end
    use strings, only: getvals, parse
    implicit none
    type(orion_data), intent(inout)  :: orion
    character(len=*), intent(in)     :: filename
    integer, intent(in)              :: nvar
    real(R8P) :: dummy_float
    integer :: err
    integer :: tecunit, ios, ios_prev
    integer :: i, j, k, b
    integer :: Nzones, nlines
    integer, allocatable :: nskip(:)
    character(len=:), allocatable :: line   ! a line of any length (read with read_whole_line): a zone header holds the block name
    character(len=:), allocatable :: args(:), subargs(:)   ! pieces of a zone header, as long as its line

    ! Open file
    open(newunit=tecunit,file=trim(filename),iostat=err,action='read',status='old')
    if (err/=0) return
    
    ! Count blocks and allocate data
    ios = 0; Nzones = 0; nlines = -1
    do while(ios==0)
      call read_whole_line(tecunit,line,ios)
      nlines = nlines+1
      if (index(line,"ZONE")>0 .and. index(line,"ZONETYPE")==0) then
        Nzones = Nzones+1
      elseif (index(line,"Zone")>0) then
        Nzones = Nzones+1
      elseif (index(line,"ZONE T")>0) then
        Nzones = Nzones+1
      endif
    enddo
    allocate(orion%block(1:Nzones))
    rewind(tecunit)

    ! Read blocks size
    ios = 0; b = 0
    do
      do while (index(line,'I=')==0 .and. ios/=iostat_end)
      call read_whole_line(tecunit,line,ios)
      enddo
      if (ios==iostat_end) exit
      b = b+1
      ! The block takes the title of its zone as its name (on the line of I=), as in tec_read_ascii
      orion%block(b)%name = zone_title(line)
      ! parse copies each piece whole (split writes it character by character), so the pieces get the length of the line:
      ! the first one holds the block name
      if (allocated(args)) deallocate(args, subargs)
      allocate(character(len=len(line)) :: args(20), subargs(2))
      call parse(line,',',args)
      do i = 1, 2
        if (index(args(i),'I=')>0) then
          call parse(args(i),'=',subargs)
          read(subargs(2),'(I8)') orion%block(b)%Ni
          orion%block(b)%Ni = orion%block(b)%Ni
          orion%block(b)%Nj = 1
          orion%block(b)%Nk = 1
        endif
      enddo
      line = 'here we go'
    enddo
    rewind(tecunit)

    ! Count not-floating lines
    allocate(nskip(Nzones))
    nskip = 0; ios = 0; b = 1; ios_prev = 0
    do
      call read_whole_line(tecunit,line,ios)
      if (ios==iostat_end) exit
      read(line,*,iostat=ios) dummy_float
      if ((ios==0 .and. index(line,'DATA')>0) .or. ios/=0) then
        nskip(b) = nskip(b)+1
        ios = 1
      endif
      if (ios==0 .and. ios_prev/=0) b = b+1
      ios_prev = ios
    enddo
    rewind(tecunit)

    ! Read all
    do b = 1, Nzones
      allocate(orion%block(b)%mesh(1:1,1:orion%block(b)%Ni,1:orion%block(b)%Nj,1:orion%block(b)%Nk))
      allocate(orion%block(b)%vars(1:nvar,1:orion%block(b)%Ni,1:orion%block(b)%Nj,1:orion%block(b)%Nk))
      call skip(tecunit,nskip(b))
      do k = 1, orion%block(b)%Nk; do j = 1, orion%block(b)%Nj; do i = 1, orion%block(b)%Ni
            read(tecunit,*,iostat=err) orion%block(b)%mesh(1,i,j,k), orion%block(b)%vars(1:nvar,i,j,k)
      enddo; enddo; enddo
    enddo

    close(tecunit)

  end function tec_read_points_multivars


  !> \\brief Extract variable names from a Tecplot VARIABLES declaration.
  !> \\details Robust parser for quoted and unquoted variable names.
  !> Supports both single and double quotes, comma/blank/tab separators,
  !> adjacent quoted names, and names containing spaces when quoted.
  !> Examples accepted include:
  !>   VARIABLES = "x", "y", "z"
  !>   VARIABLES = "x" "y" "z"
  !>   VARIABLES = "x" "y" "z" "a""b"     (the last two are the names a and b)
  !>   VARIABLES = 'x' 'y' 'z'
  !>   VARIABLES = x, y, z
  !>   VARIABLES = x y z
  !>   VARIABLES = "velocity magnitude" 'pressure'
  subroutine read_variables(line,variables)
    implicit none
    character(len=*), intent(inout) :: line
    character(len=32), allocatable, intent(out) :: variables(:)

    integer, parameter :: MAX_VARIABLES = 512
    character(len=32) :: names(MAX_VARIABLES)
    character(len=len(line)) :: name
    character(len=len(line)) :: work
    character(len=len(line)) :: upper
    integer :: nvar
    integer :: i, p, q, L, n, c
    character :: quote
    logical :: quoted
    logical :: quote_closed

    nvar = 0
    names = ' '

    work  = adjustl(line)
    upper = work
    do c = 1, len_trim(upper)
      if (iachar(upper(c:c)) >= iachar('a') .and. &
          iachar(upper(c:c)) <= iachar('z')) then
        upper(c:c) = achar(iachar(upper(c:c)) - iachar('a') + iachar('A'))
      endif
    enddo

    ! Require VARIABLES as the first keyword in the collected declaration.
    if (index(adjustl(upper),'VARIABLES') /= 1) return

    p = index(work,'=')
    if (p <= 0) then
      write(stderr,'(A)') &
        'TECPLOT ASCII READ ERROR: VARIABLES header has no "=".'
      write(stderr,'(A)') 'Header: '//trim(work)
      return
    endif

    L = len_trim(work)
    i = p + 1

    do while (i <= L)

      ! Skip separators: spaces, tabs, commas.
      do while (i <= L)
        if (work(i:i) /= ' ' .and. work(i:i) /= char(9) .and. work(i:i) /= ',') exit
        i = i + 1
      enddo
      if (i > L) exit

      ! -------------------------------------------------------------------------
      ! Quoted variable name. Both single and double quotes are accepted.
      ! Quoted names may contain blanks and commas. A quote closes the name:
      ! two quoted names with nothing between them ("a""b") are two names, the
      ! form that the varnames argument of the writers takes.
      ! -------------------------------------------------------------------------
      if (work(i:i) == '"' .or. work(i:i) == "'") then
        quoted = .true.
        quote = work(i:i)
        i = i + 1
        name = ' '
        n = 0
        quote_closed = .false.

        do while (i <= L)
          if (work(i:i) == quote) then
            i = i + 1
            quote_closed = .true.
            exit
          endif

          if (n < len(name)) then
            n = n + 1
            name(n:n) = work(i:i)
          endif
          i = i + 1
        enddo

        if (.not.quote_closed) then
          write(stderr,'(A)') &
            'TECPLOT ASCII READ ERROR: unterminated quoted variable name.'
          write(stderr,'(A)') 'VARIABLES header: '//trim(work)
          return
        endif

        if (n > 0) then
          call append_variable_name(names,nvar,MAX_VARIABLES,trim(name))
          if (nvar >= MAX_VARIABLES .and. i <= L) return
        endif

      else
        ! -----------------------------------------------------------------------
        ! Unquoted variable name. Blanks/tabs/commas delimit the token, so a name
        ! containing blanks must be quoted.
        ! -----------------------------------------------------------------------
        quoted = .false.
        q = i
        do while (q <= L)
          if (work(q:q) == ' ' .or. work(q:q) == char(9) .or. work(q:q) == ',') exit
          q = q + 1
        enddo

        if (q > i) then
          name = ' '
          name(1:min(len(name),q-i)) = work(i:q-1)
          call append_variable_name(names,nvar,MAX_VARIABLES,trim(name))
          if (nvar >= MAX_VARIABLES .and. q < L) return
        endif
        i = q
      endif

      if (quoted .and. i <= L) cycle
    enddo

    if (nvar <= 0) return

    allocate(character(32)::variables(1:nvar))
    variables = names(1:nvar)

  end subroutine read_variables


  !> \\brief Append one variable name to the temporary name array.
  subroutine append_variable_name(names,nvar,max_variables,value)
    implicit none
    character(len=32), intent(inout) :: names(:)
    integer, intent(inout) :: nvar
    integer, intent(in) :: max_variables
    character(len=*), intent(in) :: value

    if (len_trim(value) == 0) return

    if (nvar >= max_variables) then
      write(stderr,'(A,I0)') &
        'TECPLOT ASCII READ ERROR: more than MAX_VARIABLES=',max_variables
      return
    endif

    nvar = nvar + 1
    if (len_trim(value) > len(names(nvar))) then
      write(stderr,'(A)') &
        'TECPLOT ASCII READ WARNING: variable name exceeds 32 characters; truncating.'
      write(stderr,'(A)') 'Variable: '//trim(value)
    endif
    names(nvar) = value(1:min(len(value),len(names(nvar))))
  end subroutine append_variable_name


  !> \\brief Split a string into whitespace/comma-separated tokens.
  !> \\details Legacy helper retained for source compatibility.
  subroutine split_tokens(str, tokens, ntok)
    implicit none
    character(len=*), intent(in)  :: str
    character(len=32), intent(out):: tokens(:)
    integer,          intent(out) :: ntok
    integer :: i, L, s
    logical :: in_tok

    ntok = 0
    in_tok = .false.
    s = 1
    L = len(str)
    do i = 1, L
      if (str(i:i) == ' ' .or. str(i:i) == ',' .or. str(i:i) == char(9)) then
        if (in_tok) then
          if (ntok < size(tokens)) then
            ntok = ntok + 1
            tokens(ntok) = str(s:i-1)
          endif
          in_tok = .false.
        endif
      else
        if (.not.in_tok) then
          s = i
          in_tok = .true.
        endif
      endif
    enddo
    if (in_tok .and. ntok < size(tokens)) then
      ntok = ntok + 1
      tokens(ntok) = str(s:L)
    endif
  end subroutine split_tokens


  !> \brief Convert C character array to Fortran string.
  !> \details Convenience routine for converting C pointers to Fortran strings in TecIO operations.
  !> The string has the length of the array and its characters only, with no C_NULL_CHAR after them: it is kept as a
  !> Fortran string (a block name), not handed back to a C function as in the TecIO example rewriteszlf90.f90.
  !> \param[in] charArray C pointer to character array
  !> \param[in] length Length of character array
  !> \param[out] string Output Fortran string, of that length
  subroutine copyCharArrayToString(charArray, length, string)
    use iso_c_binding, only : c_ptr, c_f_pointer
    implicit none
    type(c_ptr) :: charArray
    integer length
    character(len=:), allocatable, intent(out) :: string

    character, pointer :: charPointer(:)
    integer i

    call c_f_pointer(charArray, charPointer, [length])

    allocate(character(len=length) :: string)
    do i = 1, length
        string(i:i) = charPointer(i)
    enddo

  end

# if defined(TECIO)
  !> \brief Read ORION structured multiblock data from Tecplot binary file (SZplt format).
  !> \details Uses TecIO library to read compressed Tecplot binary format (.szplt).
  !> Only compiled if TECIO is defined.
  !> Every zone is read with the number of coordinates of the file: three when a zone has
  !> more than one node plane in K, or when the file declares a third coordinate with the rule
  !> of the ASCII reader, the first three variables named x, y, z and z nodal in every zone
  !> (single node planes or lines of a 3-D field, such as a slice); otherwise two when a zone
  !> has more than one node line in J, or when the first two variables are named x and y and y is
  !> nodal in every zone (lines of a 2-D field); otherwise one (a file of lines).
  !> The coordinates are at the nodes, and the solution variables of a zone all at the nodes or all at the cells: a zone with a
  !> coordinate at the cells, or with solution variables at the nodes and at the cells, is refused (return value 1, with a
  !> message). Each block takes the location of its zone (vars from 0 at the nodes, from 1 at the cells), so the zones of a file
  !> may differ. orion%tec%node is set when the solution variables of every zone are at the nodes, cleared when those of every
  !> zone are at the cells, and left as it is when the zones differ or the file has no solution variables.
  !> \param[inout] orion ORION data structure to fill with data
  !> \param[in] filename Input file name (.szplt)
  !> \param[in] zone_mask Optional per-zone selector, indexed by zone number.
  !>            Where .false., the zone's %vars is left with the correct
  !>            variable count and no cells, and its variable data is never
  !>            read.  Coordinates are read for every zone regardless.
  !> \param[in] dims_only Optional; if .true. only zone headers are read --
  !>            names, dimensions and the variable count.  No %mesh is
  !>            allocated and no data of any kind is read.
  !> \details The two optional arguments exist so that an MPI caller can size
  !>          and partition the domain before committing memory to it: call once
  !>          with dims_only to learn every zone's extent, decide which zones
  !>          this rank owns, then call again with zone_mask.  Without them
  !>          every rank materialises the whole file, which costs O(nranks x
  !>          filesize) of read traffic and O(full domain) of memory per rank.
  !>          %vars is kept allocated with zero cells rather than deallocated
  !>          so that size(...,1) queries for the variable count keep working
  !>          on every zone; an empty allocatable costs nothing.
  !> \return i Error code (0 if successful)
  function tec_read_szplt(orion,filename,zone_mask,dims_only) result(i)
    use iso_c_binding
    implicit none
    type(orion_data), intent(inout)                        :: orion
    character(len=*), intent(in)                           :: filename
    logical,          intent(in), optional                 :: zone_mask(:)
    logical,          intent(in), optional                 :: dims_only

    integer i, j, k, cnt
    character(len=:), allocatable :: inputFileName           ! the path, of any length, and C_NULL_CHAR for TecIO
    character(len=:), allocatable :: dataSetTitle, zoneTitle  ! of the length that TecIO gives
    character(32768) varNames
    character, pointer :: stringPtr(:)
    integer nameLen, strLen, q0, q1
    integer(c_int8_t), allocatable :: int8Values(:)
    integer(c_int16_t), allocatable :: int16Values(:)
    integer(c_int32_t) :: numVars, var
    integer(c_int32_t) :: fileType
    integer(c_int32_t) :: vstart
    integer(c_int32_t) :: inputZone, numZones
    integer(c_int32_t) :: zoneType
    integer(c_int32_t) :: strandID
    integer(c_int32_t), allocatable :: int32Values(:)
    integer(c_int64_t) :: numValues
    integer(c_int64_t) :: layer, first     ! values read by one call (all, a layer of cells in K or a row in I) and the first
    integer(c_int64_t) :: iMax, jMax, kMax, ndir
    integer(c_int64_t) :: ndir_file         ! number of coordinates of the file, the same for every zone
    integer :: nnodal, ncellc, ncoordc      ! solution variables at the nodes and at the cells, coordinates at the cells, of a zone
    integer :: nzonen, nzonec               ! zones with their solution variables at the nodes, at the cells
    integer(c_int32_t) :: ispassive         ! a variable without values in a zone
    integer(c_int32_t), allocatable :: varTypes(:)
    integer(c_int32_t), allocatable :: passiveVarList(:)
    integer(c_int32_t), allocatable :: valueLocation(:)
    integer(c_int32_t), allocatable :: shareVarFromZone(:)
    real(c_float), allocatable :: floatValues(:)
    real(c_double) :: solutionTime
    real(c_double), allocatable :: doubleValues(:)
    type(c_ptr) :: inputFileHandle = C_NULL_PTR
    type(c_ptr) :: stringCPtr = C_NULL_PTR
    logical :: onlyNode, hasValues     ! the block at the nodes; a solution variable of the zone holds values
    logical :: headers_only, want_data, is_coord
    logical :: plane_xyz                    ! the file declares x, y and z as coordinates (a slice)
    logical :: line_xy                      ! the file declares x and y as coordinates (lines of a 2-D field)
    integer(c_int32_t) :: zloc              ! value location of variable 3 in a zone (1 = nodal)

    headers_only = .false.
    if (present(dims_only)) headers_only = dims_only

    inputFileName = trim(filename) // C_NULL_CHAR

    ! Open the input file for reading
    i = tecFileReaderOpen(inputFileName, inputFileHandle)

    if (i/=0) return

    ! Read info about the data set
    i = tecDataSetGetTitle(inputFileHandle, stringCPtr)
    call copyCharArrayToString(stringCPtr, &
        tecStringLength(stringCPtr), dataSetTitle)
    call tecStringFree(stringCPtr)
    i = tecDataSetGetNumVars(inputFileHandle, numVars)

    ! Export the variable names to the caller, coordinates included, with the
    ! same convention as the ASCII reader (size(orion%varnames) = ndir + nvar),
    ! so that the bands of a .szplt file can be identified by name.
    if (allocated(orion%varnames)) deallocate(orion%varnames)
    allocate(orion%varnames(numVars))
    orion%varnames = ' '
    strLen = 0
    do var = 1, numVars
        i = tecVarGetName(inputFileHandle, var, stringCPtr)
        nameLen = tecStringLength(stringCPtr)
        call c_f_pointer(stringCPtr, stringPtr, [nameLen])
        ! a name written from a quoted list keeps its quotes in the file: drop one
        ! enclosing pair, as the ASCII reader does
        q0 = 1
        q1 = nameLen
        if (nameLen >= 2) then
            if (stringPtr(1) == '"' .and. stringPtr(nameLen) == '"') then
                q0 = 2
                q1 = nameLen - 1
            endif
        endif
        do j = q0, min(q1, q0 + len(orion%varnames(var)) - 1)
            orion%varnames(var)(j-q0+1:j-q0+1) = stringPtr(j)
        enddo
        if (var .gt. 1) then
            strLen = strLen + 1
            if (strLen <= len(varNames)) varNames(strLen : strLen) = ','
        endif
        do j = 1, nameLen
            if (strLen + j <= len(varNames)) varNames(strLen + j : strLen + j) = stringPtr(j)
        enddo
        strLen = strLen + nameLen
        call tecStringFree(stringCPtr)
    enddo
    if (strLen + 1 <= len(varNames)) varNames(strLen + 1 : strlen + 1) = C_NULL_CHAR

    i = tecFileGetType(inputFileHandle, fileType)
    i = tecDataSetGetNumZones(inputFileHandle, numZones)

    ! The file declares a third coordinate, with the rule of the ASCII reader, when the first
    ! three variables are named x, y and z (any case) and z is nodal in every zone: its zones
    ! of single node planes or lines are part of a 3-D field (a slice), and z is read as the
    ! third coordinate, not as the first solution variable, so every variable keeps its own band.
    plane_xyz = .false.
    if (numVars >= 3) then
      plane_xyz = is_coordinate_name(orion%varnames(1),'X') .and. &
                  is_coordinate_name(orion%varnames(2),'Y') .and. &
                  is_coordinate_name(orion%varnames(3),'Z')
      do inputZone = 1, numZones
        if (.not. plane_xyz) exit
        i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, 3_c_int32_t, zloc)
        if (zloc /= 1) plane_xyz = .false.
      enddo
    endif

    ! The number of coordinates is that of the file (its variables are the same in every zone):
    ! three when a zone has more than one node plane in K, or when the file declares x, y and z as
    ! above (a file of single node planes or lines that is part of a 3-D field); otherwise two when
    ! a zone has more than one node line in J; otherwise one (a file of lines). Every zone is read
    ! with it: no zone takes it from the zone before it.
    ndir_file = 1
    do inputZone = 1, numZones
      i = tecZoneGetIJK(inputFileHandle, inputZone, iMax, jMax, kMax)
      if (kMax > 1) ndir_file = 3
      if (jMax > 1 .and. ndir_file < 2) ndir_file = 2
    enddo
    if (plane_xyz) ndir_file = 3
    ! The same rule for two coordinates: the first two variables named x and y (any case), y nodal in every zone, are the
    ! coordinates also of a file of single node lines, the lines of a 2-D field
    line_xy = .false.
    if (numVars >= 2) then
      line_xy = is_coordinate_name(orion%varnames(1),'X') .and. is_coordinate_name(orion%varnames(2),'Y')
      do inputZone = 1, numZones
        if (.not. line_xy) exit
        i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, 2_c_int32_t, zloc)
        if (zloc /= 1) line_xy = .false.
      enddo
    endif
    if (line_xy .and. ndir_file < 2) ndir_file = 2

    ! A file with fewer variables than the coordinates of its zones, such as a zone with more than one node line in J and the
    ! variable x only, cannot be held: it is refused, as the ASCII reader refuses it
    if (numVars < ndir_file) then
      write(stderr,'(A,I0,A,I0,A)') 'TECPLOT SZPLT READ ERROR: ', numVars, ' variables for ', ndir_file, ' coordinates.'
      write(stderr,'(A)') 'File: '//trim(filename)
      i = tecFileReaderClose(inputFileHandle)
      i = 1
      return
    endif

    ! ORION holds the coordinates at the nodes, and one location for all solution variables of a block: a zone with a coordinate
    ! at the cells, or with solution variables at the nodes and at the cells, is refused with a message (passive variables,
    ! which hold no values, do not count). Each block takes the location of its zone, so the zones of a file may differ;
    ! orion%tec%node is set when every zone has its solution variables at the nodes, cleared when every zone has them at the
    ! cells, and left as it is when the zones differ or the file has no solution variables.
    nzonen = 0; nzonec = 0
    do inputZone = 1, numZones
      nnodal = 0; ncellc = 0; ncoordc = 0
      do var = 1, numVars
        i = tecZoneVarIsPassive(inputFileHandle, inputZone, var, ispassive)
        if (ispassive /= 0) cycle
        i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, var, zloc)
        if (var <= ndir_file) then
          if (zloc /= 1) ncoordc = ncoordc + 1
        elseif (zloc == 1) then
          nnodal = nnodal + 1
        else
          ncellc = ncellc + 1
        endif
      enddo
      if (ncoordc > 0) then
        write(stderr,'(A,I0,A)') 'TECPLOT SZPLT READ ERROR: a coordinate at the cells in zone ', inputZone, &
          '; ORION reads the coordinates at the nodes.'
        write(stderr,'(A)') 'File: '//trim(filename)
        i = tecFileReaderClose(inputFileHandle)
        i = 1
        return
      endif
      if (nnodal > 0 .and. ncellc > 0) then
        write(stderr,'(A,I0,A)') 'TECPLOT SZPLT READ ERROR: solution variables at the nodes and at the cells in zone ', &
          inputZone, '; ORION reads zones whose solution variables are all at the nodes or all at the cells.'
        write(stderr,'(A)') 'File: '//trim(filename)
        i = tecFileReaderClose(inputFileHandle)
        i = 1
        return
      endif
      if (nnodal > 0) nzonen = nzonen + 1
      if (ncellc > 0) nzonec = nzonec + 1
    enddo
    if (nzonen > 0 .and. nzonec == 0) orion%tec%node = .true.
    if (nzonec > 0 .and. nzonen == 0) orion%tec%node = .false.

    ! A caller doing the two-pass dims-then-data read arrives here a second
    ! time with %block already allocated from the header pass.
    if (allocated(orion%block)) deallocate(orion%block)
    allocate(orion%block(1:numZones))

    ! Zones
    do inputZone = 1, numZones
        want_data = .true.
        if (present(zone_mask)) then
          if (inputZone <= size(zone_mask)) want_data = zone_mask(inputZone)
        endif

        i = tecZoneGetType(inputFileHandle, inputZone, zoneType)
        if (zoneType == 6 .or. zoneType == 7) &
            stop "Unsupported inputZone type."
          
        ! Retrieve info about the inputZone
        i = tecZoneGetTitle(inputFileHandle, inputZone, stringCPtr)
        call copyCharArrayToString(stringCPtr, &
            tecStringLength(stringCPtr), zoneTitle)
        call tecStringFree(stringCPtr)

        orion%block(inputZone)%name = zoneTitle
        
        i = tecZoneGetIJK(inputFileHandle, inputZone, &
            iMax, jMax, kMax)

        orion%block(inputZone)%Ni = iMax-1
        orion%block(inputZone)%Nj = jMax-1
        orion%block(inputZone)%Nk = kMax-1

        ndir = ndir_file

        allocate(valueLocation(numVars))
        do var = 1, numVars
            i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, &
                var, valueLocation(var))
        enddo

        ! The location of the block is that of the solution variables of its zone: a passive variable, which holds no values,
        ! does not count, as in the check of the locations above. A zone whose solution variables are all passive (a zone of
        ! coordinates only) takes the location of the file, orion%tec%node, at which tec_write_structured_multiblock writes
        ! every block
        onlyNode = .false.; hasValues = .false.
        do var = ndir+1, numVars
          i = tecZoneVarIsPassive(inputFileHandle, inputZone, var, ispassive)
          if (ispassive /= 0) cycle
          hasValues = .true.
          if (valueLocation(var)==1) onlyNode = .true.
        enddo
        if (numVars > ndir .and. .not.hasValues) onlyNode = orion%tec%node

        if (onlyNode) then
          vstart = 0
        else
          vstart = 1
        endif

        if (.not. headers_only) &
          allocate(orion%block(inputZone)%mesh(1:ndir,0:iMax-1,0:jMax-1,0:kMax-1))

        if (want_data .and. .not. headers_only) then
          allocate(orion%block(inputZone)%vars(1:numVars-ndir,vstart:max(vstart,iMax-1),vstart:max(vstart,jMax-1),vstart:max(vstart,kMax-1)))
        else
          ! Variable count only
          allocate(orion%block(inputZone)%vars(1:numVars-ndir,1:0,1:0,1:0))
        endif

        if (headers_only) then
          deallocate(valueLocation)
          cycle
        endif


        allocate(varTypes(numVars))
        allocate(passiveVarList(numVars))
        allocate(shareVarFromZone(numVars))
        do var = 1, numVars
            i = tecZoneVarGetType(inputFileHandle, inputZone, &
                var, varTypes(var))
            i = tecZoneVarIsPassive(inputFileHandle, inputZone, &
                var, passiveVarList(var))
            i = tecZoneVarGetSharedZone(inputFileHandle, inputZone, &
                var, shareVarFromZone(var))
        enddo

        ! i = tecZoneConnectivityGetSharedZone(inputFileHandle, &
        !     inputZone, shareConnectivityFromZone)
        ! i = tecZoneFaceNbrGetMode(inputFileHandle, inputZone, &
        !     faceNeighborMode)
        ! if (faceNeighborMode > 4) faceNeighborMode = 1
        ! i = tecZoneFaceNbrGetNumConnections(inputfileHandle, &
        !     inputZone, numFaceConnections)

        i = tecZoneGetSolutionTime(inputFileHandle, inputZone, &
            solutionTime)
        i = tecZoneGetStrandID(inputFileHandle, inputZone, strandID)
        ! if (solutionTime /= 0.0 .or. strandID /= 0) &
        !     i = tecZoneSetUnsteadyOptions(outputFileHandle, &
        !         outputZone, solutionTime, strandID)

        ! Read and write inputZone data
        do var = 1, numVars
            ! Coordinates land in %mesh and are needed on every rank
            is_coord = (valueLocation(var)==1 .and. var<=ndir)
            if (.not. want_data .and. .not. is_coord) cycle

            ! A passive variable holds no values in its zone: it reads as zero, the value Tecplot gives it
            if (passiveVarList(var) /= 0 .and. var > ndir) orion%block(inputZone)%vars(var-ndir,:,:,:) = 0.0_R8P

            if (passiveVarList(var) == 0 .and. &
                  shareVarFromZone(var) == 0) then
                i = tecZoneVarGetNumValues(inputFileHandle, &
                          inputZone, var, numValues)
                ! The TecIO reader of this repository (lib/TecIO, TecVersionId 2018.3.0.94816) does not give right, in one call,
                ! the values at the cells of some zones with one node plane or line in a direction (such as the faces i, j and k
                ! of a volume block); it gives them right one layer of cells in K at a time when KMax > 1, and one row of cells
                ! in I at a time when KMax = 1, where a layer in K is the whole zone. So a variable at the cells of a zone with
                ! IMax, JMax or KMax = 1 is read so; the variables at the nodes, and those of zones with more than one node
                ! plane in each direction, are read with one call.
                layer = max(numValues, 1_c_int64_t)
                if (valueLocation(var) /= 1 .and. (iMax == 1 .or. jMax == 1 .or. kMax == 1)) then
                  if (kMax > 1) then
                    layer = max(iMax-1, 1_c_int64_t)*max(jMax-1, 1_c_int64_t)
                  else
                    layer = max(iMax-1, 1_c_int64_t)
                  endif
                endif
                select case (varTypes(var))
                case (1) ! float
                    allocate(floatValues(numValues))
                    do first = 1, numValues, layer
                      i = tecZoneVarGetFloatValues(inputFileHandle, &
                          inputZone, var, first, min(layer, numValues-first+1), &
                          floatValues(first:))
                    enddo
                    if (valueLocation(var)==1 .and. var<=ndir) then
                      cnt = 1
                      do k = 0, kMax-1; do j = 0, jMax-1; do i = 0, iMax-1
                            orion%block(inputZone)%mesh(var,i,j,k) = floatValues(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    else
                      cnt = 1
                      do k = vstart, max(vstart,kMax-1); do j = vstart, max(vstart,jMax-1); do i = vstart, max(vstart,iMax-1)
                            orion%block(inputZone)%vars(var-ndir,i,j,k) = floatValues(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    endif
                    deallocate(floatValues)
                case (2) ! double
                    allocate(doubleValues(numValues))
                    do first = 1, numValues, layer
                      i = tecZoneVarGetDoubleValues(inputFileHandle, &
                          inputZone, var, first, min(layer, numValues-first+1), &
                          doubleValues(first:))
                    enddo
                    if (is_coord) then
                      cnt = 1
                      do k = 0, kMax-1; do j = 0, jMax-1; do i = 0, iMax-1
                            orion%block(inputZone)%mesh(var,i,j,k) = doubleValues(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    else
                      cnt = 1
                       do k = vstart, max(vstart,kMax-1); do j = vstart, max(vstart,jMax-1); do i = vstart, max(vstart,iMax-1)
                            orion%block(inputZone)%vars(var-ndir,i,j,k) = doubleValues(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    endif
                    deallocate(doubleValues)
                case (3) ! int32_t
                    allocate(int32Values(numValues))
                    do first = 1, numValues, layer
                      i = tecZoneVarGetInt32Values(inputFileHandle, &
                          inputZone, var, first, min(layer, numValues-first+1), &
                          int32Values(first:))
                    enddo
                    if (is_coord) then
                      cnt = 1
                      do k = 0, kMax-1; do j = 0, jMax-1; do i = 0, iMax-1
                            orion%block(inputZone)%mesh(var,i,j,k) = int32Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    else
                      cnt = 1
                       do k = vstart, max(vstart,kMax-1); do j = vstart, max(vstart,jMax-1); do i = vstart, max(vstart,iMax-1)
                            orion%block(inputZone)%vars(var-ndir,i,j,k) = int32Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    endif
                    deallocate(int32Values)
                case (4) ! int16_t
                    allocate(int16Values(numValues))
                    do first = 1, numValues, layer
                      i = tecZoneVarGetInt16Values(inputFileHandle, &
                          inputZone, var, first, min(layer, numValues-first+1), &
                          int16Values(first:))
                    enddo
                    if (is_coord) then
                      cnt = 1
                      do k = 0, kMax-1; do j = 0, jMax-1; do i = 0, iMax-1
                            orion%block(inputZone)%mesh(var,i,j,k) = int16Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    else
                      cnt = 1
                       do k = vstart, max(vstart,kMax-1); do j = vstart, max(vstart,jMax-1); do i = vstart, max(vstart,iMax-1)
                            orion%block(inputZone)%vars(var-ndir,i,j,k) = int16Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    endif
                    deallocate(int16Values)
                case (5) ! uint8_t
                    allocate(int8Values(numValues))
                    do first = 1, numValues, layer
                      i = tecZoneVarGetUInt8Values(inputFileHandle, &
                          inputZone, var, first, min(layer, numValues-first+1), &
                          int8Values(first:))
                    enddo
                    if (is_coord) then
                      cnt = 1
                      do k = 0, max(1,kMax-1); do j = 0, jMax-1; do i = 0, iMax-1
                            orion%block(inputZone)%mesh(var,i,j,k) = int8Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    else
                      cnt = 1
                       do k = vstart, max(vstart,kMax-1); do j = vstart, max(vstart,jMax-1); do i = vstart, max(vstart,iMax-1)
                            orion%block(inputZone)%vars(var-ndir,i,j,k) = int8Values(cnt)
                            cnt = cnt + 1
                      enddo; enddo; enddo
                    endif
                    deallocate(int8Values)
                endselect
            endif
        enddo
        
        deallocate(varTypes)
        deallocate(passiveVarList)
        deallocate(valueLocation)
        deallocate(shareVarFromZone)
                  
    enddo ! inputZone loop

    ! Close old and new files
    i = tecFileReaderClose(inputFileHandle)

  contains

    ! Is text (a variable name) the coordinate name c ('X', 'Y' or 'Z'), in any case?
    ! The same test as in tec_read_ascii.
    logical function is_coordinate_name(text,c)
      character(len=*), intent(in) :: text, c
      character(len=len(text)) :: t
      integer :: n
      t = adjustl(text)
      do n = 1, len(t)
        if (t(n:n) >= 'a' .and. t(n:n) <= 'z') t(n:n) = achar(iachar(t(n:n)) - 32)
      enddo
      is_coordinate_name = (trim(t) == c)
    endfunction is_coordinate_name

  end function tec_read_szplt
# endif


  !> \brief Skip n lines in file unit u.
  !> \param[in] u File unit number
  !> \param[in] n Number of lines to skip
  subroutine skip(u,n)
    implicit none
    integer, intent(in) :: u, n
    integer :: i
    do i = 1, n; read(u,*); enddo
  end subroutine skip


  end module Lib_Tecplot
