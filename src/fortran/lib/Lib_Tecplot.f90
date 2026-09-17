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
    character(1000)::         tecvarname          !< Variables name for tecplot header file.
    character(500)::          teczoneheader       !< Tecplot string of zone header.
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
            tecvarname = trim(tecvarname)//' '//trim(varnames)
          else
            do s = 1, Nvar
              tecvarname = trim(tecvarname)//' "var'//trim(str(.true.,s))//'"'
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
        tecvarname = ' VARIABLES ="x"'
        if (ndir>=2) tecvarname = trim(tecvarname)//' "y"'
        if (ndir==3) tecvarname = trim(tecvarname)//' "z"'
        if (.not.meshonly) then
          if (present(varnames)) then
            tecvarname = trim(tecvarname)//' '//trim(varnames)
          else
            do s = 1, Nvar
              tecvarname = trim(tecvarname)//' "var'//trim(str(.true.,s))//'"'
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
      ncell = (ci2-ci1+1)*(cj2-cj1+1)*(ck2-ck1+1)
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
                        tecnull(1:nvar),                              &
                        tecvarloc(1:nvar),                            &
                        tecnull(1:nvar),                              &
                        0)
        err=tec_dat(N=nnode,dat=orion%block(b)%mesh(1,ni1:ni2,nj1:nj2,nk1:nk2))
        if (ndir>=2) &
          err=tec_dat(N=nnode,dat=orion%block(b)%mesh(2,ni1:ni2,nj1:nj2,nk1:nk2))
        if (ndir==3) &
          err=tec_dat(N=nnode,dat=orion%block(b)%mesh(3,ni1:ni2,nj1:nj2,nk1:nk2))
        if (.not.meshonly) then
          start = 1
          if (orion%tec%node) start = 0
          if (ni2==0) ni2 = 1
          if (nj2==0) nj2 = 1
          if (nk2==0) nk2 = 1
          ! Force values if 2D
          if (ndir==2) then
            nk1 = 1-start; nk2 = 1
          endif
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
          if (ni2==0) ni2 = 1
          if (nj2==0) nj2 = 1
          if (nk2==0) nk2 = 1
          ! Force values if 2D
          if (ndir==2) then
            nk1 = 1-start; nk2 = 1
          endif
          do s=1,Nvar
            write(tecunit,FR_P,iostat=err)(((orion%block(b)%vars(s,i,j,k),i=ni1+start,ni2),j=nj1+start,nj2),k=nk1+start,nk2)
          enddo
        endif
      end select
    endfunction tec_blk_data

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
    character(1000)::         tecvarname          !< Variables name for tecplot header file.
    character(500)::          teczoneheader       !< Tecplot string of zone header.
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
  function tec_read_ascii(orion,filename) result(err)
    use, intrinsic :: iso_fortran_env, only : iostat_end
    implicit none
    type(orion_data), intent(inout) :: orion
    character(len=*), intent(in)    :: filename

    real(R8P) :: solutiontime, value
    logical   :: meshonly, zone_node
    integer   :: err
    integer   :: tecunit, ios
    integer   :: i, j, k, d, b, s
    integer   :: Nblocks, nvar, ndir
    integer   :: Imax, Jmax, Kmax
    integer   :: start
    integer   :: nmesh, nsol
    logical, allocatable :: zone_point(:), zone_node_arr(:)
    integer, allocatable :: Ni(:), Nj(:), Nk(:)
    character(1000) :: line
    character(1000) :: header
    character(10000) :: variables_header
    character(1000) :: vline
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
      read(tecunit,'(A)',iostat=ios) line
      if (ios/=0) exit

      if (index(upper_case(line),'VARIABLES')>0) then
        variables_header = trim(line)
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
      read(tecunit,'(A)',iostat=ios) line
      if (ios/=0) exit
      if (is_zone_header(line)) Nblocks = Nblocks + 1
    enddo

    if (Nblocks<=0) then
      err = 1
      close(tecunit)
      return
    endif

    allocate(Ni(Nblocks),Nj(Nblocks),Nk(Nblocks),zone_point(Nblocks),zone_node_arr(Nblocks))
    Ni = 0; Nj = 0; Nk = 1
    zone_point = .false.
    zone_node_arr = .true.

    rewind(tecunit)
    b = 0
    ios = 0
    do while (ios==0)
      read(tecunit,'(A)',iostat=ios) line
      if (ios/=0) exit
      physical_line_number = physical_line_number + 1
      if (.not.is_zone_header(line)) cycle

      b = b + 1
      header = trim(line)
      data_value_number = 0_8
      data_line_number = 0

      ! Gather continuation header lines until the first numeric data line.
      do
        read(tecunit,'(A)',iostat=ios) line
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

      call parse_zone_header(header,Ni(b),Nj(b),Nk(b),zone_point(b),zone_node_arr(b),solutiontime)
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

    ! All existing ORION structured storage assumes the same centering for all
    ! non-coordinate variables. Keep that model, but derive it from the zone headers.
    ! The current ORION structured representation has one common centering for
    ! all solution variables, so reject files mixing centering between zones.
    if (any(zone_node_arr .neqv. zone_node_arr(1))) then
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

    ndir = 3
    if (Nk(1)==1) ndir = 2
    if (Nj(1)==1 .and. Nk(1)==1) ndir = 1

    do b = 1, Nblocks
      if ((Nk(b)==1 .and. ndir==3) .or. &
          (Nk(b)>1 .and. ndir<3) .or. &
          (Nj(b)==1 .and. ndir>1)) then
        err = 1
        close(tecunit)
        return
      endif
    enddo

    if (size(orion%varnames)<ndir) then
      err = 1
      close(tecunit)
      return
    endif
    nvar = size(orion%varnames) - ndir
    if (nvar==0) meshonly = .true.

    ! POINT packing with mixed nodal/cell-centered variables has no single common
    ! point count for all variables. The current ORION representation assumes a
    ! common storage location, so require nodal data for POINT zones.
    if (any(zone_point) .and. .not.orion%tec%node) then
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
      read(tecunit,'(A)',iostat=ios) line
      if (ios/=0) exit
      if (.not.is_zone_header(line)) cycle

      b = b + 1
      header = trim(line)

      ! Find the first data line for this zone while collecting continuation header lines.
      do
        read(tecunit,'(A)',iostat=ios) line
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
              do j = start, Jmax-1
                do i = start, Imax-1
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

    logical function is_zone_header(text)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: u
      u = upper_case(text)
      is_zone_header = (index(adjustl(u),'ZONE')==1)
    endfunction is_zone_header

    logical function line_is_numeric_start(text)
      character(len=*), intent(in) :: text
      character(1000) :: t
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

    subroutine parse_zone_header(text,I_,J_,K_,point_,node_,time_)
      character(len=*), intent(in) :: text
      integer, intent(out) :: I_,J_,K_
      logical, intent(out) :: point_,node_
      real(R8P), intent(inout) :: time_
      character(1000) :: u
      character(1000) :: work
      character(100) :: token
      integer :: p, q, ios_, iv

      I_ = 0; J_ = 0; K_ = 1
      point_ = .false.
      node_ = .true.

      u = upper_case(text)
      work = u

      call get_integer_keyword(work,'I=',I_)
      call get_integer_keyword(work,'J=',J_)
      call get_integer_keyword(work,'K=',K_)

      if (index(work,'DATAPACKING=POINT')>0 .or. index(work,'F=POINT')>0) then
        point_ = .true.
      elseif (index(work,'DATAPACKING=BLOCK')>0 .or. index(work,'F=BLOCK')>0) then
        point_ = .false.
      else
        ! Tecplot's default is POINT for some contexts; for structured zones the
        ! writer in this library emits BLOCK, so retain BLOCK as the safe default.
        point_ = .false.
      endif

      if (index(work,'CELLCENTERED')>0) node_ = .false.
      if (index(work,'NODAL')>0 .and. index(work,'CELLCENTERED')==0) node_ = .true.

      p = index(work,'SOLUTIONTIME=')
      if (p>0) then
        q = p + len('SOLUTIONTIME=')
        token = ' '
        iv = 0
        do while (q<=len_trim(work) .and. iv<len(token))
          if (work(q:q)==',' .or. work(q:q)==' ' .or. work(q:q)==char(9)) exit
          iv = iv+1
          token(iv:iv) = work(q:q)
          q = q+1
        enddo
        read(token,*,iostat=ios_) time_
      endif
    endsubroutine parse_zone_header

    subroutine get_integer_keyword(text,key,out)
      character(len=*), intent(in) :: text,key
      integer, intent(out) :: out
      integer :: p, q, r, ios_, n
      character(100) :: token
      out = 0
      p = index(text,key)
      if (p<=0) return

      ! Skip optional whitespace between the keyword and its value, e.g.
      !   K=2
      !   K=  2
      !   K = 2
      q = p + len(key)
      do while (q<=len_trim(text))
        if (text(q:q)/=' ' .and. text(q:q)/=char(9)) exit
        q = q + 1
      enddo
      if (q>len_trim(text)) return

      token = ' '
      n = 0
      r = q
      do while (r<=len_trim(text))
        if (text(r:r)==',' .or. text(r:r)==' ' .or. text(r:r)==char(9)) exit
        n = n + 1
        if (n>len(token)) exit
        token(n:n) = text(r:r)
        r = r + 1
      enddo
      read(token,*,iostat=ios_) out
    endsubroutine get_integer_keyword

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

  end function tec_read_ascii


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
    character(1000) :: line
    character(100) :: args(20), subargs(2)

    ! Open file
    open(newunit=tecunit,file=trim(filename),iostat=err,action='read',status='old')
    if (err/=0) return
    
    ! Count blocks and allocate data
    ios = 0; Nzones = 0; nlines = -1
    do while(ios==0)
      read(tecunit,'(A)',iostat=ios) line
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
      read(tecunit,'(A)',iostat=ios) line
      enddo
      if (ios==iostat_end) exit
      b = b+1
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
      read(tecunit,'(A)',iostat=ios) line
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


  !> \brief Extract variable names from Tecplot VARIABLES line.
  !> \param[inout] line Input line containing variable definitions
  !> \param[out] variables Extracted variable names array
  subroutine read_variables(line,variables)
    implicit none
    character(len=*), intent(inout) :: line
    character(len=32), allocatable, intent(out) :: variables(:)
    character(len=32) :: variables_(150)
    integer :: nvar, i, L, q2

    nvar = 0

    ! Require a VARIABLES header
    if (index(line, 'VARIABLES') <= 0) return

    ! Remove the "VARIABLES =" part from the line
    line = trim(adjustl(line(index(line, '=')+1:)))
    L = len_trim(line)

    if (index(line(1:L), '"') > 0) then
      ! Quoted names. Scan quote-delimited tokens; robust to names separated by
      ! spaces/commas AND to adjacent quotes with no separator (e.g. "a""b").
      i = 1
      do while (i <= L)
        if (line(i:i) == '"') then
          q2 = i + 1
          do while (q2 <= L)
            if (line(q2:q2) == '"') exit
            q2 = q2 + 1
          end do
          if (q2 > i + 1) then          ! non-empty token
            nvar = nvar + 1
            variables_(nvar) = trim(line(i+1:q2-1))
          end if
          i = q2 + 1
        else
          i = i + 1
        end if
      end do
    else
      ! Unquoted names separated by blanks or commas
      call split_tokens(line(1:L), variables_, nvar)
    end if

    allocate(character(32)::variables(1:nvar))
    variables = variables_(1:nvar)

  end subroutine read_variables


  !> Split a string into whitespace/comma-separated tokens.
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
          ntok = ntok + 1
          tokens(ntok) = str(s:i-1)
          in_tok = .false.
        end if
      else
        if (.not. in_tok) then
          s = i
          in_tok = .true.
        end if
      end if
    end do
    if (in_tok) then
      ntok = ntok + 1
      tokens(ntok) = str(s:L)
    end if
  end subroutine split_tokens


  !> \brief Convert C character array to Fortran string.
  !> \details Convenience routine for converting C pointers to Fortran strings in TecIO operations.
  !> \param[in] charArray C pointer to character array
  !> \param[in] length Length of character array
  !> \param[out] string Output Fortran string
  subroutine copyCharArrayToString(charArray, length, string)
    use iso_c_binding, only : C_NULL_CHAR, c_ptr, c_f_pointer
    implicit none
    type(c_ptr) :: charArray
    integer length
    character(*) string

    character, pointer :: charPointer(:)
    integer i

    call c_f_pointer(charArray, charPointer, [length])

    string = ' '
    do i = 1, length
        string(i:i) = charPointer(i)
    enddo
    string(length+1:length+1) = C_NULL_CHAR

  end

# if defined(TECIO)
  !> \brief Read ORION structured multiblock data from Tecplot binary file (SZplt format).
  !> \details Uses TecIO library to read compressed Tecplot binary format (.szplt).
  !> Only compiled if TECIO is defined.
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
    character(256) inputFileName
    character(256) dataSetTitle, zoneTitle
    character(1024) varNames
    character, pointer :: stringPtr(:)
    integer nameLen, strLen
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
    integer(c_int64_t) :: iMax, jMax, kMax, ndir
    integer(c_int32_t), allocatable :: varTypes(:)
    integer(c_int32_t), allocatable :: passiveVarList(:)
    integer(c_int32_t), allocatable :: valueLocation(:)
    integer(c_int32_t), allocatable :: shareVarFromZone(:)
    real(c_float), allocatable :: floatValues(:)
    real(c_double) :: solutionTime
    real(c_double), allocatable :: doubleValues(:)
    type(c_ptr) :: inputFileHandle = C_NULL_PTR
    type(c_ptr) :: stringCPtr = C_NULL_PTR
    logical :: onlyNode
    logical :: headers_only, want_data, is_coord

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

    strLen = 0
    do var = 1, numVars
        i = tecVarGetName(inputFileHandle, var, stringCPtr)
        nameLen = tecStringLength(stringCPtr)
        call c_f_pointer(stringCPtr, stringPtr, [nameLen])
        if (var .gt. 1) then
            strLen = strLen + 1
            varNames(strLen : strLen) = ','
        endif
        do j = 1, nameLen
            varNames(strLen + j : strLen + j) = stringPtr(j)
        enddo
        strLen = strLen + nameLen
        call tecStringFree(stringCPtr)
    enddo
    varNames(strLen + 1 : strlen + 1) = C_NULL_CHAR

    i = tecFileGetType(inputFileHandle, fileType)
    i = tecDataSetGetNumZones(inputFileHandle, numZones)

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

        if (jMax>1 .and. kMax>1) then
          ndir = 3
        elseif (jMax>1 .and. kMax==1) then
          ndir = 2
        elseif (jMax==1 .and. kMax==1) then
          ndir = 1
        endif

        allocate(valueLocation(numVars))
        do var = 1, numVars
            i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, &
                var, valueLocation(var))
        enddo

        onlyNode = .false.
        do var = ndir+1, numVars
          if (valueLocation(var)==1) onlyNode = .true.
        enddo

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

            if (passiveVarList(var) == 0 .and. &
                  shareVarFromZone(var) == 0) then
                i = tecZoneVarGetNumValues(inputFileHandle, &
                          inputZone, var, numValues)
                select case (varTypes(var))
                case (1) ! float
                    allocate(floatValues(numValues))
                    i = tecZoneVarGetFloatValues(inputFileHandle, &
                        inputZone, var, 1_c_int64_t, numValues, &
                        floatValues)
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
                    i = tecZoneVarGetDoubleValues(inputFileHandle, &
                        inputZone, var, 1_c_int64_t, numValues, &
                        doubleValues)
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
                    i = tecZoneVarGetInt32Values(inputFileHandle, &
                        inputZone, var, 1_c_int64_t, numValues, &
                        int32Values)
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
                    i = tecZoneVarGetInt16Values(inputFileHandle, &
                        inputZone, var, 1_c_int64_t, numValues, &
                        int16Values)
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
                    i = tecZoneVarGetUInt8Values(inputFileHandle, &
                        inputZone, var, 1_c_int64_t, numValues, &
                        int8Values)
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


  !> \brief Read a line of arbitrary length from a formatted sequential file.
  !> \details Uses non-advancing I/O (F2003) to accumulate chunks into an
  !>          allocatable string. Same pattern as fortran-lang/stdlib getline.
  !> \param[in]  unit File unit number
  !> \param[out] line Allocatable string containing the full line
  !> \param[out] ios  I/O status (0 = success, iostat_end = end of file)
  !> Not used for compatibility issues with gfortran
  subroutine readline(unit, line, ios)
    use, intrinsic :: iso_fortran_env, only : iostat_eor
    implicit none
    integer, intent(in) :: unit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ios
    character(len=512) :: buffer
    integer :: sz

    line = ''
    do
      read(unit, '(A)', advance='no', size=sz, iostat=ios) buffer
      if (ios == iostat_eor) then
        line = line // buffer(:sz)
        ios = 0
        return
      elseif (ios /= 0) then
        line = line // buffer(:sz)
        return
      endif
      line = line // buffer
    end do
  end subroutine readline

  end module Lib_Tecplot
