!> @ingroup Library
!> @{
!> @defgroup Lib_TecplotLibrary Lib_Tecplot
!> @}

!> Pure Fortran (2003+) library to write data conforming the Tecplot standard
!> @ingroup Lib_TecplotLibrary
module Lib_Tecplot
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
  use IR_Precision
  use Lib_ORION_data
  implicit none
# if defined(TECIO)
  include "tecio.f90"
# endif


contains


  subroutine compute_dimensions(node,bc,Nx,Ny,Nz,gc,ni1,ni2,nj1,nj2,nk1,nk2,ci1,ci2,cj1,cj2,ck1,ck2)
  !---------------------------------------------------------------------------------------------------------------------------------
  ! Subroutine for computing the dimensions of the domain to be post-processed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical, intent(in) :: node 
  logical, intent(in) :: bc 
  integer, intent(IN) :: Nx, Ny, Nz, gc          ! Block-level data.
  integer, intent(OUT):: ni1,ni2,nj1,nj2,nk1,nk2 ! Bounds of dimensions of node-centered data.
  integer, intent(OUT):: ci1,ci2,cj1,cj2,ck1,ck2 ! Bounds of dimensions of cell-centered data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (node) then
    ni1 = 0 ; ni2 = Nx
    nj1 = 0 ; nj2 = Ny
    nk1 = 0 ; nk2 = Nz

    ci1 = 1 ; ci2 = Nx
    cj1 = 1 ; cj2 = Ny
    ck1 = 1 ; ck2 = Nz
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
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine compute_dimensions
  !> @}

  !> @ingroup Lib_PostProcessingPublicProcedure
  !> @{
  !> Function for writing ORION block data to Tecplot file.
  function tec_write_structured_multiblock(orion,varnames,time,filename,Nvars) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data), intent(in)              :: orion
  character(len=*), intent(in), optional    :: varnames
  real(R8P), intent(in), optional           :: time
  character(len=*), intent(in)              :: filename !< File name of the output file.
  integer, intent(in), optional             :: Nvars    !< Input number of variables saved.
  logical :: meshonly
  integer :: err
# if defined(TECIO)
  integer, external::                tecini142,    &     ! |
                                     tecauxstr142, &     ! |
                                     teczne142,    &     ! | Tecplot external functions.
                                     tecdatd142,    &    ! |
                                     tecend142           ! |
# endif
  character(1), parameter:: tecendrec = char(0) !< End-character for binary-record end.
  character(500)::          tecvarname          !< Variables name for tecplot header file.
  character(500)::          teczoneheader       !< Tecplot string of zone header.
  character(500)::          tecvarform          !< Format for variables for tecplot file.
  integer, allocatable::    tecvarloc(:)        !< Tecplot array of variables location.
  character(500)::          tecvarlocstr        !< Tecplot string of variables location.
  integer, allocatable::    tecnull(:)          !< Tecplot null array.
  integer, allocatable::    tecunit(:)          !< Free logic unit of tecplot file.
  integer::                 Nvar                !< Internal number of variables saved.
  integer::                 Nblocks,Nb_tot      !< Number of blocks.
  integer::                 b                   !< Counter.
  integer::                 NvarTot
  integer::                 gc
  real(R8P)::               time_
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------

  ! Preliminary operations
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
  gc = 1
  ! allocating dynamic arrays
  allocate(tecvarloc(1:3+Nvar))
  allocate(tecnull(1:3+Nvar))
  allocate(tecunit(Nblocks))
  ! initializing tecplot variables
  call tec_init()
  ! time
  if (present(time)) then
    time_ = time
  else
    time_ = -10._R8P
  endif
  ! initializing tecplot file
  select case(orion%tec%format)
  case('binary')
# ifdef MPI_VERSION
    stop "You can not write in binary format with MPI at the moment"
# endif
# if defined(TECIO)
    if (filename(len_trim(filename)-4:len_trim(filename))/=".plt") then
      err = tecini142(tecendrec,trim(tecvarname)//tecendrec,trim(filename)//".plt"//tecendrec,'.'//tecendrec,0,0,1,0)
    else
      err = tecini142(tecendrec,trim(tecvarname)//tecendrec,trim(filename)//tecendrec,'.'//tecendrec,0,0,1,0)
    endif
    err = tecauxstr142("Time"//tecendrec,trim(str(n=time_))//tecendrec)
# else
    stop "You can not write in binary formato without compiling against TecIO"
# endif
  case('ascii')
    do b=1,Nblocks
      if (index(filename,'.')==0) then
        open(newunit=tecunit(b),file=trim(filename)//"_"//trim(orion%block(b)%name)//".dat")
      else
        open(newunit=tecunit(b),file=trim(filename(1:index(filename,'.')-1))//"_"//trim(orion%block(b)%name)//trim(filename(index(filename,'.'):len(filename))))
      endif
      write(tecunit(b),'(A)',iostat=err) trim(tecvarname)
    enddo
    ! Open file in different processes
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
    do b=1,Nblocks
      close(tecunit(b))
    enddo
  end select
  ! deallocating dynamic arrays
  deallocate(tecvarloc)
  deallocate(tecnull)
  deallocate(tecunit)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine tec_init()
    !-------------------------------------------------------------------------------------------------------------------------------
    ! Function for initializing Tecplot specific variables.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    integer  :: s ! Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (.not.meshonly) then
      NvarTot = 3 + Nvar
    else
      NvarTot = 3
    endif
    select case(orion%tec%format)
    case('binary')
#   if defined(TECIO)
      ! header variables names
      tecvarname = 'x y z'
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
        tecvarloc(1:3) = 1 ; tecvarloc(4:3+Nvar)= 0
      endif
      ! null array
      tecnull = 0
#   else
    stop "You can not write in binary formato without compiling against TecIO"
#   endif
    case('ascii')
      ! header variables names
      tecvarname = ' VARIABLES ="x" "y" "z"'
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
          if (NvarTot==4) then
            tecvarlocstr = ', VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED)'
          else
            tecvarlocstr = ', VARLOCATION=([1-3]=NODAL,[4-'//trim(str(.true.,NvarTot))//']=CELLCENTERED)'
          endif
        endif
      else
        tecvarlocstr = ', VARLOCATION=([1-3]=NODAL)'
      endif
    end select
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine tec_init

    function tec_blk_data(b) result(err)
    !-------------------------------------------------------------------------------------------------------------------------------
    ! Function for writing block data.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    integer, intent(IN):: b           ! Block number.
    integer:: err                     ! Error trapping flag: 0 no errors, >0 error occurs.
    integer:: ni1,ni2,nj1,nj2,nk1,nk2 ! Bounds of dimensions of node-centered data.
    integer:: ci1,ci2,cj1,cj2,ck1,ck2 ! Bounds of dimensions of cell-centered data.
    integer:: nnode,ncell             ! Number of nodes and cells.
    integer:: i,j,k,s                 ! Counters.
    integer:: Nx, Ny, Nz
    integer:: start
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
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
#   if defined(TECIO)
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
      err=tec_dat(N=nnode,dat=orion%block(b)%mesh(2,ni1:ni2,nj1:nj2,nk1:nk2))
      err=tec_dat(N=nnode,dat=orion%block(b)%mesh(3,ni1:ni2,nj1:nj2,nk1:nk2))
      if (.not.meshonly) then
        start = 1
        if (orion%tec%node) start = 0
        if (ni2==0) ni2 = 1
        if (nj2==0) nj2 = 1
        if (nk2==0) nk2 = 1
        do s=1,Nvar
          err=tec_dat(N=ncell,dat=orion%block(b)%vars(s,ni1+start:ni2,nj1+start:nj2,nk1+start:nk2))
        enddo
      endif
#   endif
    case('ascii')
      ! tecplot zone header
      teczoneheader = ' ZONE  T = '//trim(orion%block(b)%name)//            &
                      ', I='//trim(str(no_sign=.true.,n=ni2-ni1+1))//       &
                      ', J='//trim(str(no_sign=.true.,n=nj2-nj1+1))//       &
                      ', K='//trim(str(no_sign=.true.,n=nk2-nk1+1))//       &
                      ', DATAPACKING=BLOCK'//adjustl(trim(tecvarlocstr))
      if (time_>0.0_R8P) &
        teczoneheader = trim(teczoneheader)//', SOLUTIONTIME='//trim(str(no_sign=.true.,n=time_))
      write(tecunit(b),'(A)',iostat=err)trim(teczoneheader)
      write(tecunit(b),FR_P,iostat=err)(((orion%block(b)%mesh(1,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      write(tecunit(b),FR_P,iostat=err)(((orion%block(b)%mesh(2,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      write(tecunit(b),FR_P,iostat=err)(((orion%block(b)%mesh(3,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      if (.not.meshonly) then
        start = 1
        if (orion%tec%node) start = 0
        if (ni2==0) ni2 = 1
        if (nj2==0) nj2 = 1
        if (nk2==0) nk2 = 1
        do s=1,Nvar
          write(tecunit(b),FR_P,iostat=err)(((orion%block(b)%vars(s,i,j,k),i=ni1+start,ni2),j=nj1+start,nj2),k=nk1+start,nk2)
        enddo
      endif
    end select
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction tec_blk_data

#   if defined(TECIO)
    ! Function interface for using "tecdat" function.
    function tec_dat(N,dat) result(err) 
      implicit none
      integer, intent(IN) :: N        ! Number of data to save.
      real(8), intent(IN) :: dat(1:N) ! Data to save.
      integer :: err                  ! Error trapping flag: 0 no errors, >0 error occurs.
      err = tecdatd142(N,dat)
    endfunction tec_dat
#   endif

  endfunction tec_write_structured_multiblock


  !> Function for reading ORION block data from Tecplot file.
  function tec_read_structured_multiblock(orion,filename) result(err)
    use, intrinsic :: iso_fortran_env, only : iostat_end
    use strings, only: getvals, parse
    implicit none
    type(orion_data), intent(inout)                        :: orion
    character(len=*), intent(in)                           :: filename
    real(R8P) :: dummy_float
    logical :: meshonly
    integer :: err, end
    integer :: tecunit, ios, ios_prev, ios2
    integer :: i, j, k, d, b, b2
    integer :: Ni(50), Nj(50), Nk(50), nskip(50)
    integer :: Nblocks, nlines, nvar
    character(500) :: line
    character(100) :: args(20), subargs(2)

    meshonly = .false.

    ! Open file
    open(newunit=tecunit,file=trim(filename),status='old',action='read')

    ios = 0
    do while(ios==0)
      read(tecunit,'(A)',iostat=ios) line
      call read_variables(line, orion%varnames)
      if (allocated(orion%varnames)) ios = 1
    enddo

    rewind(tecunit)
    
    ! Count blocks and their dimensions
    ios = 0; Nblocks = 0; nlines = -1; b = 1; ios2 = 0; nskip = 0; ios_prev = 0; b2 = 1
    do while(ios==0)
      read(tecunit,'(A)',iostat=ios) line
      nlines = nlines+1
      if (index(line,"ZONE")>0 .and. index(line,"ZONETYPE")==0) Nblocks = Nblocks+1
      if (index(line,"Zone")>0) Nblocks = Nblocks+1
      if (index(line,'I=')>0) then
        call parse(line,',',args)
        do i = 1, 6!size(args)
          if (index(args(i),'I=')>0) then
            call parse(args(i),'=',subargs)
            read(subargs(2),'(I5)') Ni(b)
          endif
          if (index(args(i),'J=')>0) then
            call parse(args(i),'=',subargs)
            read(subargs(2),'(I5)') Nj(b)
          endif
          if (index(args(i),'K=')>0) then
            call parse(args(i),'=',subargs)
            read(subargs(2),'(I5)') Nk(b)
          endif
        enddo
        b = b+1
      endif
       ! Count not-floating lines
      read(line,*,iostat=ios2) dummy_float
      if ( (ios2==0 .and. index(line,'DATA')>0) .or. ios2/=0 ) then
        nskip(b2) = nskip(b2)+1
        ios2 = 1
      endif
      if (ios2==0 .and. ios_prev/=0) b2 = b2+1
      ios_prev = ios2
    enddo
    ! Allocate data
    allocate(orion%block(1:Nblocks))
    do b = 1, Nblocks
      orion%block(b)%Ni = Ni(b)-1
      orion%block(b)%Nj = Nj(b)-1
      orion%block(b)%Nk = Nk(b)-1
    enddo
    rewind(tecunit)

    ! Count variables
    Nvar = nlines-sum(nskip)
    do b = 1, Nblocks
      nvar = nvar-3*((orion%block(b)%Ni+1)*(orion%block(b)%Nj+1)*(orion%block(b)%Nk+1))
    enddo
    nvar = nvar/(sum(orion%block(:)%Ni*orion%block(:)%Nj*orion%block(:)%Nk))
    if (nvar==0) meshonly = .true.

    ! Read all
    do b = 1, Nblocks
      allocate(orion%block(b)%mesh(1:3,0:orion%block(b)%Ni,0:orion%block(b)%Nj,0:orion%block(b)%Nk))
      if (.not.meshonly) allocate(orion%block(b)%vars(1:nvar,1:orion%block(b)%Ni,1:orion%block(b)%Nj,1:orion%block(b)%Nk))
      call skip(tecunit,nskip(b))
      do d = 1, 3
        do k = 0, orion%block(b)%Nk; do j = 0, orion%block(b)%Nj; do i = 0, orion%block(b)%Ni
              read(tecunit,*,iostat=err) orion%block(b)%mesh(d,i,j,k)
        enddo; enddo; enddo
      enddo
      end = 0
      if (orion%tec%node) end = 1
      do d = 1, nvar
        do k = 1, orion%block(b)%Nk+end; do j = 1, orion%block(b)%Nj+end; do i = 1, orion%block(b)%Ni+end
              read(tecunit,*,iostat=err) orion%block(b)%vars(d,i,j,k)
        enddo; enddo; enddo
      enddo
    enddo

    close(tecunit)

  end function tec_read_structured_multiblock


  !> Function for reading ORION block data from Tecplot file.
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
    character(500) :: line
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


  subroutine read_variables(line,variables)
    implicit none
    character(len=*), intent(inout) :: line
    character(len=32), allocatable, intent(out) :: variables(:)
    character(len=32) :: variables_(150)
    integer :: nvar, start, end_pos

    nvar = 0

    ! Find the position of "VARIABLES"
    if (index(line, 'VARIABLES') > 0) then
        ! Remove the "VARIABLES =" part from the line
        line = trim(adjustl(line(index(line, '=')+1:)))

        ! Loop to extract each variable name
        do while (len_trim(line) > 0)
            ! Find the start and end positions of the variable name
            start = index(line, '"') + 1
            end_pos = index(line(start+1:), '"') + start

            ! Store the variable name in the array
            nvar = nvar + 1
            variables_(nvar) = trim(line(start:end_pos-1))

            ! Remove the extracted variable from the line
            if (index(line(end_pos+1:), ',') > 0 .or. index(line(end_pos+1:), ' ') > 0) then
                line = trim(adjustl(line(end_pos+2:)))
            else
                line = ''
            end if
        end do
    else
      return
    end if
    
    allocate(character(32)::variables(1:nvar-3))
    variables = variables_(4:nvar)

  end subroutine read_variables


  subroutine skip(u,n)
    implicit none
    integer, intent(in) :: u, n
    integer :: i
    do i = 1, n; read(u,*); enddo
  end subroutine skip

  end module Lib_Tecplot
