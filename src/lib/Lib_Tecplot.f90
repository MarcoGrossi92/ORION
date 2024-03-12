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
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine compute_dimensions
  !> @}

  !> @ingroup Lib_PostProcessingPublicProcedure
  !> @{
  !> Function for writing ORION block data to Tecplot file.
  function tec_output(data_,varnames,time,filename) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data), intent(in)              :: data_
  character(len=*), intent(in), optional    :: varnames
  real(R8P), intent(in), optional           :: time
  character(len=*), intent(in)              :: filename            !< File name of the output file.
  logical :: meshonly
  integer :: err
# if defined (TECIO)
  integer, external::                tecini112,    &     ! |
                                     tecauxstr112, &     ! |
                                     teczne112,    &     ! | Tecplot external functions.
                                     tecdat112,    &     ! |
                                     tecend112           ! |
# endif
  character(1), parameter:: tecendrec = char(0) !< End-character for binary-record end.
  character(500)::          tecvarname          !< Variables name for tecplot header file.
  character(500)::          teczoneheader       !< Tecplot string of zone header.
  character(500)::          tecvarform          !< Format for variables for tecplot file.
  integer, allocatable::    tecvarloc(:)        !< Tecplot array of variables location.
  character(500)::          tecvarlocstr        !< Tecplot string of variables location.
  integer, allocatable::    tecnull(:)          !< Tecplot null array.
  integer::                 tecunit             !< Free logic unit of tecplot file.
  integer::                 Nvar                !< Number of variables saved.
  integer::                 Nblocks             !< Number of blocks.
  integer::                 b                   !< Counter.
  integer::                 NvarTot
  integer::                 gc
  real(R8P)::               time_
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Preliminary operations
  Nvar = 0
  meshonly = .true.
  if (allocated(data_%block(1)%vars)) then
    meshonly = .false.
    Nvar = size(data_%block(1)%vars,1)
  endif
  Nblocks = size(data_%block)
  gc = 1
  ! allocating dynamic arrays
  allocate(tecvarloc(1:3+Nvar))
  allocate(tecnull(1:3+Nvar))
  ! initializing tecplot variables
  call tec_init()
  ! time
  if (present(time)) then
    time_ = time
  else
    time_ = 0._R8P
  endif
  ! initializing tecplot file
  select case(data_%tec%format)
  case('binary')
# if defined (TECIO)
    if (filename(len_trim(filename)-4:len_trim(filename))/=".plt") then
      err = tecini112(tecendrec,trim(tecvarname)//tecendrec,trim(filename)//".plt"//tecendrec,'.'//tecendrec,0,0,1)
    else
      err = tecini112(tecendrec,trim(tecvarname)//tecendrec,trim(filename)//tecendrec,'.'//tecendrec,0,0,1)
    endif
    err = tecauxstr112("Time"//tecendrec,trim(str(n=time_))//tecendrec)
# else
    write(stderr,'(A)') 'Error1: your are trying to save binary tecplot file without compiling against the Tecplot library.'
    stop
# endif
  case('ascii')
    if (filename(len_trim(filename)-4:len_trim(filename))/=".dat") then
      open(newunit=tecunit,file=trim(filename)//".dat")
    else
      open(newunit=tecunit,file=trim(filename))
    endif
    write(tecunit,'(A)',iostat=err)trim(tecvarname)
  end select
  ! writing data blocks
  do b=1,Nblocks
    err = tec_blk_data(b = b)
  enddo
  ! finalizing tecplot file
  select case(data_%tec%format)
  case('binary')
# if defined (TECIO)
    err = tecend112()
# else
    write(stderr,'(A)') 'Error2: your are trying to save binary tecplot file without compiling against the Tecplot library.'
    stop
# endif
  case('ascii')
    close(tecunit)
  end select
  ! deallocating dynamic arrays
  deallocate(tecvarloc)
  deallocate(tecnull)
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
    select case(data_%tec%format)
    case('binary')
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
      if (data_%tec%node) then
        tecvarloc = 1
      else
        tecvarloc(1:3) = 1 ; tecvarloc(4:3+Nvar)= 0
      endif
      ! null array
      tecnull = 0
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
        if (data_%tec%node) then
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
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    Nx = size(data_%block(b)%vars,2)-1; Ny = size(data_%block(b)%vars,3)-1; Nz = size(data_%block(b)%vars,4)-1
    ! initialize the zone dimensions
    call compute_dimensions(node=data_%tec%node,bc=data_%tec%bc,             &
                            Nx=Nx,Ny=Ny,Nz=Nz,gc=gc,                         &
                            ni1=ni1,ni2=ni2,nj1=nj1,nj2=nj2,nk1=nk1,nk2=nk2, &
                            ci1=ci1,ci2=ci2,cj1=cj1,cj2=cj2,ck1=ck1,ck2=ck2)
    nnode = (ni2-ni1+1)*(nj2-nj1+1)*(nk2-nk1+1)
    ncell = (ci2-ci1+1)*(cj2-cj1+1)*(ck2-ck1+1)
    ! writing the block data
    select case(data_%tec%format)
    case('binary')
#   if defined (TECIO)
      err = teczne112('Block'//trim(strz(nz_pad=2,n=b))//tecendrec, &
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
#     else
      write(stderr,'(A)') 'Error3: your are trying to save binary tecplot file without compiling against the Tecplot library.'
      stop
#     endif
      err=tec_dat(N=nnode,dat=data_%block(b)%mesh(1,ni1:ni2,nj1:nj2,nk1:nk2))
      err=tec_dat(N=nnode,dat=data_%block(b)%mesh(2,ni1:ni2,nj1:nj2,nk1:nk2))
      err=tec_dat(N=nnode,dat=data_%block(b)%mesh(3,ni1:ni2,nj1:nj2,nk1:nk2))
      if (.not.meshonly) then
        do s=1,Nvar
          err=tec_dat(N=ncell,dat=data_%block(b)%vars(s,ci1:ci2,cj1:cj2,ck1:ck2))
        enddo
      endif
    case('ascii')
      ! tecplot zone header
      teczoneheader = ' ZONE  T = Block'//trim(strz(nz_pad=2,n=1))//        &
                      ', I='//trim(str(no_sign=.true.,n=ni2-ni1+1))//       &
                      ', J='//trim(str(no_sign=.true.,n=nj2-nj1+1))//       &
                      ', K='//trim(str(no_sign=.true.,n=nk2-nk1+1))//       &
                      ', DATAPACKING=BLOCK'//adjustl(trim(tecvarlocstr))
      write(tecunit,'(A)',iostat=err)trim(teczoneheader)
      write(tecunit,FR_P,iostat=err)(((data_%block(b)%mesh(1,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      write(tecunit,FR_P,iostat=err)(((data_%block(b)%mesh(2,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      write(tecunit,FR_P,iostat=err)(((data_%block(b)%mesh(3,i,j,k),i=ni1,ni2),j=nj1,nj2),k=nk1,nk2)
      if (.not.meshonly) then
        do s=1,Nvar
          write(tecunit,FR_P,iostat=err)(((data_%block(b)%vars(s,i,j,k),i=ci1,ci2),j=cj1,cj2),k=ck1,ck2)
        enddo
      endif
    end select
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction tec_blk_data

    function tec_dat(N,dat) result(err)
    !-------------------------------------------------------------------------------------------------------------------------------
    ! Function interface for using "tecdat" function.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    integer, intent(IN) :: N        ! Number of data to save.
    real(8), intent(IN) :: dat(1:N) ! Data to save.
    integer :: err                  ! Error trapping flag: 0 no errors, >0 error occurs.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
#   if defined (TECIO)
    err = tecdat112(N,dat,1)
#   else
    write(stderr,'(A)',iostat=err) 'Error: your are trying to save binary tecplot file without compiling against the '//&
                                   'Tecplot library.'
    stop
#   endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction tec_dat
  endfunction tec_output

  end module Lib_Tecplot