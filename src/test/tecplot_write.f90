program tecplot_write_multiblock
  use Lib_Tecplot
  implicit none
  integer, parameter :: nx1=0                              !< X lower bound extent.
  integer, parameter :: nx2=10                             !< X upper bound extent.
  integer, parameter :: ny1=0                              !< Y lower bound extent.
  integer, parameter :: ny2=10                             !< Y upper bound extent.
  integer, parameter :: nz1=0                              !< Z lower bound extent.
  integer, parameter :: nz2=10                             !< Z upper bound extent.
  type(obj_tecblock) :: block(2)
  integer            :: error                              !< Status error.
  integer            :: i, j, k                            !< Counter.

  allocate(block(1)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(block(1)%vars(1:2,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(block(2)%mesh(1:3,nx1:nx2+10,ny1:ny2,nz1:nz2))
  allocate(block(2)%vars(1:2,nx1:nx2+10,ny1:ny2,nz1:nz2))

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2
        block(1)%mesh(1,i,j,k) = dble(i)
        block(1)%mesh(2,i,j,k) = dble(j)
        block(1)%mesh(3,i,j,k) = dble(k)
        block(1)%vars(1,i,j,k) = dble(i*j*k)
        block(1)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2+10
        block(2)%mesh(1,i,j,k) = block(1)%mesh(1,nx2,j,k)+dble(i)
        block(2)%mesh(2,i,j,k) = dble(j)
        block(2)%mesh(3,i,j,k) = dble(k)
        block(2)%vars(1,i,j,k) = dble(i*j*k)
        block(2)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

  tec_format%binary = .true.
  error = tec_output(block_=block,varnames='variable1 variable2',time=0.d0,filename='solfile')

  tec_format%binary = .false.
  error = tec_output(block_=block,varnames='variable1 variable2',time=0.d0,filename='solfile')

end program tecplot_write_multiblock