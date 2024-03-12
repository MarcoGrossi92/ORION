program tecplot_write_multiblock
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  integer, parameter :: nx1=0                              !< X lower bound extent.
  integer, parameter :: nx2=10                             !< X upper bound extent.
  integer, parameter :: ny1=0                              !< Y lower bound extent.
  integer, parameter :: ny2=10                             !< Y upper bound extent.
  integer, parameter :: nz1=0                              !< Z lower bound extent.
  integer, parameter :: nz2=10                             !< Z upper bound extent.
  type(orion_data)   :: data
  integer            :: error                              !< Status error.
  integer            :: i, j, k                            !< Counter.

  data%tec%node = .false.
  data%tec%bc = .false.

  allocate(data%block(1:2))

  allocate(data%block(1)%mesh(1:3,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(data%block(1)%vars(1:2,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(data%block(2)%mesh(1:3,nx1:nx2+10,ny1:ny2,nz1:nz2))
  allocate(data%block(2)%vars(1:2,nx1:nx2+10,ny1:ny2,nz1:nz2))

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2
        data%block(1)%mesh(1,i,j,k) = dble(i)
        data%block(1)%mesh(2,i,j,k) = dble(j)
        data%block(1)%mesh(3,i,j,k) = dble(k)
        data%block(1)%vars(1,i,j,k) = dble(i*j*k)
        data%block(1)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

  do k=nz1, nz2
    do j=ny1, ny2
      do i=nx1, nx2+10
        data%block(2)%mesh(1,i,j,k) = data%block(1)%mesh(1,nx2,j,k)+dble(i)
        data%block(2)%mesh(2,i,j,k) = dble(j)
        data%block(2)%mesh(3,i,j,k) = dble(k)
        data%block(2)%vars(1,i,j,k) = dble(i*j*k)
        data%block(2)%vars(2,i,j,k) = -dble(i*j*k)
      enddo
    enddo
  enddo

  !data%tec%format = 'binary'
  !error = tec_output(data_=data,varnames='variable1 variable2',filename='solfile')

  data%tec%format = 'ascii'
  error = tec_output(data_=data,varnames='"variable1" "variable2"',filename='solfile')

end program tecplot_write_multiblock