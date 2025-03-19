program p3d_read_multiblock
  use Lib_PLOT3D
  use Lib_ORION_data
  implicit none
  type(orion_data) :: data
  integer          :: error, i, nb
  character(500)   :: varnames

  data%p3d%format = 'ascii'
  error = p3d_read_multiblock(orion=data,filename='mesh.p3d')

  nb = size(data%block)
  write(*,*) 'Blocks number = ', nb
  do i = 1, nb
    write(*,*) 'Block size    = ', data%block(i)%Ni, data%block(i)%Nj, data%block(i)%Nk
  enddo

end program p3d_read_multiblock