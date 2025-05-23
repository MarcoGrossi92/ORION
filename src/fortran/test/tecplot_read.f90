program tecplot_read_multiblock
  use Lib_Tecplot
  use Lib_ORION_data
  implicit none
  type(orion_data) :: data
  integer          :: error, i, nb
  character(500)   :: varnames

  data%tec%node = .false.
  data%tec%bc = .false.

  write(*,*) '----------------------------------'
  write(*,*) ' TEC ASCII FILES '
  write(*,*) '----------------------------------'

  data%tec%format = 'ascii'
  error = tec_read_structured_multiblock(orion=data,filename='tecfile.tec')
  nb = size(data%block)

  write(*,*) 'Blocks number = ', nb
  do i = 1, nb
    write(*,*) 'Block size    = ', data%block(i)%Ni, data%block(i)%Nj, data%block(i)%Nk
  enddo

  deallocate(data%block)

  write(*,*) '----------------------------------'
  write(*,*) ' SZPLT BINARY FILES '
  write(*,*) '----------------------------------'

  data%tec%format = 'binary'
  error = tec_read_structured_multiblock(orion=data,filename='tecfile.szplt')
  nb = size(data%block)

  write(*,*) 'Blocks number = ', nb
  do i = 1, nb
    write(*,*) 'Block name    = ', trim(data%block(i)%name) 
    write(*,*) 'Block size    = ', data%block(i)%Ni, data%block(i)%Nj, data%block(i)%Nk
  enddo

end program tecplot_read_multiblock