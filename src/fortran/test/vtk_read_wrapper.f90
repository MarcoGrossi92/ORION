  program vtk_read_multiblock
  USE IR_Precision
  USE Lib_VTK, only: vtk_read_structured_multiblock
  use Lib_ORION_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks reading.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data):: orion
  integer(I4P):: E_IO, i
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  orion%vtk%format='raw'
  E_IO = vtk_read_structured_multiblock(orion=orion,vtmpath='field',vtspath='')

  write(*,*) 'Blocks number = ', size(orion%block)
  do i = 1, size(orion%block)
    write(*,*) 'Block name    = ', orion%block(i)%name
    write(*,*) 'Block size    = ', orion%block(i)%Ni, orion%block(i)%Nj, orion%block(i)%Nk
  enddo

  end program vtk_read_multiblock