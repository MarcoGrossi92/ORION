  program vtk_read_multiblock
  USE IR_Precision
  USE Lib_VTK, only: vtk_MBS_input
  use Lib_ORION_data
  USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks reading.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(orion_data):: orion
  integer(I4P):: E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  orion%vtk%format='binary'
  E_IO = vtk_MBS_input(orion=orion,path='field')

  end program vtk_read_multiblock