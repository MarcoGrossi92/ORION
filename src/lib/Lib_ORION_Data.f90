!> @ingroup Library
!> @{
!> @defgroup Lib_ORION_dataLibrary Lib_ORION_data
!> @}

!> Module to store the ORION main data structure
!> @ingroup Lib_ORION_dataLibrary
module Lib_ORION_data
  use IR_Precision
  implicit none
  save
  private

  type :: Type_tec_Format
    character(6):: format = 'binary'  !> Binary or ascii file.
    logical     :: node   = .false.   !> Node or cell data location.
    logical     :: bc     = .false.   !> Saving or not boundary conditions cells.
  endtype Type_tec_Format

  type :: Type_vtk_Format
    character(6):: format = 'binary'  !> Binary or ascii file.
    logical     :: node   = .false.   !> Node or cell data location.
  endtype Type_vtk_Format

  type :: obj_block
    integer :: Ni, Nj, Nk
    real(R8P), dimension(:,:,:,:), allocatable :: mesh
    real(R8P), dimension(:,:,:,:), allocatable :: vars
  end type obj_block

  type, public :: orion_data
    type(obj_block), allocatable :: block(:)
    type(Type_tec_Format) :: tec
    type(Type_vtk_Format) :: vtk
  endtype orion_data

end module Lib_ORION_data