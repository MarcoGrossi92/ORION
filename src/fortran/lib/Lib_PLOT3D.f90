module Lib_PLOT3D
  use, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT
  use IR_Precision
  use Lib_ORION_data
  implicit none
  private


contains


  function p3d_write_multiblock(orion,filename) result(err)
    implicit none
    type(orion_data), intent(in)              :: orion
    character(len=*), intent(in)              :: filename
    integer :: err
    integer :: unit, b, d, k, j, i, Nblocks
    
    Nblocks = size(orion%block)

    ! initializing file
    select case(orion%p3d%format)
    case('binary')
      stop "You can not write in binary P3D format"
    case('ascii')
      if (index(filename,'.')==0) then
        open(newunit=units,file=trim(filename)//".p3d")
      else
        open(newunit=unit,file=trim(filename))
      endif
    end select

    ! writing data blocks
    write(unit,*) Nblocks
    
    do b = 1, Nblocks
      write(unit,*) orion%block(b)%Ni+1, orion%block(b)%Nj+1, orion%block(b)%Nk+1
    enddo

    do b = 1, Nblocks
      do d = 1, 3
        do k = 0, orion%block(b)%Nk
          do j = 0, orion%block(b)%Nj
            do i = 0, orion%block(b)%Ni
              write(unit,*) orion%block(b)%mesh(d,i,j,k)
            enddo
          enddo
        enddo
      enddo
    enddo

    close(unit)

  endfunction p3d_write_multiblock


  function p3d_read_multiblock(orion,filename) result(err)
    use, intrinsic :: iso_fortran_env, only : iostat_end
    use strings, only: getvals, parse
    implicit none
    type(orion_data), intent(inout)              :: orion
    character(len=*), intent(in)                 :: filename
    integer :: err
    integer :: i, j, k, d, b
    integer :: Nblocks

    open(newunit=unit,file=trim(filename))

    read(unit,*) Nblocks
    allocate(orion%block(1:Nblocks))

    do b = 1, Nblocks
      read(unit,*) orion%block(b)%Ni, orion%block(b)%Nj, orion%block(b)%Nk
      orion%block(b)%Ni = orion%block(b)%Ni-1
      orion%block(b)%Nj = orion%block(b)%Nj-1
      orion%block(b)%Nk = orion%block(b)%Nk-1
    enddo

    do b = 1, Nblocks
      allocate(orion%block(b)%mesh(1:3,0:orion%block(b)%Ni,0:orion%block(b)%Nj,0:orion%block(b)%Nk))
      do d = 1, 3
        do k = 0, orion%block(b)%Nk; do j = 0, orion%block(b)%Nj; do i = 0, orion%block(b)%Ni
              read(unit,*,iostat=err) orion%block(b)%mesh(d,i,j,k)
        enddo; enddo; enddo
      enddo
    enddo

    close(unit)

  endfunction p3d_read_multiblock


  end module Lib_PLOT3D