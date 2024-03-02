  program vtk_write_multiblock
  USE IR_Precision
  USE Lib_VTK
  USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
  !---------------------------------------------------------------------------------------------------------------------------------
  !> Procedure for testing multi-blocks VTM functions.
  !>
  !> There are 4 subset of data organized into 2 blocks. All the subsets are simple StructuredGrid prisms shifted along x direction.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::                           nx1=0_I4P,nx2=90_I4P,ny1=0_I4P,ny2=50_I4P,nz1=0_I4P,nz2=50_I4P
  integer(I4P), parameter::                           nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  integer(I4P), parameter::                           nnvar=(nx2)*(ny2)*(nz2)
  real(8), dimension(nx1:nx2,ny1:ny2,nz1:nz2)::       x,y,z
  real(8), dimension(nx1+1:nx2,ny1+1:ny2,nz1+1:nz2):: v, v2
  integer(I4P)::                                      i,j,k,b,mf(1:4),E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing multi-blocks VTM functions'
  ! arrays initialization
  do k=nz1,nz2
   do j=ny1,ny2
     do i=nx1,nx2
       x(i,j,k) = i*1._R8P
       y(i,j,k) = j*1._R8P
       z(i,j,k) = k*1._R8P
     enddo
   enddo
  enddo
  do k=nz1+1,nz2
   do j=ny1+1,ny2
     do i=nx1+1,nx2
       v(i,j,k) = (i*j*k)*1._R8P
       v2(i,j,k) = -(i*j*k)*1._R8P
     enddo
   enddo
  enddo
  ! vts
  do b=1,4 ! loop over blocks
    E_IO = VTK_INI_XML(cf=mf(b),output_format='binary', filename='XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
    if (b>1) then
      x = x + nx2*1._R8P
    endif
    E_IO = VTK_GEO_XML(cf=mf(b),nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                       X=reshape(x(nx1:nx2,:,:),(/nn/)),                                     &
                       Y=reshape(y(nx1:nx2,:,:),(/nn/)),                                     &
                       Z=reshape(z(nx1:nx2,:,:),(/nn/)))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'cell', var_block_action = 'open')
    E_IO = VTK_VAR_XML(cf=mf(b),NC_NN = nnvar, varname = 'var1', var = reshape(v(nx1+1:nx2,:,:),[nnvar]))
    E_IO = VTK_VAR_XML(cf=mf(b),NC_NN = nnvar, varname = 'var2', var = reshape(v2(nx1+1:nx2,:,:),[nnvar]))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'cell', var_block_action = 'close')
    E_IO = VTK_GEO_XML(cf=mf(b))
    E_IO = VTK_END_XML()
  enddo
  ! vtm
  E_IO = VTM_INI_XML('XML_M-STRG.vtm')
  E_IO = VTM_BLK_XML(block_action='open')
  E_IO = VTM_WRF_XML(flist=(/('XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts',b=1,4)/))
  E_IO = VTM_BLK_XML(block_action='close')
  E_IO = VTM_END_XML()

  end program vtk_write_multiblock