  program vtk_many_blocks
  !---------------------------------------------------------------------------------------------------------------------------------
  !> vtk_write_structured_multiblock and vtk_read_structured_multiblock write and read any number of blocks: the writer kept
  !> the file index of each block in an array of 99 and the reader the block names listed in the .vtm in an array of 16, so a
  !> multi-block field of more blocks was written or read out of bounds.
  !>
  !> For 1, 16 and 17 blocks (3-D, a few cells each, different sizes) in each of the ascii, binary and raw formats, and for 130
  !> blocks in the ascii format, the program writes a field without time, reads it back and checks the number of blocks, their
  !> names and sizes, every coordinate and every variable. Every value is an integer, so it reads back bit for bit in every
  !> format. The field of 130 blocks goes past the 99 file indexes of the writer. One format is enough there: VTK_INI_XML
  !> stores the index of a block file before it looks at the format. 130 rather than 100: past the array, a gfortran release
  !> build of the old writer went on for a few blocks without any sign, then crashed (from 110 blocks in our runs).
  !> Exit status 0 when every check passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_ORION_data
  use Lib_VTK, only: vtk_write_structured_multiblock, vtk_read_structured_multiblock
  implicit none
  character(len=6), parameter :: formats(3) = ['ascii ', 'binary', 'raw   ']
  integer(I4P),     parameter :: counts(3) = [1_I4P, 16_I4P, 17_I4P]
  integer :: checks = 0, failures = 0
  integer :: f, c

  do f = 1, size(formats)
    do c = 1, size(counts)
      call round_trip(counts(c), trim(formats(f)))
    enddo
  enddo
  call round_trip(130_I4P, 'ascii')

  write(*,'(A,I0,A,I0,A)') 'vtk_many_blocks: ', checks, ' checks, ', failures, ' failed'
  if (failures > 0) stop 1

  contains

  subroutine check(condition, message)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Count a check and report it when it fails.
  !---------------------------------------------------------------------------------------------------------------------------------
  logical,          intent(in) :: condition
  character(len=*), intent(in) :: message
  !---------------------------------------------------------------------------------------------------------------------------------

  checks = checks + 1
  if (.not.condition) then
    failures = failures + 1
    write(*,'(A)') '  FAIL: '//message
  endif
  end subroutine check

  subroutine round_trip(nb, fmt)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write nb blocks in the given format, read them back and compare.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     intent(in) :: nb
  character(len=*), intent(in) :: fmt
  type(orion_data)             :: written, back
  character(len=64)            :: label, vtm
  character(len=16)            :: varnames
  integer(I4P)                 :: err, b, i, j, k, ni, nj, nk
  !---------------------------------------------------------------------------------------------------------------------------------

  allocate(written%block(1:nb))
  do b = 1, nb
    ni = 1 + mod(b, 3); nj = 1 + mod(b, 2); nk = 1 + mod(b, 4)
    written%block(b)%name = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt//'_B'//trim(str(.true.,b))
    written%block(b)%Ni = ni; written%block(b)%Nj = nj; written%block(b)%Nk = nk
    allocate(written%block(b)%mesh(1:3,0:ni,0:nj,0:nk))
    allocate(written%block(b)%vars(1:2,1:ni,1:nj,1:nk))
    do k = 0, nk
      do j = 0, nj
        do i = 0, ni
          written%block(b)%mesh(1,i,j,k) = real(i + 10*b, R8P)
          written%block(b)%mesh(2,i,j,k) = real(j, R8P)
          written%block(b)%mesh(3,i,j,k) = real(k + 1, R8P)
        enddo
      enddo
    enddo
    do k = 1, nk
      do j = 1, nj
        do i = 1, ni
          written%block(b)%vars(1,i,j,k) = real(i + 10*j + 100*k + 1000*b, R8P)
          written%block(b)%vars(2,i,j,k) = -real(b, R8P)
        enddo
      enddo
    enddo
  enddo
  written%vtk%format = fmt
  vtm = 'many_blocks_'//trim(str(.true.,nb))//'_'//fmt
  label = trim(str(.true.,nb))//' blocks '//fmt
  varnames = 'a b'
  err = vtk_write_structured_multiblock(orion=written, vtspath='', vtmpath=trim(vtm), varnames=varnames)
  call check(err == 0, trim(label)//': write error')
  back%vtk%format = fmt
  err = vtk_read_structured_multiblock(orion=back, vtspath='', vtmpath=trim(vtm))
  call check(err == 0, trim(label)//': read error')
  call check(allocated(back%block), trim(label)//': no block read')
  if (.not.allocated(back%block)) return
  call check(size(back%block) == nb, trim(label)//': '//trim(str(.true.,size(back%block)))//' blocks read')
  if (size(back%block) /= nb) return
  do b = 1, nb
    associate(w => written%block(b), r => back%block(b))
    call check(trim(r%name) == trim(w%name), trim(label)//': name of block '//trim(str(.true.,b)))
    call check(r%Ni == w%Ni .and. r%Nj == w%Nj .and. r%Nk == w%Nk, trim(label)//': size of block '//trim(str(.true.,b)))
    call check(size(r%mesh,1) == 3, trim(label)//': mesh of block '//trim(str(.true.,b))//' not 3-D')
    if (size(r%mesh,1) /= 3) cycle
    if (any(lbound(r%mesh) /= lbound(w%mesh)) .or. any(ubound(r%mesh) /= ubound(w%mesh))) then
      call check(.false., trim(label)//': mesh bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%mesh == w%mesh), trim(label)//': coordinates of block '//trim(str(.true.,b)))
    if (any(lbound(r%vars) /= lbound(w%vars)) .or. any(ubound(r%vars) /= ubound(w%vars))) then
      call check(.false., trim(label)//': variable bounds of block '//trim(str(.true.,b)))
      cycle
    endif
    call check(all(r%vars == w%vars), trim(label)//': variables of block '//trim(str(.true.,b)))
    end associate
  enddo
  end subroutine round_trip
  endprogram vtk_many_blocks
