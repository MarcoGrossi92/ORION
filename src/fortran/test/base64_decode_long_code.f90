  program base64_decode_long_code
  !---------------------------------------------------------------------------------------------------------------------------------
  !> b64_decode fills the array it is given and does not decode past its end. A code longer than the array, as the code of a VTK
  !> binary data array that holds more values than the reader counts in its piece, gives the array its first bytes. The program
  !> encodes 100 bytes (136 characters), decodes the code into arrays of 1, 2, 3, 4, 5, 6, 50, 99 and 100 bytes and checks that
  !> each holds the first bytes encoded; it then decodes the code of a VTK data array, a 4-byte size and 12 64-bit values, into
  !> its 4-byte size alone. Built with bound checks or with an address sanitizer, a decoder that writes past the array stops the
  !> program; built without them, it overwrites the memory after the array without a message. Exit status 0 when every check
  !> passes, 1 otherwise.
  !---------------------------------------------------------------------------------------------------------------------------------
  use IR_Precision
  use Lib_Base64, only: b64_encode, b64_decode, pack_data
  implicit none
  integer,          parameter   :: sizes(9) = [1, 2, 3, 4, 5, 6, 50, 99, 100]
  integer(I1P)                  :: bytes(100)
  integer(I1P), allocatable     :: out(:), packed(:)
  character(len=:), allocatable :: code
  real(R8P)                     :: values(12)
  integer                       :: checks = 0, failures = 0, i, n

  do i = 1, size(bytes)
    bytes(i) = int(mod(37*i, 256) - 128, I1P)
  enddo
  call b64_encode(n=bytes, code=code)
  call check(len(code) == 136, 'the code of 100 bytes has 136 characters')
  do i = 1, size(sizes)
    n = sizes(i)
    allocate(out(n))
    out = 0_I1P
    call b64_decode(code=code, n=out)
    call check(all(out == bytes(1:n)), 'decoded into '//trim(str(.true., n))//' bytes: the first '//trim(str(.true., n))// &
               ' bytes encoded')
    deallocate(out)
  enddo

  values = [(real(i, R8P), i = 1, size(values))]
  call pack_data(a1=[int(size(values)*BYR8P, I4P)], a2=values, packed=packed)
  call b64_encode(n=packed, code=code)
  allocate(out(BYI4P))
  out = 0_I1P
  call b64_decode(code=code, n=out)
  call check(transfer(out, 0_I4P) == size(values)*BYR8P, 'a VTK data array of 12 values decoded into its 4-byte size: 96')

  write(*,'(A,I0,A,I0,A)') 'base64_decode_long_code: ', checks, ' checks, ', failures, ' failed'
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
  endprogram base64_decode_long_code
