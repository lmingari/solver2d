module KindType
    use, intrinsic :: iso_fortran_env, only : int32, real32, real64
    implicit none
    !
    public
    !
    integer, parameter :: ip = int32
    integer, parameter :: sp = real32
    integer, parameter :: dp = real64
    integer, parameter :: rp = real32
    !
    integer, parameter :: s_name = 24
    integer, parameter :: s_file = 64
    integer, parameter :: s_msg  = 128
    !
end module KindType

