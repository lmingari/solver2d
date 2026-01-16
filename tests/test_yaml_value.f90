program test_yaml_value
    use KindType
    use YAML_Value
    implicit none

    ! Define an allocatable class variable
    class(yaml_value_t), allocatable :: val1, val2
    class(yaml_value_t), allocatable :: val_copy
!    type(yaml_real) :: rval

    val1 = yaml_value_t()
    print *, "Value: ", val1%to_string()
    print *, "Type: ", val1%get_type()

    val2 = yaml_value_t(5.0_rp)
    print *, "Value: ", val2%to_string()
    print *, "Type: ", val2%get_type()

!    call rval%set(12.0_rp)
!    print *, "String: ", rval%to_string()
!    print *, "Value: ",  rval%get()
!    print *, "Type: ",   rval%get_type()

    write(*,*) "cloning...."
    val_copy = val1%clone()
    print *, "Original: ", val1%to_string()
    print *, "Clone: ", val_copy%to_string()

    if(allocated(val1)) deallocate(val1)
    if(allocated(val2)) deallocate(val2)
    if(allocated(val_copy)) deallocate(val_copy)

end program test_yaml_value
