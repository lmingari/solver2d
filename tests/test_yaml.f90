program test_yaml
    implicit none
    call process
end program test_yaml

subroutine process
    use YAML
    use YAML_Node
    implicit none
    type(yaml_t) :: config
    type(yaml_node_t), pointer :: node_ptr
    call config%open('test.yaml')
!    call config%root%print
    node_ptr => config%get_node('pepe2','new_lis')
    if(associated(node_ptr)) call node_ptr%print
end subroutine process
