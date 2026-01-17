program test_yaml
    implicit none
    call process
end program test_yaml

subroutine process
    use YAML
    implicit none
    type(yaml_t) :: config
    call config%open('test.yaml')
    call config%print
end subroutine process
