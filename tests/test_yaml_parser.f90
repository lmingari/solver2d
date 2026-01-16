program test_yaml_parser
    implicit none
    call process
end program test_yaml_parser

subroutine process
    use KindType
    use YAML_Parser
    use YAML_Value
    implicit none

    type(yaml_parser_t) :: parser
    character(:), allocatable :: key
    class(yaml_value_t), allocatable :: val
    integer(ip) :: indent
    logical :: eof

    call parser%open('test.yaml')
    do
        call parser%next(key, val, indent, eof)
        if(eof) exit
        write(*,*) repeat('+',indent), key, ': ', val%to_string()
    enddo
end subroutine process
