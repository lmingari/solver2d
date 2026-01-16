program test_yaml_parser
    implicit none
    call process
end program test_yaml_parser

subroutine process
    use KindType
    use YAML_Parser
    implicit none

    type(yaml_parser_t) :: parser
    character(:), allocatable :: key, raw_val
    integer(ip) :: dtype, indent
    logical :: eof

    call parser%open('test.yaml')
    do
        call parser%next(key, raw_val, dtype, indent, eof)
        if(eof) exit
        write(*,*) repeat('+',indent), key, ': ', raw_val, indent
    enddo
end subroutine process
