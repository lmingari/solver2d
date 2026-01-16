module YAML
    use KindType, only: ip, rp
    use YAML_Node
    use YAML_Parser
    implicit none

    private
    PUBLIC :: yaml_t

    type :: yaml_t
        character(len=:), allocatable :: fname
        type(yaml_node_t), pointer :: root => null()
    contains
        procedure :: open  => yaml_open
        procedure :: close => yaml_close
        procedure :: yaml_get_node1
        procedure :: yaml_get_node2
        generic   :: get_node => yaml_get_node1, yaml_get_node2
        final ::  destroy_yaml
    end type yaml_t

contains

    !
    !>>>1 Constructors
    !

    !
    !>>>1 Destructor
    !

    subroutine destroy_yaml(self)
        type(yaml_t), intent(inout) :: self
        call self%close
        write(*,*) "Destroyed yaml_t"
    end subroutine destroy_yaml

    !
    !>>>1 Methods: open
    !

    subroutine yaml_open(self, fname)
        class(yaml_t), intent(inout) :: self
        character(*), intent(in) :: fname
        !
        type(yaml_parser_t) :: parser
        type(yaml_node_t), pointer :: current_parent
        type(yaml_node_t), pointer :: new_node
        type(yaml_node_t), pointer :: last_node
        !
        character(:), allocatable :: key, raw_val
        integer(ip) :: dtype, indent
        logical :: eof
        !
        self%fname = fname
        self%root => yaml_node_t('root')
        !
        ! Tmp pointers
        current_parent => self%root
        nullify(last_node)

        call parser%open(fname)
        read_yaml: do
            call parser%next(key, raw_val, dtype, indent, eof)
            if(eof) exit read_yaml
            new_node => yaml_node_t(key, indent)
            !
            ! Find new parent
            if(associated(last_node)) then
                if(indent>last_node%indent) then
                    current_parent => last_node
                elseif(indent<last_node%indent) then
                    current_parent => last_node%find_parent(indent)
                else
                    current_parent => last_node%parent
                endif
                if (.not. associated(current_parent)) current_parent => self%root
            endif
            !
            ! Add node to parent
            call current_parent%add_child(new_node)
            !
            ! Update last node
            last_node => new_node
       end do read_yaml
    end subroutine yaml_open

    !
    !>>>1 Methods: close
    !

    subroutine yaml_close(self)
        class(yaml_t), intent(inout) :: self
        !
        if(associated(self%root)) then
            deallocate(self%root)
        endif
    end subroutine yaml_close

    !
    !>>>1 Methods: getter
    !

    function yaml_get_node1(self,key1) result(node_ptr)
        class(yaml_t),    intent(in) :: self
        character(len=*), intent(in) :: key1
        type(yaml_node_t), pointer :: node_ptr
        !
        node_ptr => self%root%find_child(key1)
    end function yaml_get_node1

    function yaml_get_node2(self,key1,key2) result(node_ptr)
        class(yaml_t),    intent(in) :: self
        character(len=*), intent(in) :: key1
        character(len=*), intent(in) :: key2
        type(yaml_node_t), pointer :: node_ptr
        !
        node_ptr => self%root%find_child(key1)
        if(associated(node_ptr)) then
            node_ptr => node_ptr%find_child(key2)
        endif
    end function yaml_get_node2
    
end module YAML
