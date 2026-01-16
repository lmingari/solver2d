module YAML_Node
    use KindType, only: ip, rp
    use YAML_Value
    implicit none

    private
    public :: yaml_node_t

    ! ============================================
    ! yaml_node_t type (key-value pairs)
    ! ============================================

    type :: yaml_node_t
        integer(ip) :: indent = -1
        character(len=:),    allocatable :: key
        class(yaml_value_t), allocatable :: value
        type(yaml_node_t), pointer :: parent       => null()
        type(yaml_node_t), pointer :: first_child  => null()
        type(yaml_node_t), pointer :: last_child   => null()
        type(yaml_node_t), pointer :: next_sibling => null()
    contains
        procedure :: add_child        => node_add_child
        procedure :: get_parent       => node_get_parent
        procedure :: get_first_child  => node_get_first_child
        procedure :: get_last_child   => node_get_last_child
        procedure :: get_next_sibling => node_get_next_sibling
        procedure :: find_child       => node_find_child
        procedure :: find_parent      => node_find_parent
        procedure :: print => node_print
        final :: destroy_node
    end type yaml_node_t

    interface yaml_node_t
        module procedure create_node
    end interface

contains

    !
    !>>>1 Constructors
    !

    function create_node(key, indent) result(node_ptr)
        character(len=*), intent(in)      :: key
        integer(ip), optional, intent(in) :: indent
        type(yaml_node_t), pointer :: node_ptr

        allocate(node_ptr)
        node_ptr%key = key
        if(present(indent)) node_ptr%indent = indent
    end function create_node

    !
    !>>>1 Destructors
    !

    subroutine destroy_node(this)
        type(yaml_node_t), intent(inout) :: this
        type(yaml_node_t), pointer :: current, next_node

        ! 1. Recursively clean up children
        current => this%first_child
        do while (associated(current))
            next_node => current%next_sibling
            ! Assume dynamic allocation
            deallocate(current)
            current => next_node
        end do
        write(*,*) "Destroyed node"

        ! 2. Nullify pointers to break associations
        this%parent        => null()
        this%first_child   => null()
        this%last_child    => null()
        this%next_sibling  => null()
    end subroutine destroy_node

    !
    !>>>1 Methods: print
    !

    recursive subroutine node_print(this)
        class(yaml_node_t), target, intent(in) :: this
        !
        type(yaml_node_t), pointer :: current
        character(len=100) :: indent_str
        !
        indent_str = repeat('+', max(0,this%indent))

        ! Start with key
        if (allocated(this%key)) then
            write(*,'(A)',advance='no') trim(indent_str) // this%key // ": "
        else
            write(*,'(A)',advance='no') trim(indent_str) // "root: "
        end if
        
        ! Add value
        if (allocated(this%value)) then
            write(*,'(A)') this%value%to_string()
        else
            write(*,'(A)') "null"
        end if
        
        ! Add children if any
        current => this%first_child
        do while(associated(current))
            call current%print
            current => current%next_sibling
        enddo
    end subroutine node_print

    !
    !>>>1 Methods: getters
    !
    
    function node_get_parent(this) result(parent)
        class(yaml_node_t), intent(in) :: this
        type(yaml_node_t), pointer :: parent
        parent => this%parent
    end function node_get_parent

    function node_get_first_child(this) result(child)
        class(yaml_node_t), intent(in) :: this
        type(yaml_node_t), pointer :: child
        child => this%first_child
    end function node_get_first_child

    function node_get_last_child(this) result(child)
        class(yaml_node_t), intent(in) :: this
        type(yaml_node_t), pointer :: child
        child => this%last_child
    end function node_get_last_child

    function node_get_next_sibling(this) result(sibling)
        class(yaml_node_t), intent(in) :: this
        type(yaml_node_t), pointer :: sibling
        sibling => this%next_sibling
    end function node_get_next_sibling

    !
    !>>>1 Methods: find
    !

    function node_find_child(this, key) result(child)
        class(yaml_node_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(yaml_node_t), pointer :: child
        
        child => this%first_child
        do while (associated(child))
        ! LAM: Check key is allocated?
            write(*,*) "checking ", child%key
            if (child%key == key) return
            child => child%next_sibling
        end do
        
        child => null()
    end function node_find_child

    ! Get parent node at or above specified indent level
    function node_find_parent(this, indent) result(parent)
        class(yaml_node_t), intent(in) :: this
        integer(ip), intent(in) :: indent
        type(yaml_node_t), pointer :: parent
        !
        parent => this%parent
        do while(associated(parent)) 
            if(parent%indent<indent) exit
            parent => parent%parent
        enddo

    end function node_find_parent

    !
    !>>>1 Methods: add_child
    !

    subroutine node_add_child(this, child_node)
        class(yaml_node_t), target, intent(inout) :: this
        type(yaml_node_t),  target, intent(inout) :: child_node
        type(yaml_node_t), pointer :: current
        
        child_node%parent => this
        
        if (.not. associated(this%first_child)) then
            this%first_child => child_node
            this%last_child  => child_node
        else
            this%last_child%next_sibling => child_node
            this%last_child => child_node
        end if
    end subroutine node_add_child
    
end module YAML_Node
