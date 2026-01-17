module YAML_Value
    use KindType, only: ip, rp, size_t
    implicit none
    !
    private
    PUBLIC :: yaml_value_t

    ! ============================================
    ! Abstract base type for all YAML values
    ! ============================================

    type, abstract :: yaml_value_t
        character(len=:), allocatable :: raw_string
    contains
        ! Abstract methods that must be implemented
        procedure(to_string_abs), deferred :: to_string
        procedure(get_type_abs),  deferred :: get_type
        procedure(clone_abs),     deferred :: clone
    end type yaml_value_t

    abstract interface
        function to_string_abs(this) result(str)
            import :: yaml_value_t
            class(yaml_value_t), intent(in) :: this
            character(len=:), allocatable :: str
        end function to_string_abs

        function get_type_abs(this) result(name)
            import :: yaml_value_t
            class(yaml_value_t), intent(in) :: this
            character(len=20) :: name
        end function get_type_abs

        function clone_abs(this) result(copy)
            import :: yaml_value_t
            class(yaml_value_t), intent(in) :: this
            class(yaml_value_t), allocatable :: copy
        end function clone_abs
    end interface

    ! ============================================
    ! Concrete derived types
    ! ============================================
    
    ! Null type
    type, extends(yaml_value_t) :: yaml_null
    contains
        procedure :: to_string => null_to_string
        procedure :: get_type => null_get_type
        procedure :: clone => clone_null
    end type yaml_null

    ! Real number type
    type, extends(yaml_value_t) :: yaml_real
        real(rp) :: value = 0.0_rp
    contains
        procedure :: to_string => real_to_string
        procedure :: get_type => real_get_type
        procedure :: clone => clone_real
        procedure :: get => get_real_value
        procedure :: set => set_real_value
    end type yaml_real

    ! Integer type
    type, extends(yaml_value_t) :: yaml_int
        integer(ip) :: value = 0_ip
    contains
        procedure :: to_string => int_to_string
        procedure :: get_type => int_get_type
        procedure :: clone => clone_int
        procedure :: get => get_int_value
        procedure :: set => set_int_value
    end type yaml_int

    ! Logical type
    type, extends(yaml_value_t) :: yaml_bool
        logical :: value = .false.
    contains
        procedure :: to_string => bool_to_string
        procedure :: get_type => bool_get_type
        procedure :: clone => clone_bool
        procedure :: get => get_bool_value
        procedure :: set => set_bool_value
    end type yaml_bool

    ! String type
    type, extends(yaml_value_t) :: yaml_string
        character(len=:), allocatable :: value
    contains
        procedure :: to_string => string_to_string
        procedure :: get_type => string_get_type
        procedure :: clone => clone_string
        procedure :: get => get_string_value
        procedure :: set => set_string_value
    end type yaml_string

    ! Array type
    type, extends(yaml_value_t) :: yaml_array
        class(yaml_value_t), dimension(:), allocatable :: values
    contains
        procedure :: to_string => array_to_string
        procedure :: get_type => array_get_type
        procedure :: clone => clone_array
        procedure :: get => get_array_element
!        procedure :: set => set_array_element
!        procedure :: append => append_to_array
        procedure :: get_size => get_array_size
!        final :: destroy_array
    end type yaml_array

    interface yaml_value_t
        module procedure create_null_value
        module procedure create_bool_value
        module procedure create_int_value
        module procedure create_real_value
        module procedure create_string_value
        !
        module procedure create_null_array
        module procedure create_bool_array
        module procedure create_int_array
        module procedure create_real_array
        module procedure create_string_array
    end interface

contains

    !
    !>>>1 Constructors
    !

    function create_null_value() result(res)
        class(yaml_value_t), allocatable :: res

        allocate(yaml_null :: res)
        res%raw_string = "null"
    end function create_null_value

    function create_real_value(r) result(res)
        real(rp), intent(in) :: r
        class(yaml_value_t), allocatable :: res
        
        ! Allocate the result as the specific extended type
        allocate(yaml_real :: res)
        
        select type (c => res)
        type is (yaml_real)
            c%value = r
        end select
    end function create_real_value

    function create_int_value(i) result(res)
        integer(ip), intent(in) :: i
        class(yaml_value_t), allocatable :: res
        
        allocate(yaml_int :: res)
        
        select type (c => res)
        type is (yaml_int)
            c%value = i
        end select
    end function create_int_value

    function create_bool_value(b) result(res)
        logical, intent(in) :: b
        class(yaml_value_t), allocatable :: res
        
        allocate(yaml_bool :: res)
        
        select type (c => res)
        type is (yaml_bool)
            call c%set(b)
        end select
    end function create_bool_value

    function create_string_value(str) result(res)
        character(len=*), intent(in) :: str
        class(yaml_value_t), allocatable :: res
        
        allocate(yaml_string :: res)

        select type (c => res)
        type is (yaml_string)
            call c%set(str)
        end select
    end function create_string_value

    function create_null_array(arr_size) result(res)
        type(size_t), intent(in) :: arr_size
        class(yaml_value_t), allocatable :: res
        type(yaml_array),    allocatable :: tmp

        allocate(tmp)
        allocate(yaml_null :: tmp%values(arr_size%n))
!        res%raw_string = "null"
        call move_alloc(tmp,res)
    end function create_null_array

    function create_bool_array(arr_size,bvals) result(res)
        type(size_t), intent(in) :: arr_size
        logical,      intent(in) :: bvals(arr_size%n)
        class(yaml_value_t), allocatable :: res
        type(yaml_array),    allocatable :: tmp

        allocate(tmp)
        allocate(yaml_bool :: tmp%values(arr_size%n))
        select type(values => tmp%values)
        type is (yaml_bool)
            values(:)%value = bvals
        end select
        call move_alloc(tmp,res)
    end function create_bool_array

    function create_int_array(arr_size,ivals) result(res)
        type(size_t), intent(in) :: arr_size
        integer(ip),  intent(in) :: ivals(arr_size%n)
        class(yaml_value_t), allocatable :: res
        type(yaml_array),    allocatable :: tmp
        !
        allocate(tmp)
        allocate(yaml_int :: tmp%values(arr_size%n))
        select type(values => tmp%values)
        type is (yaml_int)
            values(:)%value = ivals
        end select
        call move_alloc(tmp,res)
    end function create_int_array

    function create_real_array(arr_size,rvals) result(res)
        type(size_t), intent(in) :: arr_size
        real(rp),     intent(in) :: rvals(arr_size%n)
        class(yaml_value_t), allocatable :: res
        type(yaml_array),    allocatable :: tmp
        !
        allocate(tmp)
        allocate(yaml_real :: tmp%values(arr_size%n))
        select type(values => tmp%values)
        type is (yaml_real)
            values(:)%value = rvals
        end select
        call move_alloc(tmp,res)
    end function create_real_array

    function create_string_array(arr_size,svals) result(res)
        type(size_t), intent(in) :: arr_size
        character(*), intent(in) :: svals(arr_size%n)
        class(yaml_value_t), allocatable :: res
        type(yaml_array),    allocatable :: tmp
        integer(ip) :: i
        !
        allocate(tmp)
        allocate(yaml_string :: tmp%values(arr_size%n))
        select type(values => tmp%values)
        type is (yaml_string)
            do i=1,arr_size%n
                call values(i)%set( svals(i) )
            enddo
        end select
        call move_alloc(tmp,res)
    end function create_string_array

    !
    !>>>1 Methods: to_string
    !

    function null_to_string(this) result(str)
        class(yaml_null), intent(in) :: this
        character(len=:), allocatable :: str
        str = "null"
    end function null_to_string

    function real_to_string(this) result(str)
        class(yaml_real), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        
        write(buffer, '(G0)') this%value
        str = trim(buffer)
    end function real_to_string

    function int_to_string(this) result(str)
        class(yaml_int), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        
        write(buffer, '(I0)') this%value
        str = trim(buffer)
    end function int_to_string

    function bool_to_string(this) result(str)
        class(yaml_bool), intent(in) :: this
        character(len=:), allocatable :: str
        
        if (this%value) then
            str = "true"
        else
            str = "false"
        end if
    end function bool_to_string

    function string_to_string(this) result(str)
        class(yaml_string), intent(in) :: this
        character(len=:), allocatable :: str
        str = '"' // this%value // '"'
    end function string_to_string

    function array_to_string(this) result(str)
        class(yaml_array), intent(in) :: this
        character(len=:), allocatable :: str
        integer :: i
        
        str = "["
        do i = 1, size(this%values)
            str = str // this%values(i)%to_string()
            if (i < size(this%values)) str = str // ", "
        end do
        str = str // "]"
    end function array_to_string

    !
    !>>>1 Methods: get_type
    !

    function null_get_type(this) result(name)
        class(yaml_null), intent(in) :: this
        character(len=20) :: name
        name = "null"
    end function null_get_type

    function real_get_type(this) result(name)
        class(yaml_real), intent(in) :: this
        character(len=20) :: name
        name = "real"
    end function real_get_type

    function int_get_type(this) result(name)
        class(yaml_int), intent(in) :: this
        character(len=20) :: name
        name = "integer"
    end function int_get_type

    function bool_get_type(this) result(name)
        class(yaml_bool), intent(in) :: this
        character(len=20) :: name
        name = "boolean"
    end function bool_get_type

    function string_get_type(this) result(name)
        class(yaml_string), intent(in) :: this
        character(len=20) :: name
        name = "string"
    end function string_get_type

    function array_get_type(this) result(name)
        class(yaml_array), intent(in) :: this
        character(len=20) :: name
        name = "array"
    end function array_get_type

    !
    !>>>1 Methods: clone
    !
    
    function clone_null(this) result(copy)
        class(yaml_null), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_null), allocatable :: null_copy
        
        allocate(null_copy)
        null_copy%raw_string = this%raw_string
        call move_alloc(null_copy, copy)
    end function clone_null

    function clone_real(this) result(copy)
        class(yaml_real), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_real), allocatable :: real_copy

        allocate(real_copy)
        real_copy%raw_string = this%raw_string
        real_copy%value = this%value
        call move_alloc(real_copy, copy)
    end function clone_real

    function clone_int(this) result(copy)
        class(yaml_int), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_int), allocatable :: int_copy

        allocate(int_copy)
        int_copy%raw_string = this%raw_string
        int_copy%value = this%value
        call move_alloc(int_copy, copy)
    end function clone_int

    function clone_bool(this) result(copy)
        class(yaml_bool), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_bool), allocatable :: bool_copy
        
        allocate(bool_copy)
        bool_copy%raw_string = this%raw_string
        bool_copy%value = this%value
        call move_alloc(bool_copy, copy)
    end function clone_bool

    function clone_string(this) result(copy)
        class(yaml_string), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_string), allocatable :: str_copy
        
        allocate(str_copy)
        str_copy%raw_string = this%raw_string
        str_copy%value = this%value
        call move_alloc(str_copy, copy)
    end function clone_string

    function clone_array(this) result(copy)
        class(yaml_array), intent(in) :: this
        class(yaml_value_t), allocatable :: copy
        type(yaml_array), allocatable :: arr_copy
!        integer :: i
        
        allocate(arr_copy)
        !LAM: BUG
!        allocate(arr_copy%values(size(this%values)))
!        do i = 1, size(this%values)
!            arr_copy%values(i) = this%values(i)%clone()
!        end do
        call move_alloc(arr_copy, copy)
    end function clone_array

    !
    !>>>1 Methods: getters
    !
    
    function get_real_value(this) result(val)
        class(yaml_real), intent(in) :: this
        real(rp) :: val
        val = this%value
    end function get_real_value

    function get_int_value(this) result(val)
        class(yaml_int), intent(in) :: this
        integer(ip) :: val
        val = this%value
    end function get_int_value

    function get_bool_value(this) result(val)
        class(yaml_bool), intent(in) :: this
        logical :: val
        val = this%value
    end function get_bool_value
    
    function get_string_value(this) result(val)
        class(yaml_string), intent(in) :: this
        character(len=:), allocatable :: val
        val = this%value
    end function get_string_value

    function get_array_element(this, index) result(val)
        class(yaml_array), intent(in) :: this
        integer(ip), intent(in) :: index
        class(yaml_value_t), allocatable :: val

        if(.not.allocated(this%values)) return
        
        if (index >= 1 .and. index <= size(this%values)) then
            val = this%values(index)%clone()
        end if
    end function get_array_element

    function get_array_size(this) result(n)
        class(yaml_array), intent(in) :: this
        integer(ip) :: n
       if (allocated(this%values)) then
            n = size(this%values)
        else
            n = 0
        end if
    end function get_array_size

    !
    !>>>1 Methods: setters
    !

    subroutine set_real_value(this, val)
        class(yaml_real), intent(inout) :: this
        real(rp), intent(in) :: val
        character(len=32) :: buffer
        
        this%value = val
        write(buffer, '(G0)') val
        this%raw_string = trim(buffer)
    end subroutine set_real_value

    subroutine set_int_value(this, val)
        class(yaml_int), intent(inout) :: this
        integer(ip), intent(in) :: val
        character(len=32) :: buffer
        
        this%value = val
        write(buffer, '(I0)') val
        this%raw_string = trim(buffer)
    end subroutine set_int_value

    subroutine set_bool_value(this, val)
        class(yaml_bool), intent(inout) :: this
        logical, intent(in) :: val
        this%value = val
        if (val) then
            this%raw_string = "true"
        else
            this%raw_string = "false"
        end if
    end subroutine set_bool_value

    subroutine set_string_value(this, val)
        class(yaml_string), intent(inout) :: this
        character(len=*), intent(in) :: val
        this%value = strip_quotes(val)
        this%raw_string = val
    end subroutine set_string_value

    !
    !>>>1 Auxiliary functions
    !

    pure function strip_quotes(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        character(len=:), allocatable :: tmp
        integer :: n

        tmp = trim(adjustl(s))
        n = len_trim(tmp)

        if(n>1 .and. tmp(1:1) == '"' .and. tmp(n:n) == '"') then
            res = tmp(2:n-1)
        else
            res = tmp
        end if
    end function

end module YAML_Value
