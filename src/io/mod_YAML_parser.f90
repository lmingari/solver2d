module YAML_Parser
    use KindType
    implicit none
    !
    private
    !
    PUBLIC :: yaml_parser_t
    !
    ! Value types enumeration
    integer(ip), parameter :: Y_NULL  = 0
    integer(ip), parameter :: Y_BOOL  = 1
    integer(ip), parameter :: Y_INT   = 2
    integer(ip), parameter :: Y_REAL  = 3
    integer(ip), parameter :: Y_STR   = 4
    integer(ip), parameter :: Y_ARRAY = 5
    !
    ! Parser class
    type :: yaml_parser_t
      private
      integer(ip) :: unit = -1
      logical :: is_open = .false.
      character(1024) :: buffer
      logical :: has_buffered_line = .false.
      character(len=1) :: delimiter = ','
    contains
      procedure :: open => parser_open
      procedure :: close => parser_close
      procedure :: next => parser_next
      procedure, private :: read_line => parser_read_line
      procedure, private :: block2flow => parser_array_block2flow
      final :: destroy_parser
    end type yaml_parser_t

contains

    !
    !>>>1 Destructor
    !

    subroutine destroy_parser(self)
        type(yaml_parser_t), intent(inout) :: self
        write(*,*) "calling destroy"
        call self%close
    end subroutine destroy_parser

    !
    !>>>1 Methods: open
    !

    subroutine parser_open(this, fname, iostat)
        class(yaml_parser_t), intent(inout) :: this
        character(len=*), intent(in) :: fname
        integer(ip), intent(out), optional :: iostat
        integer(ip) :: ios
        !
        open(newunit=this%unit, file=fname, status='old', action='read', iostat=ios)
        if(present(iostat)) iostat = ios
        this%is_open = .true.
    end subroutine parser_open

    !
    !>>>1 Methods: close
    !

    subroutine parser_close(this)
      class(yaml_parser_t), intent(inout) :: this

      if (this%is_open) then
          write(*,*) "closing..."
        close(this%unit)
        this%is_open = .false.
      end if
    end subroutine parser_close

    !
    !>>>1 Methods: read_line
    !

    ! Read from file or buffer
    subroutine parser_read_line(self, line, indent, eof)
        class(yaml_parser_t), intent(inout) :: self
        character(:), allocatable, intent(out) :: line
        integer(ip), intent(out) :: indent
        logical, intent(out) :: eof
        !
        character(1024) :: raw_line
        integer(ip) :: istat
        !
        line = ''; indent = -1; eof = .false.
        !
        items_loop: do
            if(self%has_buffered_line) then
                raw_line = self%buffer
                self%has_buffered_line = .false.
            else
                read(self%unit,'(A)', iostat=istat) raw_line
                if (istat.ne.0) then
                    eof = .true.; return
                endif
            endif
            if (is_valid_line(raw_line)) exit
        enddo items_loop
        !
        indent = len_trim(raw_line) - len_trim(adjustl(raw_line))
        line = trim(raw_line)
    end subroutine parser_read_line

    !
    !>>>1 Methods: next
    !

    subroutine parser_next(self, key, raw_val, dtype, indent, eof)
        class(yaml_parser_t), intent(inout) :: self
        character(:), allocatable, intent(out) :: key, raw_val
        integer(ip), intent(out) :: dtype
        integer(ip), intent(out) :: indent
        logical, intent(out) :: eof
        !
        character(:), allocatable :: line
        integer(ip) :: colon_pos
        !
        key = ""; raw_val = ""; eof = .false.
        !
        call self%read_line(line, indent, eof)
        if(eof) return
        !
        colon_pos = index(line, ':')
        if(colon_pos.le.0) return !LAM here?
        !
        key = trim(adjustl(line(1:colon_pos-1)))
        raw_val = trim(adjustl(line(colon_pos+1:)))
        !
        !Potential Block-style array (- item)
        if (len_trim(raw_val) == 0) then
            call self%block2flow(raw_val)
        endif
        !
        dtype = get_dtype(raw_val)
    end subroutine parser_next

    !
    !>>>1 Methods: block2flow
    !

    subroutine parser_array_block2flow(self, raw_val)
        class(yaml_parser_t), intent(inout) :: self
        character(:), allocatable, intent(out) :: raw_val
        !
        integer(ip) :: dash_pos
        integer(ip) :: indent, new_indent
        logical :: eof
        character(:), allocatable :: line
        !
        raw_val = ""; indent = -1
        !
        read_loop: do
            call self%read_line(line,new_indent,eof)
            if(eof) exit read_loop
            if(indent<0) indent = new_indent
            !
            dash_pos = index(line, '-')
            if (dash_pos>0 .and. indent==new_indent) then
                ! Append value from "- value"
                if (len(raw_val) > 0) raw_val = raw_val // self%delimiter
                raw_val = raw_val // trim(adjustl(line(dash_pos+1:)))
            else
                self%buffer = line
                self%has_buffered_line = .true.
                exit read_loop
            endif
        enddo read_loop
        !
        ! Convert to flow-style array
        ! 
        if(len_trim(raw_val)>0) then
            raw_val = '[' // raw_val // ']'
        endif
        !
    end subroutine parser_array_block2flow

    !
    !>>>1 Auxiliary routines
    !

    function is_valid_line(line) result(res)
        character(len=*), intent(in) :: line
        logical :: res
        character(len=:), allocatable :: trimmed
        trimmed = trim(adjustl(line))
        res = (len_trim(trimmed)>0 .and. trimmed(1:1).ne.'#')
    end function is_valid_line

    function get_dtype(str) result(dtype)
        character(*), intent(in) :: str
        integer(ip) :: dtype
        integer(ip) :: istat
        integer(ip) :: i_tmp
        real(rp) :: r_tmp
        character(len=:), allocatable :: trimmed

        trimmed = to_lowercase(trim(adjustl(str)))

        ! Set default
        dtype = Y_STR

        ! Check Null
        if (len_trim(trimmed) == 0) then
            dtype = Y_NULL; return
        end if

        ! Check Boolean/NULL
        select case(trimmed)
        case("~","null")
            dtype = Y_NULL; return
        case("true","yes")
            dtype = Y_BOOL; return
        case("false","no")
            dtype = Y_BOOL; return
        end select

        ! Check Array
        if (trimmed(1:1) == '[' .and. &
            trimmed(len(trimmed):) == ']') then
            dtype = Y_ARRAY; return
        endif

        ! Check Integer
        read(str, *, iostat=istat) i_tmp
        if (istat == 0 .and. index(str, '.') == 0) then
            dtype = Y_INT; return
        end if

        ! Check Real
        read(str, *, iostat=istat) r_tmp
        if (istat == 0) then
            dtype = Y_REAL; return
        end if

    end function

    function is_array(str, delimiter)
        character(*), intent(in) :: str
        character(1), intent(in) :: delimiter
        !
        character(:), allocatable :: inner
        !
        istart = index(str,'[')
        iend   = index(str,']')
        if(istart==0 .or. iend==0) return
        if(istart>iend) return
        !
        inner = str(istart:iend)
        !
        ! Count elements
        n = 1
        do i = 1, len(inner)
            if (inner(i:i) == ',') n = n + 1
        end do
        !
        allocate(dtypes(n))
    end function is_array

    function to_lowercase(str) result(res)
        character(*), intent(in) :: str
        character(len=:), allocatable :: res
        integer(ip) :: i, code
        res = str
        do i = 1, len(str)
            code = iachar(str(i:i))
            if (code >= 65 .and. code <= 90) then
                res(i:i) = achar(code + 32)
            end if
        end do
    end function

end module YAML_Parser

! vim: set foldmethod=marker foldmarker=>>>,<<< foldlevel=0 :
