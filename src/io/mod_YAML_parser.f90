module YAML_Parser
    use KindType, only: ip, rp, size_t
    use YAML_Value, only: yaml_value_t
    implicit none
    !
    private
    PUBLIC :: yaml_parser_t
    !
    ! Value types enumeration
    integer(ip), parameter :: Y_NULL  = 0
    integer(ip), parameter :: Y_BOOL  = 1
    integer(ip), parameter :: Y_INT   = 2
    integer(ip), parameter :: Y_REAL  = 3
    integer(ip), parameter :: Y_ARRAY = 4
    integer(ip), parameter :: Y_STR   = 5
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
      procedure :: open      => parser_open
      procedure :: close     => parser_close
      procedure :: next      => parser_next
      procedure :: get_value => parser_get_value
      procedure, private :: read_line  => parser_read_line
      procedure, private :: block2flow => parser_array_block2flow
      procedure, private, nopass :: parser_parse_nulls
      procedure, private, nopass :: parser_parse_bools
      procedure, private, nopass :: parser_parse_integers
      procedure, private, nopass :: parser_parse_reals
      generic :: parse_values => parser_parse_nulls, parser_parse_bools, parser_parse_integers, parser_parse_reals
      final :: destroy_parser
    end type yaml_parser_t

contains

    !
    !>>>1 Destructor
    !

    subroutine destroy_parser(self)
        type(yaml_parser_t), intent(inout) :: self
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

    subroutine parser_next(self, key, val, indent, eof)
        class(yaml_parser_t), intent(inout) :: self
        character(:), allocatable, intent(out) :: key
        class(yaml_value_t), allocatable, intent(out) :: val
        integer(ip), intent(out) :: indent
        logical, intent(out) :: eof
        !
        character(:), allocatable :: line, raw_val
        integer(ip) :: colon_pos
        !
        key = ""; eof = .false.
        !
        call self%read_line(line, indent, eof)
        if(eof) return
        !
        colon_pos = index(line, ':')
        if(colon_pos.le.0) return !LAM here?
        !
        key     = trim(adjustl(line(1:colon_pos-1)))
        raw_val = trim(adjustl(line(colon_pos+1:)))
        !
        !Potential Block-style array (- item)
        if (len_trim(raw_val) == 0) then
            call self%block2flow(raw_val)
        endif
        !
        call self%get_value(raw_val, val)
        !        
    end subroutine parser_next

    !
    !>>>1 Methods: get_value
    !

    subroutine parser_get_value(self,str,val)
        class(yaml_parser_t), intent(inout) :: self
        character(*), intent(in) :: str
        class(yaml_value_t), allocatable, intent(out) :: val
        !
        character(len=:), allocatable :: raw_values(:)
        integer(ip) :: istat, n
        logical :: is_array
        !
        raw_values = split_array(str,self%delimiter)
        n = size(raw_values)

        is_array = .true.
        if(n==0) then
            is_array = .false.
            n = 1
            deallocate(raw_values)
            allocate(character(len=len(str)) :: raw_values(n))
            raw_values(1) = str
        endif

        ! Try nulls
        call self%parse_values(n,raw_values,istat)
        if(istat==0) then
            if(is_array) then
                val = yaml_value_t(size_t(n))
            else
                val = yaml_value_t()
            endif
            return
        endif

        ! Try boolean
        block
            logical :: b_tmp(n)
            call self%parse_values(n,raw_values,b_tmp,istat)
            if(istat==0) then
                if(is_array) then
                    val = yaml_value_t(size_t(n),b_tmp)
                else
                    val = yaml_value_t(b_tmp(1))
                endif
                return
            endif
        end block

        ! Try integer
        block
            integer(ip) :: i_tmp(n)
            call self%parse_values(n,raw_values,i_tmp,istat)
            if(istat==0) then
                if(is_array) then
                    val = yaml_value_t(size_t(n),i_tmp)
                else
                    val = yaml_value_t(i_tmp(1))
                endif
                return
            endif
        end block

        ! Try real
        block
            real(rp) :: r_tmp(n)
            call self%parse_values(n,raw_values,r_tmp,istat)
            if(istat==0) then
                if(is_array) then
                    val = yaml_value_t(size_t(n),r_tmp)
                else
                    val = yaml_value_t(r_tmp(1))
                endif
                return
            endif
        end block

        ! Default to strings
        if(is_array) then
            val = yaml_value_t(size_t(n),raw_values)
        else
            val = yaml_value_t(str)
        endif

    end subroutine parser_get_value

    !
    !>>>1 Methods: parse_values
    !

    subroutine parser_parse_nulls(n,strings,istat)
        integer(ip),  intent(in) :: n
        character(*), intent(in) :: strings(n)
        integer(ip),  intent(out) :: istat
        !
        integer(ip) :: i
        character(8), allocatable :: str
        !
        istat = 0
        !
        ! Check Null
        do i=1,n
            str = adjustl(strings(i))
            if(len_trim(str)==0) cycle
            select case(to_lowercase(str))
            case("~","null")
                continue
            case default
                istat = 1; return
            end select
        enddo
    end subroutine parser_parse_nulls

    subroutine parser_parse_bools(n,strings,bval,istat)
        integer(ip),  intent(in) :: n
        character(*), intent(in) :: strings(n)
        logical,      intent(out) :: bval(n)
        integer(ip),  intent(out) :: istat
        !
        integer(ip) :: i
        character(8), allocatable :: str
        !
        istat = 0
        !
        ! Check Boolean
        do i=1,n
            str = trim(adjustl(strings(i)))
            select case(to_lowercase(str))
            case("true","yes")
                bval(i) = .true.
            case("false","no")
                bval(i) = .false.
            case default
                istat = 1; return
            end select
        enddo
    end subroutine parser_parse_bools

    subroutine parser_parse_integers(n,strings,ival,istat)
        integer(ip),  intent(in) :: n
        character(*), intent(in) :: strings(n)
        integer(ip),  intent(out) :: ival(n)
        integer(ip),  intent(out) :: istat
        !
        integer(ip) :: i
        !
        ! Check Integer
        do i=1,n
            associate(str => strings(i))
            read(str, *, iostat=istat) ival(i)
            if(index(str,'.').ne.0) istat = 1
            if(istat.ne.0) return
            end associate
        enddo
    end subroutine parser_parse_integers

    subroutine parser_parse_reals(n,strings,rval,istat)
        integer(ip),  intent(in) :: n
        character(*), intent(in) :: strings(n)
        real(rp),    intent(out) :: rval(n)
        integer(ip), intent(out) :: istat
        !
        integer(ip) :: i
        !
        ! Check Real
        do i=1,n
            associate(str => strings(i))
            read(str, *, iostat=istat) rval(i)
            if(istat.ne.0) return
            end associate
        enddo
    end subroutine parser_parse_reals

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
    !>>>1 Auxiliary routines: is_valid_line
    !

    function is_valid_line(line) result(res)
        character(len=*), intent(in) :: line
        logical :: res
        character(len=:), allocatable :: trimmed
        trimmed = trim(adjustl(line))
        res = (len_trim(trimmed)>0 .and. trimmed(1:1).ne.'#')
    end function is_valid_line

!    function get_dtype(str) result(dtype)
!        character(*), intent(in) :: str
!        integer(ip) :: dtype
!        integer(ip) :: istat
!        integer(ip) :: i_tmp
!        real(rp) :: r_tmp
!        character(len=:), allocatable :: trimmed
!
!        trimmed = to_lowercase(trim(adjustl(str)))
!
!        ! Set default
!        dtype = Y_STR
!
!        ! Check Null
!        if (len_trim(trimmed) == 0) then
!            dtype = Y_NULL; return
!        end if
!
!        ! Check Boolean/NULL
!        select case(trimmed)
!        case("~","null")
!            dtype = Y_NULL; return
!        case("true","yes")
!            dtype = Y_BOOL; return
!        case("false","no")
!            dtype = Y_BOOL; return
!        end select
!
!        ! Check Array
!        if (trimmed(1:1) == '[' .and. &
!            trimmed(len(trimmed):) == ']') then
!            dtype = Y_ARRAY; return
!        endif
!
!        ! Check Integer
!        read(str, *, iostat=istat) i_tmp
!        if (istat == 0 .and. index(str, '.') == 0) then
!            dtype = Y_INT; return
!        end if
!
!        ! Check Real
!        read(str, *, iostat=istat) r_tmp
!        if (istat == 0) then
!            dtype = Y_REAL; return
!        end if
!
!    end function get_dtype

    !
    !>>>1 Auxiliary routines: split_array
    !
    
    function split_array(str, delimiter) result(raw_values)
        character(*), intent(in) :: str
        character(*), intent(in) :: delimiter
        character(:), allocatable :: raw_values(:)
        !
        integer(ip) :: i, is, ie, count, idel, maxlen
        character(:), allocatable :: input_str
        !
        allocate(character(len=0) :: raw_values(0))
        !
        ! Check array datatype
        is = index(str,'[')
        ie = index(str,']')
        if(is==0 .or. ie==0) return
        if(is>ie) return
        !
        input_str = str(is+1:ie-1)
        maxlen = 1
        !
        ! Count elements
        count = 1
        is = 1
        idel = index(input_str,delimiter)
        do while(idel>0)
            count = count + 1
            is = is + idel + len(delimiter) - 1
            idel = index(input_str(is:),delimiter)
            maxlen = max(maxlen,idel-1)
        end do
        !
        ! Returns split string
        deallocate(raw_values)
        allocate(character(len=maxlen) :: raw_values(count))
        !
        is = 1
        do i = 1, count-1
          idel = index(input_str(is:), delimiter)
          ie = is + idel - 1
          raw_values(i) = input_str(is:ie-1)
          is = ie + len(delimiter)
        enddo
        raw_values(count) = input_str(is:)
    end function split_array

    !
    !>>>1 Auxiliary routines: to_lowercase
    !

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
