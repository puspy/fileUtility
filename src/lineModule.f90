module mLine
    
    use data_type_manager
    
    use mWord, only: word_, newByDefault => newWordByDefault_mWord, newByString => newWordByString_mWord
    
    implicit none
    
    private
    
    public :: line_
    public :: newLineByDefault_mLine, newLineByString_mLine, deleteLine_mLine
    
    ! Parameters
    character(len=1), parameter :: defaultSep = " "
    
    type line_
        
        private
        character(len=:), allocatable :: string   !String
        integer(kind=itype)           :: length,& !Length of the string
                                         posi     !Position on string
        integer(kind=itype)           :: err_stat !Error status
        character(len=chtext)         :: err_msg  !Error message
        
    contains
        
        !Public type bound procedures.
        procedure :: getNextWordDefaultSeparator_line_
        procedure :: getNextWordBySeparator_line_
        generic   :: getNextWord       => getNextWordDefaultSeparator_line_,&
                                          getNextWordBySeparator_line_
        procedure :: isEqualToString_line_
        procedure :: isEqualToWord_line_
        generic   :: isEqual           => isEqualToString_line_,&
                                          isEqualToWord_line_
        procedure :: upcase            => upcase_line_
        procedure :: downcase          => downcase_line_
        procedure :: count             => count_line_
        procedure :: writeLineOnScreen_line_
        procedure :: writeLineOnUnit_line_
        generic   :: write             => writeLineOnScreen_line_,&
                                          writeLineOnUnit_line_
        procedure :: getErrorStatus    => getErrorStatus_line_
        procedure :: getErrorMessage   => getErrorMessage_line_
        
        !Private type bound procedures.
        procedure :: saveCharsUntilSep => saveCharsUntilSep_line_
    
    end type
    
    contains
    
    !Module procedure.
    !Public module procedures.
    
    ! Constructor
    subroutine newLineByDefault_mLine(line) 
        type(line_), intent(out) :: line
        line%length = 0
        line%posi = 0
        line%err_stat = 0
        line%err_msg = ""
        return
    end subroutine
    
    subroutine newLineByString_mLine(line, string, success)
        type(line_), intent(out)          :: line
        character(len=*), intent(in)      :: string
        logical(kind=lgtype), intent(out) :: success
        success = .false.
        line%length = len( trim(adjustl(string)) )
        line%posi = 0
        if ( allocated( line%string ) ) then
            deallocate( line%string, stat = line%err_stat, errmsg = line%err_msg )
            if ( line%err_stat /= 0 ) return
        endif
        allocate( character(len=line%length) :: line%string, stat = line%err_stat, errmsg = line%err_msg )
        
        if ( line%err_stat /= 0 ) return
        
        line%string = trim(adjustl(string))
        success = .true.
        return
    end subroutine
    
    !Destructor
    subroutine deleteLine_mLine(line, success)
        type(line_), intent(inout)        :: line
        logical(kind=lgtype), intent(out) :: success
        
        success = .false.
        if ( allocated( line%string ) ) deallocate( line%string, stat=line%err_stat, errmsg=line%err_msg )
        if ( line%err_stat /= 0 ) return
        line%length = 0
        line%posi = 0
        line%err_stat = 0
        line%err_msg = ""
        success = .true.
        return
    end subroutine
    
    !Private module procedures.
    
    !Type bound procedures
    !Public type bound procedures.
    
    function getNextWordDefaultSeparator_line_(this, success)
        type(word_)                       :: getNextWordDefaultSeparator_line_
        class(line_), intent(inout)       :: this
        logical(kind=lgtype), intent(out) :: success
        getNextWordDefaultSeparator_line_ = this%getNextWordBySeparator_line_(" ", success)
        return
    end function
    
    function getNextWordBySeparator_line_(this, sep, success)
        type(word_)                       :: getNextWordBySeparator_line_
        class(line_), intent(inout)       :: this
        character(len=1), intent(in)      :: sep
        logical(kind=lgtype), intent(out) :: success
        character(len=this%length)        :: string
        integer(kind=itype)               :: ipos
        
        if ( this%posi == this%length) then
            call newByDefault(getNextWordBySeparator_line_)
            return
        endif
        do ipos=this%posi+1, this%length
            if ( this%string(ipos:ipos) == sep ) exit
            string(ipos:ipos) = this%string(ipos:ipos)
        enddo
        call newByString(getNextWordBySeparator_line_, string(1:ipos-1), success)
        this%posi=ipos
        return
    end function
    
    function isEqualToString_line_(this, string)
        logical(kind=lgtype)         :: isEqualToString_line_ 
        class(line_), intent(in)     :: this
        character(len=*), intent(in) :: string
        isEqualToString_line_ = this%string == trim(adjustl(string))
        return
    end function
    
    function isEqualToWord_line_(this, line)
        logical(kind=lgtype)     :: isEqualToWord_line_
        class(line_), intent(in) :: this
        type(line_), intent(in)  :: line
        isEqualToWord_line_ = .false.
        if ( this%length /= line%length ) return
        if ( this%string /= line%string ) return
        isEqualToWord_line_ = .true.
        return
    end function
    
    subroutine upcase_line_(this)
        class(line_), intent(inout) :: this
        if ( this%length == 0 ) return
        call upcase(this%string)
        return
    end subroutine
    
    subroutine downcase_line_(this)
        class(line_), intent(inout) :: this
        if ( this%length == 0 ) return
        call downcase(this%string)
        return
    end subroutine
    
    function count_line_(this)
        integer(kind=itype)      :: count_line_
        class(line_), intent(in) :: this
        count_line_ = len(trim(adjustl(this%string)))
        return
    end function
    
    subroutine writeLineOnScreen_line_(this, success)
        class(line_), intent(inout)       :: this
        logical(kind=lgtype), intent(out) :: success
        success = .false.
        write(*,'(a)', iostat=this%err_stat, iomsg=this%err_msg) this%string
        if ( this%err_stat /= 0 ) return
        success = .true.
        return
    end subroutine
    
    subroutine writeLineOnUnit_line_(this, unit, success)
        class(line_), intent(inout)       :: this
        integer(kind=itype), intent(in)   :: unit
        logical(kind=lgtype), intent(out) :: success
        success = .false.
        write(unit,'(a)', iostat=this%err_stat, iomsg=this%err_msg) this%string
        if ( this%err_stat /= 0 ) return
        success = .true.
        return
    end subroutine    
    
    function getErrorStatus_line_(this)
        integer(kind=itype)      :: getErrorStatus_line_
        class(line_), intent(in) :: this
        getErrorStatus_line_ = this%err_stat
        return
    end function
    
    function getErrorMessage_line_(this)
        class(line_), intent(in)                          :: this
        character(len=len( trim(adjustl(this%err_msg)) )) :: getErrorMessage_line_
        if ( this%err_stat == 0 ) then
            getErrorMessage_line_ = " "
        else
            getErrorMessage_line_ = trim(adjustl(this%err_msg))
        endif
        return
    end function
    
    !Private type bound procedures.
    
    subroutine saveCharsUntilSep_line_(this)
        class(line_), intent(in) :: this
        return
    end subroutine
    
end module