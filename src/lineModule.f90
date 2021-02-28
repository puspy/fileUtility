module fu$mLine
    
    use data_type_manager
    
    implicit none
    
    private
    
    public :: fu$line_
    
    ! Parameters
    character(len=1), parameter :: defaultSep = " "
    
    type fu$line_
        
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
        procedure :: getErrorMessage   => getErrorMessage_line_
        
        !Private type bound procedures.
        procedure :: saveCharsUntilSep => saveCharsUntilSep_line_
    
    end type
    
    ! Constructor interface
    interface fu$new
        module procedure :: newLineByDefault_mLine
        module procedure :: newLineByString_mLine    
    end interface
    
    ! Destructor
    interface fu$delete
        module procedure :: deleteLine_mLine
    end interface
    
    contains
    
    !Module procedure.
    !Public module procedures.
    
    ! Constructor
    subroutine newLineByDefault_mLine(line) 
        type(fu$line_), intent(out) :: line
        line%length = 0
        line%posi = 0
        line%err_stat = 0
        line%err_msg = ""
        return
    end subroutine
    
    subroutine newLineByString_mLine(line, string, success)
        type(fu$line_), intent(out)       :: line
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
        type(fu$line_), intent(inout)     :: line
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
    
    subroutine getNextWordDefaultSeparator_line_(this)
        class(fu$line_), intent(in) :: this
        return
    end subroutine
    
    subroutine getNextWordBySeparator_line_(this, sep)
        class(fu$line_), intent(in)  :: this
        character(len=*), intent(in) :: sep
        return
    end subroutine
    
    function getErrorMessage_line_(this)
        class(fu$line_), intent(in) :: this
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
        class(fu$line_), intent(in) :: this
        return
    end subroutine
    
end module