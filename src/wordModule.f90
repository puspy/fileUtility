module mWord
    
    use data_type_manager
    
    implicit none
    
    private
    
    public :: word_
    public :: newWordByDefault_mWord, newWordByString_mWord, delete_mWord
    
    type word_
        
        private
        character(len=:), allocatable :: string
        integer(kind=itype)           :: length
        integer(kind=itype)           :: err_stat
        character(len=chtext)         :: err_msg
        
    contains
    
        procedure :: getLength       => getLength_word_
        procedure :: isEqualToString_word_
        procedure :: isEqualToWord_word_
        generic   :: isEqual         => isEqualToString_word_,&
                                        isEqualToWord_word_
        procedure :: upcase          => upcase_word_
        procedure :: downcase        => downcase_word_
        procedure :: count           => count_word_
        procedure :: writeOnScreen_word_
        procedure :: writeOnUnit_word_
        generic   :: write           => writeOnScreen_word_,&
                                        writeOnUnit_word_        
        procedure :: getErrorMessage => getErrorMessage_word_
        
    end type
    
    contains
    
    !Module procedures
    !Public module procedures
    
    !Constructors
    subroutine newWordByDefault_mWord(word)
        type(word_), intent(out) :: word
        word%length = 0
        word%err_stat = 0
        word%err_msg = ""
        return
    end subroutine
    
    subroutine newWordByString_mWord(word, string, success)
        type(word_), intent(out)          :: word
        character(len=*), intent(in)      :: string
        logical(kind=lgtype), intent(out) :: success
        
        success = .false.
        word%length = len(string)
        
        if ( allocated( word%string ) ) then
            deallocate( word%string, stat=word%err_stat, errmsg=word%err_msg )
            if ( word%err_stat /= 0 ) return
        endif
        
        allocate( character(len=len( trim(adjustl(string)) )) :: word%string, stat=word%err_stat, errmsg=word%err_msg )
        if ( word%err_stat /= 0 ) return
        
        word%string = trim(adjustl(string))
        success = .true.
        
        return
    end subroutine
    
    !Destructors
    
    subroutine delete_mWord(word, success)
        type(word_), intent(inout)        :: word
        logical(kind=lgtype), intent(out) :: success
        
        success = .false.
        if ( allocated( word%string ) ) then
            deallocate( word%string, stat=word%err_stat, errmsg=word%err_msg )
            if ( word%err_stat /= 0 ) return
        endif
        word%length = 0
        word%err_stat = 0
        word%err_msg = ""
        success = .true.
        return
    end subroutine
    
    !Private module procedures
    
    !Type bound procedures
    !Public type bound procedures
    
    function getLength_word_(this)
        integer(kind=itype)      :: getLength_word_
        class(word_), intent(in) :: this
        getLength_word_ = this%length
        return
    end function
    
    function isEqualToString_word_(this, string)
        logical(kind=lgtype)         :: isEqualToString_word_ 
        class(word_), intent(in)     :: this
        character(len=*), intent(in) :: string
        isEqualToString_word_ = this%string == trim(adjustl(string))
        return
    end function
    
    function isEqualToWord_word_(this, word)
        logical(kind=lgtype)     :: isEqualToWord_word_
        class(word_), intent(in) :: this
        type(word_), intent(in)  :: word
        isEqualToWord_word_ = .false.
        if ( this%length /= word%length ) return
        if ( this%string /= word%string ) return
        isEqualToWord_word_ = .true.
        return
    end function
    
    subroutine upcase_word_(this)
        class(word_), intent(inout) :: this
        if ( this%length == 0 ) return
        call upcase(this%string)
        return
    end subroutine
    
    subroutine downcase_word_(this)
        class(word_), intent(inout) :: this
        if ( this%length == 0 ) return
        call downcase(this%string)
        return
    end subroutine
    
    function count_word_(this)
        integer(kind=itype)      :: count_word_
        class(word_), intent(in) :: this
        count_word_ = len(trim(adjustl(this%string)))
        return
    end function
    
    subroutine writeOnScreen_word_(this, success)
        class(word_), intent(inout)       :: this
        logical(kind=lgtype), intent(out) :: success
        success = .false.
        write(*,'(a)', iostat=this%err_stat, iomsg=this%err_msg) this%string
        if ( this%err_stat /=  0 ) return
        success = .true.
        return
    end subroutine
    
    subroutine writeOnUnit_word_(this, funit, success)
        class(word_), intent(inout)       :: this
        integer(kind=itype), intent(in)   :: funit
        logical(kind=lgtype), intent(out) :: success
        success = .false.
        write(funit,'(a)', iostat=this%err_stat, iomsg=this%err_msg) this%string
        if ( this%err_stat /= 0 ) return
        success = .true.
        return
    end subroutine    
    
    function getErrorMessage_word_(this)
        class(word_), intent(in)                          :: this
        character(len=len( trim(adjustl(this%err_msg)) )) :: getErrorMessage_word_
        if ( this%err_stat == 0 ) then
            getErrorMessage_word_ = " "
        else
            getErrorMessage_word_ = trim(adjustl(this%err_msg))
        endif
        return
    end function  
    
    !Private type bound procedures
    
end module