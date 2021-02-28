module fu$mWord
    
    use data_type_manager
    
    implicit none
    
    type fu$word_
        character(len=:), allocatable :: string
        integer(kind=itype)           :: length
        integer(kind=itype)           :: err_stat
        character(len=chtext)         :: err_msg
    end type
    
    interface fu$new
        module procedure :: newWordByDefault_mWord
        module procedure :: newWordByString_mWord
    end interface
    
    interface fu$delete
        module procedure :: delete_mWord
    end interface
    
    contains
    
    !Module procedures
    !Public module procedures
    
    !Constructors
    subroutine newWordByDefault_mWord(word)
        type(fu$word_), intent(out) :: word
        word%length = 0
        word%err_stat = 0
        word%err_msg = ""
        return
    end subroutine
    
    subroutine newWordByString_mWord(word, string, success)
        type(fu$word_), intent(out)       :: word
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
        type(fu$word_), intent(inout)     :: word
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
    
    !Private type bound procedures
    
end module