! Module created by Marc Puigpinos Blazquez on 28/02/2021
! readLine procedure is obtained from the book Modern Fortran in Practice. Author: Arjen Markus.

module mFile
    
    use iso_fortran_env
    
    use data_type_manager
    
    implicit none
    
    private
    
    public :: fileHandler_
    
#ifdef UNIX
    character(len=1), parameter :: sep = "/"
#elsif WIN
    character(len=1), parameter :: sep = "\"
#else
    character(len=1), parameter :: sep = "/"
#endif
    type fileHandler_
        
        integer(kind=itype)           :: unit
        character(len=:), allocatable :: address
        character(len=:), allocatable :: fileName
        integer(kind=itype)           :: err_stat
        character(len=chtext)         :: err_msg
        
    contains
    
        procedure :: openFileByFileName_fileHandler_
        procedure :: openFileByAddressAndFileName_fileHandler_
        generic   :: open            => openFileByFileName_fileHandler_,&
                                        openFileByAddressAndFileName_fileHandler_
        procedure :: close           => closeFile_fileHandler_
        procedure :: getErrorMessage => getErrorMessage_fileHandler_
        procedure :: readLine        => readLine_fileHandler_
        
    end type
    
    contains
    
    subroutine openFileByFileName_fileHandler_(this, fileName, success)
        class(fileHandler_), intent(inout) :: this
        character(len=*), intent(in)       :: fileName
        logical(kind=lgtype), intent(out)  :: success
        
        success = .false.
        
        if ( allocated( this%fileName ) ) then
            deallocate( this%fileName, STAT=this%err_stat, ERRMSG=this%err_msg )
            if ( this%err_stat /= 0 ) return
        endif
        allocate( character(len=len( trim(adjustl(fileName)) )) :: this%fileName, STAT=this%err_stat, ERRMSG=this%err_msg )
        if ( this%err_stat /= 0 ) return
        this%fileName = trim(adjustl(fileName))
        
        open(newunit=this%unit, file=this%fileName, iostat = this%err_stat, iomsg = this%err_msg)
        if ( this%err_stat /= 0 ) return
        
        success = .true.
        
        return
    end subroutine
    
    subroutine openFileByAddressAndFileName_fileHandler_(this, address, fileName, success)
        class(fileHandler_), intent(inout) :: this
        character(len=*), intent(in)       :: address
        character(len=*), intent(in)       :: fileName
        logical(kind=lgtype), intent(out)  :: success 
        
        success = .false.
        
        if ( allocated( this%address ) ) then
            deallocate( this%address, STAT=this%err_stat, ERRMSG=this%err_msg )
            if ( this%err_stat /= 0 ) return
        endif
        allocate( character(len=len( trim(adjustl(address)) )) :: this%address, STAT=this%err_stat, ERRMSG=this%err_msg )
        if ( this%err_stat /= 0 ) return
        this%address = trim(adjustl(address))   
        
        call this%open(this%address//sep//trim(adjustl(fileName)), success)
        
        return
    end subroutine
    
    subroutine closeFile_fileHandler_(this, success)
        class(fileHandler_), intent(inout) :: this
        logical(kind=lgtype), intent(out)  :: success
        
        success = .false.
        close(this%unit, iostat=this%err_stat, iomsg=this%err_msg)
        if ( this%err_stat /= 0 ) return
        success = .true.
        
        return
    end subroutine
    
    function getErrorMessage_fileHandler_(this)
        class(fileHandler_), intent(in)                   :: this
        character(len=len( trim(adjustl(this%err_msg)) )) :: getErrorMessage_fileHandler_
        if ( this%err_stat == 0 ) then
            getErrorMessage_fileHandler_ = ""
        else
            getErrorMessage_fileHandler_ = trim(adjustl(this%err_msg))
        endif
        return
    end function
    
    subroutine readLine_fileHandler_(this, line, success)
        class(fileHandler_), intent(inout)         :: this
        character(len=:), allocatable, intent(out) :: line
        logical(kind=lgtype), intent(out)          :: success
        
        character(len=0)                           :: newline
        
        success = .false.
        call readline_piece_by_piece(newline)
        
        contains
        
        recursive subroutine readline_piece_by_piece(newline)
            character(len=*)  :: newline
            
            character(len=10) :: piece
            integer           :: sz
            
            read(this%unit, '(a)', advance = 'no', size = sz, iostat = this%err_stat, iomsg = this%err_msg) piece
            if ( this%err_stat /= 0 .and. this%err_stat /= iostat_eor ) then
                allocate( character(len=len(newline)) :: line )
                line = newline
                return
            endif
            
            ! Have gotten to the endo fo the line or not?
            if ( sz >= len(piece) ) then
                call readline_piece_by_piece(newline//piece)
            else
                allocate( character(len=len(newline)+sz) :: line )
                line = newline // piece(1:sz)
                success = .true.
            endif
            
        end subroutine
        
    end subroutine
    
end module