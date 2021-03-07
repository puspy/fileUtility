module mVersion
    
    implicit none
    
    private
    
    public :: version_
    
    type version_
        character(len=2) :: major = "1"
        character(len=2) :: minor = "0"
        character(len=3) :: build = "2"
    contains
        procedure :: setVersion => setVersion_version_
        procedure :: getVersion => getVersion_version_
    end type
    
    contains
    
    subroutine setVersion_version_(this, major, minor, build)
        class(version_), intent(out) :: this
        character(len=2), intent(in)   :: major,&
                                          minor
        character(len=3), intent(in)   :: build
        this%major = major
        this%minor = minor
        this%build = build
        return
    end subroutine
    
    subroutine getVersion_version_(this, version)
        class(version_), intent(in) :: this
        character(len=9), intent(out)  :: version
        version = trim(adjustl(this%major))//"."//trim(adjustl(this%minor))//"."//trim(adjustl(this%build))
        return
    end subroutine
    
end module