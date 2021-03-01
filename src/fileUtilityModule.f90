module mFileUtility
    
    use mVersion
    
    use mWord
    
    use mLine
    
    use mFile
    
    implicit none
    
    private
    
    public :: fu$version, fu$word_, fu$line_, fu$fileHandler_
    public :: fu$new, fu$delete
    
    type, extends(version_) :: fu$version_
    end type
    
    type, extends(word_) :: fu$word_
    end type
    
    type, extends(line_) :: fu$line_
    end type
    
    type, extends(fileHandler_) :: fu$fileHandler_
    end type
    
    interface fu$new
        module procedure :: newWordByDefault_mWord
        module procedure :: newWordByString_mWord
        module procedure :: newLineByDefault_mLine
        module procedure :: newLineByString_mLine
    end interface
    
    interface fu$delete
        module procedure :: delete_mWord
        module procedure :: deleteLine_mLine
    end interface
    
    type(fu$version_) :: fu$version
    
end module