!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE GETDIRQQ
!DEC$ DEFINE CV=7
!DEC$ REAL:4
!DEC$ IF(CV==6)
    USE DFWIN
    USE DFLIB, ONLY : SPLITPATHQQ, SYSTEMQQ, GETDRIVEDIRQQ
    USE DFLIB, ONLY : QWIN$FRAMEWINDOW, GETHWNDQQ, NARGS, GETARG
!DEC$ ELSEIF(CV==7)
    USE IFWIN
    USE IFPORT, ONLY : SPLITPATHQQ, SYSTEMQQ, GETDRIVEDIRQQ
    USE IFQWIN, ONLY : QWIN$FRAMEWINDOW, GETHWNDQQ
!DEC$ ENDIF

IMPLICIT NONE

CHARACTER(600):: FileName
CHARACTER(40) :: MyName

PRIVATE
PUBLIC :: get_dir_and_catch

CONTAINS

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE get_dir_and_catch(fn)
INTEGER(4)                :: length, IERROR, iret, i, idum, na, j, k
LOGICAL(KIND=4)           :: STATUS, bRET
CHARACTER(3)              :: drive
CHARACTER(*), INTENT(OUT) :: fn
CHARACTER(256)            :: path, ext, ALLFILTERS
CHARACTER(60)             :: DLGTITLE, code
CHARACTER(256)            :: message, dum1, dum2
LOGICAL                   :: ex
TYPE(T_OPENFILENAME) :: opn      

na = NARGS()
code = '-a'  !treat as default filname

message=''
SELECT CASE(code)
CASE ('-a') !use popup
    ALLFILTERS            = 'All files(*.*)'//CHAR(0)//'*.*'//CHAR(0)//CHAR(0)
    DLGTITLE              = 'Select a SHETRAN library (xml) file'C
    opn%lStructSize       = SIZEOF(Opn)
    opn%HWNDOWNER         = GETHWNDQQ(QWIN$FRAMEWINDOW)
    opn%HINSTANCE         = NULL
    opn%LPSTRFILTER       = LOC(ALLFILTERS)
    opn%LPSTRCUSTOMFILTER = NULL
    opn%NMAXCUSTFILTER    = NULL
    opn%NFILTERINDEX      = 1
    opn%LPSTRFILE         = LOC(FileName) 
    opn%NMAXFILE          = LEN(FileName) 
    opn%LPSTRFILETITLE    = NULL 
    opn%NMAXFILETITLE     = NULL
    opn%LPSTRINITIALDIR   = NULL
    opn%LPSTRTITLE        = LOC(DLGTITLE)
    opn%FLAGS             = NULL 
    opn%NFILEOFFSET       = NULL
    opn%NFILEEXTENSION    = NULL
    opn%LPSTRDEFEXT       = NULL
    opn%LCUSTDATA         = NULL
    opn%LPFNHOOK          = NULL
    opn%LPTEMPLATENAME    = NULL 
    bRET                  = GETOPENFILENAME(opn)
    CALL COMDLGER(IERROR)
CASE DEFAULT
    message = 'Unrecognised command line argument ' // TRIM(code) // ' Recognise only -a, -c and -f'
END SELECT
IF(message/='') GOTO 1000

INQUIRE(FILE=filename, EXIST=ex)
IF(.NOT.ex) THEN
    IF(LEN_TRIM(filename)==0) THEN
        message = 'Missing filename   Use -f filneme'
    ELSE
        message = 'Cannot find rundata file '//TRIM(filename)
    ENDIF
    GOTO 1000
ENDIF
fn = TRIM(Filename)

RETURN
1000 PRINT*, message
STOP
END SUBROUTINE get_dir_and_catch

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE comdlger(IRET)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!* ERRORS HANDLING FOR COMMON DIALOGS
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  INTEGER(KIND=4):: IRET
  CHARACTER(30)  :: MSG1
  CHARACTER(210) :: MSG2

  IRET = COMMDLGEXTENDEDERROR()
  MSG1 = 'FILE OPEN DIALOG FAILURE'C
  SELECT CASE(IRET)
    CASE (CDERR_FINDRESFAILURE)
        MSG2 = 'The common dialog box procedure failed to find a specified resource.'C
    CASE (CDERR_INITIALIZATION)
        MSG2 = 'The common dialog box procedure failed during initialization. &
        This error often occurs when insufficient memory is available.'C
    CASE (CDERR_LOCKRESFAILURE)
        MSG2 = 'The common dialog box procedure failed to lock a specified resource.'C
    CASE (CDERR_LOADRESFAILURE)
        MSG2 = 'The common dialog box procedure failed to load a specified resource.'C
    CASE (CDERR_LOADSTRFAILURE)
        MSG2 = 'The common dialog box procedure failed to load a specified string.'C
    CASE (CDERR_MEMALLOCFAILURE)
        MSG2 = 'The common dialog box procedure was unable to allocate memory for &
        internal structures.'C
    CASE (CDERR_MEMLOCKFAILURE)
        MSG2 = 'The common dialog box procedure was unable to lock the memory associated &
        with a handle.'C
    CASE (CDERR_NOHINSTANCE)
        MSG2 = 'The ENABLETEMPLATE flag was specified in the Flags member of a structure &
        for the corresponding common dialog box, but the application failed to provide a &
        corresponding instance handle.'C
    CASE (CDERR_NOHOOK)
        MSG2 = 'The ENABLEHOOK flag was specified in the Flags member of a structure for &
        the corresponding common dialog box, but the application failed to provide a &
        pointer to a corresponding hook function'C
    CASE (CDERR_NOTEMPLATE)
        MSG2 = 'The ENABLETEMPLATE flag was specified in the Flags member of a structure &
        for the corresponding common dialog box, but the application failed to provide a &
        corresponding template.'C
    CASE (CDERR_STRUCTSIZE)
        MSG2 = 'The lStructSize member of a structure for the corresponding common dialog &
        box is invalid.'C
    CASE (FNERR_BUFFERTOOSMALL)
        MSG2 = 'The buffer for a filename is too small. (This buffer is pointed to by the &
        lpstrFile member of the structure for a common dialog box.)'C
    CASE (FNERR_INVALIDFILENAME)
        MSG2 = 'A filename is invalid.'C
    CASE (FNERR_SUBCLASSFAILURE)
        MSG2 = 'An attempt to subclass a list box failed because insufficient memory was &
        available.'C
      CASE DEFAULT
        MSG2 = 'Unknown error number'C
  END SELECT
  IF(IRET /= 0)THEN
!    IRET = MESSAGEBOXQQ(MSG2, MSG1, MB$ICONEXCLAMATION .OR. MB$OK)  !compiler conflicts
    PRINT*, MSG1
    PRINT*, MSG2
    STOP
  ENDIF
END SUBROUTINE comdlger
END MODULE GETDIRQQ