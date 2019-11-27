PROGRAM harness_shetran
USE IFPORT
USE GETDIRQQ, ONLY : GET_DIR_AND_CATCH
USE DFLIB, ONLY : SPLITPATHQQ      
use xmlparse
IMPLICIT NONE

INTEGER(4)                :: res,i,length
CHARACTER(128), PARAMETER :: exename1='shetran-prepare-2.2.9.exe'
CHARACTER(128), PARAMETER :: exename2='sv4.4.5x64.exe'
CHARACTER(512)            :: filnam, exedir
CHARACTER(1024)           :: c
LOGICAL                   :: Lres
CHARACTER(3)              ::drive
CHARACTER(256)            ::path, ext, basedir
integer*4                 ::lengthpath
CHARACTER(100)             ::MyName      

type(XML_PARSE)   :: info
character*300             ::xmlfilefull
character(len=300)                      :: tag
logical                                :: endtag
character(len=300), dimension(1:1,1:1)  :: attribs
integer                                :: no_attribs
character(len=300), dimension(1:1)     :: dummy
character(len=300), dimension(1:1)     :: projectfile
character(len=300), dimension(1:1)     :: catchmentname
character(len=300)                     :: catchmentname2
   integer                                :: no_data

!find directory where executables are
res = GETCWD(exedir)
!open the get_file dialog
CALL GET_DIR_AND_CATCH (filnam)


xmlfilefull = trim(filnam)
open(10,FILE=xmlfilefull,err=9999,status='old')

goto 9998

9999 write (*,*) 'Error openinig file ',xmlfilefull
close(10)
pause
stop 1

9998 close (10)
call xml_open( info, xmlfilefull, .true. )
call xml_options( info, report_details = .false. )
call xml_get( info, tag, endtag, attribs, no_attribs, projectfile, no_data )
if ( xml_error(info) ) then
       stop
endif
call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
if ( xml_error(info) ) then
     stop
endif
call xml_get( info, tag, endtag, attribs, no_attribs, catchmentname, no_data )
if ( xml_error(info) ) then
    stop
endif
call xml_close( info )


Lres = CHANGEDIRQQ (TRIM(exedir))
WRITE(c,'(A)') TRIM(exename1)//' "'//TRIM(filnam)//'"'
!WRITE(c,'(A)') TRIM(exename)//' -f "'//TRIM(filnam)//'"'

!print*, trim(c)

res=system(trim(c))

lengthpath = SPLITPATHQQ(filnam, drive, path, MyName, ext)
basedir= trim(drive)//trim(path)

WRITE(c,'(A)') TRIM(exename2)//' -f "'//trim(basedir)//'rundata_'//trim(catchmentname(1))//'.txt'//'"'
!print*, trim(c)

res=system(trim(c))

pause

END PROGRAM harness_shetran