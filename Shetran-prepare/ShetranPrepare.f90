    program ShetranPrepare
        use PrepareMod
        implicit none

        character*200 xmlfilename,xmlfilefull
        integer(2) n1
        integer istatus
        integer, parameter :: XmlFileNumber = 10



        write(*,*)
        write(*,*), 'Shetran Prepare'
        write(*,*), '***************'
        write(*,*) 
        write(*,*), 'This executable reads an xml file and the corresponding map file (ASC format)'  
        write(*,*), 'and produces the Shetran input files'    
        write(*,*) 


      n1=1
      CALL GETARG(n1,xmlfilename)

 !      xmlfilename = "C:\Users\steve\Documents\shetran-prepare-v2\Shetran-prepare\examples-test\foston100m-snow\Foston_Beck_at_Foston_MillLibraryFile_snow.xml"
        
        xmlfilefull = trim(xmlfilename)
        open(XmlFileNumber,FILE=xmlfilefull,iostat=istatus,status='old')
        if (istatus/=0) then
             write (*,*) 'Error openinig file ',xmlfilefull
            close(10)
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
        endif
        close (XmlFileNumber)

        call PrepareInputFiles(xmlfilefull)
    
        write(*,*) 
        write(*,*), 'This executable has successfully compelted'    

    
    
        stop

    
    end program ShetranPrepare
