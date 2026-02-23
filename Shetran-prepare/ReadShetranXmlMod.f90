module ReadShetranXmlMod
    use XmlParseMod
    implicit none
    integer, parameter :: characterlength = 200
    !real                                  :: cstcap(numbervegtypes),lai(numbervegtypes),rootingdepth(numbervegtypes),aepe(numbervegtypes),stricklerveg(numbervegtypes)
    real, allocatable                     :: cstcap(:),lai(:),rootingdepth(:),aepe(:),stricklerveg(:)
    real, allocatable                     :: soildepth(:)
    real, allocatable                     :: thsat(:),thres(:),ksat(:),vgn(:),vga(:),specstor(:)
    real, allocatable                     :: baseflowboundary(:),baseflowboundary2(:)
    real                                  :: initialpsl,prectmstep,petmstep
    real                                  :: rivergridacc,channeldp,channelmindrop,stricklerriv,stricklerlake
    real                                  :: channelwidthfactor,channelwidthpower,channelbankheight
    real                                  :: maxrainfalltimestep,standardtimestep, increasingtimestep,simulateddischargetimestep
    real                                  :: standardh5output,threedimensionalh5output
    real                                  :: snowddf
    integer, allocatable                  :: soilcats(:),soillayers(:),soilnumbers2(:),bfbcats(:)
    integer                               :: day,month,year,endday,endmonth,endyear
    integer                               :: hour,minute,endhour,endminute
    integer                               :: icountveg,icountsoil,icountsoilcat,icountsoilcatmax,icountsoilcatmax1,icountbfb
    integer, allocatable                  :: extradischarge(:)
    integer                               :: icountdischargepoints
    logical                               :: issnow,Issediment,IsBanks,IsSolute,IsMeteorologicalDataIncludeDate,isspatialpsl,isextradischarge
    character(len=characterlength),allocatable      :: soiltypes(:)
    character(len=characterlength),allocatable      :: vegtypes(:)
    character(len=characterlength), dimension(1:1)    :: tmaxfile,tminfile
    character(len=characterlength), dimension(1:1)    :: precfile,pefile
    character(len=characterlength), dimension(1:1)    :: catchmentname,demmeanname,demminname,maskname,vegname,soilname,lakename,precipname,pename
    

    contains
    
!******************************** subroutine to read xml data ************
    subroutine read_xml_file(xmlfilefull)
!************************************************************************

   IMPLICIT NONE

   type(XML_PARSE)   :: info

   character(len=characterlength)                      :: tag
   logical                                             :: endtag
   character(len=characterlength), dimension(1:1,1:1)  :: attribs
   integer                                            :: no_attribs
   character(len=*), intent(in)                       :: xmlfilefull
   character(len=characterlength), dimension(1:1)     :: initialconditions,xspatialpsl
   character(len=characterlength), dimension(1:1)     :: prectimestep
   character(len=characterlength), dimension(1:1)     :: petimestep
   character(len=characterlength), dimension(1:1)     :: xIsSnow,XIsSediment,xIsBanks,XIsSolute,xIsMeteorologicalDataIncludeDate
   character(len=characterlength), dimension(1:1)     :: xday,xmonth,xyear
   character(len=characterlength), dimension(1:1)     :: xendday,xendmonth,xendyear
   character(len=characterlength), dimension(1:1)     :: xhour,xminute
   character(len=characterlength), dimension(1:1)     :: xendhour,xendminute
   character(len=characterlength), dimension(1:1)     :: rivergridaccum
   character(len=characterlength), dimension(1:1)     :: channeldrop, channelminimumdrop
   character(len=characterlength), dimension(1:1)     :: StricklerChannelCoeff,StricklerLakeCoeff
   character(len=characterlength), dimension(1:1)     :: xchannelwidthfactor,xchannelwidthpower,xchannelbankheight
   character(len=characterlength), dimension(1:1)     :: simulateddischargetimestepa
   character(len=characterlength), dimension(1:1)     :: maxrainfalltimestepa,standardtimestepa, increasingtimestepa
   character(len=characterlength), dimension(1:1)     :: standardh5outputa,threedimensionalh5outputa
   character(len=characterlength), dimension(1:1)     :: snowddfa,extradischargea
   character(len=characterlength), dimension(1:1)     :: baseflowboundarya
   integer                                :: no_data
   integer                                :: icount,soilnumber,numbersoiltypes,icountbfbmax
   character(len=characterlength),dimension(1) :: data
   logical                          :: vegfirst,soilfirst,soilcatfirst,baseflowboundaryfirst

character(len=characterlength) :: token,line
integer :: pos, next, lenline

   

!********catchment name - compulsory ********   
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   catchmentname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="CatchmentName" .and. .not. endtag) then 
          catchmentname = data(1)
          exit
        endif
        
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   enddo

   call xml_close(info)

   

!********DEM Mean - compulsory ********   
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   demmeanname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="DEMMeanFileName" .and. .not. endtag) then 
          demmeanname = data(1)
          exit
        endif
        
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
  
 !********Mask - compulsory ********   
  call xml_open( info, xmlfilefull, .true. )
  call xml_options( info, report_details = .false. )
  maskname = 'Empty'
  do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MaskFileName" .and. .not. endtag) then 
          maskname = data(1)
          exit
        endif
        
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   enddo

   call xml_close(info)
  
!********DEM Min - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   demminname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="DEMminFileName" .and. .not. endtag) then 
          demminname = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
!********Veg Map - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   vegname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="VegMap" .and. .not. endtag) then 
          vegname = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
!********Soil Map - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   soilname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="SoilMap" .and. .not. endtag) then 
          soilname = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
   
!********Lake Map - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   lakename = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="LakeMap" .and. .not. endtag) then 
          lakename = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
    
!********Precip Map - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   precipname = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="PrecipMap" .and. .not. endtag) then 
          precipname = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
 !********PET Map - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   pename = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="PeMap" .and. .not. endtag) then 
          pename = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

   
 !********IsSnow - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   IsSnow = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="Snow" .and. .not. endtag) then 
          XIsSnow = data(1)
          read(XIsSnow(1),*,err=961,end=961) IsSnow
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

    !********IsSediment - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   IsSediment = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="Sediment" .and. .not. endtag) then 
          XIsSediment = data(1)
          read(XIsSediment(1),*,err=962,end=962) IsSediment
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

    !********IsBanks - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   IsBanks = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="Banks" .and. .not. endtag) then 
          XIsBanks = data(1)
          read(XIsBanks(1),*,err=963,end=963) IsBanks
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

    !********IsSolute - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   IsSolute = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="Solute" .and. .not. endtag) then 
          XIsSolute = data(1)
          read(XIsSolute(1),*,err=964,end=964) IsSolute
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)



   
   
  !*******StartYear - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   year = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StartYear" .and. .not. endtag) then 
          xyear = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xyear(1),*,err=971,end=971) year
   
 !*******StartMonth - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   month = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StartMonth" .and. .not. endtag) then 
          xmonth = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xmonth(1),*,err=972,end=972) month
   
   
 !*******StartDay - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   day = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StartDay" .and. .not. endtag) then 
          xday = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xday(1),*,err=973,end=973) day
 
   
   
 !*******StartHour - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   hour = 0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StartHour" .and. .not. endtag) then 
          xhour = data(1)
          read(xhour(1),*,err=974,end=974) hour
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
   
   
 !*******StarMinute - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   minute = 0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StartMinute" .and. .not. endtag) then 
          xminute = data(1)
          read(xminute(1),*,err=975,end=975) minute
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
   
   !*******EndYear - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   endyear = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EndYear" .and. .not. endtag) then 
          xendyear = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xendyear(1),*,err=981,end=981) endyear
   
 !*******EndMonth - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   endmonth = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EndMonth" .and. .not. endtag) then 
          xendmonth = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xendmonth(1),*,err=982,end=982) endmonth
    
   
 !*******EndDay - compulsory ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   endday = -999
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EndDay" .and. .not. endtag) then 
          xendday = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   read(xendday(1),*,err=983,end=983) endday
  
      
   
 !*******EndHour - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   endhour = 0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EndHour" .and. .not. endtag) then 
          xendhour = data(1)
          read(xendhour(1),*,err=976,end=976) endhour
        exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
   
   
 !*******EndMinute - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   endminute = 0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EndMinute" .and. .not. endtag) then 
          xendminute = data(1)
          read(xendminute(1),*,err=977,end=977) endminute
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
   

   

  !********Vegetation Details - array sizes******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   icountveg=1
   vegfirst=.true.
   do
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
  
        if (tag=="VegetationDetail" .and. .not. endtag) then
                if (vegfirst) then
                    vegfirst=.false.
                else
                    line=data(1)
                    lenline = len_trim(line)
! An extra VegetationDetails with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of VegetationDetails
                    if (lenline==0) exit
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! icount
                    read(token, *,err=985,end=985) icount
                    icountveg=max(icount,icountveg)
                endif
          endif
    
    
    enddo

   call xml_close(info)
   allocate (cstcap(icountveg))
   allocate (lai(icountveg))
   allocate (rootingdepth(icountveg))
   allocate (aepe(icountveg))
   allocate (stricklerveg(icountveg))
   allocate (vegtypes(icountveg))
   cstcap=1.0
   lai=1.0
   rootingdepth=1.0
   aepe=0.6
   stricklerveg=2.0
   vegtypes='Dummy'
 !********End of Vegetation Details - array sizes******************  

   
   !********Vegetation Details - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    vegfirst=.true.
    vegtypes(1)='Sample Vegetation- Grassland'
    cstcap(1)=1.5
    lai(1)=1.0
    rootingdepth(1)=1.0
    aepe(1)=0.6
    stricklerveg(1) = 1.0
    icountveg=1


    do
! This XML file is not in a normal format. All the data that is on one line should be on seperate lines. Just leaving it for the moment
! Vegetation Details is not actually needed. It was in the previous code and still exists in previous XML files.
! Vegetation Details data can be in any order with missing numbers        

        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="VegetationDetail" .and. .not. endtag) then
                if (vegfirst) then
                    vegfirst=.false.
                else
                    line=data(1)
                    lenline = len_trim(line)
! An extra VegetationDetails with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of VegetationDetails
                    if (lenline==0) exit
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! icount
                    read(token, *,err=985,end=985) icount
                    icountveg=max(icount,icountveg)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    !veg type
                    vegtypes(icount) = trim(token)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! cstcap
                    read(token, *,err=985,end=985) cstcap(icount)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! lai
                    read(token, *,err=985,end=985) lai(icount)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! rootingdepth
                    read(token, *,err=985,end=985) rootingdepth(icount)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! aepe
                    read(token, *,err=985,end=985) aepe(icount)
                    pos = pos + next
                    ! if (next == 0) then no more commas
                    token = adjustl(line(pos:lenline))
                    ! aepe
                    read(token, *,err=985,end=985) stricklerveg(icount)
                endif
          endif
    
    
    enddo
   
   call xml_close(info)
 !********End of Vegetation Details - optional******************  
   
  !********Soils - array sizes******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   icountsoil=1
   soilfirst=.true.
   do
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
  
        if (tag=="SoilProperty" .and. .not. endtag) then
                if (soilfirst) then
                    soilfirst=.false.
                else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra SoilProperty with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of SoilProperty
                    if (lenline==0) exit
                   pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! icount
                    read(token, *,err=986,end=986) soilnumber
                    icountsoil=max(soilnumber,icountsoil)
                endif
          endif
    enddo
   call xml_close(info)

   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    soilcatfirst=.true.
! icountsoilcat is the number of lines read in and icountsoilmaxcat is the maximum category number
    icountsoilcat=1
    icountsoilcatmax=1
    do
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="SoilDetail" .and. .not. endtag) then
                if (soilcatfirst) then
                    soilcatfirst=.false.
                    icountsoilcat=0
                    icountsoilcatmax=0
               else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra SoilDetail with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of SoilDetail
                    if (lenline==0) exit
                    icountsoilcat=icountsoilcat+1
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! icount
                    read(token, *,err=987,end=987) icountsoilcatmax1
                    icountsoilcatmax=max(icountsoilcatmax,icountsoilcatmax1)
                endif
          endif
    enddo
   call xml_close(info)

! set the array size to be the maximum of either the soil number or the soil caterories.
! Add one so no out of bound error in VS08 caculation
numbersoiltypes=max(icountsoil,icountsoilcat,icountsoilcatmax)+1
allocate (soildepth(numbersoiltypes))
allocate (thsat(numbersoiltypes))
allocate (thres(numbersoiltypes))
allocate (ksat(numbersoiltypes))
allocate (vgn(numbersoiltypes))
allocate (vga(numbersoiltypes))
allocate (specstor(numbersoiltypes))
allocate (soilcats(numbersoiltypes))
allocate (soillayers(numbersoiltypes))
allocate (soilnumbers2(numbersoiltypes))
allocate (soiltypes(numbersoiltypes))
soildepth=1.0
thsat=0.5
thres=0.2
ksat=1.0
vgn=1.5
vga=0.01
specstor=0.0001
soilcats=0
soillayers=0
soilnumbers2=0
soiltypes='Dummy'

 !********End of Soils - array sizes******************  

   
!********Soil Prpoerty - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    soilfirst=.true.
    soiltypes(1)='Sample Soil - Sandy loam'
    thsat(1)=0.412
    thres(1)=0.098
    ksat(1)=20.0
    vga(1) = 0.0144
    vgn(1)=1.736
    specstor(1)=0.0001
    icountsoil=1

 
    do
! This XML file is not in a normal format. All the data that is on one line should be on seperate lines. Just leaving it for the moment
! Soil Properties is not actually needed. It was in the previous code and still exists in previous XML files.
! Soil property data can be in any order with missing numbers        

        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="SoilProperty" .and. .not. endtag) then
                if (soilfirst) then
                    soilfirst=.false.
                else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra SoilProperty with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of SoilProperty
                    if (lenline==0) exit
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! soilnumber
                    read(token, *,err=986,end=986) soilnumber
                    icountsoil=max(soilnumber,icountsoil)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    !soil type
                    soiltypes(soilnumber) = trim(token)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! thsat
                    read(token, *,err=986,end=986) thsat(soilnumber)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! thres
                    read(token, *,err=986,end=986) thres(soilnumber)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! ksat
                    read(token, *,err=986,end=986) ksat(soilnumber)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! vga
                    read(token, *,err=986,end=986) vga(soilnumber)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')

                    ! test to see if vgn is the last value or if there is another one specstor
                    if (next == 0) then
                        token = adjustl(line(pos:lenline))
                    ! vgn
                        read(token, *,err=986,end=986) vgn(soilnumber)
                    else
                        token = adjustl(line(pos:pos+next-2))
                     ! vgn
                       read(token, *,err=986,end=986) vgn(soilnumber)
                        pos = pos + next
                        token = adjustl(line(pos:lenline))
                       ! specstor
                      read(token, *,err=986,end=986) specstor(soilnumber)
                   endif
                endif
          endif
    
    
    enddo
   
   call xml_close(info)
 !********End of soil properties - optional******************  
   

   
!********Soil Detail - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    soilcatfirst=.true.
    soilcats(1)=1
    soillayers(1)=1
    soilnumbers2(1)=1
    soildepth(1)=1.0
    icountsoilcat=1

 
    do
! This XML file is not in a normal format. All the data that is on one line should be on seperate lines. Just leaving it for the moment
! Soil Details is not actually needed. It was in the previous code and still exists in previous XML files.
! icountsoilcat is the number of lines of data read. It is assumed the categories are read in numerical order.
! There can be missing categories. The missing categories will be added to the vsd file with dummy daya
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="SoilDetail" .and. .not. endtag) then
                if (soilcatfirst) then
                    soilcatfirst=.false.
                    icountsoilcat=0
                else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra SoilDetail with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of SoilDetail
                    if (lenline==0) exit
                    icountsoilcat=icountsoilcat+1
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! soilcategory
                    read(token, *,err=987,end=987) soilcats(icountsoilcat)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    !soil layer
                    read(token, *,err=987,end=987) soillayers(icountsoilcat)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    ! Soil Type
                    read(token, *,err=987,end=987) soilnumbers2(icountsoilcat)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    ! if (next == 0) then no more commas
                    token = adjustl(line(pos:lenline))
                    ! depth
                    read(token,*,err=987,end=987) soildepth(icountsoilcat)
                endif
          endif
    
    
    enddo
   
   call xml_close(info)
 !********End of soil properties - optional******************  
   
   !*******Initial water table depth (PSL) - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   initialpsl = 0.0
   do

       call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="InitialConditions" .and. .not. endtag) then 
          xspatialpsl = data(1)
          read(xspatialpsl(1),*,err=989,end=989) initialpsl
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
   
   end do

   call xml_close(info)
   
    
 !********Initial spatial water table depth (PSL) - Optional  ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   isspatialpsl = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="SpatialVariableInitCond" .and. .not. endtag) then 
          xspatialpsl = data(1)
          read(xspatialpsl(1),*,err=966,end=966) isspatialpsl
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
 
    !********MeteorologicalDataIncludeDate - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   IsMeteorologicalDataIncludeDate = .False.
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MeteorologicalDataIncludeDate" .and. .not. endtag) then 
          XIsMeteorologicalDataIncludeDate = data(1)
          read(XIsMeteorologicalDataIncludeDate(1),*,err=965,end=965) IsMeteorologicalDataIncludeDate
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
  
   
  !*******Precipitation Filename - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   precfile = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="PrecipitationTimeSeriesData" .and. .not. endtag) then 
          precfile = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

   
   
  !*******Precipitation Timestep - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   prectmstep = 24.0
   do

       call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="PrecipitationTimeStep" .and. .not. endtag) then 
          prectimestep = data(1)
          read(prectimestep(1),*,err=990,end=990) prectmstep
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
   
   end do

   call xml_close(info)
   
   
  !*******PET Filename - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   pefile = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EvaporationTimeSeriesData" .and. .not. endtag) then 
          pefile = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
  !*******PET Timestep - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   petmstep = 24.0
   do

       call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="EvaporationTimeStep" .and. .not. endtag) then 
           petimestep = data(1)
          read(petimestep(1),*,err=991,end=991) petmstep
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
   
   end do

   call xml_close(info)
    
   !*******Maximum Temperature Filename - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   tmaxfile = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MaxTempTimeSeriesData" .and. .not. endtag) then 
          tmaxfile = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
  
   !*******Minimum Temperature Filename - Optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   tminfile = 'Empty'
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MinTempTimeSeriesData" .and. .not. endtag) then 
          tminfile = data(1)
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
  
   
   
 !*******RiverGridSquaresAccumulated - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   rivergridacc = 10.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="RiverGridSquaresAccumulated" .and. .not. endtag) then 
          rivergridaccum = data(1)
          read(rivergridaccum(1),*,err=992,end=992) rivergridacc
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   

    
 !*******DropFromGridToChannelDepth - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   channeldp = 2.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="DropFromGridToChannelDepth" .and. .not. endtag) then 
          channeldrop = data(1)
          read(channeldrop(1),*,err=993,end=993) channeldp
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
   
   
  !*******MinimumDropBetweenChannels - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   channelmindrop = 1.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MinimumDropBetweenChannels" .and. .not. endtag) then 
          channelminimumdrop = data(1)
          read(channelminimumdrop(1),*,err=994,end=994) channelmindrop
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
    
  !*******StricklerChannelCoeff - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   stricklerriv = 50.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StricklerChannelCoeff" .and. .not. endtag) then 
          StricklerChannelCoeff = data(1)
          read(StricklerChannelCoeff(1),*,err=950,end=950) stricklerriv
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
     
  !*******StricklerLakeCoeff - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   stricklerlake = 10.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StricklerLakeCoeff" .and. .not. endtag) then 
          StricklerLakeCoeff = data(1)
          read(StricklerLakeCoeff(1),*,err=951,end=951) stricklerlake
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
      
  !*******ChannelWidthFactor - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   channelwidthfactor = 15.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="ChannelWidthFactor" .and. .not. endtag) then 
          xchannelwidthfactor = data(1)
          read(xchannelwidthfactor(1),*,err=952,end=952) channelwidthfactor
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
  !*******ChannelWidthPower - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   channelwidthpower = 0.5
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="ChannelWidthPower" .and. .not. endtag) then 
          xchannelwidthpower = data(1)
          read(xchannelwidthpower(1),*,err=953,end=953) channelwidthpower
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   
  !*******ChannelBankHeight - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   channelbankheight = 1.5
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="ChannelBankHeight" .and. .not. endtag) then 
          xchannelbankheight = data(1)
          read(xchannelbankheight(1),*,err=954,end=954) channelbankheight
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   

  !*******MaxRainfallTimestep - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   maxrainfalltimestep = 0.5
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="MaxRainfallTimestep" .and. .not. endtag) then 
          maxrainfalltimestepa = data(1)
          read(maxrainfalltimestepa(1),*,err=955,end=955) maxrainfalltimestep
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
   
  
  !*******RegularTimestep - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   standardtimestep = 1.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="RegularTimestep" .and. .not. endtag) then 
          standardtimestepa = data(1)
          read(standardtimestepa(1),*,err=995,end=995) standardtimestep
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
   
  !*******IncreasingTimestep - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   increasingtimestep = 0.05
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="IncreasingTimestep" .and. .not. endtag) then 
          increasingtimestepa = data(1)
          read(increasingtimestepa(1),*,err=996,end=996) increasingtimestep
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

   
   !*******SimulatedDischargeTimestep - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   simulateddischargetimestep = 24.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="SimulatedDischargeTimestep" .and. .not. endtag) then 
          simulateddischargetimestepa = data(1)
          read(simulateddischargetimestepa(1),*,err=997,end=997) simulateddischargetimestep
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
   

   
    !*******StandardH5Ouptut - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   standardh5output = 24.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="StandardH5Output" .and. .not. endtag) then 
          standardh5outputa = data(1)
          read(standardh5outputa(1),*,err=956,end=956) standardh5output
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

   
    !*******ThreeDimensionalH5Ouptut - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   threedimensionalh5output = 730.0
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="ThreeDimensionalH5Output" .and. .not. endtag) then 
          threedimensionalh5outputa = data(1)
          read(threedimensionalh5outputa(1),*,err=957,end=957) threedimensionalh5output
         exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)

   
   
   
   
  !******SnowmeltDegreeDayFactor - optional ********  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   snowddf = 0.0002
   do

    call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) ! get an element from xml structure

        if (tag=="SnowmeltDegreeDayFactor" .and. .not. endtag) then 
          snowddfa = data(1)
          read(snowddfa(1),*,err=998,end=998) snowddf
          exit
        endif
       
    if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure
    
   end do

   call xml_close(info)
 
   
   
 !*******BaseFlowBoundary - optional -array sizes********  
   
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    baseflowboundaryfirst=.true.
    icountbfb=0
    do
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="BaseFlowBoundary" .and. .not. endtag) then
                if (baseflowboundaryfirst) then
                    baseflowboundaryfirst=.false.
                    icountbfb=0
                else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra BaseFlowBoundary with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of BaseFlowBoundary
                    if (lenline==0) exit
                    icountbfb=icountbfb+1
                endif
          endif
    enddo
   call xml_close(info)

   icountbfbmax=max(numbersoiltypes,icountbfb)
   allocate (baseflowboundary(icountbfbmax))
   allocate (baseflowboundary2(icountbfbmax))
   allocate (bfbcats(icountbfbmax))
   baseflowboundary = 0.0
   baseflowboundary2 = 0.0
   bfbcats=1
    
!********BaseFlowBoundary - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    baseflowboundaryfirst=.true.

 
    do
! This XML file is not in a normal format. All the data that is on one line should be on seperate lines. Just leaving it for the moment
! icountbfb is the number of lines of data read. It is assumed the categories are read in numerical order.
! There can be missing categories. The missing categories will assumed tro be zero
        ! categories should carrospond to those in SoilDetails
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="BaseFlowBoundary" .and. .not. endtag) then
                if (baseflowboundaryfirst) then
                    baseflowboundaryfirst=.false.
                    icountbfb=0
                else
                    line=data(1)
                    lenline = len_trim(line)
 ! An extra BaseFlowBoundary with a FALSE tag is read at the end of this section of data. If it is blank then we have reached the end of BaseFlowBoundary
                    if (lenline==0) exit
                    icountbfb=icountbfb+1
                    pos = 1
                    next = scan(line(pos:lenline), ',')
                    token = adjustl(line(pos:pos+next-2))
                    read(token, *,err=958,end=958) bfbcats(icountbfb)
                    pos = pos + next
                    next = scan(line(pos:lenline), ',')
                    ! if (next == 0) then no more commas
                    token = adjustl(line(pos:lenline))
                    ! depth
                    read(token,*,err=958,end=958) baseflowboundary(icountbfb)
                endif
          endif
    
    
    enddo
   
   call xml_close(info)
 !********End of BaseFlowBoundary - optional******************  

   
   
   
   
!********Extra Discharge points array sizes - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    isextradischarge=.false.
    icountdischargepoints=0

 
    do
! The extra discharge points can be in any order
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="ExtraDischargePoints" .and. .not. endtag) then
                 isextradischarge=.true.
                 icountdischargepoints=icountdischargepoints+1
        endif
    
    
    enddo
   
   call xml_close(info)
   
 allocate (extradischarge(icountdischargepoints))
  
   
 !********End of Extra Discharge points array sizes - optional******************  

   
   
!********Extra Discharge points - optional******************  
   call xml_open( info, xmlfilefull, .true. )
   call xml_options( info, report_details = .false. )
   
    isextradischarge=.false.
    icountdischargepoints=0

 
    do
! The extra discharge points can be in any order
        call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data) 
        if (.not. xml_ok(info)) exit ! exit the loop at the end of the xml structure

        if (tag=="ExtraDischargePoints" .and. .not. endtag) then
                 isextradischarge=.true.
                 icountdischargepoints=icountdischargepoints+1
                 extradischargea = data(1)
                 read(extradischargea(1),*,err=999,end=999) extradischarge(icountdischargepoints)
        endif
    
    
    enddo
   
   call xml_close(info)
 !********End of Extra Discharge points - optional******************  

  

   return
950     print*
        print*,'Error reading xml library file at TAG StricklerChannelCoeff. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
951     print*
        print*,'Error reading xml library file at TAG StricklerLakeCoeff. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
952     print*
        print*,'Error reading xml library file at TAG ChannelWidthFactor. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
953     print*
        print*,'Error reading xml library file at TAG ChannelWidthPower. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
954     print*
        print*,'Error reading xml library file at TAG ChannelBankHeight. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
955     print*
        print*,'Error reading xml library file at TAG MaxRainfallTimestep. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
956     print*
        print*,'Error reading xml library file at TAG StandardH5Output. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
957     print*
        print*,'Error reading xml library file at TAG ThreeDimensionalH5Output. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
958     print*
        print*,'Error reading xml library file at TAG BaseFlowBoundary. '
        print*,'The first TAG BaseFlowBoundary is text describing the data.'
        print*,'In subsequent lines the  following format is needed: 2 numbers the first 1 an integer'
        print*,'The error was reading this line', line
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
961     print*
        print*,'Error reading xml library file at TAG Snow. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
962     print*
        print*,'Error reading xml library file at TAG Sediment. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
963     print*
        print*,'Error reading xml library file at TAG Banks. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
964     print*
        print*,'Error reading xml library file at TAG Solute. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
965     print*
        print*,'Error reading xml library file at TAG MeteorologicalDataIncludeDate. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
966     print*
        print*,'Error reading xml library file at TAG MeteorologicalDataIncludeDate. The value must be logical (i.e True or False)'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
971     print*
        print*,'Error reading xml library file at TAG StartYear. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
972     print*
        print*,'Error reading xml library file at TAG StartMonth. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
973     print*
        print*,'Error reading xml library file at TAG StartDay. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
974     print*
        print*,'Error reading xml library file at TAG StartHour. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
975     print*
        print*,'Error reading xml library file at TAG StartMinute. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
976     print*
        print*,'Error reading xml library file at TAG EndHour. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
977     print*
        print*,'Error reading xml library file at TAG EndMinute. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
981     print*
        print*,'Error reading xml library file at TAG EndYear. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
982     print*
        print*,'Error reading xml library file at TAG EndMonth. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
983     print*
        print*,'Error reading xml library file at TAG EndDay. The value must be an integer'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
985     print*
        print*,'Error reading xml library file at TAG VegetationDetail. '
        print*,'The first TAG VegetationDetail is text describing the data.'
        print*,'In subsequent lines the  following format is needed: number,text and then 5 more numbers'
        print*,'The error was reading this line', line
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
986     print*
        print*,'Error reading xml library file at TAG SoilProperty. '
        print*,'The first TAG SoilProperty is text describing the data.'
        print*,'In subsequent lines the  following format is needed: number(integer),text and then 5 or 6 more numbers'
        print*,'The error was reading this line', line
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
987     print*
        print*,'Error reading xml library file at TAG SoilDetail. '
        print*,'The first TAG SoilDetail is text describing the data.'
        print*,'In subsequent lines the  following format is needed: 4 numbers the first 3 integers'
        print*,'The error was reading this line', line
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
989     print*
        print*,'Error reading xml library file at TAG InitialConditions. The value must be a number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
990     print*
        print*,'Error reading xml library file at TAG PrecipitationTimeSeriesData. The value must be a number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
991     print*
        print*,'Error reading xml library file at TAG EvaporationTimeStep. The value must be a number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
992     print*
        print*,'Error reading xml library file at TAG RiverGridSquaresAccumulated. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
993     print*
        print*,'Error reading xml library file at TAG DropFromGridToChannelDepth. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
994     print*
        print*,'Error reading xml library file at TAG MinimumDropBetweenChannels. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
995     print*
        print*,'Error reading xml library file at TAG RegularTimestep. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
996     print*
        print*,'Error reading xml library file at TAG IncreasingTimestep. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
997     print*
        print*,'Error reading xml library file at TAG SimulatedDischargeTimestep. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
998     print*
        print*,'Error reading xml library file at TAG SnowmeltDegreeDayFactor. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   
999     print*
        print*,'Error reading xml library file at TAG ExtraDischargePoints. The value must be an number'
        print*
        write(*,'(''paused, type [enter] to continue'')')
        read (*,*)
        stop   

        
!******************************** end subroutine to read xml data ************
        end subroutine read_xml_file
!************************************************************************

             
    end module ReadShetranXmlMod

    
    
