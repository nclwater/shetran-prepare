subroutine read_xml_file(xmlfilefull,catchmentname,demmeanname,demminname,maskname,vegname,soilname,lakename,precipname,pename,precfile,pefile,  &
             vegtypes,cstcap,lai,rootingdepth,aepe,stricklerveg,soilcats,soillayers,soilnumbers,soilnumbers2,soiltypes,soildepth,thsat,thres,ksat,vgn,vga, &
             initialpsl,prectmstep,petmstep,day,month,year,endday,endmonth,endyear,icountveg,icountsoil,icountsoilcat, &
             rivergridacc,channeldp,channelmindrop,tmaxfile,tminfile,standardtimestep,increasingtimestep, &
             regulartmstep,snowddf)
   use xmlparse

   implicit none

   logical           :: mustread
   type(XML_PARSE)   :: info

   character(len=200)                      :: tag
   logical                                :: endtag
   character(len=200), dimension(1:1,1:1)  :: attribs
   integer                                :: no_attribs
   character(len=200), dimension(1:1)     :: dummy
   character(len=*), intent(in)           :: xmlfilefull
   character(len=200), dimension(1:1)     :: projectfile
   character(len=200), dimension(1:1),intent(out)     :: catchmentname
   character(len=200), dimension(1:1),intent(out)     :: demmeanname
   character(len=200), dimension(1:1),intent(out)     :: demminname
   character(len=200), dimension(1:1),intent(out)     :: maskname
   character(len=200), dimension(1:1),intent(out)     :: vegname
   character(len=200), dimension(1:1),intent(out)     :: soilname
   character(len=200), dimension(1:1),intent(out)     :: lakename
   character(len=200), dimension(1:1),intent(out)     :: precipname
   character(len=200), dimension(1:1),intent(out)     :: pename
   character(len=200), dimension(1:1)     :: vegdetail
   character(len=200), dimension(1:1)     :: soildetail
   character(len=200), dimension(1:1)     :: soilproperty
   character(len=200), dimension(1:1)     :: overflowrough
   character(len=200), dimension(1:1)     :: initialconditions
   character(len=200), dimension(1:1),intent(out)     :: precfile
   character(len=200), dimension(1:1)     :: prectimestep
   character(len=200), dimension(1:1),intent(out)     :: pefile
   character(len=200), dimension(1:1),intent(out)     :: tmaxfile
   character(len=200), dimension(1:1),intent(out)     :: tminfile
   character(len=200), dimension(1:1)     :: petimestep
   character(len=200), dimension(1:1)     :: xday,xmonth,xyear
   character(len=200), dimension(1:1)     :: xendday,xendmonth,xendyear
  ! character(len=200), dimension(1:1)     :: starttime
  ! character(len=200)                     :: starttm
  ! character(len=200), dimension(1:1)     :: endtime
  ! character(len=200)                     :: endtm
   character(len=200), dimension(1:1)     :: rivergridaccum
   character(len=200), dimension(1:1)     :: channeldrop
   character(len=200), dimension(1:1)     :: channelminimumdrop
   character(len=200), dimension(1:1)     :: snowddfa,regulartmstepa
   character(len=200), dimension(1:1)     :: standardtimestepa, increasingtimestepa
   integer                                :: no_data
   integer                                :: i,icount,icounter
   character(len=200)                      :: vegtype,soiltype
   character(len=200)                      :: vegtypes(100),soiltypes(40000)
!   character*200                     :: vegtypes(100),soiltypes(10000)
   real                                   :: cst,la,root,ev,strv
   real,intent(out)                                   :: cstcap(100)
   real,intent(out)                                   :: lai(100)
   real,intent(out)                                   :: rootingdepth(100)
   real,intent(out)                                   :: aepe(100)
   real,intent(out)                                   :: stricklerveg(100)
   real                                   :: soilcat,soillayer,sd,ths,thr,ks,va,vn,soilnumber,soilnumber2
   integer,intent(out)                                   :: soilcats(40000)
   integer,intent(out)                                   :: soillayers(40000)
   integer,intent(out)                                   :: soilnumbers(40000)
   integer,intent(out)                                   :: soilnumbers2(40000)
   real,intent(out)                                   :: soildepth(40000)
   real,intent(out)                                   :: thsat(40000)
   real,intent(out)                                   :: thres(40000)
   real,intent(out)                                   :: ksat(40000)
   real,intent(out)                                   :: vgn(40000)
   real,intent(out)                                   :: vga(40000)
   real,intent(out)                                   :: initialpsl
   real,intent(out)                                   :: prectmstep
   real,intent(out)                                   :: petmstep
   real,intent(out)                                   :: rivergridacc,channeldp,channelmindrop,snowddf,regulartmstep
   real,intent(out)                                   :: standardtimestep, increasingtimestep
   integer,intent(out)                                :: day,month,year
   integer,intent(out)                                :: endday,endmonth,endyear
   integer,intent(out)                                :: icountveg,icountsoil,icountsoilcat



   call xml_open( info, xmlfilefull, .true. )
!   call xml_options( info, report_lun = 30, report_details = .true. )
   call xml_options( info, report_details = .false. )
!temporariliy deleted
!shetran input start tag
!   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
!   if ( xml_error(info) ) then
!       stop
!   endif

!project file
   call xml_get( info, tag, endtag, attribs, no_attribs, projectfile, no_data )
!   write(21,*) trim(tag),' ',trim(projectfile(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!catchment name
   call xml_get( info, tag, endtag, attribs, no_attribs, catchmentname, no_data )
!   write(21,*) trim(tag),' ',trim(catchmentname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!dem name
   call xml_get( info, tag, endtag, attribs, no_attribs, demmeanname, no_data )
!   write(21,*) trim(tag),' ',trim(demname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!dem name
   call xml_get( info, tag, endtag, attribs, no_attribs, demminname, no_data )
!   write(21,*) trim(tag),' ',trim(demname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!mask name
   call xml_get( info, tag, endtag, attribs, no_attribs, maskname, no_data )
!   write(21,*) trim(tag),' ',trim(maskname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!veg name
   call xml_get( info, tag, endtag, attribs, no_attribs, vegname, no_data )
!   write(21,*) trim(tag),' ',trim(vegname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!soil name
   call xml_get( info, tag, endtag, attribs, no_attribs, soilname, no_data )
!   write(21,*) trim(tag),' ',trim(soilname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!lake name
   call xml_get( info, tag, endtag, attribs, no_attribs, lakename, no_data )
!   write(21,*) trim(tag),' ',trim(soilname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


   !precip name
   call xml_get( info, tag, endtag, attribs, no_attribs, precipname, no_data )
!   write(21,*) trim(tag),' ',trim(soilname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


   !PE name
   call xml_get( info, tag, endtag, attribs, no_attribs, pename, no_data )
!   write(21,*) trim(tag),' ',trim(soilname(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!vegetation details start tag
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!vegetation detail
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, vegdetail, no_data )
!   write(21,*) trim(tag),' ',trim(vegdetail(1))
   if ( xml_error(info) ) then
       stop
   endif

   do

     if (trim(tag).eq.'VegetationDetails') then
        exit
     else
        read(vegdetail(1),*,err=767,end=767) icount,vegtype,cst,la,root,ev,strv
        vegtypes(icount)=vegtype
        cstcap(icount)=cst
        lai(icount)=la
        rootingdepth(icount)=root
        aepe(icount)=ev
        stricklerveg(icount) = strv
        call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!        write(21,*) trim(tag),' ',trim(dummy(1))
        if ( xml_error(info) ) then
          stop
        endif
        call xml_get( info, tag, endtag, attribs, no_attribs, vegdetail, no_data )
!        write(21,*) trim(tag),' ',trim(vegdetail(1))
        if ( xml_error(info) ) then
          stop
        endif

     endif

   enddo
   icountveg=icount
!end of vegetation details

!*****sb 290415
!soil properties start tag
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!  write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!soil property
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, soilproperty, no_data )
!   write(21,*) trim(tag),' ',trim(soilproperty(1))
   if ( xml_error(info) ) then
       stop
   endif

   icount=1
   do

     if (trim(tag).eq.'SoilProperties') then
        exit
     else
        read(soilproperty(1),*,err=867,end=867) soilnumber,soiltype,ths,thr,ks,va,vn
	    soilnumbers(icount)=soilnumber
        soiltypes(icount)=soiltype
!        print*,ths,icount,thsat(icount)
        thsat(icount)=ths
        thres(icount)=thr
        ksat(icount)=ks
        vga(icount)=va
        vgn(icount)=vn
!        print*,soiltypes(icount)
!        print*,ths,icount,thsat(icount)
        call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!        write(21,*) trim(tag),' ',trim(dummy(1))
        if ( xml_error(info) ) then
          stop
        endif
        call xml_get( info, tag, endtag, attribs, no_attribs, soilproperty, no_data )
!        write(21,*) trim(tag),' ',trim(soilproperty(1))
        if ( xml_error(info) ) then
         stop
        endif
		icount=icount+1

     endif

   enddo
   icountsoil=icount-1
!end of soil details
!*****sb 290415


   
  

!soil details start tag
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!  write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!soil detail
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, soildetail, no_data )
!   write(21,*) trim(tag),' ',trim(soildetail(1))
   if ( xml_error(info) ) then
       stop
   endif

   icount=1
   do

     if (trim(tag).eq.'SoilDetails') then
        exit
     else
        read(soildetail(1),*,err=967,end=967) soilcat,soillayer,soilnumber2,sd
        soilcats(icount)=soilcat
		soillayers(icount)=soillayer
        soilnumbers2(icount)=soilnumber2
        soildepth(icount)=sd
        call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!        write(21,*) trim(tag),' ',trim(dummy(1))
        if ( xml_error(info) ) then
          stop
        endif
        call xml_get( info, tag, endtag, attribs, no_attribs, soildetail, no_data )
!        write(21,*) trim(tag),' ',trim(soildetail(1))
        if ( xml_error(info) ) then
          stop
        endif
		icount=icount+1

     endif

   enddo
   icountsoilcat=icount-1
!end of soil details   


!initial conditions
   call xml_get( info, tag, endtag, attribs, no_attribs, initialconditions, no_data )
!   write(21,*) trim(tag),' ',trim(initialconditions(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(initialconditions(1),*,err=968,end=968) initialpsl


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!precipitation data
   call xml_get( info, tag, endtag, attribs, no_attribs, precfile, no_data )
!   write(21,*) trim(tag),' ',trim(precfile(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, prectimestep, no_data )
!   write(21,*) trim(tag),' ',trim(prectimestep(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(prectimestep(1),*,err=969,end=969) prectmstep


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!pe data
   call xml_get( info, tag, endtag, attribs, no_attribs, pefile, no_data )
!   write(21,*) trim(tag),' ',trim(pefile(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, petimestep, no_data )
!   write(21,*) trim(tag),' ',trim(petimestep(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(petimestep(1),*,err=970,end=970) petmstep



   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!tmax data
   call xml_get( info, tag, endtag, attribs, no_attribs, tmaxfile, no_data )
!   write(21,*) trim(tag),' ',trim(tmaxfile(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif
!tmin data
   call xml_get( info, tag, endtag, attribs, no_attribs, tminfile, no_data )
!   write(21,*) trim(tag),' ',trim(tminfile(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif




!start and end time
   call xml_get( info, tag, endtag, attribs, no_attribs, xday, no_data )
!   write(21,*) trim(tag),' ',trim(starttime(1))
   if ( xml_error(info) ) then
       stop
   endif
      call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!start and end time
   call xml_get( info, tag, endtag, attribs, no_attribs, xmonth, no_data )
!   write(21,*) trim(tag),' ',trim(starttime(1))
   if ( xml_error(info) ) then
       stop
   endif

    read(xday(1),*) day
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!start and end time
   call xml_get( info, tag, endtag, attribs, no_attribs, xyear, no_data )
!   write(21,*) trim(tag),' ',trim(starttime(1))
   if ( xml_error(info) ) then
       stop
   endif

    read(xday(1),*,err=971,end=971) day
    read(xmonth(1),*,err=971,end=971) month
    read(xyear(1),*,err=971,end=971) year



!   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
!   if ( xml_error(info) ) then
!       stop
!   endif

!start and end time
!   call xml_get( info, tag, endtag, attribs, no_attribs, starttime, no_data )
!   write(21,*) trim(tag),' ',trim(starttime(1))
!   if ( xml_error(info) ) then
!       stop
!   endif
!   starttm=starttime(1)
!   read(starttm(1:2),*) day
!   read(starttm(4:5),*) month
!   read(starttm(7:10),*) year

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, xendday, no_data )
!   write(21,*) trim(tag),' ',trim(endtime(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, xendmonth, no_data )
!   write(21,*) trim(tag),' ',trim(endtime(1))
   if ( xml_error(info) ) then
       stop
   endif
   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   call xml_get( info, tag, endtag, attribs, no_attribs, xendyear, no_data )
!   write(21,*) trim(tag),' ',trim(endtime(1))
   if ( xml_error(info) ) then
       stop
   endif

    read(xendday(1),*,err=971,end=971) endday
    read(xendmonth(1),*,err=971,end=971) endmonth
    read(xendyear(1),*,err=971,end=971) endyear



!  call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
!   if ( xml_error(info) ) then
!       stop
!   endif

!   call xml_get( info, tag, endtag, attribs, no_attribs, endtime, no_data )
!   write(21,*) trim(tag),' ',trim(endtime(1))
!   if ( xml_error(info) ) then
!       stop
!   endif
!   endtm=endtime(1)
!   read(endtm(1:2),*) endday
!   read(endtm(4:5),*) endmonth
!   read(endtm(7:10),*) endyear

   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!rivergridsquares accumulated
   call xml_get( info, tag, endtag, attribs, no_attribs, rivergridaccum, no_data )
!   write(21,*) trim(tag),' ',trim(rivergridaccum(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(rivergridaccum(1),*,err=971,end=971) rivergridacc


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

!chnnel below grid square
   call xml_get( info, tag, endtag, attribs, no_attribs, channeldrop, no_data )
!   write(21,*) trim(tag),' ',trim(channeldrop(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(channeldrop(1),*,err=971,end=971) channeldp


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!chnnel below grid square
   call xml_get( info, tag, endtag, attribs, no_attribs, channelminimumdrop, no_data )
!   write(21,*) trim(tag),' ',trim(channeldrop(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(channelminimumdrop(1),*,err=971,end=971) channelmindrop


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   !simulation timestep
   call xml_get( info, tag, endtag, attribs, no_attribs, standardtimestepa, no_data )
   if ( xml_error(info) ) then
       stop
   endif
   read(standardtimestepa(1),*,err=971,end=971) standardtimestep


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

      !increasing timestep
   call xml_get( info, tag, endtag, attribs, no_attribs, increasingtimestepa, no_data )
   if ( xml_error(info) ) then
       stop
   endif
   read(increasingtimestepa(1),*,err=971,end=971) increasingtimestep


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif

   
   

!regular timstep (hours)
   call xml_get( info, tag, endtag, attribs, no_attribs, regulartmstepa, no_data )
!   write(21,*) trim(tag),' ',trim(regulartmstep(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(regulartmstepa(1),*,err=971,end=971) regulartmstep


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
       stop
   endif


!snow degree day factor mm/s/C
   call xml_get( info, tag, endtag, attribs, no_attribs, snowddfa, no_data )
!   write(21,*) trim(tag),' ',trim(snowddf(1))
   if ( xml_error(info) ) then
       stop
   endif
   read(snowddfa(1),*,err=971,end=971) snowddf


   call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
!   write(21,*) trim(tag),' ',trim(dummy(1))
   if ( xml_error(info) ) then
      stop
   endif



   call xml_close( info )
   return

767     print*
        print*,'Error reading xml library file. Around vegetation details are incorrect'
        print*
        pause
        stop
867     print*
        print*,'Error reading xml library file. Around soil properties are incorrect'
        print*
        pause
        stop
967     print*
        print*,'Error reading xml library file. Around soil details are incorrect'
        print*
        pause
        stop
968     print*
        print*,'Error reading xml library file. Around initial conditions is incorrect'
        print*
        pause
        stop
969     print*
        print*,'Error reading xml library file. Around precipitation timestep is incorrect'
        print*
        pause
        stop
970     print*
        print*,'Error reading xml library file. Around PE timestep is incorrect'
        print*
        pause
        stop
971     print*
        print*,'Error reading xml library file. Something between start day and snowmelt degree day factor is incorrect'
        print*
        pause
        stop   
   
end subroutine read_xml_file
