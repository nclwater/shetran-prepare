module PrepareMod
! Prepare SHETRAN files

   use ReadShetranXmlMod,           only: catchmentname,demmeanname,demminname,maskname,vegname,soilname,lakename,precipname,pename
   use ReadShetranXmlMod,           only: precfile,pefile
   use ReadShetranXmlMod,           only: vegtypes,cstcap,lai,rootingdepth,aepe,stricklerveg 
   use ReadShetranXmlMod,           only: soilcats,soillayers,soilnumbers2,soiltypes,soildepth
   use ReadShetranXmlMod,           only: thsat,thres,ksat,vgn,vga,specstor
   use ReadShetranXmlMod,           only: extradischarge
   use ReadShetranXmlMod,           only: bfbcats
   use ReadShetranXmlMod,           only: initialpsl,prectmstep,petmstep
   use ReadShetranXmlMod,           only: day,month,year,endday,endmonth,endyear
   use ReadShetranXmlMod,           only: hour,minute,endhour,endminute
! icountveg is the maximum VegType isn the xml file, icountsoil is the maximum SoilType in the xml file and icountsoilcat is the maximum Soil category type in the XML file
   use ReadShetranXmlMod,           only: icountveg,icountsoil,icountsoilcat,icountbfb,icountdischargepoints
   use ReadShetranXmlMod,           only: issnow,issediment,isbanks,issolute,IsMeteorologicalDataIncludeDate,isspatialpsl,isextradischarge

!  rivergridacc is the number of upstreamg rid squares needed to produce a river link
!  channeldp is drop from grid elevation to channel depth elevation
!  channelmindrop is drop along a channel that is being followed
   use ReadShetranXmlMod,           only: rivergridacc,channeldp,channelmindrop,stricklerriv,stricklerlake
   use ReadShetranXmlMod,           only: channelwidthfactor,channelwidthpower,channelbankheight
   use ReadShetranXmlMod,           only: maxrainfalltimestep,standardtimestep, increasingtimestep,simulateddischargetimestep 
   use ReadShetranXmlMod,           only: standardh5output,threedimensionalh5output
   use ReadShetranXmlMod,           only: snowddf
   use ReadShetranXmlMod,           only: baseflowboundary,baseflowboundary2
   use ReadShetranXmlMod,           only: tmaxfile, tminfile
   use ReadShetranXmlMod,           only: read_xml_file
   use RestMod,                     only: form,rootdensity,hour_from_date,PrecipTest,PetTest
   implicit none
	
   contains
    
   Subroutine PrepareInputFiles(xmlfilefull)

  real, parameter :: removesink = 0.1

! net rainfall(mm) to calculate mean annual flow and hence stream width (see Leopold & Maddock (1953) and Park, C.C. (1977) World-wide variations in hydraulic geometry exponents of stream channels)
! A large value of net rainfall (precipitation-AE) is used so channel are usually larger than reality. They can be scaled using the channel width factor and coeff
! bankfull flow is better but unknown
  real, parameter :: netrainfall = 1000.0
! width is proportinal to channelwidthfactor* maf^channelwidthpower * upstream contributing area. These are the default values
!  real, parameter :: channelwidthfactor = 15.0
!  real, parameter :: channelwidthpower = 0.5

!  real, parameter :: stricklerriv = 50.0
!  real, parameter :: stricklerlake = 10.0
  integer, parameter :: InDemMean = 11
  integer, parameter :: InMask = 12
  integer, parameter :: InDemMin = 13
  integer, parameter :: InVeg = 14
  integer, parameter :: InSoil = 15
  integer, parameter :: InPE = 16
  integer, parameter :: InPrec = 17
  integer, parameter :: InLake = 19
  integer, parameter :: OUTFRD = 20
  integer, parameter ::  OUTOCD = 21
  integer, parameter ::  OUTETD = 22
  integer, parameter ::  OUTVSD = 23
  integer, parameter ::  OUTRUN = 24
  integer, parameter ::  OUTVIS = 25
  integer, parameter ::  OUTRIV = 26
  integer, parameter ::  OUTSMD = 27
  integer, parameter ::  SamplePRD = 28
  integer, parameter ::  SampleEPD = 29
  integer, parameter :: InConopyStorage = 30
  integer, parameter :: NFMStorage = 31
  integer, parameter :: NFMForest = 32
  integer, parameter :: ChangedPETFile = 33
  integer, parameter :: ChangedPETFileTemp = 34
  integer, parameter :: OUTSYD = 35
  integer, parameter :: OUTBKD = 36
  integer, parameter :: OUTCMD = 37
  integer, parameter :: OUTBFB = 38
  integer, parameter :: InSpatialPSL = 39
  integer, parameter :: OutSpatialPSL = 40
  integer, parameter :: OutRivLoc = 41
  integer, parameter :: OutDischargePoints = 42
  integer, parameter :: InPrecTS = 43
  integer, parameter :: InPeTS = 44
  integer, parameter :: OutElmNum = 45
  integer, parameter :: logfile = 51

  
  !     maximum number of columns and rows
  ! used to allocate array sizes
  integer ncolsmax
  integer nrowsmax



      CHARACTER*200 FILFRD,FILOCD,FILETD,FILVSD,FILVIS,FILSMD
      CHARACTER*200 FILFRD2,FILOCD2,FILETD2,FILVSD2,FILVIS2,FILSMD2
      CHARACTER*200 FILSYD2,FILSYD,FILSPR,FILCPR
      CHARACTER*200 FILCMD,FILCMD2,FILBKD,FILBKD2,FILBFB,FILBFB2
      CHARACTER*200 FILRUN,FILPRD,FILEPD,FILTIM,FILPRI,FILRIV,FILRIVLOC,FILDISPOINT,FILDISPOINT2,FILELMNUM
      CHARACTER*200 FILDIS,FILVSE,FILMAS,FILCVI,FILHDF,FILDIS2,FILLOG


      character*1, allocatable             :: xmap(:,:)
      character*1, allocatable             :: alinkew(:,:),alinkns(:,:)
      character*1, allocatable             :: vismask(:,:)
      character*7, allocatable             :: aform(:)
      CHARACTER*200 FILEO1
      CHARACTER*200 filecstcap,filespatialpsl,filespatialpslout,filespatialpslout2
      CHARACTER*200 NFMStorageName,NFMForestName
      CHARACTER*200 precfilescaler,pefilescaler
      CHARACTER*200 precfile2,pefile2
      character*200 basedir, xmlfilename,buildloc,xmlfilefull
      CHARACTER*200 changePETfile,tempPEtfile
      CHARACTER*200 spatialpslline
      character*5 acols,arows
      character*8 acellsize
      character*10 axllcorner,ayllcorner
      character msg*520
      character anovalue
      CHARACTER   MSG2*100
      CHARACTER Delimeter,Delimeter2
 
      integer, allocatable             :: cornerval(:,:)
      integer, allocatable             :: poscol(:),posrow(:)
      integer, allocatable             :: catch(:,:)
      integer, allocatable             :: catchboundary(:,:)
      integer, allocatable             :: catchrow(:)
      integer, allocatable             :: catchgeometry(:,:)
      integer, allocatable             :: vegdist(:,:),soildist(:,:)
      integer, allocatable             :: lakedist(:,:),pedist(:,:)
      integer, allocatable             :: raindist(:,:)
      integer, allocatable             :: NFMStorageDist(:,:)
      integer, allocatable             :: NFMForestDist(:,:),NFMStorageDist2(:,:)
      integer, allocatable             :: NFMForestDist2(:,:)
      integer, allocatable             :: accum(:,:)
      integer, allocatable             :: numbermet(:)
      integer, allocatable             :: cstcapnopoints(:)
      integer, allocatable             :: streamsize(:)
      integer, allocatable             :: elementnumber(:,:)
      integer rivergridacci
      integer ncols,nrows,i,j,k,l,m,ncols2,nrows2,count,ncols3,nrows3
      integer colposmin,rowposmin,change
      integer crncolpos,crnrowpos,crnmax
      integer direction,maxacc
      integer itemp,number
      integer numberunique
      integer numberuniquer
      integer cv,rv,cvadd,rvadd
      integer msgs,msge
      integer linkoutdir,linkoutr,linkoutc
      integer numlinks,linkoutnum
      integer ndefct,nxsect,iface
      integer ncspairs
      integer badnum,badrow,badcol
      integer length,length2
      integer nrowsm1,ncolsm1
      integer nf,nrd
      integer nmcellroot,numberedge
      integer novalue,novalueDEM
      integer maxcatnumber,soilcatchange
      integer istatus
      integer cstcapnoveg,cstcapnoyear
      integer io
      integer vislayer,finaldivider
      integer countiteration
      integer countnumber
      integer outletcrnrowpos,outletcrncolpos,outletcornerval
      integer pc
      integer finaldel,finaldel2
      integer maxcstcapnopoints
 !PEDistMax is the maximum value in the PE distribution map file, RainDistMax is the maximum value in the Rain distribution map file
      integer PeDistMax,RainDistMax
      integer extradischargeface
      integer elementcount

      real, allocatable                     :: dem(:,:),demrow(:)
      real, allocatable                     :: demmean(:,:),demrowmean(:)
      real, allocatable                     :: ellinkew(:,:),ellinkns(:,:)
      real, allocatable                     :: strlinkew(:,:),strlinkns(:,:)
      real, allocatable                     :: linkelv(:),linkstr(:)
      real, allocatable                     :: posval(:)
      real, allocatable                     :: pet(:),petchange2(:)
      real, allocatable                     :: cstcapratio(:,:),cstcaptime(:,:)
      real demne,demse,demsw,demnw,demmincorner,demminedge
      real demn,dems,demw,deme
      real demmin,mindem
      real temp
      real dum1,dum2,dum3,dum4,dum5,dum6,dummin,cheldum,pchdp
      real linkelvmin
      real wdepth,str,coeff,subrio
      real cellsize,xllcorner,yllcorner
      real plai,clai,ck,cb
      real ps1(7),fet(7),depth(50),rdf(50,50)
      real vislayerdepth(50)
      real satstor
      real soildepthmin
      real streamwidth1,streamwidth2,maffactor
      real demminoutletproblem
      real tempvalue
      real maxsoildepth
      real StartFrom1950,EndFrom1950,simulationTimeHours
!array size set to 365 with a value for every day of the year. Multiple years repeat values
      real :: PrecipTestData(365),PetTestData(365)

      
      logical, allocatable          :: linkew(:,:),linkns(:,:)
      logical, allocatable          :: savelinkew(:,:),savelinkns(:,:)
      logical, allocatable          :: isaccum(:,:)
      logical, allocatable          :: cornerdone(:,:)
      logical, allocatable          :: cornerdonep(:,:)
      logical, allocatable          :: savecornerdone(:,:)
      logical notlowpoint,isok
      logical outletlink
!      logical isunique
      logical islakename
      logical IsStorageFile,IsForestFile,isspatialpslfile
      logical meetexit
      logical iswritefile
      logical file_exists


      


 9301 FORMAT(':FR1 - TEST CATCHMENT- FR COMPONENT DATA SET')
 9302 FORMAT(':FR2 - GRID SQUARES IN THE X Y DIRECTIONS')
 9304 FORMAT(':FR4 - START TIME OF WATER FLOW COMPONENT SIMULATION')
 9306 FORMAT(':FR6 - END TIME OF WATER FLOW COMPONENT SIMULATION')
 9391 FORMAT(':FR7a - START TIME OF SEDIMENT TRANPORT COMPONENT SIMULATION') 
 9392 FORMAT(':FR7c - START TIME OF CONTAMINANT MIGRATION COMPONENT SIMULATION')
 9308 FORMAT(':FR8 - GRID SPACING IN X DIRECTION')
 9310 FORMAT(':FR10 - GRID SPACING IN Y DIRECTION')
 9312 FORMAT(':FR12 - PRINT CONTROL PARAMETERS')
 9320 FORMAT(':FR20 - BASIC TIMESTEP DATA')
 9322 FORMAT(':FR22 - PRINT CONTROL PARAMETERS')
 9324 FORMAT(':FR24 - COMPONENT EXECUTION CONTROL PARAMETERS (SM,BK,SY,CM)')
 9326 FORMAT(':FR26 - HOTSTART PARAMETERS')
 9328 FORMAT(':FR28 - NO. OF MET./RAINFALL STATIONS, VEG./SOIL TYPES AND SOIL LAYER CAT.')
 9330 FORMAT(':FR30 - RIVER LINING PARAMETERS')
 9332 FORMAT(':FR32 - DEFAULT VALUES FOR MET,RAINFALL,VEG,SOIL LAYER CATEGORIES')
 9334 FORMAT(':FR34 - COMPUTATIONAL GRID DEFINITION')
 9335 FORMAT(':FR35a - E-W FLOW CODES (n-s links)')
 9336 FORMAT(':FR35c - N-S FLOW CODES (e-w links)')
 9337 FORMAT(':FR37 - GROUND SURFACE ELEVATION')
 9343 FORMAT(':FR43 - MET STATIONS')
 9346 FORMAT(':FR46 - RAIN STATIONS')
 9349 FORMAT(':FR49 - VEGETATION TYPES')
 9352 FORMAT(':FR52 - OUTPUT DISCHARGE TIMESTEP(HOURS)')
 9401 FORMAT(':OC1 - TEST CATCHMENT- OC COMPONENT DATA SET: NT, NCATR, KONT, BIOWAT')
 9402 FORMAT(':OC2 - TIMESTEP CONTROL (currently not used)')
 9403 FORMAT(':OC3 - OTHER PARAMETERS. SMIN,CDRS,TDC,TFC,DET. (SMIN & DET not used)') 
 9414 FORMAT(':OC14 STRX')
 9417 FORMAT(':OC17 STRY')
 9420 FORMAT(':OC20 - HEAD AND FLUX BOUNDARY CONDITIONS: NOCHB, NOCFB, NOCPB')
 9430 FORMAT(':OC30 - NUMBER OF DEFAULT CHANNEL CROSS-SECTIONS')    
 9432 FORMAT(':OC32 - DEFAULT CHANNEL DESCRIPTION')    
 9435 FORMAT(':OC35 - CHANNEL LINK DATA (LINK NO.,BED ELEV.,DEPTH, STR. COEFF.,DEF X-S)')
 9501 FORMAT(':ET1 - TEST CATCHMENT- ET COMPONENT DATA SET')
 9503 FORMAT(':ET3 - DTMET : TIMESTEP FOR INPUT OF RAIN AND MET. DATA')
 9505 FORMAT(':ET5 - MEASPE (0 NOT MEASURED, 1 MEASURED)')
 9557 FORMAT(':ET7 - VEGETATION TYPE ')
 9509 FORMAT(':ET9 - CONTROLS FOR TIME VARYING PARAMETERS FOR VEGETATION TYPE')
 9515 FORMAT(':ET15 - PSI/RCF/FET FUNCTION FOR VEGETATION TYPE')
 9517 FORMAT(':ET17 - DEPTH/RDF FOR VEGETATION TYPE')
 9518 FORMAT(':ET11 - time varying cstcap')
 9519 FORMAT(':ET13 - time varying cstcap')
 9601 FORMAT(':VS01 simulation title')
 9602 FORMAT(':VS02 logical flags: BFAST,BSOILP,BHELEV')
 9603 FORMAT(':VS03 integer variables NS,NCSZON,NCRBED,INITYP')
 9604 FORMAT(':VS04 real variables VSIPSD,VSZMIN,VSZMAX,VSWV,VSWL')
 9605 FORMAT(':VS05 physical property data (IS,IVSFLG,IVSNTB / KX,KY,KZ,THSAT,THRES,SS,N,ALF) ')
 9606 FORMAT(':VS06 cell sizes')
 9608 FORMAT(':VS08 no. of categories for layer definitions')
 9658 FORMAT(':VS08a category; no. of layers; soil/lith type; depth')
 9659 FORMAT(':VS08b category codes for links')
 9668 FORMAT(':VS08c  soil and aquifer distribution grid')
 9669 FORMAT(':VS09 soil type for each link')
 9670 FORMAT(':VS09a soil depth for each link')
 9610 FORMAT(':VS10 user defined connectivities')
 9611 FORMAT(':VS11 no. of categories for boundary conditions')
 9612 FORMAT(':VS17 - grid types of boundary conditions')
 9613 FORMAT(':VS18 - grid categories of boundary conditions')
 9614 FORMAT(':Base Flow boundary conditions (m3/s)')
 9701 FORMAT('Rundata file -Test data')
 9710 FORMAT('10: frame				---------- INPUT DATA')
 9711 FORMAT('11: VSS input data')
 9712 FORMAT('12: overland/channel')
 9713 FORMAT('13: evapotranspiration')
 9714 FORMAT('14: No longer used')
 9715 FORMAT('15: snowmelt')
 9716 FORMAT('16: bank element data')
 9717 FORMAT('17: sediment yield input')
 9718 FORMAT('18: solute/contaminant input')
 9719 FORMAT('19: hourly met. data			---------- MET. DATA')
 9720 FORMAT('20: precipitation data')
 9721 FORMAT('21: potential evaporation data')
 9722 FORMAT('22: time counter file			---------- OUTPUT DATA')
 9723 FORMAT('23: water flow print output')
 9724 FORMAT('24: sediment yield print ')
 9725 FORMAT('25: solute/contaminant print')
 9726 FORMAT('26: No longer used')
 9727 FORMAT('27: No longer used')
 9728 FORMAT('28: hostart file			-------INITIAL CONDITIONS ')
 9729 FORMAT('29: VSS initial conditions')
 9730 FORMAT('30: No longer used -----TIME-SERIES DATA')
 9731 FORMAT('31: well extraction (WLD)')
 9732 FORMAT('32: lateral subsurface flow boundary condition (LFB)')
 9733 FORMAT('33: lateral subsurface head boundary condition (LHB)')
 9734 FORMAT('34: lateral subsurface head gradient boundary condition (LGB)')
 9735 FORMAT('35: column base flow boundary condition (BFB)')
 9736 FORMAT('36: column base head boundary condition (BHB)')
 9737 FORMAT('37: overland/channel flow boundary condition (OFB)')
 9738 FORMAT('38: overland/channel head boundary condition (OHB)')
 9739 FORMAT('39: contaminant time-series 1 (CMT)')
 9740 FORMAT('40: contaminant time-series 2 (CMB)')
 9741 FORMAT('41: discharge at the outlet --------ADDITONAL OUTPUT')
 9742 FORMAT('42: vsi data for initial conditions')
 9743 FORMAT('43: mass balance data')
 9744 FORMAT('44: discharge at the outlet')
 9745 FORMAT('45: maximum air temperature  ')
 9746 FORMAT('46: minimum air temperature  ')
 9747 FORMAT('47: extra discharge points')
 9748 FORMAT('48: visualisation plan ------VISUALISATION OUTPUT')
 9749 FORMAT('49: check visulisation plan')
 9750 FORMAT('50: HDF output')
 9901 FORMAT('SM01: snow')
 9903 FORMAT('SM03: ddf, rhos, tsin, nsd, msm ')
 9907 FORMAT('SM07: uniform depth')



 9100 FORMAT(2I7)
 9101 FORMAT(5I7)
 9102 FORMAT(7A7)
 9103 FORMAT(5A7)
 9104 FORMAT(10A7)
 9105 FORMAT(4A7)
 9106 FORMAT(4A7)
 9107 FORMAT(4A7)
 9108 FORMAT(I7,1X,500I1)
 9109 FORMAT(I7,1X,500A1)
 9110 FORMAT(10(A7))
 9111 FORMAT(2I7,4(I7,A7))
 9113 FORMAT(5(I7,2A7))
 9115 FORMAT(I7,1X,72I1)
 9116 FORMAT(A70,A1)
 9117 FORMAT(I7,I7,I7,2A7)
 9118 FORMAT(I7,3A7)
 9119 FORMAT(A7,I7,2A7)
 9120 FORMAT(2I7)
 9121 FORMAT(a7,10I7)
 9122 FORMAT(11A7)
 9123 FORMAT(10i7)
 9124 FORMAT(5I7,1x,A7)
 9200 FORMAT(A80)
 9201 FORMAT(A20)
 9203 FORMAT(3A7,A20)
 9204 FORMAT(3A7,A10,a30)
 9205 FORMAT(I7,3A7,I7)
 9206 FORMAT(7X,I7,4A7)
 9207 FORMAT(I7,2A7,A10,a60)
 9208 FORMAT(I7,1X,500I6)
 9209 FORMAT(500I6)
 9210 FORMAT(4(A7))
 9211 FORMAT(I7)
 9255 FORMAT(7A7/5A7,I7,3A7)
 9260 FORMAT(A100)
 9261 FORMAT(A1,A2,I3,A2,I3,A40)
 9262 FORMAT(A60)
 9263 FORMAT(I4)
 9264 FORMAT(500I1)
 9265 FORMAT(500A1)
! sjb visualisation plan change these ok if > 10
 9271 FORMAT(A1,A2,I4,A2,I4,A40)
 9272 FORMAT(A1,A2,I4,A2,I3,A40)
 9273 FORMAT(A1,A2,I3,A2,I4,A40)
 9274 FORMAT(A1,A2,I3,A2,I3,A40)
 9275 FORMAT(A1,A2,I2,A2,I2,A40)
 9276 FORMAT(A1,A2,I2,A2,I3,A40)
 9277 FORMAT(A1,A2,I3,A2,I2,A40)
 9404 FORMAT(10(A7,1x))
 9405 FORMAT(a7,i7,a7,a7)
 
      
     call read_xml_file(xmlfilefull)

      if (trim(catchmentname(1)) == "Empty") then
          write (*,*) 'The CatchmentName is empty in the XML and it is compulsory. The tag is "CatchmentName". Please correct this error and try again'
          write(*,*)
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
     
     rivergridacci=nint(rivergridacc)


      Delimeter='/'
      Delimeter2='\'
      finaldel= index(xmlfilefull, Delimeter, .TRUE.)
      finaldel2= index(xmlfilefull, Delimeter2, .TRUE.)
      basedir=xmlfilefull(1:max(finaldel,finaldel2))
      write(*,*)
      write(*,*),"The working folder is:"
      write(*,*),basedir
      write(*,*)

      FILLOG = trim(basedir)//'input_'//trim(catchmentname(1))//'_log.txt'
      open(logfile,FILE=FILLOG,recl=200)

      write (logfile,*) 'XML Filename = ', trim(xmlfilefull)
      write (logfile,*)      
 
       if (trim(maskname(1)) == "Empty") then
          write (*,*) 'The mask filename is empty in the XML and it is compulsory. The tag is "MaskFileName". Please correct this error and try again'
          write(*,*)
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif          
      maskname=trim(basedir)//trim(maskname(1))
      write (logfile,*) 'Catchment Mask Filename = ', trim(maskname(1))
     

      if (trim(demmeanname(1)) == "Empty") then
          write (*,*) 'The mean DEM filename is empty in the XML and it is compulsory. The tag is "DEMMeanFileName". Please correct this error and try again'
          write(*,*)
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif          
      demmeanname=trim(basedir)//trim(demmeanname(1))
      write (logfile,*) 'DEM Mean Filname = ', trim(demmeanname(1))
 
      if ((trim(demminname(1)) == "Empty").or.(trim(demminname(1)).eq.('')).or.(trim(demminname(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Minimum DEM Filename = The minimum DEM filename is empty in the XML. The tag is "DEMminFileName". The mean DEM file will be used to caculate the location and elevation of channels'
          write(logfile,*) '********************************************'
          write(logfile,*)
          demminname=demmeanname
     else
          demminname=trim(basedir)//trim(demminname(1))
     endif          
     write (logfile,*) 'DEM Minimum Filname = ', trim(demminname(1))

 
      if ((trim(vegname(1)) == "Empty").or.(trim(vegname(1)).eq.('')).or.(trim(vegname(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Land use distribution Filename = The Land use distribution filename is empty in the XML. The tag is "VegMap". The same default land use will be assumed throught the catchment'
          write(logfile,*) '********************************************'
          write(logfile,*)
          vegname=maskname
     else
          vegname=trim(basedir)//trim(vegname(1))
     endif          
     write (logfile,*) 'Land Use Distribution Filname = ', trim(vegname(1))
     

     if ((trim(soilname(1)) == "Empty").or.(trim(soilname(1)).eq.('')).or.(trim(soilname(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Soil Category distribution Filename = The Soil Category distribution filename is empty in the XML.  The tag is "SoilMap". The same default land use will be assumed throught the catchment'
          write(logfile,*) '********************************************'
          write(logfile,*)
          soilname=maskname
     else
          soilname=trim(basedir)//trim(soilname(1))
     endif          
     write (logfile,*) 'Soil Category Distribution Filname = ', trim(soilname(1))
 
 
     if ((trim(lakename(1)) == "Empty").or.(trim(lakename(1)).eq.('')).or.(trim(lakename(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Lake distribution Filename = The Lake distribution filename is empty in the XML. The tag is "LakeMap". There are assumed to be no lakes'
          write(logfile,*) '********************************************'
          write(logfile,*)
          islakename=.false.
     else
          lakename=trim(basedir)//trim(lakename(1))
          write (logfile,*) 'Lake Distribution Filname = ', trim(lakename(1))
          islakename=.true.
    endif          
     
    
     
 
     if ((trim(precipname(1)) == "Empty").or.(trim(precipname(1)).eq.('')).or.(trim(precipname(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Precipitation Distribution Filename = The Precipitation distribution filename is empty in the XML. The tag is "PrecipMap". The same precipitation will be assumed throught the catchment'
          write(logfile,*) '********************************************'
          write(logfile,*)
          precipname=maskname
     else
          precipname=trim(basedir)//trim(precipname(1))
     endif          
     write (logfile,*) 'Precipitation Distribution Filname = ', trim(precipname(1))
     
     if ((trim(pename(1)) == "Empty").or.(trim(pename(1)).eq.('')).or.(trim(pename(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Potential Evaporation Distribution Filename = The Potential Evaporation distribution filename is empty in the XML.  The tag is "PeMap". The same PET will be assumed throught the catchment'
          write(logfile,*) '********************************************'
          write(logfile,*)
          pename=maskname
     else
          pename=trim(basedir)//trim(pename(1))
     endif          
     write (logfile,*) 'Potential Evaporation Distribution Filname = ', trim(pename(1))
     

      

      FILFRD = trim(basedir)//'input_'//trim(catchmentname(1))//'_frd.txt'
      OPEN (OUTFRD,FILE=FILFRD)
      FILFRD2 = 'input_'//trim(catchmentname(1))//'_frd.txt'
      FILOCD = trim(basedir)//'input_'//trim(catchmentname(1))//'_ocd.txt'
      OPEN (OUTOCD,FILE=FILOCD)
      FILOCD2 = 'input_'//trim(catchmentname(1))//'_ocd.txt'
      FILETD = trim(basedir)//'input_'//trim(catchmentname(1))//'_etd.txt'
      OPEN (OUTETD,FILE=FILETD)
      FILETD2 = 'input_'//trim(catchmentname(1))//'_etd.txt'
      if (IsSnow) then
        FILSMD = trim(basedir)//'input_'//trim(catchmentname(1))//'_smd.txt'
        OPEN (OUTSMD,FILE=FILSMD)
        FILSMD2 = 'input_'//trim(catchmentname(1))//'_smd.txt'
      endif
! banks are automatically set up if solute is true
      if (IsSolute) IsBanks = .True.
      if (IsBanks) then
        FILBKD = trim(basedir)//'input_'//trim(catchmentname(1))//'_bkd.txt'
        OPEN (OUTBKD,FILE=FILBKD)
        FILBKD2 = 'input_'//trim(catchmentname(1))//'_bkd.txt'
      endif
      if (IsSediment) then
        FILSYD = trim(basedir)//'input_'//trim(catchmentname(1))//'_syd.txt'
        OPEN (OUTSYD,FILE=FILSYD)
        FILSYD2 = 'input_'//trim(catchmentname(1))//'_syd.txt'
      endif

        ! Solute and contaminant transport are used interchangeably here. Solute is the TAG bname in the XML file but CMD (contaminant data) is the file name
      if (IsSolute) then
        FILCMD = trim(basedir)//'input_'//trim(catchmentname(1))//'_cmd.txt'
        OPEN (OUTCMD,FILE=FILCMD)
        FILCMD2 = 'input_'//trim(catchmentname(1))//'_cmd.txt'
      endif
      FILVSD = trim(basedir)//'input_'//trim(catchmentname(1))//'_vsd.txt'
      OPEN (OUTVSD,FILE=FILVSD)
      FILVSD2 = 'input_'//trim(catchmentname(1))//'_vsd.txt'
      FILRUN = trim(basedir)//'rundata_'//trim(catchmentname(1))//'.txt'
      OPEN (OUTRUN,FILE=FILRUN)
      FILRIV = trim(basedir)//'input_'//trim(catchmentname(1))//'_river_network.asc'
      OPEN (OUTRIV,FILE=FILRIV)
      FILRIVLOC = trim(basedir)//'input_'//trim(catchmentname(1))//'_river_location.txt'
      OPEN (OutRivLoc,FILE=FILRIVLOC)
      write(OutRivLoc,*) 'channel number, x value, y value, elevation'
      FILELMNUM = trim(basedir)//'input_'//trim(catchmentname(1))//'_element_number.asc'
      OPEN (OutElmNum,FILE=FILELMNUM)
      FILDISPOINT = trim(basedir)//'input_'//trim(catchmentname(1))//'_discharge_points.txt'
      OPEN (OutDischargePoints,FILE=FILDISPOINT)
      write(OutDischargePoints,'(A)') 'Extra discharge points. These will be included in the output_CATCHNAME_discharge_sim_regulartimestep.txt file. The format is SHETRAN element number and face number (1= E, 2=N, 3=W, 4=S)'
      write(OutDischargePoints,'(A,I0)') 'Number_of_points ',icountdischargepoints
      FILDISPOINT2 = 'input_'//trim(catchmentname(1))//'_discharge_points.txt'
      FILVIS = trim(basedir)//'input_'//trim(catchmentname(1))// '_visualisation_plan.txt'
      OPEN (OUTVIS,FILE=FILVIS)
      FILVIS2 = 'input_'//trim(catchmentname(1))//'_visualisation_plan.txt'
      FILTIM = 'output_'//trim(catchmentname(1))//'_tim.txt'
      FILPRI = 'output_'//trim(catchmentname(1))//'_pri.txt'

      FILDIS = 'output_'//trim(catchmentname(1))//'_discharge_sim_regulartimestep.csv'
      FILDIS2 = 'output_'//trim(catchmentname(1))//'_discharge_sim_everytimestep.csv'
      FILVSE = 'output_'//trim(catchmentname(1))//'_vsi.txt'
      FILMAS = 'output_'//trim(catchmentname(1))//'_mb.csv'
      FILCVI = 'output_'//trim(catchmentname(1))//'_check_vis_plan.txt'
      FILHDF =  'output_'//trim(catchmentname(1))//'_shegraph.h5'
      FILSPR =  'output_'//trim(catchmentname(1))//'_spr.txt'
      FILCPR =  'output_'//trim(catchmentname(1))//'_cpr.txt'

       if (icountbfb /= 0) then
        FILBFB = trim(basedir)//'input_'//trim(catchmentname(1))//'_bfb.txt'
        OPEN (OUTBFB,FILE=FILBFB)
        FILBFB2 = 'input_'//trim(catchmentname(1))//'_bfb.txt'
      endif







!      FILEO1=trim(basedir)//'\output_'//'developer-output.txt'
!      OPEN(30,FILE=FILEO1,STATUS='UNKNOWN')

      
      
! open and read mean DEM file      
      OPEN(InDemMean,FILE=demmeanname,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The mean DEM file ',trim(demmeanname(1)), ' specified in the tag DEMMeanFileName does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
      
! ncols and nrows are used to define the array sizes
      read(InDemMean,*) acols,ncols
      read(InDemMean,*) arows,nrows

! further down the code nrows=nrows+2 and  ncols=ncols+2
      nrowsmax=nrows+2
      ncolsmax=ncols+2

 !character
      allocate (xmap((nrowsmax+1)*2,(ncolsmax+1)*2))
      allocate (alinkew(nrowsmax+1,ncolsmax))
      allocate (alinkns(nrowsmax,ncolsmax+1))
      allocate (vismask(nrowsmax,ncolsmax))
      allocate (aform(ncolsmax*nrowsmax))
      xmap=''
      alinkew = ''
      alinkns = ''
      vismask = ''
      aform = ''
     
 !integer
      allocate (cornerval(nrowsmax+1,ncolsmax+1))
 
      ! array size for number of channel links e.g. poscol set to nrowsmax * ncolsmax
      allocate (poscol(ncolsmax*nrowsmax))
      allocate (posrow(ncolsmax*nrowsmax))
      allocate (catch(nrowsmax,ncolsmax))
      allocate (catchboundary(nrowsmax,ncolsmax))
      allocate (catchrow(ncolsmax))
      allocate (catchgeometry(-1:nrowsmax*10,-1:ncolsmax*10))
      allocate (vegdist(nrowsmax,ncolsmax))
      allocate (soildist(nrowsmax,ncolsmax))
      allocate (lakedist(nrowsmax,ncolsmax))
      allocate (pedist(nrowsmax,ncolsmax))
      allocate (raindist(nrowsmax,ncolsmax))
!      allocate (pedist2(nrowsmax,ncolsmax))
!      allocate (raindist2(nrowsmax,ncolsmax))
      allocate (NFMStorageDist(nrowsmax,ncolsmax))
      allocate (NFMStorageDist2(nrowsmax,ncolsmax))
      allocate (NFMForestDist(nrowsmax,ncolsmax))
      allocate (NFMForestDist2(nrowsmax,ncolsmax))
      allocate (accum(0:nrowsmax+1,0:ncolsmax+1))
      !array size for number of met and rainfall station set to nrowsmax * ncolsmax
      allocate (numbermet(ncolsmax*nrowsmax))
      allocate (elementnumber(nrowsmax,ncolsmax))     
!      allocate (peall(ncolsmax*nrowsmax))
!      allocate (peunique(ncolsmax*nrowsmax))
!      allocate (rainall(ncolsmax*nrowsmax))
!      allocate (rainunique(ncolsmax*nrowsmax))
!      allocate (petchange1(ncolsmax*nrowsmax))
!      allocate (petchange3(ncolsmax*nrowsmax))
      cornerval=0
      poscol=0
      posrow=0
      catch=0
      catchboundary=0
      catchrow=0
      catchgeometry=0
      vegdist=0
      soildist=0
      lakedist=0
      pedist=0
      raindist=0
!     pedist2=0
!     raindist2=0
      NFMStorageDist=0
      NFMStorageDist2=0
      NFMForestDist=0
      NFMForestDist2=0
      accum=0
      numbermet=0
      elementnumber = 0
 !     peall=0
 !     peunique=0
 !     rainall=0
 !     rainunique=0
 !     petchange1=0
 !     petchange3=0
     
      
!real
      allocate (dem(0:nrowsmax+1,0:ncolsmax+1))
      allocate (demrow(ncolsmax))
      allocate (demmean(0:nrowsmax+1,0:ncolsmax+1))
      allocate (demrowmean(ncolsmax))
      allocate (ellinkew(nrowsmax+1,ncolsmax))
      allocate (ellinkns(nrowsmax,ncolsmax+1))
      allocate (strlinkew(nrowsmax+1,ncolsmax))
      allocate (strlinkns(nrowsmax,ncolsmax+1))
      allocate (linkelv(ncolsmax*nrowsmax))
      allocate (linkstr(ncolsmax*nrowsmax))
      allocate (posval(ncolsmax*nrowsmax))
      allocate (pet(ncolsmax*nrowsmax))
      allocate (petchange2(ncolsmax*nrowsmax))
      dem=0.0
      demrow=0.0
      demmean=0.0
      ellinkew=0.0
      ellinkns=0.0
      strlinkew=0.0
      strlinkns=0.0
      linkelv=0.0
      linkstr=0.0
      posval=0.0
      pet=0.0
      petchange2=1.0
      
      !logical
      allocate (linkew(nrowsmax+1,ncolsmax+1))
      allocate (linkns(nrowsmax+1,ncolsmax+1))
      allocate (savelinkew(nrowsmax+1,ncolsmax+1))
      allocate (savelinkns(nrowsmax+1,ncolsmax+1))
      allocate (isaccum(nrowsmax,ncolsmax))
      allocate (cornerdone(nrowsmax+1,ncolsmax+1))
      allocate (savecornerdone(nrowsmax+1,ncolsmax+1))
      allocate (cornerdonep(nrowsmax+1,ncolsmax+1))
      allocate (streamsize(ncolsmax*nrowsmax))
      linkew=.false.
      linkns=.false.
      savelinkew=.false.
      savelinkns=.false.
      isaccum=.false.
      cornerdone=.false.
      savecornerdone=.false.
      cornerdonep=.false.
      streamsize=.false.

      read(InDemMean,*) axllcorner,xllcorner
      read(InDemMean,*) ayllcorner,yllcorner
      read(InDemMean,*) acellsize,cellsize 
      read(InDemMean,*) anovalue,novalueDEM
      do i=2,nrows+1
          read(InDemMean,*) (demrowmean(j),j=2,ncols+1)
          do j=2,ncols+1
             demmean(i,j) = demrowmean(j)
          enddo
      enddo
      close(InDemMean)

      
      
      
      
      
! open and read mask file      
      OPEN(InMask,FILE=maskname,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The mask file ',trim(maskname(1)), ' specified in the tag DEMMMaskFileName does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
      read(InMask,*) acols,ncols2
      if (ncols.ne.ncols2) then
          write(*,*),'Number of columns not equal in the elevation and catchment grids' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
     endif
     read(InMask,*) arows,nrows2
     if (nrows.ne.nrows2) then
          write(*,*),'Number of rows not equal in the elevation and catchment grids' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
         stop
     endif
     read(InMask,*) axllcorner,xllcorner
     read(InMask,*) ayllcorner,yllcorner
     read(InMask,*) acellsize,cellsize 
     read(InMask,*) anovalue,novalue
      do i=1,nrows+2
          do j=1,ncols+2
              catch(i,j) = novalue
          enddo
      enddo
      do i=2,nrows+1
          read(InMask,*) (catchrow(j),j=2,ncols+1)
          do j=2,ncols+1
             catch(i,j) = catchrow(j)
          enddo
      enddo

      do i=2,nrows+1
           do j=2,ncols+1
              if (catch(i,j).ne.novalue) then
                if ((catch(i-1,j).eq.novalue).and.(catch(i+1,j).eq.novalue).and.(catch(i,j+1).eq.novalue).and.(catch(i,j-1).eq.novalue)) then
                    write(*,*)

                    write(*,*),'*********************************'

                    write(*,*), 'There is grid square in the mask not adjacent horizontally or vertically to another grid square. This grid square has been removed'
                    write(*,*),'the issue is in row ',i-1,'column ',j-1

                    write(*,*)

                    write (logfile,*)
                    write (logfile,*), 'There is grid square in the mask not adjacent horizontally or vertically to another grid square. This grid square has been removed'
                    write (logfile,*) 'The issue is in row ',i-1,'column ',j-1
                    write (logfile,*)
   
                    catch(i,j)=novalue
                endif
             endif
          enddo
      enddo
      close(InMask)
      
      
      
      
      
 ! open and read min DEM file      
      OPEN(InDemMin,FILE=demminname,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The DEM minimum file ',trim(demminname(1)), ' specified in the tag DEMminFileName does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
      read(InDemMin,*) acols,ncols3
      if (ncols.ne.ncols3) then
          write(*,*),'Number of columns not equal in the elevation grids' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
      read(InDemMin,*) arows,nrows3
      if (nrows.ne.nrows3) then
          write(*,*), 'Number of rows not equal in the elevation grids' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
      stop
      endif
      read(InDemMin,*) axllcorner,xllcorner
      read(InDemMin,*) ayllcorner,yllcorner
      read(InDemMin,*) acellsize,cellsize 
      read(InDemMin,*) anovalue,novalueDEM
      do i=2,nrows+1
          read(InDemMin,*) (demrow(j),j=2,ncols+1)
          do j=2,ncols+1
             dem(i,j) = demrow(j)
          enddo
      enddo
      close(InDemMin)

   
      
      
      

       
  ! open and read lake file      
      if(islakename) then
          OPEN(InLake,FILE=lakename(1),STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The Lake Map file ',trim(lakename(1)), ' specified in the tag LakeMap does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
          read(InLake,*) 
          read(InLake,*) 
          read(InLake,*) 
          read(InLake,*) 
          read(InLake,*) 
          read(InLake,*) 
          do i=2,nrows+1
          read(InLake,*) (lakedist(i,j),j=2,ncols+1)
             do j=2,ncols+1
                 if (lakedist(i,j).lt.0) then 
                   lakedist(i,j)=0
                endif
             enddo
         enddo
      close(InLake)
      else 
         do i=2,nrows+1
             do j=2,ncols+1
                lakedist(i,j)=0
             enddo
         enddo
      endif

!***************************check start and end dates**************************
      if (year<1950) then 
          write(*,*),'The starting year must be great than or equal to 1950' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
       if (month<1.or.month>12) then 
          write(*,*),'The starting month must be between 1 and 12' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
       if (day<1.or.day>31) then 
          write(*,*),'The starting day must be between 1 and 31' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
       if (hour<0) then 
          write(*,*),'The starting hour must be greater than or equal to 0' 
          write(*,*),'The value has been set to 0' 
          hour = 0
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
     endif
       if (hour>23) then 
          write(*,*),'The starting hour must be less than or equal to 23' 
          write(*,*),'The value has been set to 23' 
          hour = 23
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
      endif
       if (minute<0) then 
          write(*,*),'The starting minute must be greater than or equal to 0' 
          write(*,*),'The value has been set to 0' 
          minute = 0
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
     endif
       if (minute>59) then 
          write(*,*),'The starting day must be less than or equal to 59' 
          write(*,*),'The value has been set to 59' 
          minute = 59
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
     endif
       if (endyear<1950) then 
          write(*,*),'The ending year must be great than or equal to 1950' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
       if (endmonth<1.or.endmonth>12) then 
          write(*,*),'The ending month must be between 1 and 12' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
       if (endday<1.or.endday>31) then 
          write(*,*),'The ending day must be between 1 and 31' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
       endif
       if (endhour<0) then 
          write(*,*),'The ending hour must be greater than or equal to 0' 
          write(*,*),'The value has been set to 0' 
          endhour = 0
          write(*,'(''paused, type [enter] to continue'')')
           read (*,*)
     endif
       if (endhour>23) then 
          write(*,*),'The ending hour must be less than or equal to 23' 
          write(*,*),'The value has been set to 23' 
          endhour = 23
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
      endif
       if (endminute<0) then 
          write(*,*),'The ending minute must be greater than or equal to 0' 
          write(*,*),'The value has been set to 0' 
          endminute = 0
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
      endif
       if (endminute>59) then 
          write(*,*),'The ending day must be less than or equal to 59' 
          write(*,*),'The value has been set to 59' 
          endminute = 59
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
      endif
       StartFrom1950 = hour_from_date(year, month, day, hour, minute)
       EndFrom1950 = hour_from_date(endyear, endmonth, endday, endhour, endminute)
       simulationTimeHours=EndFrom1950-StartFrom1950
       if (simulationTimeHours<1) then
          write(*,*),'The end simulation time is before or equal to the start simulation time' 
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
       endif

        
! *************************************************************
!Temperature time series data
! ************************************************************
     if ((trim(tmaxfile(1)) == "Empty").or.(trim(tmaxfile(1)).eq.('')).or.(trim(tmaxfile(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'The Minimum Temperature Time Series File Name = The Miniumum Temperature Time Series File is empty in the XML. The tag is "MaxTempTimeSeriesData". This is used in the snow melt component and if there is no file specified a constant value of 10C is used'
          write(logfile,*) '********************************************'
          write(logfile,*)
     else
          write (logfile,*) 'Maximum Temperature Time Series File Name = ', trim(tmaxfile(1))
     endif          
     
     if ((trim(tminfile(1)) == "Empty").or.(trim(tminfile(1)).eq.('')).or.(trim(tminfile(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'The Minimum Temperature Time Series File Name = The Miniumum Temperature Time Series File is empty in the XML. The tag is "MinTempTimeSeriesData". This is used in the snow melt component and if there is no file specified a constant value of 10C is used'
          write(logfile,*) '********************************************'
          write(logfile,*)
     else
          write (logfile,*) 'Minimum Temperature Time Series File Name = ', trim(tminfile(1))
     endif          
       
   
    
    
      depth(1:27) = (/ 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0 /)
      call rootdensity(rdf)

      
      
!*****************Time series for canopy storage*******
!******************************************************
! this is optional and not usually used
      allocate (cstcapnopoints(icountveg))
     filecstcap=trim(basedir)//'canopystorage.txt'
!if the file is not present then time varying canopy storage is not used (normal case)
      open(InConopyStorage,FILE=filecstcap,STATUS='OLD',iostat=istatus)
      
      if (istatus/=0) then
         do i=1,icountveg
            cstcapnopoints(i)=0
         enddo
     else
      
! find number of data point to allocate array sizes      
      read(InConopyStorage,*,err=901)
      read(InConopyStorage,*,err=901)
      read(InConopyStorage,*,err=901) cstcapnoveg
      do i=1,cstcapnoveg
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) cstcapnopoints(i)
          maxcstcapnopoints = max(maxcstcapnopoints,cstcapnopoints(i))
!          print*,cstcapnopoints(i)
          if (cstcapnopoints(i).gt.0) then
              do j=1,cstcapnopoints(i)
                 read(InConopyStorage,*,err=901) 
             enddo
          endif 
      enddo

      allocate (cstcapratio(icountveg,maxcstcapnopoints))
      allocate (cstcaptime(icountveg,maxcstcapnopoints))

!read in the canopy storage data      
      rewind(InConopyStorage)    
      read(InConopyStorage,*,err=901)
      read(InConopyStorage,*,err=901)

      read(InConopyStorage,*,err=901) cstcapnoveg
      do i=1,cstcapnoveg
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) 
          read(InConopyStorage,*,err=901) cstcapnopoints(i)
          maxcstcapnopoints = max(maxcstcapnopoints,cstcapnopoints(i))
!          print*,cstcapnopoints(i)
          if (cstcapnopoints(i).gt.0) then
              do j=1,cstcapnopoints(i)
                 read(InConopyStorage,*,err=901) cstcapratio(i,j),cstcaptime(i,j)
!                print*,cstcapratio(i,j),cstcaptime(i,j)
             enddo
          endif 
      enddo
      do i=cstcapnoveg+1,icountveg
         cstcapnopoints(i)=0
      enddo
      
    endif
    close(InConopyStorage)
!*****************Endo ofTime series for canopy storage
!******************************************************


      
      
!*****************Spatial variable Water Table*******
!******************************************************
! this is optional. The simple method is to use the Initial water table depth below ground.
! if SpatialVariableInitCond = True and the file output_CATCHNAME_vsi.txt present then sptial psl is used. Heads in each cell are defined
if (isspatialpsl) then
    
     filespatialpsl=trim(basedir)//'output_'//trim(catchmentname(1))//'_vsi.txt'
     filespatialpslout=trim(basedir)//'input_'//trim(catchmentname(1))//'_vsi.txt'
     filespatialpslout2='input_'//trim(catchmentname(1))//'_vsi.txt'
      inquire(file=filespatialpsl, exist=file_exists)
      if (.not.file_exists) then
           isspatialpslfile=.False.
          write(logfile,*) 'The file ',trim(filespatialpsl),' has not been opened and so spatially variable iniitial water table depths will not be applied'
      else
        open(InSpatialPSL,FILE=filespatialpsl)
        isspatialpslfile=.true.
         iswritefile=.false.
         open(OutSpatialPSL,FILE=filespatialpslout,STATUS='Replace',iostat=istatus)
         write(logfile,*) 'Initial spatially variable water table values have been written to this file ' // trim(filespatialpslout) // '. This is achieved by defining the heads in each cell'
        do 
             read(InSpatialPSL,'(A)',iostat=io) spatialpslline
             if (io /= 0) exit   ! exits on EOF (<0) or read error (>0)
             if (trim(spatialpslline) == ' Heads at end of simulation') then
                 iswritefile=.true.
                 spatialpslline = 'Heads at the start of the simulation. An element number is specified then the value in each cell in that element is given starting at the bottom cell'
             endif
             if (iswritefile) then
                  write (OutSpatialPSL,'(A)') trim(spatialpslline)
             endif
        enddo
                
            
      endif
      close(InSpatialPSL)
endif
!*****************Endo ofTime series for sptial initial water table
!******************************************************



      





!     add an extra grid square around entire catchment
!     to stop a bug in SHETRAN
     nrows=nrows+2
      ncols=ncols+2

      do i=1,nrows
          do j=1,ncols
             if (catch(i,j).eq.novalue) then
                  demmean(i,j) = 1.0e10
             endif
          enddo
      enddo


      do i=1,nrows
          do j=1,ncols
             if (catch(i,j).eq.novalue) then
                  dem(i,j) = 1.0e10
             endif
          enddo
      enddo



!      print*,'t1'



!************************ Title *****************************************      
      write (MSG,9301)
      write (OUTFRD,9200) MSG

!********* Number of Grid Squares ***************************************      
      write (MSG,9302)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9100) ncols,nrows

!********* Start/End of simulations **************************************      
      write (MSG,9304)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9101) year,month,day,hour,minute

      write (MSG,9306)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9101) endyear,endmonth,endday,endhour,endminute

      write (MSG,9391)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9101) year,month,day,hour,minute

      write (MSG,9392)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9101) year,month,day,hour,minute
      
!******************* Grid Sizes ****************************************
      ncolsm1=ncols-1
      write (MSG,9308)
      write (OUTFRD,9200) MSG
      do I = 1,ncolsm1
         AFORM(I) =FORM(cellsize)
      enddo
      write(OUTFRD,9110) (AFORM(I),I=1,ncolsm1)
      
      nrowsm1=nrows-1
      write (MSG,9310)
      write (OUTFRD,9200) MSG
      do I = 1,nrowsm1
         AFORM(I) =FORM(cellsize)
      enddo
      write(OUTFRD,9110) (AFORM(I),I=1,nrowsm1)

!*************** Print output timestep *********************************
      write (MSG,9312)
      write (OUTFRD,9200) MSG
      AFORM(1) = FORM(1.0)
      AFORM(2) = FORM(20000.0)
      write (OUTFRD,9102) AFORM(1),'2','T','F','F','F',AFORM(2)

!***************** Basic timestep data **********************************
      write (MSG,9320)
      write (OUTFRD,9200) MSG
      AFORM(1) = FORM(maxrainfalltimestep)
      AFORM(2) = FORM(increasingtimestep)
      AFORM(3) = FORM(99999.)
      AFORM(4) = FORM(standardtimestep)
      write (OUTFRD,9103) AFORM(1),AFORM(2),AFORM(3),AFORM(4),'T'

!****************** Print control parameters ***************************      
      write (MSG,9322)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9104) 'F','F','F','F','F','F','F','F','F','F'

!**************** Component execution control parameters ****************      
      write (MSG,9324)
      write (OUTFRD,9200) MSG
      write (OUTFRD,'(4(6X,L1))') issnow, isbanks,issediment,issolute
      
!***************** Hotstart parameters *********************************
      write (MSG,9326)
      write (OUTFRD,9200) MSG
      AFORM(1)=FORM(0.0)
      AFORM(2)=FORM(0.0)
      write (OUTFRD,9106) 'F','F',AFORM(1),AFORM(2)

!**************************** NFM Storage and forest************************
      IsStorageFile=.true.
      NFMStorageName=trim(basedir)//trim(catchmentname(1))//'_NFM_storage.asc'
      OPEN(NFMStorage,FILE=NFMStorageName,STATUS='OLD',IOSTAT=io)
      
      If (io /=0) then
          IsStorageFile=.false.
      else
! extra storage and woodland is optional and not usually used 
      write (logfile,*) 'NFM_storage distribution Filename = ',trim(NFMStorageName)
         read(NFMStorage,*) 
         read(NFMStorage,*) 
         read(NFMStorage,*) 
         read(NFMStorage,*) 
         read(NFMStorage,*) 
         read(NFMStorage,*) 
         do i=2,nrows-1
            read(NFMStorage,*) (NFMStorageDist(i,j),j=2,ncols-1)
         enddo
      endif
      close(NFMStorage)
        
      IsForestFile=.true.
      NFMForestName=trim(basedir)//trim(catchmentname(1))//'_NFM_woodland.asc'
      OPEN(NFMForest,FILE=NFMForestName,STATUS='OLD',IOSTAT=io)
      If (io /=0) then
          IsForestFile=.false.
      else
      write (logfile,*) 'NFM_forest distribution Filename = ',trim(NFMForestName)
         read(NFMForest,*) 
         read(NFMForest,*) 
         read(NFMForest,*) 
         read(NFMForest,*) 
         read(NFMForest,*) 
         read(NFMForest,*) 
         do i=2,nrows-1
            read(NFMForest,*) (NFMForestDist(i,j),j=2,ncols-1)
         enddo
      endif
      close(NFMForest)

!***************************** PE Types ************************
      OPEN(InPE,FILE=pename,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The potential evaporation distribution map file ',trim(pename(1)), ' specified in the tag PeMap does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
      read(InPE,*) 
      read(InPE,*) 
      read(InPE,*) 
      read(InPE,*) 
      read(InPE,*) 
      read(InPE,*) 
! In previous version PEdist was sorted and data in the time series file corresponded with the sorted value.
! Now values in PEdist are assumed to correspond to column numbers in the time series file
      do i=2,nrows-1
         read(InPE,*) (pedist(i,j),j=2,ncols-1)
      enddo
      PeDistMax= maxval(pedist)


!****************************** rain Types ************************

      OPEN(InPrec,FILE=precipname,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The precipitation distribution map file ',trim(precipname(1)), ' specified in the tag PrecipMap does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
      read(InPrec,*) 
      read(InPrec,*) 
      read(InPrec,*) 
      read(InPrec,*) 
      read(InPrec,*) 
      read(InPrec,*) 
! In previous version where Raindist was sorted and data in the time series file corresponded with the sorted value.
! Now values in Raindist are assumed to correspond to column numbers in the time series file
      do i=2,nrows-1
         read(InPrec,*) (raindist(i,j),j=2,ncols-1)
      enddo
      RainDistMax= maxval(raindist)

       
! *************************************************************
! Precipitation time series data
! ************************************************************
     if ((trim(precfile(1)) == "Empty").or.(trim(precfile(1)).eq.('')).or.(trim(precfile(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Precipitation Time Series File Name = The Precipitation Time Series File is empty in the XML. The tag is "PrecipitationTimeSeriesData". Sample precipitation data will be used'
          write(logfile,*) '********************************************'
          write(logfile,*)
          precfilescaler='Empty'
     else
          precfilescaler=trim(basedir)//trim(precfile(1))
          write(logfile,*)
          write (logfile,*) 'Precipitation Time Series File Name = ', trim(precfilescaler)
     endif          

       

      if (trim(precfilescaler).eq.'Empty') then
! if no precipitation data then use the sample data  
! use the correct number of value (to the nearest year) and write it to the prd file
         precfilescaler =trim(basedir)//'input_'//'sample_prd.txt'
         open(SamplePRD,FILE=precfilescaler)
         write(SamplePRD,*) 'Sample Precipitation Data. Daily Values with units of mm/day'
         filprd ='input_'//'sample_prd.txt'
         call PrecipTest(PrecipTestData)
         do i = 1,int(simulationTimeHours/8760)+1
             do j=1,365
                  write(SamplePRD,'(F8.2)') PrecipTestData(j)
             enddo
         enddo
         close(SamplePRD)
      else 
        filprd=precfile(1)
      endif

    
      

 ! check precipitation time series data exists     
      OPEN(InPrecTS,FILE=precfilescaler,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The precipitation time series file ',trim(precfilescaler), ' specified in the tag PrecipitationTimeSeriesData does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
     write(logfile,'(A,I0,A)') ' The precipitation time series file should have a single header line and then at least ', RainDistMax, ' values (this is the maximum value in the PrecMap file) on each line'
         
    close(InPrecTS)
      

      
        
! *************************************************************
! PET time series data
! ************************************************************
    
     
     if ((trim(pefile(1)) == "Empty").or.(trim(pefile(1)).eq.('')).or.(trim(pefile(1)).eq.('none'))) then
          write(logfile,*)
          write(logfile,*) '***********NOTE*****************************'
          write (logfile,*) 'Potential Evaporation Time Series File Name = The Potential Evaporation Time Series File is empty in the XML. The tag is "EvaporationTimeSeriesData". Sample potential evaporation data will be used'
          write(logfile,*) '********************************************'
          write(logfile,*)
          pefilescaler='Empty'
     else
          pefilescaler=trim(basedir)//trim(pefile(1))
           write(logfile,*)
         write (logfile,*) 'Potential Evaporation Time Series File name = ', trim(pefilescaler)
     endif          
       
      
      if (trim(pefilescaler).eq.'Empty') then
! if no  PET data then use the sample data       
! use the correct number of values and write it to the epd file
         pefilescaler = trim(basedir)//'input_'//'sample_epd.txt'
         OPEN(SampleEPD,FILE=pefilescaler)
         write(SampleEPD,*) 'Sample Potential Evaporation Data. Daily Values with units of mm/day'
         filepd ='input_'//'sample_epd.txt'
         call PetTest(PetTestData)
         do i = 1,int(simulationTimeHours/8760)+1
             do j=1,365
                  write(SampleEPD,'(F8.2)') PetTestData(j)
             enddo
        enddo
         close(SampleEPD)
    else 
        filepd=pefile(1)
    endif

       

! check PE time series data exists     
      OPEN(InPeTS,FILE=pefilescaler,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The potential evaporation time series file ',trim(pefilescaler), ' specified in the tag EvaporationTimeSeriesData does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
          endif
          write(logfile,'(A,I0,A)') ' The potential evaporation time series file should have a single header line and then at least ', PEDistMax, ' values (this is the maximum value in the PEMap file) on each line'
          write(logfile,*)
          close(InPeTS)
   
       
      

      
      
      
      
!********** Number of meteorological stations, raingauges  **************
!********** vegetation types and soil types(no longer used her) *********
      write (MSG,9328)
      write (OUTFRD,9200) MSG
      if (IsForestFile) then
         write (OUTFRD,9117) PedistMax,RainDistMax,icountveg+10,'1','1'
      else
         write (OUTFRD,9117) PedistMax,RainDistMax,icountveg,'1','1'
      endif


!***************** River lining parameters ******************************
      write (MSG,9330)
      write (OUTFRD,9200) MSG
      AFORM(1)=FORM(0.1)
      AFORM(2)=FORM(0.0)
      write (OUTFRD,9107) 'F',AFORM(1),AFORM(2),'F'

!********* Default values for meteorological, rain, vegetation ********* 
!************************ and soil codes  ******************************
      write (MSG,9332)
      write (OUTFRD,9200) MSG
      write (OUTFRD,9107) '0','0','0','1'

!******** start of section for elevation,channel,etc
!***************************************************
!     fill surrounding elements
      do i=0,0
          do j=0,ncols+1
              demmean(i,j) = 1.0e10
          enddo
      enddo
      do i=nrows+1,nrows+1
          do j=0,ncols+1
              demmean(i,j) = 1.0e10
          enddo
      enddo
      j=0
      do i=1,nrows
          demmean(i,j) = 1.0e10
      enddo
      j=ncols+1
      do i=1,nrows
          demmean(i,j) = 1.0e10
      enddo

      do i=0,0
          do j=0,ncols+1
              dem(i,j) = 1.0e10
          enddo
      enddo
      do i=nrows+1,nrows+1
          do j=0,ncols+1
              dem(i,j) = 1.0e10
          enddo
      enddo
      j=0
      do i=1,nrows
          dem(i,j) = 1.0e10
      enddo
      j=ncols+1
      do i=1,nrows
          dem(i,j) = 1.0e10
      enddo

         

! check that all the elevations are greater than zero
! correct bad elevation 30032012
      badnum=0
      do i=1,nrows
          do j=1,ncols
             if (demmean(i,j).le.0.0) then
!             badnum=badnum+1
             write(*,'(A50,A44)') 'There was a grid square within the catchment mask with an elevation equal to or less than zero'
             write(*,'(A14,i4, A12,I4)') 'Located at row', i-1, '  and column', j-1
             write(*,*)
             write(*,'(A46)') 'Shetran will attempt to automatically correct'
             write(*,*)
             write(*,'(''paused, type [enter] to continue'')')
             read (*,*)
                if (catch(i,j+1).ne.novalue) then
                   demmean(i,j)=max(demmean(i,j),demmean(i,j+1))
                endif
                if (catch(i+1,j+1).ne.novalue) then
                   demmean(i,j)=max(demmean(i,j),demmean(i+1,j+1))
                endif
                if (catch(i+1,j).ne.novalue) then
                   demmean(i,j)=max(demmean(i,j),demmean(i+1,j))
                endif
                if (catch(i+1,j-1).ne.novalue) then
                  demmean(i,j)=max(demmean(i,j),demmean(i+1,j-1))
                endif
                if (catch(i,j-1).ne.novalue) then
                  demmean(i,j)=max(demmean(i,j),demmean(i,j-1))
                endif
                if (catch(i-1,j-1).ne.novalue) then
                  demmean(i,j)=max(demmean(i,j),demmean(i-1,j-1))
                endif
                if (catch(i-1,j+1).ne.novalue) then
                  demmean(i,j)=max(demmean(i,j),demmean(i-1,j+1))
                endif
                if (catch(i-1,j+1).ne.novalue) then
                 demmean(i,j)=max(demmean(i,j),demmean(i-1,j+1))
                endif
                if  (demmean(i,j).le.0.0) then
                  write(*,*)
                  write(*,'(A42)') 'Shetran is unable to correct this problem'
                  write(*,'(A33)') 'The simulation will probably fail'
                  write(*,*)
                  write(*,'(''paused, type [enter] to continue'')')
                  read (*,*)
                endif
             endif
          enddo
      enddo

      badnum=0
      do i=1,nrows
          do j=1,ncols
             if (dem(i,j).le.0.0) then
!             badnum=badnum+1
             write(*,'(A50,A44)') 'There was a grid square within the catchment mask with an elevation equal to or less than zero'
             write(*,'(A14,i4, A12,I4)') 'Located at row', i-1, '  and column', j-1
             write(*,*)
             write(*,'(A46)') 'Shetran will attempt to automatically correct'
             write(*,*)
!             pause
               if (catch(i,j+1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i,j+1))
               endif
               if (catch(i+1,j+1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i+1,j+1))
               endif
               if (catch(i+1,j).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i+1,j))
               endif
               if (catch(i+1,j-1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i+1,j-1))
               endif
               if (catch(i,j-1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i,j-1))
               endif
               if (catch(i-1,j-1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i-1,j-1))
               endif
               if (catch(i-1,j+1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i-1,j+1))
               endif
               if (catch(i-1,j+1).ne.novalue) then
                  dem(i,j)=max(dem(i,j),dem(i-1,j+1))
               endif
               if  (dem(i,j).le.0.0) then
                 write(*,*)
                 write(*,'(A42)') 'Shetran is unable to correct this problem'
                 write(*,'(A33)') 'The simulation will probably fail'
                 write(*,*)
                 write(*,'(''paused, type [enter] to continue'')')
                 read (*,*)
               endif
             endif
          enddo
      enddo



!!! NEW CODE 161112       real demne,demse,demsw,demnw,demmincorner,demminedge
!!! If there is a drop diagonally but not one sideways the sideway value is dropped
      
!     Find min elevation within the catchment
!     This must be at the catchment outlet
      demmin=2.0e10
      do i=1,nrows
          do j=1,ncols
             if (dem(i,j).lt.demmin) then
                  demmin=dem(i,j)
                  colposmin=j
                  rowposmin=i
             endif
          enddo
      enddo

    
    
    
      do i=1,nrows
         do j=1,ncols      
            demn=dem(i-1,j)
            dems=dem(i+1,j)
            deme=dem(i,j+1)
            demw=dem(i,j-1)


            demne=dem(i-1,j+1)
            demse=dem(i+1,j+1)
            demsw=dem(i+1,j-1)
            demnw=dem(i-1,j-1)



            demminedge=min(demn,dems,deme,demw)
            demmincorner=min(demne,demse,demnw,demsw)
            if ((j.eq.colposmin).and.(i.eq.rowposmin)) then
                notlowpoint=.false.
            else
                notlowpoint=.true.
            endif
            if ((dem(i,j).le.demminedge).and.(notlowpoint).and.(catch(i,j).ne.novalue)) then
!                       write(823,*) i,j,demminedge
                 if (dem(i,j).ge.demmincorner) then
                      if (demmincorner.eq.demne) then
!               write(823,*) 'ne',i,j,dem(i,j),demn,dems,deme,demw,demne
                         if (demn.lt.deme) then
                            dem(i-1,j) = max((demne+dem(i,j))/2.0,demmin)
                         else   
                            dem(i,j+1) =max((demne+dem(i,j))/2.0,demmin)
                         endif
                      endif
                      if (demmincorner.eq.demnw) then
!                       write(823,*) 'nw',i,j
                         if (demn.lt.demw) then
                            dem(i-1,j) =max((demnw+dem(i,j))/2.0,demmin)
                         else   
                            dem(i,j-1) =max((demnw+dem(i,j))/2.0,demmin)
                         endif
                     endif
                     if (demmincorner.eq.demsw) then
!                       write(823,*) 'sw',i,j
                         if (dems.lt.demw) then
                            dem(i+1,j) =max((demsw+dem(i,j))/2.0,demmin)
                         else   
                            dem(i,j-1) =max((demsw+dem(i,j))/2.0,demmin)
                         endif
                     endif
                     if (demmincorner.eq.demse) then
!                        write(823,*) 'se',i,j
                          if (dems.lt.deme) then
                            dem(i+1,j) =max((demse+dem(i,j))/2.0,demmin)
                          else   
                            dem(i,j+1) =max((demse+dem(i,j))/2.0,demmin)
                          endif
                     endif
                 endif
              endif
           enddo
      enddo

!!!END OF NEW CODE 161112



      
!     Find min elevation within the catchment
!     This must be at the catchment outlet
      do
      demmin=2.0e10
         do i=1,nrows
            do j=1,ncols
                if (dem(i,j).lt.demmin) then
                  demmin=dem(i,j)
                  colposmin=j
                  rowposmin=i
                endif
           enddo
         enddo

!     test to see if min at catchment boundary ?
         demn=dem(rowposmin,colposmin-1)
         dems=dem(rowposmin,colposmin+1)
         deme=dem(rowposmin+1,colposmin)
         demw=dem(rowposmin-1,colposmin)
         if ((demn.gt.0.99e10).or.(dems.gt.0.99e10).or.(deme.gt.0.99e10).or.(demw.gt.0.99e10)) then
            isok=.true.
         endif

!     if not at boundary add 1.0m to min. elevation and try again
         if (.not.isok) then
            dem(rowposmin,colposmin)=demmin+1.0
         else
            exit
         endif
      enddo



      write(*,*)
      write(*,'(A31,F8.2)') 'Minimum catchment elevation is ',demmin 
      write(*,'(A7,I4,A12,I4)') 'At row ', rowposmin,' and column ',colposmin
      write(*,*)


!!!! Change to this bit of code demn, etc and demminedge 161112      
!     Remove sinks by removesink. Count is number of sinks
    do
        count=0
        do i=1,nrows
             do j=1,ncols      
                  demn=dem(i-1,j)
                  dems=dem(i+1,j)
                  deme=dem(i,j+1)
                  demw=dem(i,j-1)
                  demminedge=min(demn,dems,deme,demw)
                  if ((j.eq.colposmin).and.(i.eq.rowposmin)) then
                      notlowpoint=.false.
                  else
                      notlowpoint=.true.
                  endif
                  if ((dem(i,j).le.demminedge).and.(notlowpoint).and.(catch(i,j).ne.novalue)) then
                       dem(i,j)=demminedge+removesink
                       count=count+1
                  endif
              enddo
        enddo
!          print*,count
        if (count==0) exit
    enddo

!!!! END of code change




      

!     order the elements in the catchment.
!     posval(1) is the dem of lowest elevation
!     poscol(1) is its column position
!     posrow(1) is its row  position
      number=0
      do i=1,nrows
         do j=1,ncols
             if (dem(i,j).lt.0.99e10) then
                  number=number+1
                  posval(number)=dem(i,j)
                  poscol(number)=j
                  posrow(number)=i
             endif
         enddo
      enddo
 
      do
         change=0          
!     uses a very basic bubble sort
         do i=2,number
           if (posval(i).lt.posval(i-1)) then
              temp=posval(i-1)
              posval(i-1)=posval(i)
              posval(i)=temp
              itemp=poscol(i-1)
              poscol(i-1)=poscol(i)
              poscol(i)=itemp
              itemp=posrow(i-1)
              posrow(i-1)=posrow(i)
              posrow(i)=itemp
              change=change+1
           endif
        enddo
        if (change==0) exit
    enddo

!     accumulate water flow
!     Basic assumption is that water accumulates along steepest gradient
      cv=poscol(1)
      rv=posrow(1)
      accum(rv,cv)=0
      do i=number,2,-1
          cv=poscol(i)
          rv=posrow(i)
          mindem = min(dem(rv,cv+1),dem(rv,cv-1),dem(rv+1,cv),dem(rv-1,cv))
!     easterly direction
          if (dem(rv,cv+1).eq.mindem) then
              rvadd=rv
              cvadd=cv+1
!     used for outlet element. to find out which direction
!     water is leaving the catchment
              if (i.eq.2) then
                 direction=2
              endif
!     westerly direction
          elseif (dem(rv,cv-1).eq.mindem) then
              rvadd=rv
              cvadd=cv-1
              if (i.eq.2) then
                direction=4
              endif
!     southerly direction
          elseif (dem(rv+1,cv).eq.mindem) then
              rvadd=rv+1
              cvadd=cv
              if (i.eq.2) then
                 direction=3
              endif
!     northerly direction
          elseif (dem(rv-1,cv).eq.mindem) then
              rvadd=rv-1
              cvadd=cv
              if (i.eq.2) then
                 direction=1
              endif
          endif
          accum(rvadd,cvadd)=accum(rv,cv)+accum(rvadd,cvadd)+1
      enddo


! new code 180907 if minmium elevevation is surrounded by 3 other
! elements within the catchment then the output direction is known
      cv=poscol(1)
      rv=posrow(1)
      numberedge=0
      if (dem(rv,cv+1).gt.0.99e10) numberedge=numberedge+1
      if (dem(rv,cv-1).gt.0.99e10) numberedge=numberedge+1
      if (dem(rv+1,cv).gt.0.99e10) numberedge=numberedge+1
      if (dem(rv-1,cv).gt.0.99e10) numberedge=numberedge+1
      if (numberedge.eq.1) then
        if (dem(rv-1,cv).gt.0.99e10) direction=1
        if (dem(rv,cv+1).gt.0.99e10) direction=2
        if (dem(rv+1,cv).gt.0.99e10) direction=3
        if (dem(rv,cv-1).gt.0.99e10) direction=4
      endif

!     new code 110209 reduce rivergridacci if it is too large so that at least two
!     river links produced 

      if (rivergridacci.ge.int(real(number)/2.0)) then 
         rivergridacci=int(real(number)/2.0)-1
         write(*,*)
         write(*,*), "flow accumulation parameter reduced to ", rivergridacci
         write(*,*)

      endif

!   end of new code 110209



!     put an extra accumulation point in outside the catchment
!     direction 1=n,2=e,3=s,4=w
!     this also gives the outlet direction of the weir
      if (direction.eq.1) then
          accum(rvadd-1,cvadd)= accum(rvadd,cvadd)+1
      elseif (direction.eq.2) then
          accum(rvadd,cvadd+1)= accum(rvadd,cvadd)+1
      elseif (direction.eq.3) then
          accum(rvadd+1,cvadd)= accum(rvadd,cvadd)+1
      else  
          accum(rvadd,cvadd-1)= accum(rvadd,cvadd)+1
      endif

!     cornerval is the maximum value of the 4 accumulated water
!     flows around each corner point
!     this is used to find the river channels (links) and their elevations
      do i=1,nrows+1
          do j=1,ncols+1
              cornerval(i,j)=max(accum(i-1,j-1),accum(i-1,j),accum(i,j-1),accum(i,j))
          enddo
      enddo

!      do i=1,nrows+1
!              write(30,'(200(i4,1X))') (cornerval(i,j),j=1,ncols+1)
!      enddo




!     find the position of the links and their elevations
!     ***************************************************
!     NOTE: RIVER CHANNELS FLOWS ALONG THE EDGE OF GRID SQUARES
!     there are 2 main parts to this procedure, which consists of following river channels
!     along from their source to the outlet or until another river channel is met:
!     1) Going through elements from highest to lowest.
!       If accumulated flow is greater than specified limit then a river exists
!       If no river channels have previously been specifed around the grid squre
!       then find the position of the river around the grid squre and its elevation
!     2) Follow along a river channel
!     2a) check to see if another river channel is nearby. If it is then meet that channel
!     2b) continue along the river channel


!     VARIABLE NAMES
!     cornerdone - Has a river channel been specified for this corner
!     cornerdonep - In the previous loop has a river channel been specified
!     cornerval - Accumulated flow at this corner
!     linkns - river channel heading in a north south direction. True if a channel exists
!     linkew - river channel heading in a east west direction. True if a channel exists


      do i=1,nrows+1
         do j=1,ncols+1
            cornerdone(i,j)=.false.
            cornerdonep(i,j)=.false.
         enddo
      enddo
      do i=1,nrows+1
         do j=1,ncols
             linkew=.false.
         enddo
      enddo

      do i=1,nrows
         do j=1,ncols+1
             linkns=.false.
         enddo
      enddo





!     Part 1
!     ******
!     uses a method by considering the corners of grid squares
!     starts with the highest elements
      countnumber=0
      meetexit=.false.
      do i=number,2,-1
          cv=poscol(i)
          rv=posrow(i)
          if (accum(rv,cv).ge.rivergridacci) then

!          print*,i,cv,rv
!     look at corners around to check link has not already been established
             if ((.not.cornerdone(rv,cv)).and.(.not.cornerdone(rv+1,cv)).and.(.not.cornerdone(rv,cv+1)).and.(.not.cornerdone(rv+1,cv+1))) then

!     works out where the inital link in this river should go
                maxacc=max(accum(rv,cv+1),accum(rv,cv-1),accum(rv+1,cv),accum(rv-1,cv))
                if (accum(rv,cv+1).eq.maxacc) then
!     east west link ->  heading east
                     demn=dem(rv-1,cv)
                     dems=dem(rv+1,cv)
!     north side
                     if (demn.lt.dems) then
                          linkew(rv,cv)=.true.    
                          savelinkew(rv,cv)=.true.

!                         channel elevation
                          dems=dem(rv,cv)
                          ellinkew(rv,cv)=min(demn,dems)-channeldp
                          strlinkew(rv,cv)=stricklerriv
                          pchdp=min(demn,dems)-channeldp
!                         end channel elevation

                          crncolpos=cv+1
                          crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv,cv+1)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv,cv+1)=.true.
!     south side
                    else
                          linkew(rv+1,cv)=.true.
                          savelinkew(rv+1,cv)=.true.

!                         channel elevation
                          demn=dem(rv,cv)
                          ellinkew(rv+1,cv)=min(demn,dems)-channeldp
                          strlinkew(rv+1,cv)=stricklerriv
                          pchdp=min(demn,dems)-channeldp
!                         end channel elevation

                          crncolpos=cv+1
                          crnrowpos=rv+1
                          cornerdone(rv+1,cv)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv+1,cv)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                    endif
                elseif (accum(rv,cv-1).eq.maxacc) then
!     east west link -> heading west
                    demn=dem(rv-1,cv)
                    dems=dem(rv+1,cv)
                    if (demn.lt.dems) then
                           linkew(rv,cv)=.true.
                           savelinkew(rv,cv)=.true.

!                         channel elevation
                          dems=dem(rv,cv)
                          ellinkew(rv,cv)=min(demn,dems)-channeldp
                          strlinkew(rv,cv)=stricklerriv
                          pchdp=min(demn,dems)-channeldp
!                         end channel elevation

                          crncolpos=cv
                          crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv,cv+1)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv,cv+1)=.true.
                   else
                          linkew(rv+1,cv)=.true.
                          savelinkew(rv+1,cv)=.true.

!                         channel elevation
                          demn=dem(rv,cv)
                          ellinkew(rv+1,cv)=min(demn,dems)-channeldp
                          strlinkew(rv+1,cv)=stricklerriv
                          pchdp=min(demn,dems)-channeldp
!                         end channel elevation

                          crncolpos=cv
                          crnrowpos=rv+1
                          cornerdone(rv+1,cv)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv+1,cv)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                    endif
                elseif (accum(rv+1,cv).eq.maxacc) then
!     north south link -> heading south
                     deme=dem(rv,cv+1)
                     demw=dem(rv,cv-1)
                     if (deme.lt.demw) then
                          linkns(rv,cv+1)=.true.
                          savelinkns(rv,cv+1)=.true.

!                         channel elevation
                          demw=dem(rv,cv)
                          ellinkns(rv,cv+1)=min(deme,demw)-channeldp
                          strlinkns(rv,cv+1)=stricklerriv
                          pchdp=min(deme,demw)-channeldp
!                         end channel elevation

                          crncolpos=cv+1
                          crnrowpos=rv+1
                          cornerdone(rv,cv+1)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv,cv+1)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                     else
                          linkns(rv,cv)=.true.
                          savelinkns(rv,cv)=.true.

!                         channel elevation
                          deme=dem(rv,cv)
                          ellinkns(rv,cv)=min(deme,demw)-channeldp
                          strlinkns(rv,cv)=stricklerriv
                          pchdp=min(deme,demw)-channeldp
!                         end channel elevation

                          crncolpos=cv
                          crnrowpos=rv+1
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv+1,cv)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv+1,cv)=.true.
                    endif
                 else
!     north south link -> heading north
                      deme=dem(rv,cv+1)
                      demw=dem(rv,cv-1)
                      if (deme.lt.demw) then
                          linkns(rv,cv+1)=.true.
                          savelinkns(rv,cv+1)=.true.
                          crncolpos=cv+1

!                         channel elevation
                          demw=dem(rv,cv)
                          ellinkns(rv,cv+1)=min(deme,demw)-channeldp
                          strlinkns(rv,cv+1)=stricklerriv
                          pchdp=min(deme,demw)-channeldp
!                         end channel elevation

                          crnrowpos=rv
                          cornerdone(rv,cv+1)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv,cv+1)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                      else
                          linkns(rv,cv)=.true.
                          savelinkns(rv,cv)=.true.

!                         channel elevation
                          deme=dem(rv,cv)
                          ellinkns(rv,cv)=min(deme,demw)-channeldp
                          strlinkns(rv,cv)=stricklerriv
                          pchdp=min(deme,demw)-channeldp
!                         end channel elevation

                          crncolpos=cv
                          crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv+1,cv)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv+1,cv)=.true.
                      endif
                  endif

!     Part 2
!     ******
!     go from corner to corner to outlet of the catchment, along a river channel
                  countnumber=countnumber+1
                  do while (cornerval(crnrowpos,crncolpos).lt.number)

!     Part 2a
!     *******
!    finish this river channel if a previous one is met. This is ignored
!    if the corner value in the one that is met is not greater than or equal
!    to the current one
!     previous one is east
                   if ((cornerdonep(crnrowpos,crncolpos+1)).and.(cornerval(crnrowpos,crncolpos+1).ge.cornerval(crnrowpos,crncolpos))) then
                      linkew(crnrowpos,crncolpos)=.true.

!                     channel elevation
                      if (linkew(crnrowpos,crncolpos+1)) then
                          dum1=ellinkew(crnrowpos,crncolpos+1)
                      else 
                          dum1=1.0e10
                      endif

                      if (linkns(crnrowpos-1,crncolpos+1)) then
                          dum2=ellinkns(crnrowpos-1,crncolpos+1)
                      else 
                          dum2=1.0e10
                      endif


                      if (linkns(crnrowpos,crncolpos+1)) then
                          dum3=ellinkns(crnrowpos,crncolpos+1)
                      else 
                          dum3=1.0e10
                      endif
                      cheldum=min(dum1,dum2,dum3)
                      ellinkew(crnrowpos,crncolpos)=0.5*(cheldum+pchdp)
                      strlinkew(crnrowpos,crncolpos)=stricklerriv
                      pchdp=0.5*(cheldum+pchdp)
!                     end of channel elevation

                      meetexit=.true.
                      exit
!     previous one is west
                   elseif((cornerdonep(crnrowpos,crncolpos-1)).and.(cornerval(crnrowpos,crncolpos-1).ge.cornerval(crnrowpos,crncolpos))) then
                      linkew(crnrowpos,crncolpos-1)=.true.

!                     channel elevation
                      if (linkew(crnrowpos,crncolpos-2)) then
                          dum1=ellinkew(crnrowpos,crncolpos-2)
                      else 
                          dum1=1.0e10
                      endif

                      if (linkns(crnrowpos-1,crncolpos-1)) then
                          dum2=ellinkns(crnrowpos-1,crncolpos-1)
                      else 
                          dum2=1.0e10
                      endif


                      if (linkns(crnrowpos,crncolpos-1)) then
                          dum3=ellinkns(crnrowpos,crncolpos-1)
                      else 
                          dum3=1.0e10
                      endif
                      cheldum=min(dum1,dum2,dum3)
                      ellinkew(crnrowpos,crncolpos-1)=0.5*(cheldum+pchdp)
                      strlinkew(crnrowpos,crncolpos-1)=stricklerriv
                      pchdp=0.5*(cheldum+pchdp)
!                     end of channel elevation

                      meetexit=.true.
                      exit
!     previous one is south
                   elseif((cornerdonep(crnrowpos+1,crncolpos)).and.(cornerval(crnrowpos+1,crncolpos).ge.cornerval(crnrowpos,crncolpos))) then
                      linkns(crnrowpos,crncolpos)=.true.

!                     channel elevation
                      if (linkns(crnrowpos+1,crncolpos)) then
                          dum1=ellinkns(crnrowpos+1,crncolpos)
                      else 
                          dum1=1.0e10
                      endif

                      if (linkew(crnrowpos+1,crncolpos-1)) then
                          dum2=ellinkew(crnrowpos+1,crncolpos-1)
                      else 
                          dum2=1.0e10
                      endif


                      if (linkew(crnrowpos+1,crncolpos)) then
                          dum3=ellinkew(crnrowpos+1,crncolpos)
                      else 
                          dum3=1.0e10
                      endif
                      cheldum=min(dum1,dum2,dum3)
                      ellinkns(crnrowpos,crncolpos)=0.5*(cheldum+pchdp)
                      strlinkns(crnrowpos,crncolpos)=stricklerriv
                      pchdp=0.5*(cheldum+pchdp)
!                     end of channel elevation

                      meetexit=.true.
                      exit
!     previous one is north
                   elseif((cornerdonep(crnrowpos-1,crncolpos)).and.(cornerval(crnrowpos-1,crncolpos).ge.cornerval(crnrowpos,crncolpos))) then
                      linkns(crnrowpos-1,crncolpos)=.true.

!                     channel elevation
                      if (linkns(crnrowpos-2,crncolpos)) then
                         dum1=ellinkns(crnrowpos-2,crncolpos)
                      else 
                          dum1=1.0e10
                      endif

                      if (linkew(crnrowpos-1,crncolpos-1)) then
                          dum2=ellinkew(crnrowpos-1,crncolpos-1)
                      else 
                          dum2=1.0e10
                      endif


                      if (linkew(crnrowpos-1,crncolpos)) then
                          dum3=ellinkew(crnrowpos-1,crncolpos)
                      else 
                          dum3=1.0e10
                      endif
                      cheldum=min(dum1,dum2,dum3)
                      ellinkns(crnrowpos-1,crncolpos)=0.5*(cheldum+pchdp)
                      strlinkns(crnrowpos-1,crncolpos)=stricklerriv
                      pchdp=0.5*(cheldum+pchdp)
!                     end of channel elevation

                      meetexit=.true.
                      exit
!     Part 2b
!     *******
!     no previous channel carry on existing river channel
                   else

                      crnmax=max(cornerval(crnrowpos,crncolpos+1),cornerval(crnrowpos,crncolpos-1),cornerval(crnrowpos+1,crncolpos),cornerval(crnrowpos-1,crncolpos))
!     east west link ->  heading east
                       if (crnmax.eq.cornerval(crnrowpos,crncolpos+1)) then
                           linkew(crnrowpos,crncolpos)=.true.
                           savelinkew(crnrowpos,crncolpos)=.true.

                           dum1=dem(crnrowpos-1,crncolpos)
                           dum2=dem(crnrowpos,crncolpos)
                           cheldum=min(dum1,dum2)
                           if ((cheldum-channeldp).le.(pchdp-channelmindrop)) then
                               ellinkew(crnrowpos,crncolpos)= cheldum-channeldp
                               strlinkew(crnrowpos,crncolpos) = stricklerriv
                           else
                               ellinkew(crnrowpos,crncolpos)= pchdp-channelmindrop
                               strlinkew(crnrowpos,crncolpos) = stricklerlake
                           endif
                           pchdp=min((cheldum-channeldp),(pchdp-channelmindrop))
 
                          
! extra code 040315 reduce the outlet link by 1m dano100m example
                           if (cornerval(crnrowpos,crncolpos+1).ge.number) then
                              ellinkew(crnrowpos,crncolpos) = ellinkew(crnrowpos,crncolpos)-1.0
!                                                                                         ******
                           endif
! end extra code

                           crncolpos=crncolpos+1
                           cornerdone(crnrowpos,crncolpos)=.true.
                           savecornerdone(crnrowpos,crncolpos)=.true.
!     east west link -> heading west
                       elseif (crnmax.eq.cornerval(crnrowpos,crncolpos-1)) then
                          linkew(crnrowpos,crncolpos-1)=.true.
                          savelinkew(crnrowpos,crncolpos-1)=.true.
                          dum1=dem(crnrowpos-1,crncolpos-1)
                          dum2=dem(crnrowpos,crncolpos-1)
                          cheldum=min(dum1,dum2)

                          if ((cheldum-channeldp).le.(pchdp-channelmindrop)) then
                              ellinkew(crnrowpos,crncolpos-1)= cheldum-channeldp
                              strlinkew(crnrowpos,crncolpos-1) = stricklerriv
                          else
                              ellinkew(crnrowpos,crncolpos-1)= pchdp-channelmindrop
                              strlinkew(crnrowpos,crncolpos-1) = stricklerlake
                          endif
                          pchdp=min((cheldum-channeldp),(pchdp-channelmindrop))

! extra code 040315 reduce the outlet link by 1m dano100m example
                          if (cornerval(crnrowpos,crncolpos-1).ge.number) then
                               ellinkew(crnrowpos,crncolpos-1) = ellinkew(crnrowpos,crncolpos-1)-1.0
                          endif
! end extra code
                          crncolpos=crncolpos-1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
!     north south link -> heading south
                      elseif (crnmax.eq.cornerval(crnrowpos+1,crncolpos)) then
                          linkns(crnrowpos,crncolpos)=.true.
                          savelinkns(crnrowpos,crncolpos)=.true.
                          dum1=dem(crnrowpos,crncolpos-1)
                          dum2=dem(crnrowpos,crncolpos)
                          cheldum=min(dum1,dum2)
                          if ((cheldum-channeldp).le.(pchdp-channelmindrop)) then
                              ellinkns(crnrowpos,crncolpos)= cheldum-channeldp
                              strlinkns(crnrowpos,crncolpos) = stricklerriv
                          else
                              ellinkns(crnrowpos,crncolpos)= pchdp-channelmindrop
                              strlinkns(crnrowpos,crncolpos) = stricklerlake
                          endif
                          pchdp=min((cheldum-channeldp),(pchdp-channelmindrop))

! extra code 040315 reduce the outlet link by 1m dano100m example
                          if (cornerval(crnrowpos+1,crncolpos).ge.number) then
                              ellinkns(crnrowpos,crncolpos) = ellinkns(crnrowpos,crncolpos)-1.0
                          endif
! end extra code

                          crnrowpos=crnrowpos+1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
!     north south link -> heading north
                    elseif (crnmax.eq.cornerval(crnrowpos-1,crncolpos)) then
                          linkns(crnrowpos-1,crncolpos)=.true.
                          savelinkns(crnrowpos-1,crncolpos)=.true.
                          dum1=dem(crnrowpos-1,crncolpos-1)
                          dum2=dem(crnrowpos-1,crncolpos)
                          cheldum=min(dum1,dum2)

                          if ((cheldum-channeldp).le.(pchdp-channelmindrop)) then
                              ellinkns(crnrowpos-1,crncolpos)= cheldum-channeldp
                              strlinkns(crnrowpos-1,crncolpos) = stricklerriv
                         else
                              ellinkns(crnrowpos-1,crncolpos)= pchdp-channelmindrop
                              strlinkns(crnrowpos-1,crncolpos) = stricklerlake
                         endif

                         pchdp=min((cheldum-channeldp),(pchdp-channelmindrop))

! extra code 040315 reduce the outlet link by 1m dano100m example
                         if (cornerval(crnrowpos-1,crncolpos).ge.number) then
                             ellinkns(crnrowpos-1,crncolpos) = ellinkns(crnrowpos-1,crncolpos)-1.0
                         endif
! end extra code
                         crnrowpos=crnrowpos-1
                         cornerdone(crnrowpos,crncolpos)=.true.
                         savecornerdone(crnrowpos,crncolpos)=.true.
                    endif
                endif
!      write(434,*)cornerval(crnrowpos,crncolpos),crnrowpos,crncolpos,number
             enddo


! new code 280108 
! apart from the main channel (countnumber=0) all other channels should leave do while loop via exit code with meetexit=true any that do not are removed
                if (countnumber.eq.1) then

!set outlet flow accumulation=0 so no river join here and cause crashes im shetran
                    outletcrnrowpos=crnrowpos
                    outletcrncolpos=crncolpos
                    outletcornerval=cornerval(crnrowpos,crncolpos)
                    cornerval(outletcrnrowpos,outletcrncolpos)=0
!end of set outlet flow accum

                    do k=1,nrows+1
                       do j=1,ncols+1
                          savecornerdone(k,j) = .false. 
                          savelinkew(k,j) = .false.
                          savelinkns(k,j) = .false.
                       enddo
                    enddo
                elseif (meetexit) then
                    do k=1,nrows+1
                       do j=1,ncols+1
                          savecornerdone(k,j) = .false. 
                           savelinkew(k,j) = .false.
                         savelinkns(k,j) = .false.
                       enddo
                    enddo
                else
!                    print*,'yes'
                    do k=1,nrows+1
                       do j=1,ncols+1
                         if (savecornerdone(k,j)) then
                            cornerdone(k,j)=.false.
                            savecornerdone(k,j) = .false. 
                         endif
                         if (savelinkew(k,j)) then
                            linkew(k,j)= .false.                    
                            savelinkew(k,j) = .false.
                         endif
                         if (savelinkns(k,j)) then
                            linkns(k,j)= .false.                    
                            savelinkns(k,j) = .false.
                         endif
                      enddo
                   enddo
              endif
              meetexit=.false.
! end of 280108




!     which corners have a specified link
!     used to attract a new river channel to an existing one
               do k=1,nrows+1
                    do j=1,ncols+1
                         if (cornerdone(k,j)) then
                             cornerdonep(k,j)=.true.
                         endif
                    enddo
              enddo

           endif
        endif
    enddo


! new code 280108 
! reset outlet flow accumulation
    cornerval(outletcrnrowpos,outletcrncolpos)=outletcornerval
! end of 280108


!     End of find the position of the links
!     *************************************

         write(logfile,*) 
         write(logfile,*) 'Iteration process for removing sinks'
         write(logfile,*) '************************************'



!     find river channel (link) element numbers and position of outlet
      k=0
      linkelvmin=1.0e10
      i=nrows+1
      do j=1,ncols
         if (linkew(i,j)) then
            k=k+1
            linkelv(k)= ellinkew(i,j)
            linkstr(k)= strlinkew(i,j)
            if (linkelv(k).lt.linkelvmin) then
                linkelvmin=linkelv(k)
                linkoutnum=k
                linkoutdir=1
                linkoutr=i
                linkoutc=j
!               new code 031212
                demminoutletproblem=min(dem(i,j),dem(i-1,j))
            endif
         endif
      enddo
      do i=nrows,1,-1
         do j=1,ncols+1
            if (linkns(i,j)) then
               k=k+1
               linkelv(k)= ellinkns(i,j)
               linkstr(k)= strlinkns(i,j)
               if (linkelv(k).lt.linkelvmin) then
                   linkelvmin=linkelv(k)
                   linkoutnum=k
                   linkoutdir=2
                   linkoutr=i
                   linkoutc=j
!               new code 031212
                   demminoutletproblem=min(dem(i,j),dem(i,j-1))
               endif
            endif
         enddo
         do j=1,ncols
            if (linkew(i,j)) then
               k=k+1
               linkelv(k)= ellinkew(i,j)
               linkstr(k)= strlinkew(i,j)
               if (linkelv(k).lt.linkelvmin) then
                   linkelvmin=linkelv(k)
                   linkoutnum=k
                   linkoutdir=1
                   linkoutr=i
                   linkoutc=j
!               new code 031212
                   demminoutletproblem=min(dem(i,j),dem(i-1,j))
               endif
            endif
         enddo
      enddo
      numlinks=k
!      do i=1,k
!      print*,linkelv(i)
!      enddo
!      print*,linkelvmin,linkoutdir,linkoutr,linkoutc

!               new code 031212. problem with location of river link
!      if direction of outlet is wrong then outlet link can be in the wrong place.
! this bumps up DEM values so it still works.
       do i=1,nrows
          do j=1,ncols
              if (dem(i,j).lt.demminoutletproblem) then 
                  dem(i,j)= demminoutletproblem+0.1
              endif
          enddo
      enddo
!              endof new code new code 031212

!go through links to check there is always a downward pathway
      do i=1,nrows+1
          do j=1,ncols
              if (.not.linkew(i,j)) then 
                  ellinkew(i,j)=1.0e10
              endif
          enddo
      enddo
      do i=1,nrows
          do j=1,ncols+1
              if (.not.linkns(i,j)) then 
                    ellinkns(i,j)=1.0e10
              endif
          enddo
      enddo

!     ew links
      countiteration=0
      do
      count=0
! 105  count=0
         countiteration=countiteration+1
         do i=2,nrows
            do j=2,ncols-1
              if (linkew(i,j)) then
                  if ((i.eq.linkoutr).and.(j.eq.linkoutc).and.(linkoutdir.eq.1)) then
                      outletlink=.true.
                  else
                      outletlink=.false.
                  endif
                  dum1=ellinkew(i,j-1)
                  dum2=ellinkns(i-1,j)
                  dum3=ellinkns(i,j)
                  dum4=ellinkns(i-1,j+1)
                  dum5=ellinkns(i,j+1)
                  dum6=ellinkew(i,j+1)
                  dummin=min(dum1,dum2,dum3,dum4,dum5,dum6)
! sb 240909 add 0.01
! sb 041010 added channelmindrop-0.0001
                  if ((ellinkew(i,j).le.dummin+channelmindrop-0.0001).and.(.not.outletlink)) then
                     ellinkew(i,j)=ellinkew(i,j)+channelmindrop
                     count=count+1
                  endif
              endif
            enddo
         enddo
!     ns links
         do i=2,nrows-1
            do j=2,ncols
              if (linkns(i,j)) then
                  if ((i.eq.linkoutr).and.(j.eq.linkoutc).and.(linkoutdir.eq.2)) then
                      outletlink=.true.
                  else
                      outletlink=.false.
                  endif
                  dum1=ellinkns(i-1,j)
                  dum2=ellinkew(i,j-1)
                  dum3=ellinkew(i,j)
                  dum4=ellinkew(i+1,j-1)
                  dum5=ellinkew(i+1,j)
                  dum6=ellinkns(i+1,j)
                  dummin=min(dum1,dum2,dum3,dum4,dum5,dum6)
! sb 240909 add 0.01
! sb 041010 added channelmindrop-0.0001
                  if ((ellinkns(i,j).le.dummin+channelmindrop-0.0001).and.(.not.outletlink)) then
                     ellinkns(i,j)=ellinkns(i,j)+channelmindrop
                     count=count+1
                  endif
              endif
            enddo
         enddo
         write(logfile,*) 'Number of river chanel sinks = ',count
         if (countiteration.eq.1000) then
            write(*,*)
            write(*,*),'there is a problem finding a downward flow path, please check poistion of river channels in SHETRAN Results Viewer'
            write(*,*)
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            exit
         endif

      if (count==0) exit
    enddo
!end of go through links for downward pathway


!     find river channel (link) element numbers again
!     in case of changes
      k=0
      linkelvmin=1.0e10
      i=nrows+1
      do j=1,ncols
         if (linkew(i,j)) then
            k=k+1
            linkelv(k)= ellinkew(i,j)
!               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2,A,I0,1x,I0,1x,I0)') k,',',xllcorner+((j-1.5)*cellsize),',',yllcorner+((nrows-i)*cellsize),',',linkelv(k),', E,',cornerval(i,j-1),cornerval(i,j),cornerval(i,j+1)
               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2)') k,',',xllcorner+((j-2.0)*cellsize),',',yllcorner+((nrows-i-0.5)*cellsize),',',linkelv(k)
               do m=1,icountdischargepoints
                   if (extradischarge(m)==k) then
                       if (cornerval(i,j+1).ge.cornerval(i,j)) then
                           extradischargeface=1 
                       else 
                            extradischargeface=3
                       endif
                      write(OutDischargePoints,'(I0,1X,I0)') extradischarge(m),extradischargeface
                      exit
                   endif
              enddo
                     
                   
                 
            if ((lakedist(i,j).eq.1).or.(lakedist(i-1,j).eq.1)) then
                linkstr(k)=stricklerlake
            else
                linkstr(k)=stricklerriv
            endif
!            linkstr(k)= strlinkew(i,j)
            streamsize(k)=min(cornerval(i,j),cornerval(i,j+1))
            if (linkelv(k).lt.linkelvmin) then
                linkelvmin=linkelv(k)
                linkoutnum=k
            endif
         endif
      enddo
      do i=nrows,1,-1
         do j=1,ncols+1
            if (linkns(i,j)) then
                k=k+1
                linkelv(k)= ellinkns(i,j)
!               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2,A,I0,1x,I0,1x,I0)') k,',',xllcorner+((j-2)*cellsize),',',yllcorner+((nrows-0.5-i)*cellsize),',',linkelv(k),', N, ',cornerval(i-1,j),cornerval(i,j),cornerval(i+1,j)
               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2)') k,',',xllcorner+((j-2.0)*cellsize),',',yllcorner+((nrows-i-0.5)*cellsize),',',linkelv(k)
                do m=1,icountdischargepoints
                   if (extradischarge(m)==k) then
                       if (cornerval(i+1,j).ge.cornerval(i,j)) then
                           extradischargeface=4 
                       else 
                            extradischargeface=2
                       endif
                      write(OutDischargePoints,'(I0,1X,I0)') extradischarge(m),extradischargeface
                      exit
                   endif
               enddo
              if ((lakedist(i,j).eq.1).or.(lakedist(i,j-1).eq.1)) then
                   linkstr(k)=stricklerlake
               else
                   linkstr(k)=stricklerriv
               endif
 !                linkstr(k)= strlinkns(i,j)
               streamsize(k)=min(cornerval(i,j),cornerval(i+1,j))
               if (linkelv(k).lt.linkelvmin) then
                   linkelvmin=linkelv(k)
                   linkoutnum=k
               endif
            endif
         enddo
         do j=1,ncols
            if (linkew(i,j)) then
               k=k+1
               linkelv(k)= ellinkew(i,j)
!               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2,A,I0,1x,I0,1x,I0)') k,',',xllcorner+((j-1.5)*cellsize),',',yllcorner+((nrows-i)*cellsize),',',linkelv(k),', E,',cornerval(i,j-1),cornerval(i,j),cornerval(i,j+1)
               write(OutRivLoc,'(I0,A,F12.2,A,F12.2,A,F10.2)') k,',',xllcorner+((j-1.5)*cellsize),',',yllcorner+((nrows-i)*cellsize),',',linkelv(k)
                 do m=1,icountdischargepoints
                   if (extradischarge(m)==k) then
                       if (cornerval(i,j+1).ge.cornerval(i,j)) then
                           extradischargeface=1 
                       else 
                            extradischargeface=3
                       endif
                      write(OutDischargePoints,'(I0,1X,I0)') extradischarge(m),extradischargeface
                      exit
                   endif
               enddo
              if ((lakedist(i,j).eq.1).or.(lakedist(i-1,j).eq.1)) then
                  linkstr(k)=stricklerlake
               else
                  linkstr(k)=stricklerriv
               endif
!            linkstr(k)= strlinkew(i,j)
               streamsize(k)=min(cornerval(i,j),cornerval(i,j+1))
               if (linkelv(k).lt.linkelvmin) then
                  linkelvmin=linkelv(k)
                  linkoutnum=k
               endif
            endif
         enddo
      enddo

!************new code 1/2/11 sb ************************
! direction (which is the outlet face) is sometimes wrong recaculate here

      if (linkoutdir.eq.1) then
         if (cornerval(linkoutr,linkoutc).gt.cornerval(linkoutr,linkoutc+1)) then
            direction=4
          else
            direction=2
          endif
      else
          if (cornerval(linkoutr,linkoutc).gt.cornerval(linkoutr+1,linkoutc)) then
            direction=1
          else
            direction=3
          endif
      endif
!********end of new code**********************************                 

!***************temporary output**************************
!      do i=1,nrows+1
!         write(20,'(42L1)')(linkew(i,j),j=1,ncols)
!      enddo
!      write(20,*)
!      do i=1,nrows
!         write(20,'(43L1)')(linkns(i,j),j=1,ncols+1)
!      enddo

!      do i=1,nrows
!              write(30,'(40(g10.4,1X))') (dem(i,j),j=1,ncols)
!      enddo
!      do i=1,number
!          write(20,*) posval(i),posrow(i),poscol(i)
!      enddo
!      do i=1,nrows
!              write(30,'(40(i6,1X))') (accum(i,j),j=1,ncols)
!      enddo
!      do i=1,nrows+1
!             write(20,'(40(l4,1X))') (linkew(i,j),j=1,ncols)
!      enddo
!      do i=1,nrows
!              write(20,'(41(l4,1X))') (linkns(i,j),j=1,ncols+1)
!      enddo


!      do i=1,2*nrows+1
!         do j=1,2*ncols+1
!             xmap(i,j)=' '
!         enddo
!      enddo


      do i=1,nrows+1
         do j=1,ncols+1
            if (linkew(i,j)) then
                xmap(((2*(i-1))+1),(2*j))='-'
            else
                xmap(((2*(i-1))+1),(2*j))='.'
            endif
            if (linkns(i,j)) then
                xmap((2*i),((2*(j-1))+1))='|'
            else
                xmap((2*i),((2*(j-1))+1))='.'
            endif
         enddo
      enddo

!      do i=1,2*nrows
!         write(30,'(84(a1))') (xmap(i,j),j=1,2*ncols)
!      enddo


!      do i=1,42
!         do j=1,42
!            if (linkew(i,j)) then
!                msgs=12*(j-1)+1
!                msge=msgs+5
!                write(msg(msgs:msge),'(a6)') '    '
!                msgs=12*(j-1)+7
!                msge=msgs+5
!                write(msg(msgs:msge),'(i6)') int(ellinkew(i,j))
!            else
!                msgs=12*(j-1)+1
!                msge=msgs+11
!                write(msg(msgs:msge),'(a12)') '        '
!            endif
!         enddo
!         write(30,'(a520)') msg
!         do j=1,42
!            if (linkns(i,j)) then
!                msgs=12*(j-1)+1
!                msge=msgs+5
!                write(msg(msgs:msge),'(i6)') int(ellinkns(i,j))
!                msgs=12*(j-1)+7
!                msge=msgs+5
!                write(msg(msgs:msge),'(i6)') int(dem(i,j))
!            else
!                msgs=12*(j-1)+1
!                msge=msgs+5
!                write(msg(msgs:msge),'(a6)') '    '
!                msgs=12*(j-1)+7
!                msge=msgs+5
!                write(msg(msgs:msge),'(i6)') int(dem(i,j))
!            endif
!         enddo
!         write(30,'(a520)') msg
!      enddo
!***************end of temporary output**************************



!**********Final output*************************************************
!********* Computational grid definition ******************************* 
      do i=1,nrows
         do j=1,ncols
            if (catch(i,j).ne.novalue) then
               catch(i,j) = 1
            else
               catch(i,j) = 0
            endif
         enddo
      enddo
      write (MSG2,9334)
      write (outfrd,9200) MSG2
      do I = 1,nrows
        k=nrows-i+1
        write (outfrd,9108) k,(catch(i,j),j=1,ncols)
      enddo  
!
!**************** Flow Codes ******************************************
!
      do i=1,nrows
        do j=1,ncols+1
          if (linkns(i,j)) then
              alinkns(i,j)='R'
          else
              alinkns(i,j)='.'
          endif
        enddo
      enddo  
      do I = 1,nrows+1
        do j=1,ncols
          if (linkew(i,j)) then
              alinkew(i,j)='R'
          else
              alinkew(i,j)='.'
          endif
        enddo
      enddo 
      if (linkoutdir.eq.1) then
          alinkew(linkoutr,linkoutc)='W' 
      else
          alinkns(linkoutr,linkoutc)='W'
      endif 
      write (MSG2,9335)
      write (outfrd,9200) MSG2
      do I = 1,nrows
         k=nrows-i+1
         write (outfrd,9109) k,(alinkns(i,j),j=1,ncols+1)
      enddo  
      write (MSG2,9336)
      write (outfrd,9200) MSG2
      do I = 1,nrows+1
         k=nrows-i+2
         write (outfrd,9109) k,(alinkew(i,j),j=1,ncols)
      enddo  
!                
      do i=1,nrows
            do l=1,10
                do j=1,ncols
                    do k =1,10
                       if  (catch(i,j).eq.0) then
                           catchgeometry((i-1)*10+l,(j-1)*10+k) = 0
                       else 
                           catchgeometry((i-1)*10+l,(j-1)*10+k) = 1
                       endif
                    enddo
                enddo
           enddo
      enddo

      do i=1,nrows
        do l=1,10
           do j=1,ncols+1
             if (linkns(i,j)) then
                catchgeometry((i-1)*10+l,(j-1)*10-1) = 2
                catchgeometry((i-1)*10+l,(j-1)*10) = 2
                catchgeometry((i-1)*10+l,(j-1)*10+1) = 2
            endif
          enddo
        enddo
      enddo  
      do I = 1,nrows+1
          do j=1,ncols
              do l=1,10
                 if (linkew(i,j)) then
                     catchgeometry((i-1)*10-1,(j-1)*10+l) = 2
                     catchgeometry((i-1)*10,(j-1)*10+l) = 2
                     catchgeometry((i-1)*10+1,(j-1)*10+l) = 2
                endif
              enddo
          enddo
      enddo 
      write(OUTRIV,'(A5,a4,i8)') 'ncols','    ',ncols*10
      write(OUTRIV,'(A5,a4,i8)') 'nrows','    ',nrows*10
      write(OUTRIV,'(A9,a4,f12.2)') 'xllcorner','    ',xllcorner-cellsize
      write(OUTRIV,'(A9,a4,f12.2)') 'yllcorner','    ',yllcorner-cellsize
      write(OUTRIV,'(A8,a4,f10.1)') 'cellsize','    ',cellsize/10.0
      write(OUTRIV,'(A12,a4,i8)') 'NODATA_value','    ',0
      do I = 1,nrows*10
         write(OUTRIV,'(*(I0,1x))') (catchgeometry(i,j),j=1,ncols*10)
      enddo 
      write(OUTELMNUM,'(A5,a4,i8)') 'ncols','    ',ncols
      write(OUTELMNUM,'(A5,a4,i8)') 'nrows','    ',nrows
      write(OUTELMNUM,'(A9,a4,f12.2)') 'xllcorner','    ',xllcorner-cellsize
      write(OUTELMNUM,'(A9,a4,f12.2)') 'yllcorner','    ',yllcorner-cellsize
      write(OUTELMNUM,'(A8,a4,f10.1)') 'cellsize','    ',cellsize
      write(OUTELMNUM,'(A12,a4,i8)') 'NODATA_value','    ',0
      if (isbanks) then
          elementcount = numlinks*3
      else 
          elementcount = numlinks
      endif
      do i=nrows,1,-1
        do j = 1,ncols
            if (catch(i,j).eq.1) then
              elementcount = elementcount + 1
              elementnumber(i,j) = elementcount
            endif
        enddo
     enddo
     do i=1,nrows
         write(OUTELMNUM,'(*(I0,1x))') (elementnumber(i,j),j=1,ncols)
      enddo 
     
      write(logfile,*) 


!!!! 020514 remove mean DEM sinks. Check link or grid square nearest    
!    Count is number of sinks
!!!+2.0 is to account for the banks
     do
         count=0   
         do i=1,nrows
              do j=1,ncols      
                  if (linkew(i,j)) then
                     demn = ellinkew(i,j)+2.0
                  else
                     demn=demmean(i-1,j)
                  endif

                  if (linkew(i+1,j)) then
                     dems = ellinkew(i+1,j)+2.0
                  else
                     dems=demmean(i+1,j)
                  endif


                  if (linkns(i,j)) then
                     demw = ellinkns(i,j)+2.0
                  else
                     demw=demmean(i,j-1)
                  endif

                  if (linkns(i,j+1)) then
                     deme = ellinkns(i,j+1)+2.0
                  else
                     deme=demmean(i,j+1)
                  endif

                  demminedge=min(demn,dems,deme,demw)
                  if ((j.eq.colposmin).and.(i.eq.rowposmin)) then
                      notlowpoint=.false.
                  else
                      notlowpoint=.true.
                  endif
      
!                print*,demn,dems,deme,demw,i,j,colposmin,rowposmin,catch(i,j)
    
                  if ((demmean(i,j).le.demminedge).and.(notlowpoint).and.(catch(i,j).eq.1)) then
                     demmean(i,j)=demminedge+removesink
                     count=count+1
                  endif
              enddo
          enddo
          write(logfile,*) 'Number of land element sinks = ', count
          if (count==0) exit
     enddo

!!!! END of code change



!
!************* Ground surface Elevations *******************************
!
      write (MSG2,9337)
      write (outfrd,9200) MSG2
      do I = 1,nrows
        k=nrows-i+1
        do J = 1,ncols
            if (demmean(i,j).gt.9.9e9) then
               demmean(i,j)=0.0
            endif
        enddo
        write (outfrd,9101) k
        do J=1,ncols
           AFORM(J) = FORM(demmean(i,j))
        enddo   
        write(outfrd,9110) (AFORM(J),J=1,ncols)
      enddo  

!
!***************************** PE Types2 ************************
!       do i=1,numberunique
!       print*, i, peunique(i)
!      enddo
! previous version where PEdist was sorted and data in the time series file corresponded with the sorted value.
! Now values in PEdist are assumed to correspond to column numbers in the time series file

        do i=2,nrows-1
           do j=2,ncols-1
              if (pedist(i,j).eq.-9999) then
                pedist(i,j)= 0
              endif
              if ((catch(i,j).eq.1).and.(pedist(i,j).eq.0)) then
                  pedist(i,j)=1
              endif
          enddo
      enddo      
      
      write (MSG,9343)
      write (OUTFRD,9200) MSG
!      if (numberunique.lt.10) then
      if (pedistMax.lt.10) then
         do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9108) k,(pedist(i,j),j=1,ncols)
         enddo  
      else
         do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9100) k
            write (outfrd,9209)(pedist(i,j),j=1,ncols)
         enddo  
      endif


!
!***************************** rain Types2 ************************
! previous version where Raindist was sorted and data in the time series file corresponded with the sorted value.
! Now values in Raindist are assumed to correspond to column numbers in the time series file

        do i=2,nrows-1
           do j=2,ncols-1
             if (raindist(i,j).eq.-9999) then
                 raindist(i,j)= 0
             endif
             if ((catch(i,j).eq.1).and.(raindist(i,j).eq.0)) then
                raindist(i,j)=1
            endif
         enddo
      enddo

     
      write (MSG,9346)
      write (OUTFRD,9200) MSG
      if (RainDistMax.lt.10) then
         do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9108) k,(raindist(i,j),j=1,ncols)
         enddo  
      else
         do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9100) k
            write (outfrd,9209) (raindist(i,j),j=1,ncols)
         enddo  
      endif



!
!***************************** Vegetation Types ************************
!
      if (icountveg.gt.1) then
         OPEN(InVeg,FILE=vegname,STATUS='OLD',IOSTAT=io)
          if (io /=0) then
              write (*,'(3A)') 'The Vegetation map file ',trim(vegname(1)), 'specified in the tag VegMap does not exist. Please correct this error and try again'
              write(*,*)
              write(*,'(''paused, type [enter] to continue'')')
              read (*,*)
              stop
      endif
endif
      write (MSG,9349)
      write (OUTFRD,9200) MSG
      if (icountveg.gt.1) then
         read(InVeg,*) 
         read(InVeg,*) 
         read(InVeg,*) 
         read(InVeg,*) 
         read(InVeg,*) 
         read(InVeg,*) 
         do i=2,nrows-1
         read(InVeg,*) (vegdist(i,j),j=2,ncols-1)
             do j=2,ncols-1
                if (vegdist(i,j).lt.0) then 
                   vegdist(i,j)=0
                   write (logfile,*) 
                   write (logfile,*) 'A negative value has been specfied for a location in the vegetation map file. It has been changed to category 0'

                   !              make sure every grid square inside is defined
                    if ((catch(i,j).eq.1).and.(vegdist(i,j).eq.0)) then
                       write (logfile,*) 
                       write (logfile,*) 'A zero value has been specfied for a location in the vegetation map file that is within the catchment. It has been changed to category 1'

                       vegdist(i,j)=1
                    endif
                endif
!               make sure there is a vegetation defined for each category in the veg map file. If the value is too high then set it to one
                if (vegdist(i,j).gt.icountveg) then
                       write (logfile,*) 
                       write (logfile,*) 'A value greater than the maximimum VegetationDetail been specfied for a location in the vegetation map file. It has been changed to category 1'
                       write (logfile,'(A,I0,A,I0,A,I0)') 'The values was ',vegdist(i,j),' at location row = ',i-1,' column = ',j-1
                       vegdist(i,j) = 1
               endif
            enddo
         enddo
         do j=1,ncols
            vegdist(1,j)=0
            vegdist(nrows,j)=0
         enddo
         do i=1,nrows
            vegdist(i,1)=0
            vegdist(i,ncols)=0
         enddo
        
!*****************
!extra forest layer
         if (IsForestFile) then
        
          do I = 1,nrows
             do j = 1,ncols

                 NFMForestDist2(i,j)=vegdist(i,j)

!if there is forest then 10 extra vegetation types are added. This are based on vegetation type 1                

                
!                icountveg
!                if (vegdist(i,j).eq.1) then
                if (NFMForestDist(i,j).gt.0) then
                    if (NFMForestDist(i,j).lt.5) then
                        NFMForestDist2(i,j)=vegdist(i,j)
                    elseif (NFMForestDist(i,j).lt.15) then
                        NFMForestDist2(i,j)=icountveg+1
                    elseif (NFMForestDist(i,j).lt.25) then
                        NFMForestDist2(i,j)=icountveg+2
                    elseif (NFMForestDist(i,j).lt.35) then
                        NFMForestDist2(i,j)=icountveg+3
                    elseif (NFMForestDist(i,j).lt.45) then
                        NFMForestDist2(i,j)=icountveg+4
                    elseif (NFMForestDist(i,j).lt.55) then
                        NFMForestDist2(i,j)=icountveg+5
                    elseif (NFMForestDist(i,j).lt.65) then
                        NFMForestDist2(i,j)=icountveg+6
                    elseif (NFMForestDist(i,j).lt.75) then
                        NFMForestDist2(i,j)=icountveg+7
                    elseif (NFMForestDist(i,j).lt.85) then
                        NFMForestDist2(i,j)=icountveg+8
                    elseif (NFMForestDist(i,j).lt.95) then
                        NFMForestDist2(i,j)=icountveg+9
                    else
                        NFMForestDist2(i,j)=icountveg+10
                    endif
                endif

            enddo
        enddo

        do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9100) k
            write (outfrd,9209) (NFMForestDist2(i,j),j=1,ncols)
        enddo
        
!different output type if more than 9 veg types
        else
          if (icountveg.le.9) then
              do I = 1,nrows
                  k=nrows-i+1
               write (outfrd,9108) k,(vegdist(i,j),j=1,ncols)
             enddo  
         else
              do I = 1,nrows
                  k=nrows-i+1
                  write (outfrd,9100) k
                  write (outfrd,9209) (vegdist(i,j),j=1,ncols)
              enddo
         endif                  
        
        
        
        endif
! end of extra forest layer
!**************************
        
        
        
!        do I = 1,nrows
!          k=nrows-i+1
!          write (outfrd,9108) k,(vegdist(i,j),j=1,ncols)
!        enddo  

    else
        do I = 1,nrows
            k=nrows-i+1
            write (outfrd,9108) k,(catch(i,j),j=1,ncols)
        enddo  
    endif

    write (MSG,9352)
    write (OUTFRD,9200) MSG
    AFORM(1) =FORM(simulateddischargetimestep)
    write(OUTFRD,9110) AFORM(1)

      
    close(InVeg)

!***************OCD Data ***********************************************      
!*********************** Title *****************************************      

      write (MSG,9401)
      write (OUTOCD,9200) MSG

!********************* Get basic control parameters ********************


!10 extra negative values for storage      
      if (IsStorageFile) then      
         write (OUTOCD,9405) '1',icountveg+10,'1','F'
     else
         write (OUTOCD,9405) '1',icountveg,'1','F'
     endif
!
!************************** Timestep Control ***************************
!
!     These values are not used, but they must be present
      write (MSG,9402)
      write (OUTOCD,9200) MSG
      AFORM(1)=FORM(1.0)
      AFORM(2)=FORM(99999.0)
      write (OUTOCD,9105) AFORM(1),AFORM(2)
      
!************************* Other Parameters ****************************
!
      write (MSG,9403)
      write (OUTOCD,9200) MSG
      AFORM(1)=FORM(100.0)
!** strickler roughness depends on veg type

      AFORM(2)=FORM(0.0)
      AFORM(3)=FORM(0.0)
      AFORM(4)=FORM(99999.0)
      AFORM(5)=FORM(0.0)
      write (OUTOCD,9103) AFORM(1),AFORM(2),AFORM(3),AFORM(4),AFORM(5)
      do i=1,icountveg
         aform(i)=form(stricklerveg(i))
      enddo


!extra storage file
      if (IsStorageFile) then      
!10 extra negative values for storage      
         do i=1,10
            aform(icountveg+i)=form(real(-i*10))
!         aform(i)=form(-i*10)
         enddo
         write (outocd,'(100(a7))') (aform(i),i=1,icountveg+10)

         
          do I = 1,nrows
             do j = 1,ncols

                 NFMStorageDist2(i,j)=vegdist(i,j)

!if there is forest then 10 extra vegetation types are added. This are based on vegetation type 1                

                
!                icountveg
!                if (vegdist(i,j).eq.1) then
                if (NFMStorageDist(i,j).gt.0) then
                    if (NFMStorageDist(i,j).lt.5) then
                        NFMStorageDist2(i,j)=vegdist(i,j)
                    elseif (NFMStorageDist(i,j).lt.15) then
                        NFMStorageDist2(i,j)=icountveg+1
                    elseif (NFMStorageDist(i,j).lt.25) then
                        NFMStorageDist2(i,j)=icountveg+2
                    elseif (NFMStorageDist(i,j).lt.35) then
                        NFMStorageDist2(i,j)=icountveg+3
                    elseif (NFMStorageDist(i,j).lt.45) then
                        NFMStorageDist2(i,j)=icountveg+4
                    elseif (NFMStorageDist(i,j).lt.55) then
                        NFMStorageDist2(i,j)=icountveg+5
                    elseif (NFMStorageDist(i,j).lt.65) then
                        NFMStorageDist2(i,j)=icountveg+6
                    elseif (NFMStorageDist(i,j).lt.75) then
                        NFMStorageDist2(i,j)=icountveg+7
                    elseif (NFMStorageDist(i,j).lt.85) then
                        NFMStorageDist2(i,j)=icountveg+8
                    elseif (NFMStorageDist(i,j).lt.95) then
                        NFMStorageDist2(i,j)=icountveg+9
                    else
                        NFMStorageDist2(i,j)=icountveg+10
                    endif
                endif

            enddo
        enddo

         
         
         
       write (MSG,9414)
       write (OUTOCD,9200) MSG
       do I = 1,nrows
          k=nrows-i+1
          write (outocd,9100) k
          write (outocd,9209) (NFMStorageDist2(i,j),j=1,ncols)
       enddo  

      
       write (MSG,9417)
       write (OUTOCD,9200) MSG
       do I = 1,nrows
          k=nrows-i+1
          write (outocd,9100) k
          write (outocd,9209) (NFMStorageDist2(i,j),j=1,ncols)
       enddo  
     else

          
!standard output with no storage file
        write (outocd,'(100(a7))') (aform(i),i=1,icountveg)
        write (MSG,9414)
        write (OUTOCD,9200) MSG

!different output type if more than 9 veg types
        if (icountveg.gt.1) then
            
            if (icountveg.le.9) then
               do I = 1,nrows
                   k=nrows-i+1
                   write (outocd,9108) k,(vegdist(i,j),j=1,ncols)
               enddo  
           else
               do I = 1,nrows
                  k=nrows-i+1
                   write (outocd,9100) k
                   write (outocd,9209) (vegdist(i,j),j=1,ncols)
                enddo
           endif  
        else
!a sinlge veg tyye or no veg type defined so all 1s
            do I = 1,nrows
               k=nrows-i+1
                write (outocd,9108) k,(catch(i,j),j=1,ncols)
           enddo
        endif
      
      
      


        write (MSG,9417)
        write (OUTOCD,9200) MSG
 
!different output type if more than 9 veg types
        if (icountveg.gt.1) then
            
            if (icountveg.le.9) then
               do I = 1,nrows
                   k=nrows-i+1
                   write (outocd,9108) k,(vegdist(i,j),j=1,ncols)
               enddo  
           else
               do I = 1,nrows
                  k=nrows-i+1
                   write (outocd,9100) k
                   write (outocd,9209) (vegdist(i,j),j=1,ncols)
                enddo
           endif  
        else
!a sinlge veg tyye or no veg type defined so all 1s
            do I = 1,nrows
               k=nrows-i+1
                write (outocd,9108) k,(catch(i,j),j=1,ncols)
           enddo
        endif
      
     endif     
      
!end of extra storage file
!*************************




!******* Are there head,flux or polynomial boundary conditions ? *******
!
      write (MSG,9420)
      write (OUTOCD,9200) MSG
      write (OUTOCD,9105) '0','0','0'
!
!
!************* Channel elevations *******************************
!************* and channel widths *******************************
!
! convert net rainfall in mm to a mean annual flow in m3/s
      maffactor=cellsize*cellsize*netrainfall/(1000.0*8760.0*3600.0)

      write (MSG2,9430)
      write (outocd,9200) MSG2
      NDEFCT = 0
      write (outocd,9211) NDEFCT
      
      write (MSG2,9435)
      write (outocd,9200) MSG2
      wdepth=0.0
      nxsect=2
      coeff=50.0
      subrio=0.7
      if (direction.eq.1) then
          iface=2
      elseif (direction.eq.2) then
          iface=1
      elseif (direction.eq.3) then
          iface=4
      else
          iface=3
      endif
      do i=1,numlinks
          AFORM(1)=FORM(linkelv(i))
          AFORM(2)=FORM(WDEPTH)
          AFORM(3)=FORM(linkstr(i))    
          write (outocd,9205)i,AFORM(1),AFORM(2),AFORM(3),NXSECT       

          streamwidth1=streamsize(i)*maffactor
          streamwidth2=channelwidthfactor*(streamwidth1**channelwidthpower)
          aform(1)=form(streamwidth2)
          aform(2)=form(0.0)
          aform(3)=form(streamwidth2)
          aform(4)=form(channelbankheight)
          write (outocd,9210) AFORM(1),AFORM(2),AFORM(3),AFORM(4)


          if (i.eq.linkoutnum) then     
             AFORM(1)=FORM(COEFF)
             AFORM(2)=FORM(SUBRIO)
             AFORM(3)=FORM(linkelv(i)+0.1)
             AFORM(4)=FORM(linkelv(i)-1.0)
             write (outocd,9206) ifACE,AFORM(1),AFORM(2),AFORM(3),AFORM(4)
          endif
      enddo


!***************etd Data ***********************************************      
!*********************** Title *****************************************      
!*** Title and printing control and reading control parameters**********      

      write (MSG,9501)
      write (OUTETD,9200) MSG
      write (OUTETD,'(3A7,6X,L1)') 'F','F','T', IsMeteorologicalDataIncludeDate

!*** Timestep for input of rain and met. data *************************      

      AFORM(1)=FORM(prectmstep)
      AFORM(2)=FORM(petmstep)
      write (MSG,9503)
      write (OUTETD,9200) MSG
      write (OUTETD,9105) '1.0',AFORM(1),AFORM(2)

!**** read if potential evap is measured and thus read in ********* 
!      directly for each met. ststion in turn.
!     MEASPE=0: potential evap. not measured
!     MEASPE=1: potential evap. measured
      write (MSG,9505)
      write (OUTETD,9200) MSG
      do i=1,PeDistMax
         numbermet(i)=1
      enddo
      write (OUTETD,9123) (numbermet(i),i=1,PeDistMax)
!
!****** Vegetation types *********************************** 

      do i=1,icountveg
  
         
! find rooting depth 
         do j=1,27
            if (rootingdepth(i).le.depth(j)) then
               nmcellroot=j
               exit
            endif
        enddo


        if (lai(i).ge.1.0) then
            plai=1.0
            clai=lai(i)
        else
            plai=lai(i)
            clai=1.0
        endif

        NF=7     
        CK=0.000014
        CB=5.1
        NRD=nmcellroot

        write (MSG,'(A,I0,1X,A)') ':ET7 - VEGETATION TYPE ',i,vegtypes(i)

        write (OUTETD,9200) MSG


        AFORM(1) = FORM(PLAI)
        AFORM(2) = FORM(cstcap(i))
        AFORM(3) = FORM(CK)
        AFORM(4) = FORM(CB)
        AFORM(5) = FORM(clai)


        write (OUTETD,9255) 'F','0.','0.','0.','0.','0.','3','7',AFORM(1),AFORM(2),AFORM(3),AFORM(4),NRD,AFORM(5),'0','0'
         

!******* Check time-varying arrays 


        write (MSG,9509)
        write (OUTETD,9200) MSG

        if (cstcapnopoints(i).gt.0) then
           write (OUTETD,9105) '1','0','0','0'
           write (MSG,9518)
           write (OUTETD,9200) MSG
           cstcapnoyear=(endyear-year+1)*cstcapnopoints(i)
          
           write (OUTETD,9211) cstcapnoyear
           write (MSG,9519)
           write (OUTETD,9200) MSG
           do j=1,endyear-year+1
              do k=1,cstcapnopoints(i)
                 aform(1)=form(cstcapratio(i,k))
                 aform(2)=form(cstcaptime(i,k)+365.0*(j-1))
                 write (OUTETD,9105) aform(1),aform(2)
             enddo
           enddo

        else
           write (OUTETD,9105) '0','0','0','0'

        endif         


!************PSL/RCF/FET
        write (MSG,9515)
        write (OUTETD,9200) MSG
        PS1(1)=-1000
        PS1(2)=-150
        PS1(3)=-50
        PS1(4)=-20
        PS1(5)=-10
        PS1(6)=-1
        PS1(7)=-0.1
        FET(1)=0.0*aepe(i)
        FET(2)=0.05*aepe(i)
        FET(3)=0.20*aepe(i)
        FET(4)=0.50*aepe(i)
        FET(5)=0.80*aepe(i)
        FET(6)=1.00*aepe(i)
        FET(7)=1.00*aepe(i)
        do J=1,NF
             AFORM(1)=FORM(PS1(J))
             AFORM(2)=FORM(FET(J))
             write (OUTETD,9105) AFORM(1),'0.',AFORM(2)
        enddo



! read and write root density function data

        write (MSG,9517)
        write (OUTETD,9200) MSG

      
        do j=1,nmcellroot
            AFORM(1)=FORM(DEPTH(J))
            AFORM(2)=FORM(RDF(j,nmcellroot))
            write (OUTETD,9105) AFORM(1),AFORM(2)
        enddo

    enddo

!****** Vegetation types extra forest layer *********************************** 
    if (IsForestFile) then      

! the PET of a column is changed depending on the forest distribution of this column. 
! the assumption is that if this is used then every grid square has its own PET column. 
! petchange2 is the change factor for each column in the PEt time series file
            do i=1,nrows
               do j=1,ncols
                   if (pedist(i,j).ge.1) then
                       if (NFMForestDist2(i,j).gt.icountveg) then
                           petchange2(pedist(i,j))=1.0+(NFMForestDist2(i,j)-icountveg)/10.0
                       endif
                   endif
                enddo
             enddo
        
        

          changePETfile=trim(pefilescaler)
          tempPEtfile=trim(basedir)//'temp-PET.csv'

          OPEN(ChangedPETFile,FILE=changePETfile,STATUS='OLD')
          OPEN(ChangedPETFileTemp,FILE=tempPEtfile)
          read(ChangedPETFile,*) 
          write(ChangedPETFileTemp,*) 'Urban PET. ReducedPET in urban areas. Inceased PET with extra forest layer'              

          do
            read(ChangedPETFile, *, iostat=io) pet(1:PeDistMax)
            if (io /= 0) exit   ! exits on EOF (<0) or read error (>0)

            write(ChangedPETFileTemp, '(*(f6.2,", "))') pet(1:PeDistMax) * petchange2(1:PeDistMax)
          enddo
          
          
          rewind(ChangedPETFile)
          rewind(ChangedPETFileTemp)
          read(ChangedPETFileTemp,*) 
          write(ChangedPETFile,*) 'Urban PET. ReducedPET in urban areas. Inceased PET with extra forest layer'              

          do
            read(ChangedPETFileTemp, *, iostat=io) pet(1:PeDistMax)
            if (io /= 0) exit   ! exits on EOF (<0) or read error (>0)

            write(ChangedPETFile, '(*(f6.2,", "))') pet(1:PeDistMax)
          enddo

         close(ChangedPETFile)
         close(ChangedPETFileTemp,status="delete")
           
!*forest layers types 4-13 depending on percent cover
         do i=icountveg+1,icountveg+10
  
         
! find rooting depth 
           do j=1,27
              if (rootingdepth(1).le.depth(j)) then
                 nmcellroot=j
                 exit
             endif
           enddo


           if (lai(1).ge.1.0) then
             plai=1.0
             clai=lai(1)
           else
             plai=lai(1)
             clai=1.0
           endif

           NF=7     
           CK=0.000014
           CB=5.1
           NRD=nmcellroot

           write (MSG,'(A23,i4)') ':ET7 - VEGETATION TYPE ',i

           write (OUTETD,9200) MSG


           AFORM(1) = FORM(PLAI)
!forest fraction cstcap
           AFORM(2) = FORM(cstcap(1))
           AFORM(3) = FORM(CK)
           AFORM(4) = FORM(CB)
           AFORM(5) = FORM(clai)


           write (OUTETD,9255) 'F','0.','0.','0.','0.','0.','3','7',AFORM(1),AFORM(2),AFORM(3),AFORM(4),NRD,AFORM(5),'0','0'
         

!******* Check time-varying arrays 


           write (MSG,9509)
           write (OUTETD,9200) MSG

           if (cstcapnopoints(1).gt.0) then
              write (OUTETD,9105) '1','0','0','0'
              write (MSG,9518)
              write (OUTETD,9200) MSG
              cstcapnoyear=(endyear-year+1)*cstcapnopoints(1)
          
              write (OUTETD,9211) cstcapnoyear
              write (MSG,9519)
              write (OUTETD,9200) MSG
              do j=1,endyear-year+1
                do k=1,cstcapnopoints(1)
                   aform(1)=form(cstcapratio(i,k))
                   aform(2)=form(cstcaptime(i,k)+365.0*(j-1))
                   write (OUTETD,9105) aform(1),aform(2)
                enddo
              enddo

           else
              write (OUTETD,9105) '0','0','0','0'

           endif         


!************PSL/RCF/FET
!* add up t0 extra 350mm of pet (0.6 * 580mm) assuming 580 is pet in southern england
           write (MSG,9515)
           write (OUTETD,9200) MSG
           PS1(1)=-1000
           PS1(2)=-150
           PS1(3)=-50
           PS1(4)=-20
           PS1(5)=-10
           PS1(6)=-1
           PS1(7)=-0.1
!        FET(1)=0.0*(aepe(1)/(i-3)/5.0)
           FET(1)=0.0*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(2)=0.05*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(3)=0.20*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(4)=0.50*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(5)=0.80*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(6)=1.00*(aepe(1)-aepe(1)*(i-3)/20.0)
           FET(7)=1.00*(aepe(1)-aepe(1)*(i-3)/20.0)
           do J=1,NF
              AFORM(1)=FORM(PS1(J))
              AFORM(2)=FORM(FET(J))
              write (OUTETD,9105) AFORM(1),'0.',AFORM(2)
          enddo



! read and write root density function data

          write (MSG,9517)
          write (OUTETD,9200) MSG

      
          do j=1,nmcellroot
             AFORM(1)=FORM(DEPTH(J))
             AFORM(2)=FORM(RDF(j,nmcellroot))
             write (OUTETD,9105) AFORM(1),AFORM(2)
          enddo

       enddo
     endif


!****************vsd Data ***********************************************      
!************************ Title *****************************************      

!      if (icountsoil.gt.1) then
      OPEN(InSoil,FILE=soilname,STATUS='OLD')
!      endif

      write (MSG,9601)
      write (OUTVSD,9200) MSG
      write (OUTVSD,9201) 'VSS data'

      write (MSG,9602)
      write (OUTVSD,9200) MSG
      write (OUTVSD,9105) 'F','T','F'

      write (MSG,9603)
      write (OUTVSD,9200) MSG
      if (isspatialpslfile) then
          write (OUTVSD,9118) icountsoil,'35','0','3'
      else
          write (OUTVSD,9118) icountsoil,'35','0','1'
      endif

      write (MSG,9604)
      write (OUTVSD,9200) MSG
      AFORM(1)=FORM(initialpsl)
      write (OUTVSD,9103) AFORM(1),'0','0','1.0','1.0'

      write (MSG,9605)
      write (OUTVSD,9200) MSG

!************************ SOILS ********************************      
      do i=1,icountsoil


         write (OUTVSD,9207) i,'1','0',' ',soiltypes(i)
         AFORM(1) = FORM(ksat(i))
         AFORM(2) = FORM(thsat(i))
         AFORM(3) = FORM(thres(i))
         AFORM(4) = FORM(specstor(i))
         AFORM(5) = FORM(vgn(i))
         AFORM(6) = FORM(vga(i))
         write (OUTVSD,9404)  AFORM(1),AFORM(1),AFORM(1),AFORM(2),AFORM(3),AFORM(4),AFORM(5),AFORM(6)


      enddo

!************************ End of soils ********************************      

      maxsoildepth=maxval(soildepth(1:icountsoilcat))

      if (maxsoildepth.lt.22.0) then

         write (MSG,9606)
         write (OUTVSD,9200) MSG
         write (OUTVSD,9104) '0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1'
         write (OUTVSD,9104) '0.2','0.2','0.2','0.2','0.2','0.2','0.2','0.2','0.2','0.2'
         write (OUTVSD,9104) '1.0','1.0','1.0','1.0','1.0','1.0','1.0','1.0','1.0','1.0'
         write (OUTVSD,9104) '2.0','2.0','2.0','2.0','2.0'
      
      elseif (maxsoildepth.lt.50.0) then
          
         write (MSG,9606)
         write (OUTVSD,9200) MSG
         write (OUTVSD,9104)  '0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1'
         write (OUTVSD,9104)  '0.2','0.2','0.2','0.2','0.2','1.0','1.0','1.0','1.0','1.0'
         write (OUTVSD,9104)  '1.0','2.0','2.0','2.0','2.0','2.0','2.0','2.0','2.0','2.0'
         write (OUTVSD,9104)  '5.0','5.0','5.0','5.0','5.0'

     elseif (maxsoildepth.lt.100.0) then
          
         write (MSG,9606)
         write (OUTVSD,9200) MSG
         write (OUTVSD,9104)  '0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.1'
         write (OUTVSD,9104)  '0.2','0.2','0.2','0.2','0.2','1.0','1.0','1.0','2.0','2.0'
         write (OUTVSD,9104)  '2.0','2.0','5.0','5.0','5.0','5.0','5.0','5.0','5.0','5.0'
         write (OUTVSD,9104)  '10.0','10.0','10.0','10.0','10.0'
     
    elseif (maxsoildepth.lt.200.0) then
          
         write (MSG,9606)
         write (OUTVSD,9200) MSG
         write (OUTVSD,9104) '0.1','0.1','0.1','0.1','0.1','0.1','0.1','0.2','0.2','0.2'
         write (OUTVSD,9104) '0.2','1.0','1.0','1.0','1.0','2.0','2.0','2.0','5.0','5.0'
         write (OUTVSD,9104) '5.0','5.0','5.0','5.0','10.0','10.0','10.0','10.0','10.0','10.0'
         write (OUTVSD,9104)  '20.0','20.0','20.0','20.0','20.0'

     elseif (maxsoildepth.lt.380.0) then
          
         write (MSG,9606)
         write (OUTVSD,9200) MSG
         write (OUTVSD,9104) '0.1','0.1','0.1','0.1','0.1','0.1','0.2','0.2','0.5','0.5'
         write (OUTVSD,9104) '1.0','1.0','2.0','5.0','5.0','5.0','10.0','10.0','20.0','20.0'
         write (OUTVSD,9104) '20.0','20.0','20.0','20.0','20.0','20.0','20.0','20.0','20.0','20.0'
         write (OUTVSD,9104) '20.0','20.0','20.0','20.0','20.0'
      
      else
         write(*,*)
         write(*,*)' This version of shetran prepare has a limit of 380m soil/rock depth.'
         write(*,*)' The xml file contains deeper soils/rocks '
         write(*,*)
         write(*,'(''paused, type [enter] to continue'')')
         read (*,*)
         stop
      endif      
      
      maxcatnumber=soilcats(icountsoilcat)


      write (MSG,9608)
      write (OUTVSD,9200) MSG
      write (OUTVSD,9118) maxcatnumber,'0'

      write (MSG,9658)
      write (OUTVSD,9200) MSG

      
      do i =1,icountsoilcat
         aform(i)=form(soildepth(i))
      enddo
      
     
      soildepthmin=500.0
      pc=0
! make a dummy extra soil category one higher than the final one
      soilcats(icountsoilcat+1)=soilcats(icountsoilcat)+1
!if the first soli category is not 1 then add extra lines      
      if (soilcats(1).ne.1) then
              do j=1,soilcats(1)-1
                  write (OUTVSD,9120) j,1
                  write (OUTVSD,9121) ' ',1
                  write (OUTVSD,9122) ' ',aform(1)
              enddo
      endif
   
      do i=1,icountsoilcat
          if (soilnumbers2(i).gt.icountsoil) then  
                 write (logfile,*) 
                 write (logfile,*) 'A value greater than the maximimum SoilProperty number been specfied in SoilDetail column . It has been changed to number 1'
                 write (logfile,'(A,I0,A,I0,A,I0)') 'The values was ', soilnumbers2(i),' at soil category = ',soilcats(i),' soil layer = ',soillayers(i)
                 soilnumbers2(i)=1
          endif
      enddo
      
      
      do i=1,icountsoilcat
        do
           soilcatchange= soilcats(i+1)-soilcats(i)
           if (soilcatchange==0) then
              exit
           elseif (soilcatchange==1) then
              write (OUTVSD,9120) soilcats(i),i-pc
              write (OUTVSD,9121) ' ',(soilnumbers2(j),j=i,pc+1,-1)
              write (OUTVSD,9122) ' ',(aform(j), j=i,pc+1,-1)
              soildepthmin=min(soildepth(i),soildepthmin)
              pc=i
              exit
           else
              write (OUTVSD,9120) soilcats(i),i-pc
              write (OUTVSD,9121) ' ',(soilnumbers2(j),j=i,pc+1,-1)
              write (OUTVSD,9122) ' ',(aform(j), j=i,pc+1,-1)
              soildepthmin=min(soildepth(i),soildepthmin)
              pc=i
 ! no data for these category numbers  - use default values
              do j=1,soilcatchange-1
                  write (OUTVSD,9120) soilcats(i)+j,1
                  write (OUTVSD,9121) ' ',1
                  write (OUTVSD,9122) ' ',aform(1)
              enddo
              exit
           endif
         enddo
             
            
      enddo

      if (isbanks) then
! extra output needed in vsd file if there are banks. All links given first category type
          write (MSG,9659)
          write (OUTVSD,9200) MSG

          write (MSG,'(I0,A)') numlinks,'*1'
          write (OUTVSD,9200) MSG
     endif
      
      
      if (maxcatnumber.gt.1) then
         write (MSG2,9668)
         write (outvsd,9200) MSG2

         read(InSoil,*) 
         read(InSoil,*) 
         read(InSoil,*) 
         read(InSoil,*) 
         read(InSoil,*) 
         read(InSoil,*) 
         do i=2,nrows-1
           read(InSoil,*) (soildist(i,j),j=2,ncols-1)
           do j=2,ncols-1
              if (soildist(i,j).lt.0) then 
                 soildist(i,j)=0
                   write (logfile,*) 
                   write (logfile,*) 'A negative value has been specfied for a location in the soil cateogry map file. It has been changed to category 0'
                 if ((catch(i,j).eq.1).and.(soildist(i,j).eq.0)) then
                    soildist(i,j)=1
                       write (logfile,*) 
                       write (logfile,*) 'A zero value has been specfied for a location in the soil cateogry map file that is within the catchment. It has been changed to category 1'
                 endif
!               make sure there is a soil category defined for each category in the soil category map file. If the value is too high then set it to one
              endif
              if (soildist(i,j).gt.maxcatnumber) then
                       write (logfile,*) 
                       write (logfile,*) 'A value greater than the maximimum SoilDetail been specfied for a location in the soil cateogry map file. It has been changed to category 1'
                       write (logfile,'(A,I0,A,I0,A,I0)') 'The values was ',soildist(i,j),' at location row = ',i-1,' column = ',j-1
                       soildist(i,j) = 1
              endif
           enddo
        enddo
        do j=1,ncols
           soildist(1,j)=0
           soildist(nrows,j)=0
        enddo
        do i=1,nrows
           soildist(i,1)=0
           soildist(i,ncols)=0
        enddo
        if (maxcatnumber.lt.10) then
           do I = 1,nrows
              k=nrows-i+1
              write (outvsd,9108) k,(soildist(i,j),j=1,ncols)
           enddo  
        else
           do I = 1,nrows
              k=nrows-i+1
              write (outvsd,9208) k,(soildist(i,j),j=1,ncols)
           enddo  
        endif

     endif

      if (isbanks) then
! extra output needed in vsd file if there are banks. All links given first category type
          write (MSG,9669)
          write (OUTVSD,9200) MSG

          write (MSG,'(I0,A)') numlinks,'*1'
          write (OUTVSD,9200) MSG

          write (MSG,9670)
          write (OUTVSD,9200) MSG

          write (MSG,'(I0,A)') numlinks,'*1'
          write (OUTVSD,9200) MSG

      
      endif


     write (MSG2,9610)
     write (outvsd,9200) MSG2
     write (OUTVSD,9105) '0'
     write (MSG2,9611)
     write (outvsd,9200) MSG2

! base flow boundary additional output     
     if (icountbfb /= 0) then
         write (OUTVSD,9104) '0','0','0','0','0','1','0','0'
         if (isbanks) then
! extra output needed in vsd file if there are banks. All links given first category type
             write (MSG,9612)
             write (OUTVSD,9200) MSG

             write (MSG,'(I0,A)') numlinks,'*1'
            write (OUTVSD,9200) MSG
        endif
         write (MSG2,9612)
         write (outvsd,9200) MSG2
      
         where (catch == 1)
            catchboundary = 6
         end where
        
         do I = 1,nrows
            k=nrows-i+1
            write (outvsd,9108) k,(catchboundary(i,j),j=1,ncols)
         enddo
         if (isbanks) then
! extra output needed in vsd file if there are banks. All links given first category type
             write (MSG,9613)
             write (OUTVSD,9200) MSG

             write (MSG,'(I0,A)') numlinks,'*1'
            write (OUTVSD,9200) MSG
        endif
         write (MSG2,9613)
         write (outvsd,9200) MSG2
        if (maxcatnumber.lt.10) then
           do I = 1,nrows
              k=nrows-i+1
              write (outvsd,9108) k,(soildist(i,j),j=1,ncols)
           enddo  
        else
           do I = 1,nrows
              k=nrows-i+1
              write (outvsd,9208) k,(soildist(i,j),j=1,ncols)
           enddo  
        endif
 
! write to bfb file        
        write (MSG2,9614)
        write (outbfb,9200) MSG2
! add a year to the end time so it does not crash
         do i =1,icountbfb
             baseflowboundary2(bfbcats(i)) = baseflowboundary(i)
         enddo
         write (OUTBFB,'(5I7,*(1X,A7))') endyear+1,endmonth,endday,endhour,endminute, (form(baseflowboundary2(i)),i=1,maxcatnumber)

     else
         write (OUTVSD,9104) '0','0','0','0','0','0','0','0'
     endif

     close(InSoil)

!****************smd Data ***********************************************
!************************ Title *****************************************

if (IsSnow) then

      write (MSG,9901)
      write (OUTSMD,9200) MSG
      write (OUTSMD,9105) 'T'

      write (MSG,9903)
      write (OUTSMD,9200) MSG
      AFORM(1) = FORM(snowddf)

      write (OUTSMD,9103) aform(1),'0.6500','-1.00','0','1'

      write (MSG,9907)
      write (OUTSMD,9200) MSG
      write (OUTSMD,9105) '0.0'

endif





!****************rundata ***********************************************      
!************************ Title *****************************************      
      write (MSG2,9701)
      write (outrun,9200) MSG2
      write (MSG2,9710)
      write (outrun,9200) MSG2

      write (outrun,9200) FILFRD2

      write (MSG2,9711)
      write (outrun,9200) MSG2

      write (MSG2,9200) FILVSD2
      write (outrun,9200) MSG2

      write (MSG2,9712)
      write (outrun,9200) MSG2



      write (MSG2,9200) FILOCD2
      write (outrun,9200) MSG2

      write (MSG2,9713)
      write (outrun,9200) MSG2

      write (MSG2,9200) FILETD2
      write (outrun,9200) MSG2

      write (MSG2,9714)
      write (outrun,9200) MSG2
      write (outrun,*) 

      write (MSG2,9715)
      write (outrun,9200) MSG2

      if (IsSnow) then
         write (MSG2,9200) FILSMD2
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif

      write (MSG2,9716)
      write (outrun,9200) MSG2
      if (IsBanks .or. IsSolute) then
          write (MSG2,9200) FILBKD2
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9717)
      write (outrun,9200) MSG2
      if (IsSediment) then
         write (MSG2,9200) FILSYD2
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9718)
      write (outrun,9200) MSG2
      if (IsSolute) then
         write (MSG2,9200) FILCMD2
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9719)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9720) 
      write (outrun,9200) MSG2

!      print*,filprd

      write (MSG2,9200) filprd
      write (outrun,9200) MSG2

      write (MSG2,9721)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILEPD
      write (outrun,9200) MSG2

      write (MSG2,9722) 
      write (outrun,9200) MSG2


      write (MSG2,9200) FILTIM
      write (outrun,9200) MSG2
      write (MSG2,9723)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILPRI
      write (outrun,9200) MSG2

      write (MSG2,9724)
      write (outrun,9200) MSG2
      if (IsSediment) then
         write (MSG2,9200) FILSPR
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9725)
      write (outrun,9200) MSG2
      if (IsSolute) then
         write (MSG2,9200) FILCPR
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9726)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9727)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9728)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9729)
      write (outrun,9200) MSG2
 
      if (isspatialpslfile) then
          write (MSG2,9200) filespatialpslout2
          write (outrun,9200) MSG2
      else
         write (outrun,*) 
      endif
      write (MSG2,9730)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9731)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9732)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9733)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9734)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9735)
      write (outrun,9200) MSG2
      if (icountbfb /= 0) then
          write (MSG2,9200) FILBFB2
          write (outrun,9200) MSG2
      else
          write (outrun,*) 
      endif
      write (MSG2,9736)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9737)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9738)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9739)
      write (outrun,9200) MSG2
      write (outrun,*) 
      write (MSG2,9740)
      write (outrun,9200) MSG2
      write (outrun,*) 
 
      write (MSG2,9741)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILDIS
      write (outrun,9200) MSG2

      write (MSG2,9742)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILVSE
      write (outrun,9200) MSG2

      write (MSG2,9743)
      write (outrun,9200) MSG2

      write (MSG2,9200) FILMAS
      write (outrun,9200) MSG2

      write (MSG2,9744)
      write (outrun,9200) MSG2

      write (outrun,9200) FILDIS2
      write (MSG2,9745)
      write (outrun,9200) MSG2

      if (trim(tmaxfile(1))=='Empty') then
          write (outrun,*)
      else
          write (outrun,9200)tmaxfile
      endif
      write (MSG2,9746)
      write (outrun,9200) MSG2

      if (trim(tminfile(1))=='Empty') then
          write (outrun,*)
      else
          write (outrun,9200)tminfile
      endif
      write (MSG2,9747)
      write (outrun,9200) MSG2
      if (isextradischarge) then
          write (MSG2,9200) FILDISPOINT2
          write (outrun,9200) MSG2
      else
! the file is now written out to the rundata file even if there are no extra discharge points
!          write (MSG2,9200) FILDISPOINT2
!          write (outrun,9200) MSG2
! the file is not written out to the rundata file  if there are no extra discharge points. If it is present then regular simulated discharge output produces an extra line
         write (outrun,*) 
      endif

      write (MSG2,9748)
      write (outrun,9200) MSG2

 
      write (MSG2,9200) FILVIS2
      write (outrun,9200) MSG2

      write (MSG2,9749)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILCVI
      write (outrun,9200) MSG2

      write (MSG2,9750)
      write (outrun,9200) MSG2


      write (MSG2,9200) FILHDF
      write (outrun,9200) MSG2

!****************visulisation-plan**************************************      
!************************ Title *****************************************   
      vislayerdepth(1:35)=(/0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,15.0,17.0,19.0,21.0,23.0,500.0/)
      do i=1,35
          if (soildepthmin.le.vislayerdepth(i)) then 
            vislayer = i
            exit
          endif
      enddo
      
      
 9801 FORMAT ("'visualisation plan'")
 9802 FORMAT ("diag")
 9803 FORMAT ("item")
 9804 FORMAT ("NUMBER^1 : NAME^net_rain : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9805 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")
 9806 FORMAT ("NUMBER^2 : NAME^ph_depth : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9807 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")
 9808 FORMAT ("NUMBER^3 : NAME^theta : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9809  FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1 10 : ENDITEM")
 9810 FORMAT ("NUMBER^4 : NAME^ovr_flow : BASIS^grid_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9811 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")
 9812 FORMAT ("NUMBER^5 : NAME^srf_dep : BASIS^grid_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^none")
 9813  FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")

 9814  FORMAT ("list")
 9815 FORMAT ("6 1   !number and size")
 9816 FORMAT ("mask")
 9817 FORMAT ("times")
      
 9818 FORMAT ("8 1 !number and no. of entries")
 9820 FORMAT ("730 876000 !every 730 hours for 100 years")
 9821 FORMAT ("9 1 !number and no. of entries")
 9822 FORMAT ("24 876000 !every 24 hour for 100 years")
 9823  FORMAT ("stop")

! Snow      
 9832  FORMAT ("NUMBER^6 : NAME^snow_dep : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9833  FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : ENDITEM")

! Sediment       
 9854 FORMAT ("NUMBER^7 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9855 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^1 :ENDITEM")
 9856 FORMAT ("NUMBER^8 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9857 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^2 :ENDITEM")
 9858 FORMAT ("NUMBER^9 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9859 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^3 :ENDITEM")
 9860 FORMAT ("NUMBER^10 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9861 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^4 :ENDITEM")
 9862 FORMAT ("NUMBER^11 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9863 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^5 :ENDITEM")
 9864 FORMAT ("NUMBER^12 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9865 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^6 :ENDITEM")
 9866 FORMAT ("NUMBER^13 : NAME^s_dis : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9867 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^7 :ENDITEM")
 9868 FORMAT ("NUMBER^14 : NAME^s_v_er : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9869 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 :ENDITEM")
 9870 FORMAT ("NUMBER^15 : NAME^s_t_dp : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9871 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 :ENDITEM ")
 9872 FORMAT ("NUMBER^16 : NAME^s_t_dp : BASIS^list_as_list : SCOPE^rivers :  EXTRA_DIMENSIONS^none")
 9873  FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 :ENDITEM")

!solute
9874 FORMAT ("NUMBER^17 : NAME^c_c_dr : BASIS^list_as_list : SCOPE^rivers  :  EXTRA_DIMENSIONS^none")
9875 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : CONTAMINANT_NO^1 : LAYERS^1 3 :ENDITEM")
9876 FORMAT ("NUMBER^18 : NAME^c_c_dr : BASIS^grid_as_grid : SCOPE^squares :  EXTRA_DIMENSIONS^none")
9877 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : CONTAMINANT_NO^1 : LAYERS^1 42 :ENDITEM")



      write (MSG,9801)
      write (outvis,9260) MSG
      write (outvis,*) '!',trim(catchmentname(1))
      write (MSG,9802)
      write (outvis,9260) MSG
      write (MSG,9803)
      write (outvis,9260) MSG
      write (MSG,9804)
      write (outvis,9260) MSG
      write (MSG,9805)
      write (outvis,9260) MSG
      write (MSG,9803)
      write (outvis,9260) MSG
      write (MSG,9806)
      write (outvis,9260) MSG
      write (MSG,9807)

      write (outvis,9260) MSG
      write (MSG,9803)
      write (outvis,9260) MSG
      write (MSG,9808)
      write (outvis,9260) MSG
      write (MSG,'(A,1x,I0,A)') 'GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1',vislayer, ' : ENDITEM'
      write (outvis,9260) MSG
      write (MSG,9803)
      write (outvis,9260) MSG
      write (MSG,9810)
      write (outvis,9260) MSG
      write (MSG,9811)
      write (outvis,9260) MSG
      write (MSG,9803)
      write (outvis,9260) MSG
      write (MSG,9812)
      write (outvis,9260) MSG
      write (MSG,9813)
      write (outvis,9260) MSG
      if (IsSnow) then 
          write (MSG,9803)
          write (outvis,9260) MSG
          write (MSG,9832)
          write (outvis,9260) MSG
          write (MSG,9833)
          write (outvis,9260) MSG
      endif

      if (IsSediment) then 
          write (MSG,9803)
          write (outvis,9260) MSG
          WRITE (MSG,9854)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9855)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9856)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9857)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9858)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9859)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9860)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9861)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9862)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9863)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9864)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9865)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9866)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9867)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9868)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9869)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9870)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9871)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9803)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9872)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9873)
          WRITE (outvis,9260) MSG
      endif

      if (IsSolute) then 
          write (MSG,9803)
          write (outvis,9260) MSG
          WRITE (MSG,9874)
          WRITE (outvis,9260) MSG
          WRITE (MSG,9875)
          WRITE (outvis,9260) MSG
          write (MSG,9803)
          write (outvis,9260) MSG
          WRITE (MSG,9876)
          WRITE (outvis,9260) MSG
          write (MSG,'(A,1x,I0,A)') 'GRID_OR_LIST_NO^7 : TIMES^8 : CONTAMINANT_NO^1 : LAYERS^1',vislayer,' : ENDITEM'
         write (outvis,9260) MSG
     endif
      
!************list
      write (outvis,*)
      write (MSG,9814)
      write (outvis,9260) MSG
      write (MSG,9815)
      write (outvis,9260) MSG
      write (outvis,9263) linkoutnum

!*******sjb works for x and y >10 
      write (outvis,*)
      write (MSG,9816)
      write (outvis,9260) MSG
      if ((nrows.ge.100).and.(ncols.ge.100)) then
          write (outvis,9271) '7','1',nrows,'1',ncols,'!number and row and column limits'
      elseif ((nrows.ge.100).and.(ncols.lt.100)) then
          write (outvis,9272) '7','1',nrows,'1',ncols, '!number and row and column limits'
      elseif ((nrows.lt.100).and.(ncols.ge.100)) then
          write (outvis,9273) '7','1',nrows,'1',ncols,'!number and row and column limits'
      elseif ((nrows.lt.10).and.(ncols.lt.10)) then
          write (outvis,9275) '7','1',nrows,'1',ncols,'!number and row and column limits'
      elseif ((nrows.ge.10).and.(ncols.lt.10)) then
          write (outvis,9277) '7','1',nrows,'1',ncols,'!number and row and column limits'
      elseif ((nrows.lt.10).and.(ncols.ge.10)) then
          write (outvis,9276) '7','1',nrows,'1',ncols,'!number and row and column limits'
      else  
          write (outvis,9274) '7','1',nrows,'1',ncols,'!number and row and column limits'
      endif 



!      write (outvis,9261) '7','1',nrows,'1',ncols,
!     $   '!number and row and column limits'

      write (outvis,9262)'!row low, row high, column low, column high'

      do i=1,nrows
         do j=1,ncols
            if (catch(i,j).eq.0) then
               vismask(i,j)='.'
            else
               vismask(i,j)='1'
            endif
         enddo
      enddo

      do I = 1,nrows
         write (outvis,9265)(vismask(i,j),j=1,ncols)
      enddo  

!*********times
      
      write (outvis,*)
      write (MSG2,9817)
      write (outvis,9200) MSG2
      write (MSG2,9818)
      write (outvis,9200) MSG2
      write (outvis,'(I0,1X,I0,1X,A,I0,A)') int(threedimensionalh5output), max(int(simulationTimeHours),876000), '!every ',int(threedimensionalh5output), ' hours for at least 100 years'

      write (outvis,*)
      write (MSG2,9817)
      write (outvis,9200) MSG2
      write (MSG2,9821)
      write (outvis,9200) MSG2
      write (outvis,'(I0,1X,I0,1X,A,I0,A)') int(standardh5output), max(int(simulationTimeHours),876000), '!every ', int(standardh5output),' hours for at least 100 years'


      write (outvis,*)
      write (MSG2,9823)
      write (outvis,9200) MSG2

      
!*****************sediment file******************************************
!************************************************************************
 
   if (issediment) then  
      
      
 8901 FORMAT(':SY01 - SY job title')
 8902  FORMAT(':SY02 - SY Version number')
 8911 FORMAT (':SY11 - NSED,ISGSED,ISTEC,ISSYOK,NEPS [ISACKW,ISUSED,NFINE if NLF>0 ]')
 8912  FORMAT(':SY12 - FPCRIT DLSMAX [ , ALPHA, CONCOB, DCBEDO, FBIC, FICRIT if NLF>0 ]')
 8921 FORMAT(':SY21 - DRSED(1:NSED) : Sediment diameter')
 8922 FORMAT(':SY22 - GKR(s),GKF(s),RHOSO(s),FPCLAY(s),BKB(s) for s=1 to NS : Soil properties')
 8923 FORMAT(':SY23 - SOSDFN(s,1:NSED) for s=1 to NS :Soil composition by size group')
 8924 FORMAT(':SY24 - XDRIP(v), DRDRIP(v), FDRIP(v) for v=1 to NV :Vegetation properties')
 8931 FORMAT(':SY31 - NTSOBK(l) for l=1 to NLF : Bank soil type')
 8932 FORMAT(':SY32 - PBSED(l) for l=1 ro NLF:Porosity of bed sediment')
 8941 FORMAT(':SY41 - NCAT : no. of categories for FCG')
 8942 FORMAT(':SY41c  FCG(1:NCAT) : Ground cover fraction')
 8943 FORMAT(':SY42 - NCAT : no. of categories for FCROCK')
 8944 FORMAT(':SY42c  FCROCK(1:NCAT) :  Fraction of non-erodible ground surface')
 8945 FORMAT(':SY43 - NCAT : no. of categories for PLS porosity')
 8946 FORMAT(':SY43c  PLS(1:NCAT) : Porosity of loose sediment')
 8951 FORMAT(':SY51 - NCAT : no. of categories for DLS loose depth')
 8952 FORMAT(':SY51c  DLS(1:NCAT) : Initial loose/bed sediment depth')
 8953 FORMAT(':SY52 - NCAT : no. of categories for FBETA; or -1 to copy from SOSDFN')
 8954 FORMAT(':SY52c  FBETA(1:NCAT) : Initial fraction of loose/bed sediment')
 8955 FORMAT(':SY52d  IDUM(link) category for FBETA')
 8956 FORMAT(':SY52e  IDUM(x,y) category for FBETA')
 8957 FORMAT(':SY53 - NCAT : no. of categories for FDEL')
 8958 FORMAT(':SY53c  "FDEL"(1:NSED,c) for c=1 to NCAT : Initial suspended sediment fractions')
 8961  FORMAT(':SY61 - NSYB, NSYC(1:4) : no. of boundaries; no. of categories for each type')

      
      WRITE (MSG2,8901)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,9200) catchmentname

      WRITE (MSG2,8902)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '4.4.5'

      WRITE (MSG2,8911)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      7      1      0      1      1      1       1      1'
      WRITE (MSG2,8912)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*)'     0.25     0.05    1.0    0.0    0.01   0.0    0.0'

      WRITE (MSG2,8921)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*)'      .1D-3   .37D-3  .89D-3  1.59D-3  2.25D-3  3.25D-3  8.0D-3'


      WRITE (MSG2,8922)
      WRITE (outsyd,9200) MSG2
      do i=1,icountsoil
      WRITE (outsyd,*)'       2.5   1.0D-5   1.537D3   0.26    0.0'
      enddo

      WRITE (MSG2,8923)
      WRITE (outsyd,9200) MSG2
      do i=1,icountsoil
      WRITE (outsyd,*)'     0.60   0.20   0.10  0.05  0.03  0.02  0.0'
      enddo

      WRITE (MSG2,8924)
      WRITE (outsyd,9200) MSG2
      do i=1,icountveg
      WRITE (outsyd,*)'        3.0      0.005      0.50'
      enddo

      WRITE (MSG2,8931)
      WRITE (outsyd,9200) MSG2
      WRITE (MSG2,'(I6,A2)') numlinks,'*1'
      WRITE (outsyd,9200) MSG2

      WRITE (MSG2,8932)
      WRITE (outsyd,9200) MSG2
      WRITE (MSG2,'(I6,A5)') numlinks,'*0.650'
      WRITE (outsyd,9200) MSG2

      WRITE (MSG2,8941)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      1'
      WRITE (MSG2,8942)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      0.3'
      WRITE (MSG2,8943)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      1'
      WRITE (MSG2,8944)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      0.0'
      WRITE (MSG2,8945)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      1'
      WRITE (MSG2,8946)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      0.3'
      WRITE (MSG2,8951)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      1'
      WRITE (MSG2,8952)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      0.03'
      WRITE (MSG2,8953)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      2'
      WRITE (MSG2,8954)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*)'     0.60   0.20   0.10  0.05  0.03  0.02  0.0'
      WRITE (outsyd,*)'     0.01   0.02   0.04  0.07  0.16  0.25  0.45'
      WRITE (MSG2,8955)
      WRITE (outsyd,9200) MSG2
      WRITE (MSG2,'(I6,A2)') numlinks,'*2'
      WRITE (outsyd,9200) MSG2
      WRITE (MSG2,8956)
      WRITE (outsyd,9200) MSG2
      DO I = 1,nrows
        k=nrows-i+1
        WRITE (outsyd,9108) k,(catch(i,j),j=1,ncols)
      ENDDO
      WRITE (MSG2,8957)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      1'
      WRITE (MSG2,8958)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      7*0.0'
      WRITE (MSG2,8961)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '     0      4*0'
   endif

   
!*****************solute file******************************************
!************************************************************************
 
   if (issolute) then  
! if solute is run then banks are also required
      
      
 8830 FORMAT(':CM1 Title')
 8831 FORMAT (':CM3 - Number of contaminants')
 8832 FORMAT(':CM5 - Flux boundary at base of column')
 8833 FORMAT(':CM7 - Default cell number at base of columns (-1 uses the same as modelled region)')
 8834 FORMAT(':CM9 - Number of column where bottom cell number not default')
 8835 FORMAT(':CM13 - Non-linear adsorption')
 8836 FORMAT(':CM15 - Initial depth of bed surface layer (only important for rivers)')
 8837 FORMAT(':CM17 - Initial depth of bed deep layer (only important for rivers)')
 8838 FORMAT(':CM19 - Number of contaminant for which there are property data')
 8839 FORMAT(':CM21 - Number of soil types for which there is contaminant data')
 8840 FORMAT(':CM23 - number of sediment sizes for which there are contaminant data')
 8841 FORMAT(':CM25 - ISCNSV : Is the concaminant 1 concentration spatial variable')
 8842 FORMAT(':CM26a - CCAPIN: Relative Concentration in each link element')
 8843 FORMAT(':CM26b  NCAT : no. of typical element catorgories')
 8844 FORMAT(':CM26c  NCATTY: catagory for each element')
 8845 FORMAT(':CM26d - NTAB : no of vaules in the table for NCATTY category 1')
 8846 FORMAT(':CM26e - DTAB,CTAB: depth,relative concentration for NTAB values category 1 ')
 8847 FORMAT(':CM26d - NTAB : no of vaules in the table for NCATTY category 2 (not used in the example)')
 8848 FORMAT(':CM26e - DTAB,CTAB: depth,relative concentration for NTAB values category 2')
 8849 FORMAT(':CM27 - Concentrations in rainfall')
 8850 FORMAT(':CM29 - number of columns which receive flow from outside')
 8851 FORMAT(':CM33 - Default concentrations into the base of the columns')
 8852 FORMAT(':CM35 - Number of columns where base concentration is not default')
 8853  FORMAT(':CM39 - Rate of dry deposition for each contaminant')
 8854 FORMAT(':CM41 - Size fractions for each soil type')
 8855 FORMAT(':CM43 - Freundlich isotherm pwer constant')
 8856 FORMAT(':CM45 - Chemical decay constant')
 8857 FORMAT(':CM47 - Coefficients for exchange between bed layers')
 8858 FORMAT(':CM49 - Coefficients for exchange between bed and water')
 8859 FORMAT(':CM51 - Kd for each particle size and contaminant')
 8860 FORMAT(':CM53 - Coefficient of exchange between soil regions')
 8861  FORMAT(':CM55 - Fraction of adsorption sites in the dynamic region')
 8862 FORMAT(':CM57 - Fraction of pore water in dynamic region')
 8863 FORMAT(':CM59 - Diffusion coefficient')
 8864 FORMAT(':CM61 - Dispersion coefficient')

      
      WRITE (MSG2,8830)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,9200) catchmentname

      WRITE (MSG2,8831)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '1'

      WRITE (MSG2,8832)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') 'False'

      WRITE (MSG2,8833)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '-1'

      WRITE (MSG2,8834)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0'

      WRITE (MSG2,8835)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') 'False'

      WRITE (MSG2,8836)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.1'

      WRITE (MSG2,8837)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.5'

      WRITE (MSG2,8838)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '1'

      WRITE (MSG2,8839)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(I0)') icountsoil

      WRITE (MSG2,8840)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '7'

      WRITE (MSG2,8841)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') 'True'

      WRITE (MSG2,8842)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.001'

      WRITE (MSG2,8843)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '2'

      WRITE (MSG2,8844)
      WRITE (outcmd,9200) MSG2
      DO I = 1,nrows
        k=nrows-i+1
        WRITE (outcmd,9108) k,(catch(i,j),j=1,ncols)
      ENDDO

      WRITE (MSG2,8845)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '      3'

      WRITE (MSG2,8846)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '    0.0   10.0'
      WRITE (outcmd,'(A)') '    0.1   0.01'
      WRITE (outcmd,'(A)') '    1.0   0.01'


      WRITE (MSG2,8847)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '      5'

      WRITE (MSG2,8848)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '    0.0   0.01'
      WRITE (outcmd,'(A)') '    0.1   0.01'
      WRITE (outcmd,'(A)') '    1.0   0.01'
      WRITE (outcmd,'(A)') '    5.0   0.01'
      WRITE (outcmd,'(A)') '   20.0   0.01'

      WRITE (MSG2,8849)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.10'

      WRITE (MSG2,8850)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0'

      WRITE (MSG2,8851)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.0'

      WRITE (MSG2,8852)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0'

      WRITE (MSG2,8853)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '1.0D-07'

      WRITE (MSG2,8854)
      WRITE (outcmd,9200) MSG2
      do i=1,icountsoil
          WRITE (outcmd,'(I0,A)') i,' 0.33 0.33 0.33'
      enddo

      WRITE (MSG2,8855)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '1.0'

      WRITE (MSG2,8856)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.0'

      WRITE (MSG2,8857)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '3.0D-07'

      WRITE (MSG2,8858)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '3.0D-07'

      WRITE (MSG2,8859)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '1 1.0 1.0 1.0 1.0 1.0 1.0 1.0'

      WRITE (MSG2,8860)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A,*(1X,A))') '1',('3.0D-7', i=1,icountsoil)

      WRITE (MSG2,8861)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A,*(1X,A))') '1',('0.1', i=1,icountsoil)

      WRITE (MSG2,8862)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A,*(1X,A))') '1',('0.5', i=1,icountsoil)

      WRITE (MSG2,8863)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A)') '0.5'

      WRITE (MSG2,8864)
      WRITE (outcmd,9200) MSG2
      WRITE (outcmd,'(A,*(1X,A))') '1',('0.5', i=1,icountsoil)


   endif
   
   
   
 !*****************banks file******************************************
!************************************************************************
!note that if the banks file is used some changes are required to the vsd file.
! if solute is run then banks are also required
  
if (isbanks) then   
   
 8811  FORMAT(':BK3 - data type 1 : ZGRUND (see FR39)')
 8812  FORMAT(':BK3 - data type 2 : NMC (see FR33 and FR44)')
 8813  FORMAT(':BK3 - data type 3 : NRAINC (see FR33 and FR47)')
 8814  FORMAT(':BK3 - data type 4 : NVC (see FR33 and FR50)')
 8815  FORMAT(':BK3 - data type 5 : NLYRC (see FR33 and FR53)')
 8816  FORMAT(':BK3 - data type 6 : STRX (see OC2, OC3a, OC4 and OC16)')
 8817  FORMAT(':BK3 - data type 7 : STRY (see OC2, OC3a, OC4 and OC19)')
 8818  FORMAT(':BK3 - data type 8 : ICATUZ (see UZ23, UZ24, UZ27 and UZ29)')
 8819  FORMAT(':BK3 - data type 9 : CATUZ (see UZ31 and UZ34)')
 8820  FORMAT(':BK3 - data type 10 : SD (see SM8 and SM11)')
 8821  FORMAT(':BK3 - data type 11 : RHOSAR (see SM8 and SM14)')
 8822  FORMAT(':BK3 - data type 12 : HSZ (see SZ4 and SZ29)')
 8823  FORMAT(':BK3 - data type 13 : HRF (see OC1a and OC7)')

      WRITE (MSG2,'(A,A)') ':BK1 - BK COMPONENT DATA SET. Catchment: ',trim(catchmentname(1))
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      T'
      WRITE (outbkd,9200) MSG2

      WRITE (MSG2,8811)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8812)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8813)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8814)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8815)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8816)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8817)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8818)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8819)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8820)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8821)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8822)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2


      WRITE (MSG2,8823)
      WRITE (outbkd,9200) MSG2
      WRITE (MSG2,'(A)') '      1      0'
      WRITE (outbkd,9200) MSG2

 endif     
     

!***************end of final output************************************
!**********************************************************************

      return

901 write(*,*) "Error in reading the canopy storage capacity file"
    write(*,'(''paused, type [enter] to continue'')')
    read (*,*)
    stop

    

    end Subroutine PrepareInputFiles
      

end module PrepareMod
