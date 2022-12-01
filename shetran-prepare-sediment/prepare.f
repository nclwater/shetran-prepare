C       10        20        30        40        50        60        70    76
C Prepare SHETRAN files

      PROGRAM prepare 
      
c      USE DFLIB, ONLY : SPLITPATHQQ      
      implicit none

      integer accumfac
      real accumfac1
      integer ncolsmax,nrowsmax
      real chanfac1,chanfac2,chanfac3
      real removesink
      real netrainfall,streamwidthfac1,streamwidthfac2
      real stricklerriv,stricklerlake

c     number of grid squares accumulated rainfall before river is produced
c	parameter (accumfac=5)

c     maximum number of columns and rows
      parameter (ncolsmax=500,nrowsmax=500)

c     chanfac1 is drop from grid elevation to channel depth elevation
c     chanfac2 is drop along a channel that is being followed
c     chanfac3 is increase in elevation when reoving channel link sinks
c      parameter (chanfac1=2.0,chanfac2=1.0,chanfac3=1.0)

c remove for Liz and put in xml file
c      parameter (chanfac2=0.5,chanfac3=chanfac2)

c     Additional elevation to remove sink
      parameter (removesink=0.1)

c     annual elevation-evap(mm) to calculate mean annual flow and stream width
      parameter (netrainfall=1000.0)
c width = streamwidthfac1* maf^streamwidthfac2
      parameter (streamwidthfac1=15.0)
      parameter (streamwidthfac2=0.5)

      parameter (stricklerriv=50.0)
      parameter (stricklerlake=10.0)


      CHARACTER*200 FILFRD,FILOCD,FILETD,FILVSD,FILVIS,FILSMD
      CHARACTER*200 FILFRD2,FILOCD2,FILETD2,FILVSD2,FILVIS2,FILSMD2
      CHARACTER*200 FILSYD2,FILSYD,FILSPR
      CHARACTER*200 FILRUN,FILPRD,FILEPD,FILTIM,FILPRI,FILRIV
      CHARACTER*200 FILDIS,FILVSE,FILMAS,FILCVI,FILHDF,FILDIS2,FILLOG
      INTEGER OUTFRD,OUTOCD,OUTETD,OUTVSD
      INTEGER OUTRUN,OUTVIS,OUTRIV,OUTSMD,OUTSYD,logfile
      PARAMETER ( OUTFRD = 20 )
      PARAMETER ( OUTOCD = 21 )
      PARAMETER ( OUTETD = 22 )
      PARAMETER ( OUTVSD = 23 )
      PARAMETER ( OUTRUN = 24 )
      PARAMETER ( OUTVIS = 25 )
      PARAMETER ( OUTRIV = 26 )
      PARAMETER ( OUTSMD = 27 )
      PARAMETER ( OUTSYD = 28 )
      parameter ( logfile = 51)




      CHARACTER*200 FILEIN,FILEIN1,FILEINMIN,FILEIN2,FILEO1,FILEO2
      CHARACTER*200 catchname
      CHARACTER*200 workspace,filerdf,filecstcap
      CHARACTER*200 vegname,soilname,lakename,precipname,pename
      CHARACTER*200 NFMStorageName,NFMForestName
      CHARACTER*200 tmaxfile,tminfile
      CHARACTER*200 precfile,pefile,delme
      CHARACTER*200 precfile2,pefile2
      character*200 basedir, xmlfilename,buildloc,xmlfilefull
      character*5 acols,arows
      character*8 acellsize
      character*10 axllcorner,ayllcorner
      character msg*520,msg1*520
      character*1 xmap((nrowsmax+1)*2,(ncolsmax+1)*2)
      character*1 alinkew(nrowsmax+1,ncolsmax)
      character*1 alinkns(nrowsmax,ncolsmax+1)
      character*1 vismask(nrowsmax,ncolsmax)
      character anovalue
      integer ncols,nrows,i,j,k,l,ncols2,nrows2,count,ncols3,nrows3
      integer colposmin,rowposmin,change
      integer cornerval(nrowsmax+1,ncolsmax+1)
      integer crncolpos,crnrowpos,crnmax
      integer direction,maxacc
      integer itemp,number
      integer poscol(ncolsmax*nrowsmax),posrow(ncolsmax*nrowsmax)
      integer catch(nrowsmax,ncolsmax),catchrow(ncolsmax)
      integer catchgeometry(-1:nrowsmax*10,-1:ncolsmax*10)
      integer vegdist(nrowsmax,ncolsmax),soildist(nrowsmax,ncolsmax)
      integer lakedist(nrowsmax,ncolsmax)
      integer arrayvalue,numbermet(nrowsmax*ncolsmax)
      integer peall(nrowsmax*ncolsmax),peunique(nrowsmax*ncolsmax)
      integer numberunique, pedist2(nrowsmax,ncolsmax)
      integer pedist(nrowsmax,ncolsmax),raindist(nrowsmax,ncolsmax)
      integer NFMStorageDist(nrowsmax,ncolsmax)
      integer NFMStorageDist2(nrowsmax,ncolsmax)
      integer NFMForestDist(nrowsmax,ncolsmax)
      integer NFMForestDist2(nrowsmax,ncolsmax)
      integer rainall(nrowsmax*ncolsmax),rainunique(nrowsmax*ncolsmax)
      integer numberuniquer, raindist2(nrowsmax,ncolsmax)
      integer cv,rv,cvadd,rvadd
      integer accum(0:nrowsmax+1,0:ncolsmax+1)
      integer msgs,msge
      integer linkoutdir,linkoutr,linkoutc
      integer numlinks,linkoutnum
      integer ndefct,nxsect,iface
      integer ncspairs
      integer badnum,badrow,badcol
      integer length,length2
      integer nrowsm1,ncolsm1
      integer year,month,day,hours,minute
      integer nf,nrd
      integer nrowhalf
      integer nmcellroot,numberedge
      integer novalue
      integer maxcatnumber
      real dem(0:nrowsmax+1,0:ncolsmax+1),demrow(ncolsmax)
      real demmean(0:nrowsmax+1,0:ncolsmax+1),demrowmean(ncolsmax)
      real demne,demse,demsw,demnw,demmincorner,demminedge
      real demn,dems,demw,deme
      real demmin,mindem
      real posval(ncolsmax*nrowsmax)
      real temp
      real ellinkew(nrowsmax+1,ncolsmax),ellinkns(nrowsmax,ncolsmax+1)
      real strlinkew(nrowsmax+1,ncolsmax),strlinkns(nrowsmax,ncolsmax+1)
      real dum1,dum2,dum3,dum4,dum5,dum6,dummin,cheldum,pchdp
      real linkelv(ncolsmax*nrowsmax)
      real linkstr(ncolsmax*nrowsmax)
      real linkelvmin
      real wdepth,str,coeff,subrio
      real badelev
      real cellsize,xllcorner,yllcorner
      real plai,clai,cstcap,ck,cb
      real ps1(7),fet(7),depth(50),rdf(50,50)
      real cstcapratio(50,50),cstcaptime(50,50)
      integer cstcapnopoints(50),cstcapnoveg,cstcapnoyear
      real cond,thsat,thres,satstor,vgalpha,vgn
      real regulartmstep,snowddf
      real standardtimestep,increasingtimestep
      logical linkew(nrowsmax+1,ncolsmax),linkns(nrowsmax,ncolsmax+1)
      logical savelinkew(nrowsmax+1,ncolsmax)
      logical savelinkns(nrowsmax,ncolsmax+1)
      logical notlowpoint,isok,isaccum(nrowsmax,ncolsmax)
      logical cornerdone(nrowsmax+1,ncolsmax+1)
      logical savecornerdone(nrowsmax+1,ncolsmax+1)
      logical cornerdonep(nrowsmax+1,ncolsmax+1)
      logical outletlink
      logical isunique,islakename
      logical IsStorageFile,IsForestFile
      integer io

      character*200 invegtypes(100),insoiltypes(50000)
      real incstcap(100),inlai(100),inrootingdepth(100)
      integer insoilcats(50000),insoillayers(50000)
      integer insoilnumbers(50000),insoilnumbers2(50000)
      real inaepe(100),insoildepth(50000),inthsat(50000),inthres(50000)
      real instricklerveg(50000)
      real inksat(50000),invgn(50000),invga(50000),inoverflowroughness
      real ininitialpsl,inprectmstep,inpetmstep
      integer inday,inmonth,inyear
      integer inendday,inendmonth,inendyear
      integer innmveg,innmsoil,innmsoilcat
      real insoildepthmin
      integer vislayer,finaldivider
      integer(2) n1
      integer countiteration
      integer countnumber
      integer outletcrnrowpos,outletcrncolpos,outletcornerval
      integer streamsize(ncolsmax*nrowsmax)
      real streamwidth1,streamwidth2,maffactor
      real demminoutletproblem

      real maxsoildepth

      integer cattype,ncatold,ncat,catnumber(0:1000),pc

      logical meetexit

      CHARACTER   MSG2*80,msg3*80
      CHARACTER*7 AFORM(ncolsmax*nrowsmax)
      CHARACTER   FORM*7

      CHARACTER(3) drive
      CHARACTER(256) path, ext
      integer*4 lengthpath
      CHARACTER(40) MyName      

      CHARACTER Delimeter,Delimeter2
      integer finaldel,finaldel2


 9301 FORMAT(':FR1 - TEST CATCHMENT- FR COMPONENT DATA SET')
 9302 FORMAT(':FR2 - GRID SQUARES IN THE X Y DIRECTIONS')
 9304 FORMAT(':FR4 - START TIME OF WATER FLOW COMPONENT SIMULATION')
 9306 FORMAT(':FR6 - END TIME OF WATER FLOW COMPONENT SIMULATION')
 9391 FORMAT(':FR7a - START TIME OF SEDIMENT TRANPORT COMPONENT',
     $         ' SIMULATION') 
 9392 FORMAT(':FR7c - START TIME OF CONTAMINANT MIGRATION COMPONENT',
     $         ' SIMULATION')
 9308 FORMAT(':FR8 - GRID SPACING IN X DIRECTION')
 9310 FORMAT(':FR10 - GRID SPACING IN Y DIRECTION')
 9312 FORMAT(':FR12 - PRINT CONTROL PARAMETERS')
 9320 FORMAT(':FR20 - BASIC TIMESTEP DATA')
 9322 FORMAT(':FR22 - PRINT CONTROL PARAMETERS')
 9324 FORMAT(':FR24 - COMPONENT EXECUTION CONTROL PARAMETERS ',
     $         '(SM,BK,SY,CM)')
 9326 FORMAT(':FR26 - HOTSTART PARAMETERS')
 9328 FORMAT(':FR28 - NO. OF MET./RAINFALL STATIONS, VEG./SOIL TYPES ',
     $         'AND SOIL LAYER CAT.')
 9330 FORMAT(':FR30 - RIVER LINING PARAMETERS')
 9332 FORMAT(':FR32 - DEFAULT VALUES FOR MET,RAINFALL,VEG,SOIL LAYER ',
     $         'CATEGORIES')
 9334 FORMAT(':FR34 - COMPUTATIONAL GRID DEFINITION')
 9335 FORMAT(':FR35a - E-W FLOW CODES (n-s links)')
 9336 FORMAT(':FR35c - N-S FLOW CODES (e-w links)')
 9337 FORMAT(':FR37 - GROUND SURFACE ELEVATION')
 9343 FORMAT(':FR43 - MET STATIONS')
 9346 FORMAT(':FR46 - RAIN STATIONS')
 9349 FORMAT(':FR49 - VEGETATION TYPES')
 9352 FORMAT(':FR52 - OUTPUT DISCHARGE TIMESTEP(HOURS)')

 9401 FORMAT(':OC1 - TEST CATCHMENT- OC COMPONENT DATA SET: NT, ',
     $        'NCATR, KONT, BIOWAT')
 9402 FORMAT(':OC2 - TIMESTEP CONTROL (currently not used)')
 9403 FORMAT(':OC3 - OTHER PARAMETERS. SMIN,CDRS,TDC,TFC,DET.',
     $        ' (SMIN & DET not used)') 
 9414 FORMAT(':OC14 STRX')
 9417 FORMAT(':OC17 STRY')
 9420 FORMAT(':OC20 - HEAD AND FLUX BOUNDARY CONDITIONS: NOCHB,',
     $          ' NOCFB, NOCPB')
 9430 FORMAT(':OC30 - NUMBER OF DEFAULT CHANNEL CROSS-SECTIONS')    
 9432 FORMAT(':OC32 - DEFAULT CHANNEL DESCRIPTION')    
 9435 FORMAT(':OC35 - CHANNEL LINK DATA',
     $          '(LINK NO.,BED ELEV.,DEPTH, STR. COEFF.,DEF X-S)')
 9501 FORMAT(':ET1 - TEST CATCHMENT- ET COMPONENT DATA SET')
 9503 FORMAT(':ET3 - DTMET : TIMESTEP FOR INPUT OF RAIN AND MET. DATA')
 9505 FORMAT(':ET5 - MEASPE (0 NOT MEASURED, 1 MEASURED)')
 9557 FORMAT(':ET7 - VEGETATION TYPE ')
 9509 FORMAT(':ET9 - CONTROLS FOR TIME VARYING PARAMETERS FOR',
     $         ' VEGETATION TYPE')
 9515 FORMAT(':ET15 - PSI/RCF/FET FUNCTION FOR VEGETATION TYPE')
 9517 FORMAT(':ET17 - DEPTH/RDF FOR VEGETATION TYPE')
 9518 FORMAT(':ET11 - time varying cstcap')
 9519 FORMAT(':ET13 - time varying cstcap')
 9601 FORMAT(':VS01 simulation title')
 9602 FORMAT(':VS02 logical flags: BFAST,BSOILP,BHELEV')
 9603 FORMAT(':VS03 integer variables NS,NCSZON,NCRBED,INITYP')
 9604 FORMAT(':VS04 real variables VSIPSD,VSZMIN,VSZMAX,VSWV,VSWL')
 9605 FORMAT(':VS05 physical property data (IS,IVSFLG,IVSNTB / ',
     $          'KX,KY,KZ,THSAT,THRES,SS,N,ALF) ')
 9606 FORMAT(':VS06 cell sizes')
 9608 FORMAT(':VS08 no. of categories for layer definitions')
 9658 FORMAT(':VS08a category; no. of layers; soil/lith type; depth')
 9668 FORMAT(':VS08c  soil and aquifer distribution grid')
 9610 FORMAT(':VS10 user defined connectivities')
 9611 FORMAT(':VS11 no. of categories for boundary conditions')
 9701 FORMAT('Rundata file -Test data')
 9710 FORMAT('10: frame				---------- INPUT DATA')
 9711 FORMAT('11: VSS input data')
 9712 FORMAT('12: overland/channel')
 9713 FORMAT('13: evapotranspiration')
 9714 FORMAT('14: post-processing data definition (no longer used)')
 9715 FORMAT('15: snowmelt')
 9716 FORMAT('16: bank element data')
 9717 FORMAT('17: sediment yield input')
 9718 FORMAT('18: contaminant input')
 9719 FORMAT('19: hourly met. data			---------- MET. DATA')
 9720 FORMAT('20: precipitation data')
 9721 FORMAT('21: potential evaporation data')
 9722 FORMAT('22: time counter file			---------- OUTPUT DATA')
 9723 FORMAT('23: water flow print output')
 9724 FORMAT('24: sediment yield print ')
 9725 FORMAT('25: contaminant print')
 9726 FORMAT('26: debug output ')
 9727 FORMAT('27: main unformatted results file (no longer used')
 9728 FORMAT('28: hostart file			-------INITIAL CONDITIONS ')
 9729 FORMAT('29: VSS initial conditions')
 9730 FORMAT('30: time-varying vegetation (VED)-----TIME-SERIES DATA')
 9731 FORMAT('31: well extraction (WLD)')
 9732 FORMAT('32: lateral subsurface flow boundary condition (LFB)')
 9733 FORMAT('33: lateral subsurface head boundary condition (LHB)')
 9734 FORMAT
     $('34: lateral subsurface head gradient boundary condition (LGB)')
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
 9745 FORMAT('45: not used  ')
 9746 FORMAT('46: not used  ')
 9747 FORMAT('47: not used')
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
* sjb visualisation plan change these ok if > 10
 9271 FORMAT(A1,A2,I4,A2,I4,A40)
 9272 FORMAT(A1,A2,I4,A2,I3,A40)
 9273 FORMAT(A1,A2,I3,A2,I4,A40)
 9274 FORMAT(A1,A2,I3,A2,I3,A40)
 9275 FORMAT(A1,A2,I2,A2,I2,A40)
 9276 FORMAT(A1,A2,I2,A2,I3,A40)
 9277 FORMAT(A1,A2,I3,A2,I2,A40)
 9404 FORMAT(10(A7,1x))
 9405 FORMAT(a7,i7,a7,a7)


      print*
      print*, 'Shetran Prepare sediment'
      print*, '************************'
      print* 
      
      print*, 'This executable reads an XML file and the corresponding',
     $   'ASC grids and produces the Shetran input files' 
      print* 
c      read (*,*) basedir
c      print*, 'Input project name (XML file name)'
c      read (*,*) xmlfilename
c      print*, 'Input build location'
c      read (*,*) buildloc

c      n1=1
c      CALL GETARG(n1, basedir)
c       WRITE(*,*) basedir
        basedir = '.'
c      n1=2

      n1=1
      CALL GETARG(n1,xmlfilename)

c      xmlfilename  = '../84013/LibraryFile111.xml'      
c       WRITE (*,*) xmlfilename
c      n1=3
c      CALL GETARG(n1,buildloc)
c       WRITE (*,*) buildloc
        buildloc = '.'
c
c       pause
        
!      xmlfilefull = trim(basedir)//'\'//trim(xmlfilename)
      xmlfilefull = trim(xmlfilename)
c      pause
      open(10,FILE=xmlfilefull,err=9999,status='old')

      goto 9998

 9999 write (*,*) 'Error openinig file ',xmlfilefull
      close(10)
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
      stop 1



 9998 close (10)
      call read_xml_file(xmlfilefull,catchname,filein1,fileinmin,
     $ filein2,vegname,soilname,lakename,precipname,pename,
     $ precfile,pefile,invegtypes,incstcap,inlai,inrootingdepth,inaepe,
     $ instricklerveg, insoilcats,insoillayers,insoilnumbers,
     $ insoilnumbers2,insoiltypes,insoildepth,
     $ inthsat,inthres,inksat,invgn,invga,
     $ ininitialpsl,inprectmstep,inpetmstep,inday,inmonth,inyear,
     $ inendday,inendmonth,inendyear,innmveg,innmsoil,innmsoilcat,
     $ accumfac1,chanfac1,
     $ chanfac2,tmaxfile,tminfile,standardtimestep,increasingtimestep,
     $ regulartmstep,snowddf)
      chanfac3=chanfac2
      accumfac=nint(accumfac1)

!       print*,'t0'
c      print*,precipname
c      print*,'t1'
c      print*,pename
c      print*,'t2'
c      print*,precfile
c      print*,'t3'
c      print*,pefile
c      print*,'t4'


!       print*,invegtypes(1),invegtypes(2)

c      print*,snowddf

c 200  PRINT*, 
c     $   'Enter data file'
c      READ (*,*) filein
c      IF (filein1(1:1).EQ.' ') GOTO 200
c      OPEN(10,FILE=FILEIN,STATUS='OLD')
c      READ(10,*)
c      READ(10,*) CATCHNAME
c      READ(10,*)
c      READ(10,*) FILEIN1
c      READ(10,*)
c      READ(10,*) FILEIN2
c      READ(10,*)
c      READ(10,*) accumfac1
c      READ(10,*)
c      READ(10,*) chanfac1
c      accumfac=int(accumfac1)

c      LENGTH = INDEX(CATCHNAME,' ')-1
c      LENGTH2 = INDEX(basedir,' ')-1
c      FILFRD = 'input/'//CATCHNAME(1:LENGTH)//'.frd'
c      OPEN (OUTFRD,FILE=FILFRD)

****
      Delimeter='/'
      Delimeter2='\'
      finaldel= index(xmlfilefull, Delimeter, .TRUE.)
      finaldel2= index(xmlfilefull, Delimeter2, .TRUE.)
c      print*,finaldel
c      print*,finaldel2

c      lengthpath = SPLITPATHQQ(xmlfilefull, drive, path, MyName, ext)
c     basedir=trim(drive)//trim(path)
      basedir=xmlfilefull(1:max(finaldel,finaldel2))
      print*,basedir

      FILLOG = trim(basedir)//'input_'//trim(CATCHNAME)//'_log.txt'
      filein1=trim(basedir)//trim(filein1)
      fileinmin=trim(basedir)//trim(fileinmin)
      filein2=trim(basedir)//trim(filein2)
      vegname=trim(basedir)//trim(vegname)
      soilname=trim(basedir)//trim(soilname)
      pename=trim(basedir)//trim(pename)
      precipname=trim(basedir)//trim(precipname)
      NFMStorageName=trim(basedir)//trim(CATCHNAME)//'_NFM_storage.asc'
      NFMForestName=trim(basedir)//trim(CATCHNAME)//'_NFM_forest.asc'


      open(logfile,FILE=FILLOG)
      WRITE (logfile,*) 'XML filname = ', trim(xmlfilefull)
      WRITE (logfile,*)      
      WRITE (logfile,*) 'DEM filname = ', trim(filein1)
      WRITE (logfile,*) 'Minimum DEM filname = ', trim(fileinmin)
      WRITE (logfile,*) 'Catchment Mask filname = ', trim(filein2)
      WRITE (logfile,*) 
     $ 'Land use distribution filname = ', trim(vegname)
      WRITE (logfile,*) 
     $ 'Soil category distribution filname = ',trim(soilname)
      WRITE (logfile,*) 
     $ 'NFM_storage distribution filname = ',trim(NFMStorageName)
      WRITE (logfile,*) 
     $ 'NFM_storage distribution filname = ',trim(NFMForestName)

      

      FILFRD = trim(basedir)//'input_'//trim(CATCHNAME)//'_frd.txt'
      OPEN (OUTFRD,FILE=FILFRD)
      FILFRD2 = 'input_'//trim(CATCHNAME)//'_frd.txt'
      FILOCD = trim(basedir)//'input_'//trim(CATCHNAME)//'_ocd.txt'
      OPEN (OUTOCD,FILE=FILOCD)
      FILOCD2 = 'input_'//trim(CATCHNAME)//'_ocd.txt'
      FILETD = trim(basedir)//'input_'//trim(CATCHNAME)//'_etd.txt'
      OPEN (OUTETD,FILE=FILETD)
      FILETD2 = 'input_'//trim(CATCHNAME)//'_etd.txt'
      FILSMD = trim(basedir)//'input_'//trim(CATCHNAME)//'_smd.txt'
!      OPEN (OUTSMD,FILE=FILSMD)
      FILSMD2 = 'input_'//trim(CATCHNAME)//'_smd.txt'
      FILVSD = trim(basedir)//'input_'//trim(CATCHNAME)//'_vsd.txt'
      OPEN (OUTVSD,FILE=FILVSD)
      FILVSD2 = 'input_'//trim(CATCHNAME)//'_vsd.txt'
      FILSYD = trim(basedir)//'input_'//trim(CATCHNAME)//'_syd.txt'
      OPEN (OUTSYD,FILE=FILSYD)
      FILSYD2 = 'input_'//trim(CATCHNAME)//'_syd.txt'
      FILRUN = trim(basedir)//'rundata_'//trim(CATCHNAME)//'.txt'
      OPEN (OUTRUN,FILE=FILRUN)
      FILRIV = trim(basedir)//'river_network_'//trim(CATCHNAME)//'.txt'
      OPEN (OUTRIV,FILE=FILRIV)
      FILVIS = trim(basedir)//'input_'//trim(CATCHNAME)//
     $ '_visualisation_plan.txt'
      OPEN (OUTVIS,FILE=FILVIS)
      FILVIS2 = 'input_'//trim(CATCHNAME)//'_visualisation_plan.txt'
      FILTIM = 'output_'//trim(CATCHNAME)//'_tim.txt'
      FILPRI = 'output_'//trim(CATCHNAME)//'_pri.txt'

      FILDIS = 
     $ 'output_'//trim(catchname)//'_discharge_sim_regulartimestep.txt'
      FILDIS2 = 
     $ 'output_'//trim(catchname)//'_discharge_sim_everytimestep.txt'
      FILVSE = 
     $ 'output_'//trim(catchname)//'_vsi.txt'
      FILMAS = 
     $ 'output_'//trim(catchname)//'_mb.csv'
      FILCVI = 
     $ 'output_'//trim(catchname)//'_check_vis_plan.txt'
      FILHDF = 
     $ 'output_'//trim(catchname)//'_shegraph.h5'
      FILSPR =
     $ 'output_'//trim(catchname)//'_spr.txt'







!      FILEO1=trim(basedir)//'\output_'//'developer-output.txt'
!      OPEN(30,FILE=FILEO1,STATUS='UNKNOWN')
      
      OPEN(11,FILE=FILEIN1,STATUS='OLD')
      OPEN(18,FILE=FILEINMIN,STATUS='OLD')
      if ((trim(lakename).eq.('')).or.(trim(lakename).eq.('none'))) then
          islakename=.false.
          write(logfile,*)'No lake map specified'
      else
          write(logfile,*)'Lakemap = ',lakename
         islakename=.true.
         lakename=trim(basedir)//trim(lakename)
          OPEN(19,FILE=lakename,STATUS='OLD')
      endif    

      WRITE (logfile,*) 
     $ 'Precipitation Distribution Filename = ', trim(precipname)
      WRITE (logfile,*) 
     $ 'Potential Evaporation Distribution Filname = ', trim(pename)
      WRITE (logfile,*) 'Precpitation data = ',trim(precfile)
      WRITE (logfile,*) 'Potential Evaporation data = ',trim(pefile)   
      
      
      OPEN(12,FILE=FILEIN2,STATUS='OLD')
      if (trim(precfile).eq.'none') then
c         FILPRD =trim(basedir)//'\input\'//'test.prd'
         FILPRD ='input_'//'test_prd.txt'
      else 
* remove directory from precfile name
        finaldivider=0
         do i=1,200
           if (precfile(i:i).eq.'\') then
             finaldivider=i
           endif
         enddo
         j=1

c         print*,finaldivider

         do i=finaldivider+1,200
             precfile2(j:j)=precfile(i:i)
             j=j+1
         enddo
c         filprd=trim(basedir)//'\input\'//trim(precfile2)
         filprd=trim(precfile2)
      
c       print*,precfile,precfile2,filprd


      endif
      if (trim(pefile).eq.'none') then
         FILEPD = 'input_'//'test_epd.txt'
      else 
* remove directory from precfile name
         do i=1,200
           if (pefile(i:i).eq.'\') then
             finaldivider=i
           endif
         enddo
         j=1
         do i=finaldivider+1,200
             pefile2(j:j)=pefile(i:i)
             j=j+1
         enddo
         filepd=trim(pefile2)
      endif

!      print*, filein1,fileinmin,filein2,precfile


!      filerdf=trim(buildloc)//'\rdf.csv'
      filecstcap=trim(basedir)//'canopystorage.txt'
!      OPEN(15,FILE=filerdf,STATUS='OLD')
      depth(1:27) = (/ 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,
     $  1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,4.0,5.0,6.0,7.0,8.0,
     $  9.0,10.0 /)
      
      call rootdensity(rdf)
c      print*,filecstcap
      OPEN(16,FILE=filecstcap,STATUS='OLD',err=768)
      read(16,*,err=768)
      read(16,*,err=768)

      read(16,*,err=768) cstcapnoveg
      do i=1,cstcapnoveg
                read(16,*,err=768) 
                read(16,*,err=768) 
                read(16,*,err=768) 
          read(16,*,err=768) cstcapnopoints(i)
c          print*,cstcapnopoints(i)
          if (cstcapnopoints(i).gt.0) then
            do j=1,cstcapnopoints(i)

                read(16,*,err=768) cstcapratio(i,j),cstcaptime(i,j)
c                print*,cstcapratio(i,j),cstcaptime(i,j)

            enddo
          endif 
      enddo
      do i=cstcapnoveg,50
        cstcapnopoints(i)=0
      enddo
      close(16)

      goto 769

  768 do i=1,50
        cstcapnopoints(i)=0
      enddo
c      print*,filecstcap

      

  769 do i=1,50
c        print*,cstcapnopoints(i)
      enddo

      READ(11,*) acols,ncols
      READ(11,*) arows,nrows
      READ(11,*) axllcorner,xllcorner
      READ(11,*) ayllcorner,yllcorner
      READ(11,*) acellsize,cellsize 
      READ(11,*) anovalue,novalue
      if (ncols.gt.ncolsmax-2) then
          print*,'Number of columns is greater than specified maximum',
     $                 ncolsmax-2
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
          stop
      endif
      if (nrows.gt.nrowsmax-2) then
          print*,'Number of rows is greater than specified maximum',
     $                 nrowsmax-2
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
          stop
      endif

            READ(18,*) acols,ncols3
      if (ncols.ne.ncols3) then
          print*,
     $'Number of columns not equal in the elevation grids' 
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
          stop
      endif
      READ(18,*) arows,nrows3
      if (nrows.ne.nrows3) then
          print*,
     $ 'Number of rows not equal in the elevation grids' 
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
          stop
      endif
      READ(18,*) axllcorner,xllcorner
      READ(18,*) ayllcorner,yllcorner
      READ(18,*) acellsize,cellsize 
      READ(18,*) anovalue,novalue



      READ(12,*) acols,ncols2
      if (ncols.ne.ncols2) then
          print*,
     $'Number of columns not equal in the elevation and catchment grids' 
      write(*,'(''paused, type [enter] to continue'')')
      read (*,*)
          stop
      endif
      READ(12,*) arows,nrows2
      if (nrows.ne.nrows2) then
          print*,
     $ 'Number of rows not equal in the elevation and catchment grids' 
       write(*,'(''paused, type [enter] to continue'')')
       read (*,*)
          stop
      endif
      READ(12,*) axllcorner,xllcorner
      READ(12,*) ayllcorner,yllcorner
      READ(12,*) acellsize,cellsize 
      READ(12,*) anovalue,novalue


      do i=2,nrows+1
          read(11,*) (demrowmean(j),j=2,ncols+1)
              do j=2,ncols+1
             demmean(i,j) = demrowmean(j)
          enddo
      enddo

      do i=2,nrows+1
          read(18,*) (demrow(j),j=2,ncols+1)
              do j=2,ncols+1
             dem(i,j) = demrow(j)
          enddo
      enddo


      do i=1,nrows+2
              do j=1,ncols+2
             catch(i,j) = novalue
          enddo
      enddo
      do i=2,nrows+1
          read(12,*) (catchrow(j),j=2,ncols+1)
              do j=2,ncols+1
             catch(i,j) = catchrow(j)
          enddo
      enddo

      do i=2,nrows+1
           do j=2,ncols+1
              if (catch(i,j).ne.novalue) then
                if ((catch(i-1,j).eq.novalue).and.
     $          (catch(i+1,j).eq.novalue).and.
     $          (catch(i,j+1).eq.novalue).and.
     $          (catch(i,j-1).eq.novalue)) then
                    print*
                    print*,'*********************************'
                    print*,
     $                 'There is grid square in the mask not ', 
     $                 'adjacent horizontally or vertically to ',
     $                 'another grid square. This grid square has ',
     $                 'been removed'
                    print*,'The issue is in row ',i-1,'column ',j-1
                    print*
                    WRITE (logfile,*)
                    WRITE (logfile,*)
     $                 'There is grid square in the mask not ', 
     $                 'adjacent horizontally or vertically to ',
     $                 'another grid square. This grid square has ',
     $                 'been removed'
              WRITE (logfile,*) 'The issue is in row ',i-1,'column ',j-1
                    WRITE (logfile,*)

                catch(i,j)=novalue
              endif
             endif
                
           enddo
          
      enddo
      
      if(islakename) then
        READ(19,*) 
        READ(19,*) 
        READ(19,*) 
        READ(19,*) 
        READ(19,*) 
        READ(19,*) 
          do i=2,nrows+1
          read(19,*) (lakedist(i,j),j=2,ncols+1)
          do j=2,ncols+1
             if (lakedist(i,j).lt.0) then 
                lakedist(i,j)=0
             endif
          enddo
        enddo
       else 
          do i=2,nrows+1
          do j=2,ncols+1
                lakedist(i,j)=0
          enddo
        enddo
       endif





C     add an extra grid square around entire catchment
c     to stop a bug in SHETRAN
      nrows=nrows+2
      ncols=ncols+2

      do i=1,nrows
              do j=1,ncols
             if (catch(i,j).eq.novalue) then
                  demmean(i,j) = 1.0e10
             endif
          enddo
      enddo

      close(12)

      do i=1,nrows
              do j=1,ncols
             if (catch(i,j).eq.novalue) then
                  dem(i,j) = 1.0e10
             endif
          enddo
      enddo

      close(18)


c      print*,'t1'



************************ Title *****************************************      
*
      WRITE (MSG,9301)
      WRITE (OUTFRD,9200) MSG
*
*
********* Number of Grid Squares ***************************************      
*
      WRITE (MSG,9302)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9100) ncols,nrows
*
*
********* Start/End of simulations **************************************      
*
      hours=0
      minute=0
      WRITE (MSG,9304)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9101) inYEAR,inMONTH,inDAY,HOURS,MINUTE
*
      WRITE (MSG,9306)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9101) inendYEAR,inendMONTH,inendDAY,HOURS,MINUTE
*
      year=year-1
      WRITE (MSG,9391)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9101) inYEAR,inMONTH,inDAY,HOURS,MINUTE
*
      WRITE (MSG,9392)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9101) inYEAR,inMONTH,inDAY,HOURS,MINUTE
*      
*
******************** Grid Sizes ****************************************
*
      ncolsm1=ncols-1
      WRITE (MSG,9308)
      WRITE (OUTFRD,9200) MSG
      DO I = 1,ncolsm1
         AFORM(I) =FORM(cellsize)
      ENDDO
      WRITE(OUTFRD,9110) (AFORM(I),I=1,ncolsm1)
*      
      nrowsm1=nrows-1
      WRITE (MSG,9310)
      WRITE (OUTFRD,9200) MSG
      DO I = 1,nrowsm1
         AFORM(I) =FORM(cellsize)
      ENDDO
      WRITE(OUTFRD,9110) (AFORM(I),I=1,nrowsm1)
*
*
**************** Print output timestep *********************************
*
      
      WRITE (MSG,9312)
      WRITE (OUTFRD,9200) MSG
      AFORM(1) = FORM(1.0)
      AFORM(2) = FORM(20000.0)
      WRITE (OUTFRD,9102) AFORM(1),'2','T','F','F','F',AFORM(2)
*
*
***************** Basic timestep data **********************************
*
      WRITE (MSG,9320)
      WRITE (OUTFRD,9200) MSG
c sb 021009 change from 1.0 and 0.15 to make it more stable
      AFORM(1) = FORM(0.5)
      AFORM(2) = FORM(increasingtimestep)
      AFORM(3) = FORM(99999.)
      AFORM(4) = FORM(standardtimestep)
      WRITE (OUTFRD,9103) AFORM(1),AFORM(2),AFORM(3),
     $                    AFORM(4),'T'
*
*
******************* Print control parameters ***************************      
*
      WRITE (MSG,9322)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9104) 'F','F','F','F','F','F','F','F','F','F'
*
*
**************** Component execution control parameters ****************      
*
      WRITE (MSG,9324)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9105) 'F','F','T','F'
*
*
****************** Hotstart parameters *********************************
*
*
      WRITE (MSG,9326)
      WRITE (OUTFRD,9200) MSG
      AFORM(1)=FORM(0.0)
      AFORM(2)=FORM(0.0)
      WRITE (OUTFRD,9106) 'F','F',AFORM(1),AFORM(2)
*
*
****************************** NFM Storage and forest************************

c      print*,'t2'
c      print*,pename
c      print*,'t3'
      IsStorageFile=.true.
      OPEN(16,FILE=NFMStorageName,STATUS='OLD',IOSTAT=io)
      If (io.gt.0) then
          IsStorageFile=.false.
      else
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
        do i=2,nrows-1
           read(16,*) (NFMStorageDist(i,j),j=2,ncols-1)
        enddo
      endif
      close(16)
        
      IsForestFile=.true.
        OPEN(16,FILE=NFMForestName,STATUS='OLD',IOSTAT=io)
      If (io.gt.0) then
          IsForestFile=.false.
      else
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
        do i=2,nrows-1
           read(16,*) (NFMForestDist(i,j),j=2,ncols-1)
        enddo
      endif
      close(16)

      
      
*
****************************** PE Types ************************

c      print*,'t2'
c      print*,pename
c      print*,'t3'
      OPEN(16,FILE=pename,STATUS='OLD')
            READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
      READ(16,*) 
            do i=2,nrows-1
        read(16,*) (pedist(i,j),j=2,ncols-1)
      enddo
            do i=2,nrows-1
        do j=2,ncols-1
          arrayvalue=(i-2)*(ncols-2)+j-1
          peall(arrayvalue) = pedist(i,j)
c         write(617,*) arrayvalue,peall(arrayvalue)
        enddo
      enddo

c number of unique items in PE distribution
      peunique(1)=peall(1)
      numberunique=1
            do i=2,arrayvalue
        isunique = .true.
        do j=1,numberunique
        if (peunique(j).eq.peall(i)) then
           isunique = .false.
        endif
        enddo

        if (isunique) then
           numberunique=numberunique+1
           peunique(numberunique)=peall(i)
c          print*, numberunique, peunique(numberunique)
        endif
      enddo


  143      change=0
c     uses a very basic bubble sort
      do i=2,numberunique
          if (peunique(i).lt.peunique(i-1)) then
              itemp=peunique(i-1)
              peunique(i-1)=peunique(i)
              peunique(i)=itemp
              change=change+1
          endif
      enddo
      if (change.ne.0) goto 143

      if (peunique(1).eq.-9999) then
         numberunique = numberunique -1
         do i=1,numberunique
            peunique(i)=peunique(i+1)
         enddo
      endif


*
****************************** rain Types ************************

      OPEN(17,FILE=precipname,STATUS='OLD')
            READ(17,*) 
      READ(17,*) 
      READ(17,*) 
      READ(17,*) 
      READ(17,*) 
      READ(17,*) 
            do i=2,nrows-1
        read(17,*) (raindist(i,j),j=2,ncols-1)
      enddo
            do i=2,nrows-1
        do j=2,ncols-1
          arrayvalue=(i-2)*(ncols-2)+j-1
          rainall(arrayvalue) = raindist(i,j)
c         write(617,*) arrayvalue,peall(arrayvalue)
        enddo
      enddo

c number of unique items in PE distribution
      rainunique(1)=rainall(1)
      numberuniquer=1
            do i=2,arrayvalue
        isunique = .true.
        do j=1,numberuniquer
        if (rainunique(j).eq.rainall(i)) then
           isunique = .false.
        endif
        enddo

        if (isunique) then
           numberuniquer=numberuniquer+1
           rainunique(numberuniquer)=rainall(i)
c          print*, numberunique, peunique(numberunique)
        endif
      enddo


  543      change=0
c     uses a very basic bubble sort
      do i=2,numberuniquer
          if (rainunique(i).lt.rainunique(i-1)) then
              itemp=rainunique(i-1)
              rainunique(i-1)=rainunique(i)
              rainunique(i)=itemp
              change=change+1
          endif
      enddo
      if (change.ne.0) goto 543

      if (rainunique(1).eq.-9999) then
         numberuniquer = numberuniquer -1
         do i=1,numberuniquer
            rainunique(i)=rainunique(i+1)
         enddo
      endif




*
********** Number of meteorological stations, raingauges  **************
********** vegetation types and soil types(no longer used her) *********

      WRITE (MSG,9328)
      WRITE (OUTFRD,9200) MSG
      if (IsForestFile) then
       WRITE (OUTFRD,9117) numberunique,numberuniquer,innmveg+10,'1','1'
      else
        WRITE (OUTFRD,9117) numberunique,numberuniquer,innmveg,'1','1'
      endif


***************** River lining parameters ******************************
*
      WRITE (MSG,9330)
      WRITE (OUTFRD,9200) MSG
      AFORM(1)=FORM(0.1)
      AFORM(2)=FORM(0.0)
      WRITE (OUTFRD,9107) 'F',AFORM(1),AFORM(2),'F'

********** Default values for meteorological, rain, vegetation ********* 
************************* and soil codes  ******************************
*
      WRITE (MSG,9332)
      WRITE (OUTFRD,9200) MSG
      WRITE (OUTFRD,9107) '0','0','0','1'
*
*













c     fill surrounding elements
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

         

c     check that all the elevations are greater than zero
c correct bad elevation 30032012
      badnum=0
      do i=1,nrows
              do j=1,ncols
             if (demmean(i,j).le.0.0) then
c             badnum=badnum+1
             write(*,'(A50,A44)') 
     $          'There was a grid square within the catchment mask ',
     $          'with an elevation equal to or less than zero'
             write(*,'(A14,i4, A12,I4)')
     $           'Located at row', i-1, '  and column', j-1

             write(*,*)

             write(*,'(A46)')
     $         'Shetran will attempt to automatically correct'
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
                 write(*,'(A42)')
     $            'Shetran is unable to correct this problem'
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
c             badnum=badnum+1
             write(*,'(A50,A44)') 
     $          'There was a grid square within the catchment mask ',
     $          'with an elevation equal to or less than zero'
             write(*,'(A14,i4, A12,I4)')
     $           'Located at row', i-1, '  and column', j-1

             write(*,*)

             write(*,'(A46)')
     $       'Shetran will attempt to automatically correct'
             write(*,*)
c             pause
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
                 write(*,'(A42)')
     $            'Shetran is unable to correct this problem'
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
      
C     Find min elevation within the catchment
C     This must be at the catchment outlet 
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
            if ((dem(i,j).le.demminedge).and.(notlowpoint).and.
     $                             (catch(i,j).ne.novalue)) then
c                       write(823,*) i,j,demminedge
                 if (dem(i,j).ge.demmincorner) then
                   if (demmincorner.eq.demne) then
c               write(823,*) 'ne',i,j,dem(i,j),demn,dems,deme,demw,demne
                       if (demn.lt.deme) then
                           dem(i-1,j) = max((demne+dem(i,j))/2.0,demmin)
                       else   
                           dem(i,j+1) =max((demne+dem(i,j))/2.0,demmin)
                       endif
                    endif
                    if (demmincorner.eq.demnw) then
c                       write(823,*) 'nw',i,j
                        if (demn.lt.demw) then
                           dem(i-1,j) =max((demnw+dem(i,j))/2.0,demmin)
                        else   
                           dem(i,j-1) =max((demnw+dem(i,j))/2.0,demmin)
                        endif
                     endif
                     if (demmincorner.eq.demsw) then
c                       write(823,*) 'sw',i,j
                         if (dems.lt.demw) then
                           dem(i+1,j) =max((demsw+dem(i,j))/2.0,demmin)
                         else   
                           dem(i,j-1) =max((demsw+dem(i,j))/2.0,demmin)
                         endif
                     endif
                     if (demmincorner.eq.demse) then
c                        write(823,*) 'se',i,j
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



      
C     Find min elevation within the catchment
C     This must be at the catchment outlet 
 102      demmin=2.0e10
      do i=1,nrows
              do j=1,ncols
             if (dem(i,j).lt.demmin) then
                  demmin=dem(i,j)
                  colposmin=j
                  rowposmin=i
             endif
          enddo
      enddo

C     test to see if min at catchment boundary ?
      demn=dem(rowposmin,colposmin-1)
      dems=dem(rowposmin,colposmin+1)
      deme=dem(rowposmin+1,colposmin)
      demw=dem(rowposmin-1,colposmin)
      if ((demn.gt.0.99e10).or.(dems.gt.0.99e10).or.
     $    (deme.gt.0.99e10).or.(demw.gt.0.99e10)) then
          isok=.true.
      endif

C     if not at boundary add 1.0m to min. elevation and try again
      if (.not.isok) then
          dem(rowposmin,colposmin)=demmin+1.0
          goto 102
c         ********
      endif



      write(*,*)
      write(*,'(A31,F8.2)') 'Minimum catchment elevation is ',demmin 
      write(*,'(A7,I4,A12,I4)') 'At row ',
     $     rowposmin,' and column ',colposmin
      write(*,*)


!!!! Change to this bit of code demn, etc and demminedge 161112      
C     Remove sinks by removesink. Count is number of sinks
 101  count=0
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
                  if ((dem(i,j).le.demminedge).and.(notlowpoint).and.
     $                             (catch(i,j).ne.novalue)) then

                          dem(i,j)=demminedge+removesink
                          count=count+1
                  endif
              enddo
          enddo
c          print*,count
      if (count.gt.0) goto 101
c                     ********

!!!! END of code change




      

C     order the elements in the catchment. 
C     posval(1) is the dem of lowest elevation
C     poscol(1) is its column position
C     posrow(1) is its row  position
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
 103      change=0
c     uses a very basic bubble sort
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
      if (change.ne.0) goto 103
c                      ********

c     accumulate water flow
C     Basic assumption is that water accumulates along steepest gradient
      cv=poscol(1)
      rv=posrow(1)
      accum(rv,cv)=0
      do i=number,2,-1
          cv=poscol(i)
          rv=posrow(i)
          mindem = min(dem(rv,cv+1),dem(rv,cv-1),
     $         dem(rv+1,cv),dem(rv-1,cv))
C     easterly direction
          if (dem(rv,cv+1).eq.mindem) then
              rvadd=rv
              cvadd=cv+1
c     used for outlet element. to find out which direction
c     water is leaving the catchment
              if (i.eq.2) then
                 direction=2
              endif
C     westerly direction
          elseif (dem(rv,cv-1).eq.mindem) then
              rvadd=rv
              cvadd=cv-1
              if (i.eq.2) then
                 direction=4
              endif
C     southerly direction
          elseif (dem(rv+1,cv).eq.mindem) then
              rvadd=rv+1
              cvadd=cv
              if (i.eq.2) then
                 direction=3
              endif
C     northerly direction
          elseif (dem(rv-1,cv).eq.mindem) then
              rvadd=rv-1
              cvadd=cv
              if (i.eq.2) then
                 direction=1
              endif
          endif
          accum(rvadd,cvadd)=accum(rv,cv)+accum(rvadd,cvadd)+1
      enddo


C new code 180907 if minmium elevevation is surrounded by 3 other
C elements within the catchment then the output direction is known
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


*     new code 110209 reduce accumfac if it is too large so that at least two
*     river links produced 

      if (accumfac.ge.int(real(number)/2.0)) then 
         accumfac=int(real(number)/2.0)-1
      print*
      print*, "flow accumulation parameter reduced to ", accumfac
      print*
      endif

***   end of new code 110209



c     put an extra accumulation point in outside the catchment
c     direction 1=n,2=e,3=s,4=w
c     this also gives the outlet direction of the weir
      if (direction.eq.1) then
          accum(rvadd-1,cvadd)= accum(rvadd,cvadd)+1
      elseif (direction.eq.2) then
          accum(rvadd,cvadd+1)= accum(rvadd,cvadd)+1
      elseif (direction.eq.3) then
          accum(rvadd+1,cvadd)= accum(rvadd,cvadd)+1
      else  
          accum(rvadd,cvadd-1)= accum(rvadd,cvadd)+1
      endif

c     cornerval is the maximum value of the 4 accumulated water
c     flows around each corner point
c     this is used to find the river channels (links) and their elevations
      do i=1,nrows+1
          do j=1,ncols+1
              cornerval(i,j)=max(accum(i-1,j-1),accum(i-1,j),
     $        accum(i,j-1),accum(i,j))
          enddo
      enddo

!      do i=1,nrows+1
!              write(30,'(41(i4,1X))') (cornerval(i,j),j=1,ncols+1)
!      enddo




C     find the position of the links and their elevations
c     ***************************************************
c     NOTE: RIVER CHANNELS FLOWS ALONG THE EDGE OF GRID SQUARES
c     there are 2 main parts to this procedure, which consists of following river channels
c     along from their source to the outlet or until another river channel is met:
c     1) Going through elements from highest to lowest. 
c       If accumulated flow is greater than specified limit then a river exists
c       If no river channels have previously been specifed around the grid squre
c       then find the position of the river around the grid squre and its elevation
c     2) Follow along a river channel
c     2a) check to see if another river channel is nearby. If it is then meet that channel
c     2b) continue along the river channel


C     VARIABLE NAMES
c     cornerdone - Has a river channel been specified for this corner
c     cornerdonep - In the previous loop has a river channel been specified
c     cornerval - Accumulated flow at this corner
c     linkns - river channel heading in a north south direction. True if a channel exists
c     linkew - river channel heading in a east west direction. True if a channel exists


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





c     Part 1
c     ******
C     uses a method by considering the corners of grid squares
C     starts with the highest elements
      countnumber=0
      meetexit=.false.
      do i=number,2,-1
          cv=poscol(i)
          rv=posrow(i)
          if (accum(rv,cv).ge.accumfac) then

c          print*,i,cv,rv
c     look at corners around to check link has not already been established
             if ((.not.cornerdone(rv,cv)).and.(.not.cornerdone(rv+1,cv))
     $        .and.(.not.cornerdone(rv,cv+1)).and.
     $         (.not.cornerdone(rv+1,cv+1))) then

c     works out where the inital link in this river should go
                maxacc=max(accum(rv,cv+1),accum(rv,cv-1),
     $           accum(rv+1,cv),accum(rv-1,cv))
                    if (accum(rv,cv+1).eq.maxacc) then
c     east west link ->  heading east
                     demn=dem(rv-1,cv)
                     dems=dem(rv+1,cv)
C     north side
                      if (demn.lt.dems) then
                               linkew(rv,cv)=.true.    
                               savelinkew(rv,cv)=.true.

c                         channel elevation
                          dems=dem(rv,cv)
                          ellinkew(rv,cv)=min(demn,dems)-chanfac1
                          strlinkew(rv,cv)=stricklerriv
                          pchdp=min(demn,dems)-chanfac1
C                         end channel elevation

                              crncolpos=cv+1
                             crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv,cv+1)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv,cv+1)=.true.
C     south side
                      else
                          linkew(rv+1,cv)=.true.
                          savelinkew(rv+1,cv)=.true.

c                         channel elevation
                          demn=dem(rv,cv)
                          ellinkew(rv+1,cv)=min(demn,dems)-chanfac1
                          strlinkew(rv+1,cv)=stricklerriv
                          pchdp=min(demn,dems)-chanfac1
C                         end channel elevation

                              crncolpos=cv+1
                             crnrowpos=rv+1
                          cornerdone(rv+1,cv)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv+1,cv)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                      endif
                  elseif (accum(rv,cv-1).eq.maxacc) then
c     east west link -> heading west
                      demn=dem(rv-1,cv)
                      dems=dem(rv+1,cv)
                      if (demn.lt.dems) then
                            linkew(rv,cv)=.true.
                            savelinkew(rv,cv)=.true.

c                         channel elevation
                          dems=dem(rv,cv)
                          ellinkew(rv,cv)=min(demn,dems)-chanfac1
                          strlinkew(rv,cv)=stricklerriv
                          pchdp=min(demn,dems)-chanfac1
C                         end channel elevation

                              crncolpos=cv
                             crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv,cv+1)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv,cv+1)=.true.
                      else
                          linkew(rv+1,cv)=.true.
                          savelinkew(rv+1,cv)=.true.

c                         channel elevation
                          demn=dem(rv,cv)
                          ellinkew(rv+1,cv)=min(demn,dems)-chanfac1
                          strlinkew(rv+1,cv)=stricklerriv
                          pchdp=min(demn,dems)-chanfac1
C                         end channel elevation

                              crncolpos=cv
                             crnrowpos=rv+1
                          cornerdone(rv+1,cv)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv+1,cv)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                      endif
                  elseif (accum(rv+1,cv).eq.maxacc) then
c     north south link -> heading south
                      deme=dem(rv,cv+1)
                       demw=dem(rv,cv-1)
                      if (deme.lt.demw) then
                            linkns(rv,cv+1)=.true.
                            savelinkns(rv,cv+1)=.true.

c                         channel elevation
                          demw=dem(rv,cv)
                          ellinkns(rv,cv+1)=min(deme,demw)-chanfac1
                          strlinkns(rv,cv+1)=stricklerriv
                          pchdp=min(deme,demw)-chanfac1
C                         end channel elevation

                              crncolpos=cv+1
                             crnrowpos=rv+1
                          cornerdone(rv,cv+1)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv,cv+1)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                      else
                          linkns(rv,cv)=.true.
                          savelinkns(rv,cv)=.true.

c                         channel elevation
                          deme=dem(rv,cv)
                          ellinkns(rv,cv)=min(deme,demw)-chanfac1
                          strlinkns(rv,cv)=stricklerriv
                          pchdp=min(deme,demw)-chanfac1
C                         end channel elevation

                              crncolpos=cv
                             crnrowpos=rv+1
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv+1,cv)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv+1,cv)=.true.
                      endif
                  else
c     north south link -> heading north
                      deme=dem(rv,cv+1)
                      demw=dem(rv,cv-1)
                      if (deme.lt.demw) then
                            linkns(rv,cv+1)=.true.
                            savelinkns(rv,cv+1)=.true.
                              crncolpos=cv+1

c                         channel elevation
                          demw=dem(rv,cv)
                          ellinkns(rv,cv+1)=min(deme,demw)-chanfac1
                          strlinkns(rv,cv+1)=stricklerriv
                          pchdp=min(deme,demw)-chanfac1
C                         end channel elevation

                             crnrowpos=rv
                          cornerdone(rv,cv+1)=.true.
                          cornerdone(rv+1,cv+1)=.true.
                          savecornerdone(rv,cv+1)=.true.
                          savecornerdone(rv+1,cv+1)=.true.
                      else
                          linkns(rv,cv)=.true.
                          savelinkns(rv,cv)=.true.

c                         channel elevation
                          deme=dem(rv,cv)
                          ellinkns(rv,cv)=min(deme,demw)-chanfac1
                          strlinkns(rv,cv)=stricklerriv
                          pchdp=min(deme,demw)-chanfac1
C                         end channel elevation

                              crncolpos=cv
                             crnrowpos=rv
                          cornerdone(rv,cv)=.true.
                          cornerdone(rv+1,cv)=.true.
                          savecornerdone(rv,cv)=.true.
                          savecornerdone(rv+1,cv)=.true.
                      endif
                  endif

c     Part 2
c     ******
c     go from corner to corner to outlet of the catchment, along a river channel
                  countnumber=countnumber+1
                  do while (cornerval(crnrowpos,crncolpos).lt.number)

c     Part 2a
c     *******
c    finish this river channel if a previous one is met. This is ignored
c    if the corner value in the one that is met is not greater than or equal
c    to the current one
c     previous one is east
                   if ((cornerdonep(crnrowpos,crncolpos+1)).and.
     $                (cornerval(crnrowpos,crncolpos+1).ge.
     $                 cornerval(crnrowpos,crncolpos))) then
                      linkew(crnrowpos,crncolpos)=.true.

c                     channel elevation
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
c                     end of channel elevation

                      meetexit=.true.
                         exit
c     previous one is west
                   elseif((cornerdonep(crnrowpos,crncolpos-1)).and.
     $                (cornerval(crnrowpos,crncolpos-1).ge.
     $                 cornerval(crnrowpos,crncolpos))) then
                      linkew(crnrowpos,crncolpos-1)=.true.

c                     channel elevation
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
c                     end of channel elevation

                      meetexit=.true.
                         exit
c     previous one is south
                   elseif((cornerdonep(crnrowpos+1,crncolpos)).and.
     $                (cornerval(crnrowpos+1,crncolpos).ge.
     $                 cornerval(crnrowpos,crncolpos))) then
                      linkns(crnrowpos,crncolpos)=.true.

c                     channel elevation
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
c                     end of channel elevation

                      meetexit=.true.
                         exit
c     previous one is north
                   elseif((cornerdonep(crnrowpos-1,crncolpos)).and.
     $                (cornerval(crnrowpos-1,crncolpos).ge.
     $                 cornerval(crnrowpos,crncolpos))) then
                      linkns(crnrowpos-1,crncolpos)=.true.

c                     channel elevation
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
c                     end of channel elevation

                      meetexit=.true.
                         exit
c     Part 2b
c     *******
C     no previous channel carry on existing river channel
                   else

                    crnmax=max(cornerval(crnrowpos,crncolpos+1),
     $                        cornerval(crnrowpos,crncolpos-1),
     $                        cornerval(crnrowpos+1,crncolpos),
     $                        cornerval(crnrowpos-1,crncolpos))
c     east west link ->  heading east
                    if (crnmax.eq.cornerval(crnrowpos,crncolpos+1)) then
                               linkew(crnrowpos,crncolpos)=.true.
                               savelinkew(crnrowpos,crncolpos)=.true.

                          dum1=dem(crnrowpos-1,crncolpos)
                          dum2=dem(crnrowpos,crncolpos)
                          cheldum=min(dum1,dum2)
                        if ((cheldum-chanfac1).le.(pchdp-chanfac2)) then
                         ellinkew(crnrowpos,crncolpos)= cheldum-chanfac1
                         strlinkew(crnrowpos,crncolpos) = stricklerriv
                       else
                         ellinkew(crnrowpos,crncolpos)= pchdp-chanfac2
                         strlinkew(crnrowpos,crncolpos) = stricklerlake
                        endif
                          pchdp=min((cheldum-chanfac1),(pchdp-chanfac2))
 
                          
c extra code 040315 reduce the outlet link by 1m dano100m example
      if (cornerval(crnrowpos,crncolpos+1).ge.number) then
      ellinkew(crnrowpos,crncolpos) = ellinkew(crnrowpos,crncolpos)-1.0
c                                                                 ******
      endif
c end extra code
                              crncolpos=crncolpos+1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
c     east west link -> heading west
                    elseif 
     $                 (crnmax.eq.cornerval(crnrowpos,crncolpos-1)) then
                               linkew(crnrowpos,crncolpos-1)=.true.
                               savelinkew(crnrowpos,crncolpos-1)=.true.

                          dum1=dem(crnrowpos-1,crncolpos-1)
                          dum2=dem(crnrowpos,crncolpos-1)
                          cheldum=min(dum1,dum2)

                       if ((cheldum-chanfac1).le.(pchdp-chanfac2)) then
                       ellinkew(crnrowpos,crncolpos-1)= cheldum-chanfac1
                       strlinkew(crnrowpos,crncolpos-1) = stricklerriv
                       else
                       ellinkew(crnrowpos,crncolpos-1)= pchdp-chanfac2
                       strlinkew(crnrowpos,crncolpos-1) = stricklerlake
                       endif
                          pchdp=min((cheldum-chanfac1),(pchdp-chanfac2))

c extra code 040315 reduce the outlet link by 1m dano100m example
      if (cornerval(crnrowpos,crncolpos-1).ge.number) then
      ellinkew(crnrowpos,crncolpos-1) =
     $ ellinkew(crnrowpos,crncolpos-1)-1.0
      endif
c end extra code
                          crncolpos=crncolpos-1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
c     north south link -> heading south
                    elseif 
     $                 (crnmax.eq.cornerval(crnrowpos+1,crncolpos)) then
                               linkns(crnrowpos,crncolpos)=.true.
                               savelinkns(crnrowpos,crncolpos)=.true.

                          dum1=dem(crnrowpos,crncolpos-1)
                          dum2=dem(crnrowpos,crncolpos)
                          cheldum=min(dum1,dum2)
                        if ((cheldum-chanfac1).le.(pchdp-chanfac2)) then
                         ellinkns(crnrowpos,crncolpos)= cheldum-chanfac1
                         strlinkns(crnrowpos,crncolpos) = stricklerriv
                       else
                         ellinkns(crnrowpos,crncolpos)= pchdp-chanfac2
                         strlinkns(crnrowpos,crncolpos) = stricklerlake
                        endif
                          pchdp=min((cheldum-chanfac1),(pchdp-chanfac2))

c extra code 040315 reduce the outlet link by 1m dano100m example
      if (cornerval(crnrowpos+1,crncolpos).ge.number) then
      ellinkns(crnrowpos,crncolpos) = ellinkns(crnrowpos,crncolpos)-1.0
      endif
c end extra code

                              crnrowpos=crnrowpos+1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
c     north south link -> heading north
                    elseif
     $                 (crnmax.eq.cornerval(crnrowpos-1,crncolpos)) then
                               linkns(crnrowpos-1,crncolpos)=.true.
                               savelinkns(crnrowpos-1,crncolpos)=.true.

                          dum1=dem(crnrowpos-1,crncolpos-1)
                          dum2=dem(crnrowpos-1,crncolpos)
                          cheldum=min(dum1,dum2)

                       if ((cheldum-chanfac1).le.(pchdp-chanfac2)) then
                       ellinkns(crnrowpos-1,crncolpos)= cheldum-chanfac1
                       strlinkns(crnrowpos-1,crncolpos) = stricklerriv
                       else
                       ellinkns(crnrowpos-1,crncolpos)= pchdp-chanfac2
                       strlinkns(crnrowpos-1,crncolpos) = stricklerlake
                       endif

                          pchdp=min((cheldum-chanfac1),(pchdp-chanfac2))

c extra code 040315 reduce the outlet link by 1m dano100m example
      if (cornerval(crnrowpos-1,crncolpos).ge.number) then
      ellinkns(crnrowpos-1,crncolpos) 
     $ = ellinkns(crnrowpos-1,crncolpos)-1.0
      endif
c end extra code
                              crnrowpos=crnrowpos-1
                          cornerdone(crnrowpos,crncolpos)=.true.
                          savecornerdone(crnrowpos,crncolpos)=.true.
                    endif
                   endif
c      write(434,*)cornerval(crnrowpos,crncolpos),
c     $ crnrowpos,crncolpos,number
                  enddo


**** new code 280108 
**** apart from the main channel (countnumber=0) all other channels should
**** leve do while loop via exit code with meetexit=true
**** any that do not are removed
                  if (countnumber.eq.1) then

*set outlet flow accumulation=0 so no river join here and cause crashes im shetran
                    outletcrnrowpos=crnrowpos
                    outletcrncolpos=crncolpos
                    outletcornerval=cornerval(crnrowpos,crncolpos)
                    cornerval(outletcrnrowpos,outletcrncolpos)=0
*end of set outlet flow accum

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
c                    print*,'yes'
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
**** end of 280108




c     which corners have a specified link
c     used to attract a new river channel to an existing one
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


**** new code 280108 
c reset outlet flow accumulation
      cornerval(outletcrnrowpos,outletcrncolpos)=outletcornerval
**** end of 280108


C     End of find the position of the links
c     *************************************




c     find river channel (link) element numbers and position of outlet
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
c               new code 031212
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
c               new code 031212
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
c               new code 031212
                   demminoutletproblem=min(dem(i,j),dem(i-1,j))
               endif
            endif
         enddo
      enddo
      numlinks=k
c      do i=1,k
c      print*,linkelv(i)
c      enddo
c      print*,linkelvmin,linkoutdir,linkoutr,linkoutc

c               new code 031212. problem with location of river link
c      if direction of outlet is wrong then outlet link can be in the wrong place. 
c this bumps up DEM values so it still works.
      do i=1,nrows
          do j=1,ncols
              if (dem(i,j).lt.demminoutletproblem) then 
                  dem(i,j)= demminoutletproblem+0.1
                endif
          enddo
      enddo
c              endof new code new code 031212

******go through links to check there is always a downward pathway
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

c     ew links
      countiteration=0
 105  count=0
      countiteration=countiteration+1
      do i=2,nrows
          do j=2,ncols-1
              if (linkew(i,j)) then
                  if ((i.eq.linkoutr).and.(j.eq.linkoutc).and.
     $                                 (linkoutdir.eq.1)) then
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
* sb 240909 add 0.01
*** sb 041010 added chanfac2-0.0001
                  if ((ellinkew(i,j).le.dummin+chanfac2-0.0001).and.
     $                              (.not.outletlink)) then
                  ellinkew(i,j)=ellinkew(i,j)+chanfac2
                  count=count+1
                  endif
              endif
          enddo
      enddo
c     ns links
      do i=2,nrows-1
          do j=2,ncols
              if (linkns(i,j)) then
                  if ((i.eq.linkoutr).and.(j.eq.linkoutc).and.
     $                                 (linkoutdir.eq.2)) then
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
* sb 240909 add 0.01
*** sb 041010 added chanfac2-0.0001
                  if ((ellinkns(i,j).le.dummin+chanfac2-0.0001).and.
     $                              (.not.outletlink)) then
                  ellinkns(i,j)=ellinkns(i,j)+chanfac2
                  count=count+1
                  endif
              endif
          enddo
      enddo
      write(logfile,*) 'Number of river chanel sinks = ',count
      if (countiteration.eq.1000) then
         print*
         print*,'There is a problem finding a downward flow path'
         print*,'Please check poistion of river channels in Shegraph'
         print*
         write(*,'(''paused, type [enter] to continue'')')
         read (*,*)
      goto 106
c     ********
      endif

      if (count.gt.0) goto 105
c                     ********
******end of go through links for downward pathway


c     find river channel (link) element numbers again
c     in case of changes
 106     k=0
      linkelvmin=1.0e10
      i=nrows+1
      do j=1,ncols
         if (linkew(i,j)) then
            k=k+1
            linkelv(k)= ellinkew(i,j)
c            print*,i,j,lakedist(i,j),lakedist(i-1,j)
c uses lake mask to caculate if river or lake
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
c                print*,i,j,lakedist(i,j),lakedist(i,j-1)
c uses lake mask to caculate if river or lake
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
c            print*,i,j,lakedist(i,j),lakedist(i-1,j)
c uses lake mask to caculate if river or lake
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

*************new code 1/2/11 sb ************************
* direction (which is the outlet face) is sometimes wrong
* recaculate here

      if (linkoutdir.eq.1) then
         if (cornerval(linkoutr,linkoutc).
     $        gt.cornerval(linkoutr,linkoutc+1)) then
            direction=4
          else
            direction=2
          endif
      else
          if (cornerval(linkoutr,linkoutc).
     $        gt.cornerval(linkoutr+1,linkoutc)) then
            direction=1
          else
            direction=3
          endif
      endif
*********end of new code**********************************                 

****************temporary output**************************
c      do i=1,nrows+1
c         write(20,'(42L1)')(linkew(i,j),j=1,ncols)
c      enddo
c      write(20,*)
c      do i=1,nrows
c         write(20,'(43L1)')(linkns(i,j),j=1,ncols+1)
c      enddo

*      do i=1,nrows
*              write(30,'(40(g10.4,1X))') (dem(i,j),j=1,ncols)
*      enddo
c      do i=1,number
c          write(20,*) posval(i),posrow(i),poscol(i)  
c      enddo    
*      do i=1,nrows
*              write(30,'(40(i6,1X))') (accum(i,j),j=1,ncols)
*      enddo
c      do i=1,nrows+1
c             write(20,'(40(l4,1X))') (linkew(i,j),j=1,ncols)
c      enddo
c      do i=1,nrows
c              write(20,'(41(l4,1X))') (linkns(i,j),j=1,ncols+1)
c      enddo


*      do i=1,2*nrows+1
*         do j=1,2*ncols+1
*             xmap(i,j)=' '
*         enddo
*      enddo


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

*      do i=1,2*nrows
*         write(30,'(84(a1))') (xmap(i,j),j=1,2*ncols)
*      enddo


      do i=1,42
         do j=1,42
            if (linkew(i,j)) then
                msgs=12*(j-1)+1
                msge=msgs+5
                write(msg(msgs:msge),'(a6)') '    '
                msgs=12*(j-1)+7
                msge=msgs+5
                write(msg(msgs:msge),'(i6)') int(ellinkew(i,j))
            else
                msgs=12*(j-1)+1
                msge=msgs+11
                write(msg(msgs:msge),'(a12)') '        '
            endif
         enddo
*         write(30,'(a520)') msg
         do j=1,42
            if (linkns(i,j)) then
                msgs=12*(j-1)+1
                msge=msgs+5
                write(msg(msgs:msge),'(i6)') int(ellinkns(i,j))
                msgs=12*(j-1)+7
                msge=msgs+5
                write(msg(msgs:msge),'(i6)') int(dem(i,j))
            else
                msgs=12*(j-1)+1
                msge=msgs+5
                write(msg(msgs:msge),'(a6)') '    '
                msgs=12*(j-1)+7
                msge=msgs+5
                write(msg(msgs:msge),'(i6)') int(dem(i,j))
            endif
         enddo
*         write(30,'(a520)') msg
      enddo
****************end of temporary output**************************



***********Final output*************************************************
********** Computational grid definition ******************************* 
*
      do i=1,nrows
         do j=1,ncols
            if (catch(i,j).ne.novalue) then
                 catch(i,j) = 1
            else
               catch(i,j) = 0
            endif
         enddo
      enddo
      WRITE (MSG2,9334)
      WRITE (outfrd,9200) MSG2
      DO I = 1,nrows
        k=nrows-i+1
        WRITE (outfrd,9108) k,(catch(i,j),j=1,ncols)
      ENDDO  
*
***************** Flow Codes ******************************************
*
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
      WRITE (MSG2,9335)
      WRITE (outfrd,9200) MSG2
      DO I = 1,nrows
        k=nrows-i+1
        WRITE (outfrd,9109) k,(alinkns(i,j),j=1,ncols+1)
      ENDDO  
      WRITE (MSG2,9336)
      WRITE (outfrd,9200) MSG2
      DO I = 1,nrows+1
        k=nrows-i+2
        WRITE (outfrd,9109) k,(alinkew(i,j),j=1,ncols)
      ENDDO  
*                
      do i=1,nrows
       do l=1,10
                do j=1,ncols
               do k =1,10
                       if  (catch(i,j).eq.0) then
                         catchgeometry((i-1)*10+l,(j-1)*10+k) = novalue
                       else 
                         catchgeometry((i-1)*10+l,(j-1)*10+k) = 0
                       endif
               enddo
            enddo
         enddo
      enddo

      do i=1,nrows
        do l=1,10
           do j=1,ncols+1
             if (linkns(i,j)) then
                    catchgeometry((i-1)*10+l,(j-1)*10-1) = 1
                    catchgeometry((i-1)*10+l,(j-1)*10) = 1
                    catchgeometry((i-1)*10+l,(j-1)*10+1) = 1
            endif
          enddo
        enddo
      enddo  
      do I = 1,nrows+1
          do j=1,ncols
              do l=1,10
                 if (linkew(i,j)) then
                       catchgeometry((i-1)*10-1,(j-1)*10+l) = 1
                       catchgeometry((i-1)*10,(j-1)*10+l) = 1
                       catchgeometry((i-1)*10+1,(j-1)*10+l) = 1
                endif
           enddo
              enddo
      enddo 
      write(26,'(A5,a4,i8)') 'ncols','    ',ncols*10
      write(26,'(A5,a4,i8)') 'nrows','    ',nrows*10
      write(26,'(A9,a4,f12.2)') 'xllcorner','    ',xllcorner-cellsize
      write(26,'(A9,a4,f12.2)') 'yllcorner','    ',yllcorner-cellsize
      write(26,'(A8,a4,f10.1)') 'cellsize','    ',cellsize/10.0
      write(26,'(A12,a4,i8)') 'NODATA_value','    ',novalue
      do I = 1,nrows*10
         write(26,'(4000(I0,1x))') 
     $    (catchgeometry(i,j),j=1,ncols*10)
*** note 4000 here. This works with up to 400 columns
      enddo 
      write(logfile,*) 


!!!! 020514 remove mean sinks as well check if link or grid square nearest    
C     Remove sinks by removesink. Count is number of sinks
!!!+2.0 is to account for the banks
 201  count=0
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
      
c                print*,demn,dems,deme,demw,i,j,colposmin,
c     $           rowposmin,catch(i,j)
    
                if ((demmean(i,j).le.demminedge).and.(notlowpoint).and.
     $                             (catch(i,j).eq.1)) then

                          demmean(i,j)=demminedge+removesink
                          count=count+1
                  endif
              enddo
          enddo
          write(logfile,*) ' Number of land element sinks = ', count
      if (count.gt.0) goto 201
c                     ********

!!!! END of code change



*
************** Ground surface Elevations *******************************
*
      WRITE (MSG2,9337)
      WRITE (outfrd,9200) MSG2
      DO I = 1,nrows
        k=nrows-i+1
        DO J = 1,ncols
            IF (demmean(i,j).gt.9.9e9) THEN
               demmean(i,j)=0.0
            ENDIF
        ENDDO
        WRITE (outfrd,9101) k
        DO J=1,ncols
           AFORM(J) = FORM(demmean(i,j))
        ENDDO   
        WRITE(outfrd,9110) (AFORM(J),J=1,ncols)
      ENDDO  

*
****************************** PE Types2 ************************
c       do i=1,numberunique
c       print*, i, peunique(i)
c      enddo

            do i=2,nrows-1
        do j=2,ncols-1
          do k=1,numberunique
             if (pedist(i,j).eq.peunique(k)) then
                pedist2(i,j)= k
             endif
          enddo
          if (pedist(i,j).eq.-9999) then
             pedist2(i,j)= 0
          endif
          if ((catch(i,j).eq.1).and.(pedist2(i,j).eq.0)) then
                  pedist2(i,j)=1
          endif
        enddo
      enddo

      do j=1,ncols
           pedist2(1,j)=0
           pedist2(nrows,j)=0
      enddo
      do i=1,nrows
           pedist2(i,1)=0
           pedist2(i,ncols)=0
      enddo
     
      WRITE (MSG,9343)
      WRITE (OUTFRD,9200) MSG
      if (numberunique.lt.10) then
         DO I = 1,nrows
            k=nrows-i+1
           WRITE (outfrd,9108) k,(pedist2(i,j),j=1,ncols)
         ENDDO  
      else
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outfrd,9100) k
          WRITE (outfrd,9209)(pedist2(i,j),j=1,ncols)
        ENDDO  
      endif


*
****************************** rain Types2 ************************
c      do i=1,numberunique
c      print*, i, peunique(i)
c      enddo

            do i=2,nrows-1
        do j=2,ncols-1
          do k=1,numberuniquer
             if (raindist(i,j).eq.rainunique(k)) then
                raindist2(i,j)= k
             endif
          enddo
          if (raindist(i,j).eq.-9999) then
             raindist2(i,j)= 0
          endif
          if ((catch(i,j).eq.1).and.(raindist2(i,j).eq.0)) then
                  raindist2(i,j)=1
          endif
        enddo
      enddo

      do j=1,ncols
           raindist2(1,j)=0
           raindist2(nrows,j)=0
      enddo
      do i=1,nrows
           raindist2(i,1)=0
           raindist2(i,ncols)=0
      enddo
     
      WRITE (MSG,9346)
      WRITE (OUTFRD,9200) MSG
      if (numberuniquer.lt.10) then
         DO I = 1,nrows
            k=nrows-i+1
           WRITE (outfrd,9108) k,(raindist2(i,j),j=1,ncols)
         ENDDO  
      else
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outfrd,9100) k
          WRITE (outfrd,9209) (raindist2(i,j),j=1,ncols)
        ENDDO  
      endif



*
****************************** Vegetation Types ************************
*
      if (innmveg.gt.1) then
         OPEN(13,FILE=vegname,STATUS='OLD')
      endif
      WRITE (MSG,9349)
      WRITE (OUTFRD,9200) MSG
      if (innmveg.gt.1) then
        READ(13,*) 
        READ(13,*) 
        READ(13,*) 
        READ(13,*) 
        READ(13,*) 
        READ(13,*) 
          do i=2,nrows-1
          read(13,*) (vegdist(i,j),j=2,ncols-1)
          do j=2,ncols-1
             if (vegdist(i,j).lt.0) then 
                vegdist(i,j)=0
***             make sure every grid square inside is defined
                if ((catch(i,j).eq.1).and.(vegdist(i,j).eq.0)) then
                  vegdist(i,j)=1
                endif
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
        
****************************
!extra forest layer
      if (IsForestFile) then
        
        DO I = 1,nrows
        DO j = 1,ncols

        if (vegdist(i,j).eq.0) then
          NFMForestDist2(i,j)=0
        endif

        if (vegdist(i,j).eq.1) then
        if (NFMForestDist(i,j).lt.5) then
          NFMForestDist2(i,j)=1
        elseif (NFMForestDist(i,j).lt.15) then
           NFMForestDist2(i,j)=3
        elseif (NFMForestDist(i,j).lt.25) then
          NFMForestDist2(i,j)=4
        elseif (NFMForestDist(i,j).lt.35) then
          NFMForestDist2(i,j)=5
        elseif (NFMForestDist(i,j).lt.45) then
          NFMForestDist2(i,j)=6
        elseif (NFMForestDist(i,j).lt.55) then
          NFMForestDist2(i,j)=7
        elseif (NFMForestDist(i,j).lt.65) then
          NFMForestDist2(i,j)=8
        elseif (NFMForestDist(i,j).lt.75) then
          NFMForestDist2(i,j)=9
        elseif (NFMForestDist(i,j).lt.85) then
          NFMForestDist2(i,j)=10
        elseif (NFMForestDist(i,j).lt.95) then
          NFMForestDist2(i,j)=11
        else
          NFMForestDist2(i,j)=12
        endif
        endif

        if (vegdist(i,j).eq.2) then
          NFMForestDist2(i,j)=2
        endif
        enddo
        enddo

        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outfrd,9100) k
          WRITE (outfrd,9209) (NFMForestDist2(i,j),j=1,ncols)
        ENDDO
        
!different output type if more than 9 veg types
      else
          if (innmveg.le.9) then
              DO I = 1,nrows
                  k=nrows-i+1
               WRITE (outfrd,9108) k,(vegdist(i,j),j=1,ncols)
             ENDDO  
         else
              DO I = 1,nrows
                  k=nrows-i+1
                  WRITE (outfrd,9100) k
                  WRITE (outfrd,9209) (vegdist(i,j),j=1,ncols)
              ENDDO
         endif                  
        
        
        
      endif
****************************
!end of extra forest layer
        
        
        
*        DO I = 1,nrows
*          k=nrows-i+1
*          WRITE (outfrd,9108) k,(vegdist(i,j),j=1,ncols)
*        ENDDO  

      else
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outfrd,9108) k,(catch(i,j),j=1,ncols)
        ENDDO  
      endif
*
      WRITE (MSG,9352)
      WRITE (OUTFRD,9200) MSG
      AFORM(1) =FORM(regulartmstep)
      WRITE(OUTFRD,9110) AFORM(1)

      
      close(13)

****************OCD Data ***********************************************      
************************ Title *****************************************      
*
      WRITE (MSG,9401)
      WRITE (OUTOCD,9200) MSG
*
********************** Get basic control parameters ********************
*

!10 extra negative values for storage      
      if (IsStorageFile) then      
      WRITE (OUTOCD,9405) '1',innmveg+10,'1','F'
      else
      WRITE (OUTOCD,9405) '1',innmveg,'1','F'
      endif
*
*************************** Timestep Control ***************************
*
*     These values are not used, but they must be present
      WRITE (MSG,9402)
      WRITE (OUTOCD,9200) MSG
      AFORM(1)=FORM(1.0)
      AFORM(2)=FORM(99999.0)
      WRITE (OUTOCD,9105) AFORM(1),AFORM(2)
      
************************** Other Parameters ****************************
*
      WRITE (MSG,9403)
      WRITE (OUTOCD,9200) MSG
      AFORM(1)=FORM(100.0)
*** strickler roughness depends on veg type

      AFORM(2)=FORM(0.0)
      AFORM(3)=FORM(0.0)
      AFORM(4)=FORM(99999.0)
      AFORM(5)=FORM(0.0)
      WRITE (OUTOCD,9103) AFORM(1),AFORM(2),AFORM(3),
     $                      AFORM(4),AFORM(5)
      do i=1,innmveg
         aform(i)=form(instricklerveg(i))
      enddo

******************
!extra storage file
      if (IsStorageFile) then      
      !10 extra negative values for storage      
         do i=1,10
            aform(innmveg+i)=form(real(-i*10))
!         aform(i)=form(-i*10)
         enddo
         write (outocd,'(100(a7))') (aform(i),i=1,innmveg+10)

         DO I = 1,nrows
         DO j = 1,ncols

         if (vegdist(i,j).eq.0) then
             NFMStorageDist2(i,j)=0
         endif

        if (vegdist(i,j).eq.1) then
        if (NFMStorageDist(i,j).lt.5) then
          NFMStorageDist2(i,j)=1
        elseif (NFMStorageDist(i,j).lt.15) then
           NFMStorageDist2(i,j)=3
        elseif (NFMStorageDist(i,j).lt.25) then
          NFMStorageDist2(i,j)=4
        elseif (NFMStorageDist(i,j).lt.35) then
          NFMStorageDist2(i,j)=5
        elseif (NFMStorageDist(i,j).lt.45) then
          NFMStorageDist2(i,j)=6
        elseif (NFMStorageDist(i,j).lt.55) then
          NFMStorageDist2(i,j)=7
        elseif (NFMStorageDist(i,j).lt.65) then
          NFMStorageDist2(i,j)=8
        elseif (NFMStorageDist(i,j).lt.75) then
          NFMStorageDist2(i,j)=9
        elseif (NFMStorageDist(i,j).lt.85) then
          NFMStorageDist2(i,j)=10
        elseif (NFMStorageDist(i,j).lt.95) then
          NFMStorageDist2(i,j)=11
        else
          NFMStorageDist2(i,j)=12
        endif
        endif

        if (vegdist(i,j).eq.2) then
            NFMStorageDist2(i,j)=2
        endif
        enddo
         enddo
      WRITE (MSG,9414)
      WRITE (OUTOCD,9200) MSG
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outocd,9100) k
          WRITE (outocd,9209) (NFMStorageDist2(i,j),j=1,ncols)
      ENDDO  

      
      WRITE (MSG,9417)
      WRITE (OUTOCD,9200) MSG
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outocd,9100) k
          WRITE (outocd,9209) (NFMStorageDist2(i,j),j=1,ncols)
        ENDDO  
      else

          
!standard output with no storage file
          write (outocd,'(100(a7))') (aform(i),i=1,innmveg)
      WRITE (MSG,9414)
      WRITE (OUTOCD,9200) MSG

!different output type if more than 9 veg types
          if (innmveg.le.9) then
              DO I = 1,nrows
                  k=nrows-i+1
               WRITE (outocd,9108) k,(vegdist(i,j),j=1,ncols)
             ENDDO  
         else
              DO I = 1,nrows
                  k=nrows-i+1
                  WRITE (outocd,9100) k
                  WRITE (outocd,9209) (vegdist(i,j),j=1,ncols)
              ENDDO
         endif                  
      
      
      


      WRITE (MSG,9417)
      WRITE (OUTOCD,9200) MSG
 
!different output type if more than 9 veg types
          if (innmveg.le.9) then
              DO I = 1,nrows
                  k=nrows-i+1
               WRITE (outocd,9108) k,(vegdist(i,j),j=1,ncols)
             ENDDO  
         else
              DO I = 1,nrows
                  k=nrows-i+1
                  WRITE (outocd,9100) k
                  WRITE (outocd,9209) (vegdist(i,j),j=1,ncols)
              ENDDO
         endif                  
      
      endif     
!end of extra storage file
**************************



******** Are there head,flux or polynomial boundary conditions ? *******
*
      WRITE (MSG,9420)
      WRITE (OUTOCD,9200) MSG
      WRITE (OUTOCD,9105) '0','0','0'
*
*
************** Channel elevations *******************************
* and channel widths
*
      maffactor=cellsize*cellsize*netrainfall/(1000.0*8760.0*3600.0)

      WRITE (MSG2,9430)
      WRITE (outocd,9200) MSG2
      NDEFCT = 0
      WRITE (outocd,9211) NDEFCT
      
      WRITE (MSG2,9435)
      WRITE (outocd,9200) MSG2
      wdepth=0.0
*** strickler now caculated seperately for rivers and lakes 131213
c      str=20.0
      nxsect=2
*** sb 041010 changed from 10.0 to 50.0
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
          WRITE (outocd,9205)i,AFORM(1),AFORM(2),AFORM(3),
     $                        NXSECT       

          streamwidth1=streamsize(i)*maffactor
          streamwidth2=streamwidthfac1*(streamwidth1**streamwidthfac2)
               aform(1)=form(streamwidth2)
          aform(2)=form(0.0)
          aform(3)=form(streamwidth2)
*** sb 041010 changed from 2.0 to 1.5
          aform(4)=form(1.5)
          WRITE (outocd,9210) AFORM(1),AFORM(2),AFORM(3),AFORM(4)


             if (i.eq.linkoutnum) then     
               AFORM(1)=FORM(COEFF)
               AFORM(2)=FORM(SUBRIO)
               AFORM(3)=FORM(linkelv(i)+0.1)
               AFORM(4)=FORM(linkelv(i)-1.0)
               WRITE (outocd,9206) IFACE,AFORM(1),AFORM(2),
     $                          AFORM(3),AFORM(4)
             endif
*
      enddo


****************etd Data ***********************************************      
************************ Title *****************************************      
**** Title and printing control and reading control parameters**********      
*
      WRITE (MSG,9501)
      WRITE (OUTETD,9200) MSG
      WRITE (OUTETD,9105) 'F','F','T'
*
**** Timestep for input of rain and met. data *************************      
*
      AFORM(1)=FORM(inprectmstep)
      AFORM(2)=FORM(inpetmstep)
      WRITE (MSG,9503)
      WRITE (OUTETD,9200) MSG
      WRITE (OUTETD,9105) '1.0',AFORM(1),AFORM(2)
*
**** Read whether potential evap is measured and thus read in ********* 
*    directly for each met. ststion in turn.
*    MEASPE=0: potential evap. not measured
*    MEASPE=1: potential evap. measured
      WRITE (MSG,9505)
      WRITE (OUTETD,9200) MSG
      do i=1,numberunique
      numbermet(i)=1
      enddo
      WRITE (OUTETD,9123) (numbermet(i),i=1,numberunique)
*
******* Vegetation types *********************************** 

      do i=1,innmveg
  
         
* find rooting depth 
        do j=1,27
          if (inrootingdepth(i).le.depth(j)) then
            nmcellroot=j
            exit
          endif
        enddo


       if (inlai(i).ge.1.0) then
          plai=1.0
          clai=inlai(i)
        else
          plai=inlai(i)
          clai=1.0 
        endif

        NF=7     
        CK=0.000014
        CB=5.1
        NRD=nmcellroot

        WRITE (MSG,'(A23,A20)') ':ET7 - VEGETATION TYPE ',invegtypes(i)
        !print*,invegtypes(i)
        WRITE (OUTETD,9200) MSG


        AFORM(1) = FORM(PLAI)
        AFORM(2) = FORM(incstcap(i))
        AFORM(3) = FORM(CK)
        AFORM(4) = FORM(CB)
        AFORM(5) = FORM(clai)


        WRITE (OUTETD,9255) 'F','0.','0.','0.','0.','0.','3',
     $   '7',AFORM(1),AFORM(2),AFORM(3),AFORM(4),NRD,AFORM(5),'0','0'
         

******** Check time-varying arrays 


        WRITE (MSG,9509)
        WRITE (OUTETD,9200) MSG

        if (cstcapnopoints(i).gt.0) then
          WRITE (OUTETD,9105) '1','0','0','0'
          WRITE (MSG,9518)
          WRITE (OUTETD,9200) MSG
          cstcapnoyear=(inendyear-inyear+1)*cstcapnopoints(i)
          
          WRITE (OUTETD,9211) cstcapnoyear
           WRITE (MSG,9519)
          WRITE (OUTETD,9200) MSG
          do j=1,inendyear-inyear+1
             do k=1,cstcapnopoints(i)
                aform(1)=form(cstcapratio(i,k))
                aform(2)=form(cstcaptime(i,k)+365.0*(j-1))
                WRITE (OUTETD,9105) aform(1),aform(2)
            enddo
          enddo

        else
          WRITE (OUTETD,9105) '0','0','0','0'

        endif         


*************PSL/RCF/FET
        WRITE (MSG,9515)
        WRITE (OUTETD,9200) MSG
        PS1(1)=-1000
        PS1(2)=-150
        PS1(3)=-50
        PS1(4)=-20
        PS1(5)=-10
        PS1(6)=-1
        PS1(7)=-0.1
        FET(1)=0.0*inaepe(i)
        FET(2)=0.05*inaepe(i)
        FET(3)=0.20*inaepe(i)
        FET(4)=0.50*inaepe(i)
        FET(5)=0.80*inaepe(i)
        FET(6)=1.00*inaepe(i)
        FET(7)=1.00*inaepe(i)
        DO J=1,NF
             AFORM(1)=FORM(PS1(J))
             AFORM(2)=FORM(FET(J))
             WRITE (OUTETD,9105) AFORM(1),'0.',AFORM(2)
        ENDDO



* Read and write root density function data

        WRITE (MSG,9517)
        WRITE (OUTETD,9200) MSG

      
        do j=1,nmcellroot
            AFORM(1)=FORM(DEPTH(J))
            AFORM(2)=FORM(RDF(j,nmcellroot))
            WRITE (OUTETD,9105) AFORM(1),AFORM(2)
        ENDDO

      enddo

******* Vegetation types extra forest layer *********************************** 
******* Vegetation types extra forest layer *********************************** 
      if (IsForestFile) then      

*forest layers types 3-12 depending on percent cover
      do i=3,12
  
         
* find rooting depth 
        do j=1,27
          if (inrootingdepth(1).le.depth(j)) then
            nmcellroot=j
            exit
          endif
        enddo


       if (inlai(1).ge.1.0) then
          plai=1.0
          clai=inlai(1)
        else
          plai=inlai(1)
          clai=1.0 
        endif

        NF=7     
        CK=0.000014
        CB=5.1
        NRD=nmcellroot

        WRITE (MSG,'(A23,i4)') ':ET7 - VEGETATION TYPE ',i
        !print*,invegtypes(i)
        WRITE (OUTETD,9200) MSG


        AFORM(1) = FORM(PLAI)
*forest fraction cstcap
        AFORM(2) = FORM(incstcap(1)+3*incstcap(1)*(i-2)/10.0)
        AFORM(3) = FORM(CK)
        AFORM(4) = FORM(CB)
        AFORM(5) = FORM(clai)


        WRITE (OUTETD,9255) 'F','0.','0.','0.','0.','0.','3',
     $   '7',AFORM(1),AFORM(2),AFORM(3),AFORM(4),NRD,AFORM(5),'0','0'
         

******** Check time-varying arrays 


        WRITE (MSG,9509)
        WRITE (OUTETD,9200) MSG

        if (cstcapnopoints(1).gt.0) then
          WRITE (OUTETD,9105) '1','0','0','0'
          WRITE (MSG,9518)
          WRITE (OUTETD,9200) MSG
          cstcapnoyear=(inendyear-inyear+1)*cstcapnopoints(1)
          
          WRITE (OUTETD,9211) cstcapnoyear
           WRITE (MSG,9519)
          WRITE (OUTETD,9200) MSG
          do j=1,inendyear-inyear+1
             do k=1,cstcapnopoints(1)
                aform(1)=form(cstcapratio(i,k))
                aform(2)=form(cstcaptime(i,k)+365.0*(j-1))
                WRITE (OUTETD,9105) aform(1),aform(2)
            enddo
          enddo

        else
          WRITE (OUTETD,9105) '0','0','0','0'

        endif         


*************PSL/RCF/FET
        WRITE (MSG,9515)
        WRITE (OUTETD,9200) MSG
        PS1(1)=-1000
        PS1(2)=-150
        PS1(3)=-50
        PS1(4)=-20
        PS1(5)=-10
        PS1(6)=-1
        PS1(7)=-0.1
        FET(1)=0.0*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(2)=0.05*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(3)=0.20*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(4)=0.50*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(5)=0.80*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(6)=1.00*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        FET(7)=1.00*(inaepe(1)+inaepe(1)*(i-2)/10.0)
        DO J=1,NF
             AFORM(1)=FORM(PS1(J))
             AFORM(2)=FORM(FET(J))
             WRITE (OUTETD,9105) AFORM(1),'0.',AFORM(2)
        ENDDO



* Read and write root density function data

        WRITE (MSG,9517)
        WRITE (OUTETD,9200) MSG

      
        do j=1,nmcellroot
            AFORM(1)=FORM(DEPTH(J))
            AFORM(2)=FORM(RDF(j,nmcellroot))
            WRITE (OUTETD,9105) AFORM(1),AFORM(2)
        ENDDO

      enddo
      endif


****************vsd Data ***********************************************      
************************ Title *****************************************      

!      if (innmsoil.gt.1) then
           OPEN(14,FILE=soilname,STATUS='OLD')
!      endif

      WRITE (MSG,9601)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9201) 'VSS data'

      WRITE (MSG,9602)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9105) 'F','T','F'

      WRITE (MSG,9603)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9118) innmsoil,'14','0','1'

      WRITE (MSG,9604)
      WRITE (OUTVSD,9200) MSG
      AFORM(1)=FORM(ininitialpsl)
      WRITE (OUTVSD,9103) AFORM(1),'0','0','1.0','1.0'

      WRITE (MSG,9605)
      WRITE (OUTVSD,9200) MSG

************************ SOILS ********************************      
      do i=1,innmsoil

      SATSTOR=0.001

      WRITE (OUTVSD,9207) i,'1','0',' ',insoiltypes(i)
      AFORM(1) = FORM(inksat(i))
      AFORM(2) = FORM(inthsat(i))
      AFORM(3) = FORM(inthres(i))
      AFORM(4) = FORM(SATSTOR)
      AFORM(5) = FORM(invgn(i))
      AFORM(6) = FORM(invga(i))
      WRITE (OUTVSD,9404)  AFORM(1),AFORM(1),AFORM(1),
     $       AFORM(2),AFORM(3),AFORM(4),AFORM(5),AFORM(6)


      enddo

************************ End of soils ********************************      

      maxsoildepth=maxval(insoildepth(1:innmsoilcat))

      if (maxsoildepth.lt.22.0) then

      WRITE (MSG,9606)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9104) 
     $ '0.1','0.2','0.3','0.4','0.6','0.8','0.8','1.0','2.0','2.0'
      WRITE (OUTVSD,9104) 
     $ '3.0','4.0','5.0','5.0'

      elseif (maxsoildepth.lt.46.0) then

          WRITE (MSG,9606)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9104) 
     $ '0.1','0.2','0.3','0.4','0.6','0.8','1.0','2.0','3.0','4.0'
      WRITE (OUTVSD,9104) 
     $ '6.0','8.0','10.0','10.0'

      elseif (maxsoildepth.lt.100.0) then

          WRITE (MSG,9606)
      WRITE (OUTVSD,9200) MSG
      WRITE (OUTVSD,9104) 
     $ '0.1','0.2','0.3','0.4','0.6','1.0','2.0','4.0','8.0','12.0'
      WRITE (OUTVSD,9104) 
     $ '16.0','20.0','20.0','20.0'

         
      
      
      else
          print*,' '
          print*,' This version of shetran prepare has a limit of 100m',
     $     'soil/rock depth. The xml file contains deeper soils/rocks '
          print*,' '
          write(*,'(''paused, type [enter] to continue'')')
          read (*,*)
          stop
      endif
      
      
      maxcatnumber=insoilcats(innmsoilcat)


      WRITE (MSG,9608)
      WRITE (OUTVSD,9200) MSG
C      WRITE (OUTVSD,9118) cattype,'0'
      WRITE (OUTVSD,9118) maxcatnumber,'0'

      WRITE (MSG,9658)
      WRITE (OUTVSD,9200) MSG

      

      do i =1,innmsoilcat
      aform(i)=form(insoildepth(i))
      enddo
      
     
      pc=0
      insoilcats(innmsoilcat+1)=insoilcats(innmsoilcat)+1
      do i=1,innmsoilcat
c       print*,i,catnumber(cattype),insoilcats(catnumber(cattype))
c       pause
        if (insoilcats(i+1).gt.insoilcats(i)) then
          WRITE (OUTVSD,9120) insoilcats(i),i-pc
          WRITE (OUTVSD,9121) 
     $     ' ',(insoilnumbers2(j),j=i,pc+1,-1)
!     $     ' ',(j,j=insoilnumbers2(i),insoilnumbers2(pc+1),-1)
          WRITE (OUTVSD,9122) ' ',
!     $ (aform(j), j=insoilnumbers2(i),insoilnumbers2(pc+1),-1)
     $ (aform(j), j=i,pc+1,-1)
           pc=i
 !       else
 !          cattype=cattype+1  
        endif
      enddo
c      do i=1,cattype
c        WRITE (OUTVSD,9120) i,catnumber(i)-catnumber(i-1)
c        WRITE (OUTVSD,9121) ' ',(j,j=catnumber(i),catnumber(i-1)+1,-1)
c        WRITE (OUTVSD,9122) ' ',
c     $            (aform(j), j=catnumber(i),catnumber(i-1)+1,-1)
c      enddo

      if (maxcatnumber.gt.1) then
        WRITE (MSG2,9668)
        WRITE (outvsd,9200) MSG2

        READ(14,*) 
        READ(14,*) 
        READ(14,*) 
        READ(14,*) 
        READ(14,*) 
        READ(14,*) 
          do i=2,nrows-1
          read(14,*) (soildist(i,j),j=2,ncols-1)
          do j=2,ncols-1
             if (soildist(i,j).lt.0) then 
                soildist(i,j)=0
***             make sure every grid square inside is defined
                if ((catch(i,j).eq.1).and.(soildist(i,j).eq.0)) then
                  soildist(i,j)=1
                endif
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
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outvsd,9108) k,(soildist(i,j),j=1,ncols)
        ENDDO  
        else
        DO I = 1,nrows
          k=nrows-i+1
          WRITE (outvsd,9208) k,(soildist(i,j),j=1,ncols)
        ENDDO  
        endif

      endif



      WRITE (MSG2,9610)
      WRITE (outvsd,9200) MSG2
      WRITE (OUTVSD,9105) '0'
      WRITE (MSG2,9611)
      WRITE (outvsd,9200) MSG2
      WRITE (OUTVSD,9104) '0','0','0','0','0','0','0','0'

      close(14)



****************rundata ***********************************************      
************************ Title *****************************************      
      WRITE (MSG2,9701)
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9710)
      WRITE (outrun,9200) MSG2

      WRITE (outrun,9200) FILFRD2
c      j=1
c      do i=1,80
c      if (msg2(i:i).eq.' ') then
c      else
c          msg3(j:j)=msg2(i:i)
c          j=j+1
c      endif
c      enddo
c      do i=j,80
c         msg3(i:i)=' '
c      enddo 
c      WRITE (outrun,9200) MSG3
c      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9711)
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9200) FILVSD2
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9712)
      WRITE (outrun,9200) MSG2



      WRITE (MSG2,9200) FILOCD2
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9713)
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9200) FILETD2
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9714)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 

      WRITE (MSG2,9715)
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9200) 
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9716)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9717)
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9200) FILSYD2
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9718)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9719)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9720) 
      WRITE (outrun,9200) MSG2

c      print*,filprd

      WRITE (MSG2,9200) FILPRD
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9721)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILEPD
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9722) 
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILTIM
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9723)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILPRI
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9724)
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9200) FILSPR
      WRITE (outrun,9200) MSG2
      WRITE (MSG2,9725)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9726)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9727)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9728)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9729)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9730)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9731)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9732)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9733)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9734)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9735)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9736)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9737)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9738)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9739)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
      WRITE (MSG2,9740)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*) 
 
      WRITE (MSG2,9741)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILDIS
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9742)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILVSE
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9743)
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9200) FILMAS
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9744)
      WRITE (outrun,9200) MSG2

      WRITE (outrun,9200) FILDIS2
      WRITE (MSG2,9745)
      WRITE (outrun,9200) MSG2

      WRITE (outrun,*) 
      WRITE (MSG2,9746)
      WRITE (outrun,9200) MSG2

      WRITE (outrun,*) 
      WRITE (MSG2,9747)
      WRITE (outrun,9200) MSG2
      WRITE (outrun,*)




      WRITE (MSG2,9748)
      WRITE (outrun,9200) MSG2

 
      WRITE (MSG2,9200) FILVIS2
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9749)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILCVI
      WRITE (outrun,9200) MSG2

      WRITE (MSG2,9750)
      WRITE (outrun,9200) MSG2


      WRITE (MSG2,9200) FILHDF
      WRITE (outrun,9200) MSG2

*****************sediment file******************************************
************************************************************************
 8901  FORMAT(':SY01 - SY job title')
 8902  FORMAT(':SY02 - SY Version number')
 8911 FORMAT
     $(':SY11 - NSED,ISGSED,ISTEC,ISSYOK,NEPS ',
     $ '[ISACKW,ISUSED,NFINE if NLF>0 ]')
 8912  FORMAT(':SY12 - FPCRIT DLSMAX [ , ALPHA, CONCOB, DCBEDO, FBIC,',
     $  'FICRIT if NLF>0 ]')
 8921 FORMAT(':SY21 - DRSED(1:NSED) : Sediment diameter')
 8922 FORMAT(':SY22 - GKR(s),GKF(s),RHOSO(s),FPCLAY(s),BKB(s) ',
     $  'for s=1 to NS : Soil properties')
 8923 FORMAT(':SY23 - SOSDFN(s,1:NSED) for s=1 to NS :',
     $  'Soil composition by size group')
 8924 FORMAT(':SY24 - XDRIP(v), DRDRIP(v), FDRIP(v) for v=1 to NV :',
     $  'Vegetation properties')
 8931 FORMAT(':SY31 - NTSOBK(l) for l=1 to NLF : Bank soil type')
 8932 FORMAT(':SY32 - PBSED(l) for l=1 ro NLF:Porosity of bed sediment')
 8941 FORMAT(':SY41 - NCAT : no. of categories for FCG')
 8942 FORMAT(':SY41c  FCG(1:NCAT) : Ground cover fraction')
 8943 FORMAT(':SY42 - NCAT : no. of categories for FCROCK')
 8944 FORMAT(':SY42c  FCROCK(1:NCAT) : ',
     $  ' Fraction of non-erodible ground surface')
 8945 FORMAT(':SY43 - NCAT : no. of categories for PLS porosity')
 8946 FORMAT(':SY43c  PLS(1:NCAT) : Porosity of loose sediment')
 8951 FORMAT(':SY51 - NCAT : no. of categories for DLS loose depth')
 8952 FORMAT(':SY51c  DLS(1:NCAT) : Initial loose/bed sediment depth')
 8953 FORMAT(':SY52 - NCAT : no. of categories for FBETA;',
     $  ' or -1 to copy from SOSDFN')
 8954 FORMAT(':SY52c  FBETA(1:NCAT) : Initial fraction of loose/bed',
     $   'sediment')
 8955 FORMAT(':SY52d  IDUM(link) category for FBETA')
 8956 FORMAT(':SY52e  IDUM(x,y) category for FBETA')
 8957 FORMAT(':SY53 - NCAT : no. of categories for FDEL')
 8958 FORMAT(':SY53c  "FDEL"(1:NSED,c) for c=1 to NCAT : ',
     $  ' Initial suspended sediment fractions')
 8961 FORMAT(':SY61 - NSYB, NSYC(1:4) : no. of boundaries; ',
     $  'no. of categories for each type')

      WRITE (MSG2,8901)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,9200) catchname

      WRITE (MSG2,8902)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '4.4.5'

      WRITE (MSG2,8911)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*) '      7      1      0      1      1',
     $'     1       1      1'
      WRITE (MSG2,8912)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*)
     $ '      0.25     0.05    1.0    0.0    0.01   0.0    0.0'

      WRITE (MSG2,8921)
      WRITE (outsyd,9200) MSG2
      WRITE (outsyd,*)
     $'      .1D-3   .37D-3  .89D-3  1.59D-3  2.25D-3  3.25D-3  8.0D-3'


      WRITE (MSG2,8922)
      WRITE (outsyd,9200) MSG2
      do i=1,innmsoil
      WRITE (outsyd,*)
     $'       2.5   1.0D-5   1.537D3   0.26    0.0'
      enddo

      WRITE (MSG2,8923)
      WRITE (outsyd,9200) MSG2
      do i=1,innmsoil
      WRITE (outsyd,*)
     $'     0.60   0.20   0.10  0.05  0.03  0.02  0.0'
      enddo

      WRITE (MSG2,8924)
      WRITE (outsyd,9200) MSG2
      do i=1,innmveg
      WRITE (outsyd,*)
     $'        3.0      0.005      0.50'
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
      WRITE (outsyd,*)
     $'     0.60   0.20   0.10  0.05  0.03  0.02  0.0'
      WRITE (outsyd,*)
     $'     0.01   0.02   0.04  0.07  0.16  0.25  0.45'
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

****************visulisation-plan**************************************      
************************ Title *****************************************   
      if (insoildepthmin.le.1.0) then
          vislayer=int(10*insoildepthmin)+1
      elseif  (insoildepthmin.le.2.0) then
          vislayer=int(5*(insoildepthmin-1.0))+11
      else 
          vislayer=int(insoildepthmin-2.0)+16
      endif
 9801 FORMAT ("'visualisation plan'")
 9802 FORMAT ("diag")
 9803 FORMAT ("item")
 9804 FORMAT ("NUMBER^1 : NAME^net_rain : BASIS^list_as_list :",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^none")
 9805 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : ENDITEM")
 9806 FORMAT ("NUMBER^2 : NAME^ph_depth : BASIS^grid_as_grid :",
     $" SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9807 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : ENDITEM")
 9808 FORMAT ("NUMBER^3 : NAME^theta : BASIS^grid_as_grid :",
     $" SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9809  FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1 10 : ENDITEM")
 9810 FORMAT ("NUMBER^4 : NAME^ovr_flow : BASIS^grid_as_list :",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9811 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")
 9812 FORMAT ("NUMBER^5 : NAME^srf_dep : BASIS^grid_as_list :",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^none")
 9813 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 : ENDITEM")
 9854 FORMAT ("NUMBER^6 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9855 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^1 :ENDITEM")
 9856 FORMAT ("NUMBER^7 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9857 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^2 :ENDITEM")
 9858 FORMAT ("NUMBER^8 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9859 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^3 :ENDITEM")
 9860 FORMAT ("NUMBER^9 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9861 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^4 :ENDITEM")
 9862 FORMAT ("NUMBER^10 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9863 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^5 :ENDITEM")
 9864 FORMAT ("NUMBER^11 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9865 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^6 :ENDITEM")
 9866 FORMAT ("NUMBER^12 : NAME^s_dis : BASIS^list_as_list : ",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^faces")
 9867 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 : SEDIMENT_NO^7 :ENDITEM")
 9868 FORMAT ("NUMBER^13 : NAME^s_v_er : BASIS^grid_as_grid :",
     $" SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9869 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 :ENDITEM")
 9870 FORMAT ("NUMBER^14 : NAME^s_t_dp : BASIS^grid_as_grid :",
     $" SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9871 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^9 :ENDITEM ")
 9872 FORMAT ("NUMBER^15 : NAME^s_t_dp : BASIS^list_as_list :",
     $" SCOPE^rivers :  EXTRA_DIMENSIONS^none")
 9873 FORMAT ("GRID_OR_LIST_NO^6 : TIMES^9 :ENDITEM")
 9832 FORMAT ("NUMBER^6 : NAME^snow_dep : BASIS^grid_as_grid :",
     $" SCOPE^squares :  EXTRA_DIMENSIONS^none")
 9833 FORMAT ("GRID_OR_LIST_NO^7 : TIMES^8 : ENDITEM")
 9814 FORMAT ("list")
 9815 FORMAT ("6 1   !number and size")
 9816 FORMAT ("mask")
 9817 FORMAT ("times")
* sb 021009 change from 24 and 1 hour to 168 and 24 hours
 9818 FORMAT ("8 1 !number and no. of entries")
 9820 FORMAT ("730 876000 !every 168 hours for 10 years")
 9821 FORMAT ("9 1 !number and no. of entries")
 9822 FORMAT ("24 876000 !every 24 hour for 10 years")
 9823 FORMAT ("stop")


      WRITE (MSG,9801)
      WRITE (outvis,9260) MSG
      WRITE (outvis,*) '!',trim(CATCHNAME)
      WRITE (MSG,9802)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9803)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9804)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9805)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9803)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9806)
      WRITE (outvis,9260) MSG
      !if (vislayer.le.9) then 
      !  WRITE (MSG,'(A38,1x,i1,A10)') 
     $!  'GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1',vislayer,
     $!    ' : ENDITEM'
      !else
      !  WRITE (MSG,'(A38,1x,i2,A10)') 
     $!  'GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1',vislayer,
     $!    ' : ENDITEM'
      !endif
      WRITE (MSG,9807)

      WRITE (outvis,9260) MSG
      WRITE (MSG,9803)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9808)
      WRITE (outvis,9260) MSG
      if (vislayer.le.9) then 
        WRITE (MSG,'(A38,1x,i1,A10)') 
     $  'GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1',vislayer,
     $    ' : ENDITEM'
      else
        WRITE (MSG,'(A38,1x,i2,A10)') 
     $  'GRID_OR_LIST_NO^7 : TIMES^8 : LAYERS^1',vislayer,
     $    ' : ENDITEM'
      endif
      WRITE (outvis,9260) MSG
      WRITE (MSG,9803)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9810)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9811)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9803)
      WRITE (outvis,9260) MSG
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


************list
      WRITE (outvis,*)
      WRITE (MSG,9814)
      WRITE (outvis,9260) MSG
      WRITE (MSG,9815)
      WRITE (outvis,9260) MSG
      WRITE (outvis,9263) linkoutnum

*******sjb works for x and y >10 
      WRITE (outvis,*)
      WRITE (MSG,9816)
      WRITE (outvis,9260) MSG
      if ((nrows.ge.100).and.(ncols.ge.100)) then
         WRITE (outvis,9271) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      elseif ((nrows.ge.100).and.(ncols.lt.100)) then
         WRITE (outvis,9272) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      elseif ((nrows.lt.100).and.(ncols.ge.100)) then
         WRITE (outvis,9273) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      elseif ((nrows.lt.10).and.(ncols.lt.10)) then
         WRITE (outvis,9275) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      elseif ((nrows.ge.10).and.(ncols.lt.10)) then
         WRITE (outvis,9277) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      elseif ((nrows.lt.10).and.(ncols.ge.10)) then
         WRITE (outvis,9276) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
       else  
         WRITE (outvis,9274) '7','1',nrows,'1',ncols,
     $      '!number and row and column limits'
      endif 



c      WRITE (outvis,9261) '7','1',nrows,'1',ncols,
c     $   '!number and row and column limits'

      WRITE (outvis,9262)'!row low, row high, column low, column high'

      do i=1,nrows
         do j=1,ncols
            if (catch(i,j).eq.0) then
               vismask(i,j)='.'
            else
               vismask(i,j)='1'
            endif
         enddo
      enddo

      DO I = 1,nrows
        WRITE (outvis,9265)(vismask(i,j),j=1,ncols)
      ENDDO  

*********times
      WRITE (outvis,*)
      WRITE (MSG2,9817)
      WRITE (outvis,9200) MSG2
      WRITE (MSG2,9818)
      WRITE (outvis,9200) MSG2
      WRITE (MSG2,9820)
      WRITE (outvis,9200) MSG2

      WRITE (outvis,*)
      WRITE (MSG2,9817)
      WRITE (outvis,9200) MSG2
      WRITE (MSG2,9821)
      WRITE (outvis,9200) MSG2
      WRITE (MSG2,9822)
      WRITE (outvis,9200) MSG2


      WRITE (outvis,*)
      WRITE (MSG2,9823)
      WRITE (outvis,9200) MSG2


***************end of final output************************************
**********************************************************************
      print*
      print*,'Normal completion'
      print*

c      pause 


      END

      subroutine rootdensity(rdf)
      
      real rdf(50,50)
      real rdf1(50),rdf2(50),rdf3(50),rdf4(50)
      real rdf5(50),rdf6(50),rdf7(50),rdf8(50)
      real rdf9(50),rdf10(50),rdf11(50),rdf12(50)
      real rdf13(50),rdf14(50),rdf15(50),rdf16(50)
      real rdf17(50),rdf18(50),rdf19(50),rdf20(50)
      real rdf21(50),rdf22(50),rdf23(50),rdf24(50)
      real rdf25(50),rdf26(50),rdf27(50)
      integer i,j

      
      rdf1(1:27)=(/1.0,0.85,0.7,0.55,0.4,0.37,0.34,0.31,0.28,0.25,0.226
     $ ,0.202,0.178,0.154,0.13,0.126,0.122,0.118,0.114,0.11,0.1064,
     $ 0.1028,0.0992,0.0968,0.0932,0.0896,0.086/)
      rdf2(1:27)=(/0.0,0.15,0.2,0.25,0.3,0.276,0.252,0.228,0.204,0.18,
     $  0.168,0.156,0.144,0.132,0.12,0.115,0.11,0.105,0.1,0.095,0.092,
     $  0.089,0.086,0.084,0.081,0.078,0.075 /)
      rdf3(1:27)=(/0.0,0.0,0.1,0.15,0.2,0.19,0.18,0.17,0.16,0.15,
     $  0.142,0.134,0.126,0.118,0.11,0.1046,0.0992,0.0938,0.0884,
     $  0.083,0.08105,0.0791,0.07715,0.07585,0.0739,0.07195,0.07 /)
      rdf4(1:27)=(/0.0,0.0,0.0,0.05,0.07,0.08,0.09,0.1,0.11,0.12,
     $  0.116,0.112,0.108,0.104,0.1,0.0944,0.0888,0.0832,0.0776,
     $  0.072,0.07095,0.0699,0.06885,0.06815,0.0671,0.06605,0.065 /)
      rdf5(1:27)=(/0.0,0.0,0.0,0.0,0.03,0.054,0.068,0.072,0.086,0.1,
     $  0.098,0.096,0.094,0.092,0.09,0.085,0.08,0.075,0.07,0.065,
     $  0.06425,0.0635,0.06275,0.06225,0.0615,0.06075,0.06 /)
      rdf6(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.03,0.04,0.06,0.07,0.08,0.08,
     $  0.08,0.08,0.08,0.08,0.076,0.072,0.068,0.064,0.06,0.05925,0.0585,
     $  0.05775,0.05725,0.0565,0.05575,0.055 /)
      rdf7(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.03,0.04,0.05,0.06,0.062,
     $  0.064,0.066,0.068,0.07,0.067,0.064,0.061,0.058,0.055,0.05425,
     $  0.0535,0.05275,0.05225,0.0515,0.05075,0.05 /)
      rdf8(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.02,0.024,0.03,0.04,
     $  0.046,0.052,0.056,0.06,0.058,0.056,0.054,0.052,0.05,0.04925,
     $  0.0485,0.04775,0.04725,0.0465,0.04575,0.045 /)
      rdf9(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.016,0.02,0.03,
     $  0.035,0.04,
     $  0.045,0.05,0.049,0.048,0.047,0.046,0.045,0.04425,0.0435,0.04275,
     $  0.04225,0.0415,0.04075,0.04 /)
      rdf10(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.01,0.02,0.025
     $ ,0.027,0.029,
     $  0.03,0.032,0.034,0.036,0.038,0.04,0.03925,0.0385,0.03775,
     $  0.03725,0.0365,0.03575,0.035 /)
      rdf11(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.018,0.03,
     $  0.04,0.048,
     $  0.06,0.062,0.064,0.066,0.068,0.07,0.0685,0.067,0.0655,0.0645,
     $  0.063,0.0615,0.06 /)
      rdf12(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.02,
     $  0.027,0.032,0.04,0.044,
     $  0.048,0.052,0.056,0.06,0.05925,0.0585,0.05775,0.05725,0.0565,
     $  0.05575,0.055 /)
      rdf13(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.018,0.026,0.03,0.034,
     $  0.038,0.042,0.046,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05 /)
      rdf14(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.016,0.02,0.024,0.028,
     $  0.032,0.036,0.04,0.04075,0.0415,0.04225,0.04275,0.0435,0.04425,
     $  0.045 /)
      rdf15(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.01,0.018,0.022,
     $  0.025,0.028,0.03,0.0315,0.033,0.0345,0.0355,0.037,0.0385,0.04 /)
      rdf16(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.011,0.014,0.018,
     $  0.021,0.025,0.0265,0.028,0.0295,0.0305,0.032,0.0335,0.035 /)
      rdf17(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.012,0.014,0.017,
     $  0.02,0.0215,0.023,0.0245,0.0255,0.027,0.0285,0.03 /)
      rdf18(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.01,0.012,0.015,0.0165,0.018,0.0195,0.0205,0.022,0.0235,0.025/)
      rdf19(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.008,0.01,0.012,0.013,0.014,0.0155,0.017,0.0185,0.02/)
      rdf20(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.005,0.007,0.009,0.011,0.012,0.014,0.015,0.016/)
      rdf21(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0056,0.007,0.009,0.0095,0.01,0.011,0.012/)
      rdf22(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0052,0.006,0.007,0.008,0.009,0.01/)
      rdf23(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0,0.0038,0.005,0.006,0.007,0.008/)
      rdf24(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0,0.0,0.0012,0.003,0.005,0.006/)
      rdf25(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0018,0.0034,0.004/)
      rdf26(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0005,0.002/)
      rdf27(1:27)=(/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,
     $  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.001/)


      
      
      
      
      
          do j=1,27
              rdf(1,j)= rdf1(j)
              rdf(2,j)= rdf2(j)
              rdf(3,j)= rdf3(j)
              rdf(4,j)= rdf4(j)
              rdf(5,j)= rdf5(j)
              rdf(6,j)= rdf6(j)
              rdf(7,j)= rdf7(j)
              rdf(8,j)= rdf8(j)
              rdf(9,j)= rdf9(j)
              rdf(10,j)= rdf10(j)
              rdf(11,j)= rdf11(j)
              rdf(12,j)= rdf12(j)
              rdf(13,j)= rdf13(j)
              rdf(14,j)= rdf14(j)
              rdf(15,j)= rdf15(j)
              rdf(16,j)= rdf16(j)
              rdf(17,j)= rdf17(j)
              rdf(18,j)= rdf18(j)
              rdf(19,j)= rdf19(j)
              rdf(20,j)= rdf20(j)
              rdf(21,j)= rdf21(j)
              rdf(22,j)= rdf22(j)
              rdf(23,j)= rdf23(j)
              rdf(24,j)= rdf24(j)
              rdf(25,j)= rdf25(j)
              rdf(26,j)= rdf26(j)
              rdf(27,j)= rdf27(j)
         enddo
          
      return
      end


c sb changed form on 021009
************************************************************************
      CHARACTER*7 FUNCTION FORM(NUMBER)
      
*     Calculates the format a real number is to be printed out in

      IMPLICIT NONE

      REAL NUMBER

      IF (NUMBER.LE.-9.949E9) THEN
         write(FORM,'(1P,E7.0E2)') NUMBER
      ELSEIF ((NUMBER.GE.-9.949E9).AND.(NUMBER.LT.-9999.49)) THEN
         write(FORM,'(1P,E7.1E1)') NUMBER
      ELSEIF ((NUMBER.GE.-9999.49).AND.(NUMBER.LT.-999.949)) THEN
         write(FORM,'(1X,F6.0)') NUMBER
      ELSEIF ((NUMBER.GE.-999.949).AND.(NUMBER.LT.-99.9949)) THEN
         write(FORM,'(1X,F6.1)') NUMBER
      ELSEIF ((NUMBER.GE.-99.9949).AND.(NUMBER.LT.-9.99949)) THEN
         write(FORM,'(1X,F6.2)') NUMBER
      ELSEIF ((NUMBER.GE.-9.99949).AND.(NUMBER.LT.-0.00949)) THEN
         write(FORM,'(1X,F6.3)') NUMBER
      ELSEIF ((NUMBER.GE.-0.00949).AND.(NUMBER.LT.-9.49E-10)) THEN
         write(FORM,'(1P,E7.1E1)') NUMBER
      ELSEIF ((NUMBER.GE.-9.49E-10).AND.(NUMBER.LT.0)) THEN
         write(FORM,'(1P,E7.0E2)') NUMBER
      ELSEIF (NUMBER.EQ.0) THEN
         write(FORM,'(4X,A3)') '0.0'
      ELSEIF ((NUMBER.GT.0).AND.(NUMBER.LT.9.949E-10)) THEN
         write(FORM,'(1P,E7.1E2)') NUMBER
      ELSEIF ((NUMBER.GE.9.949E-10).AND.(NUMBER.LT.0.000949)) THEN
         write(FORM,'(1P,E7.2E1)') NUMBER
      ELSEIF ((NUMBER.GE.0.000949).AND.(NUMBER.LT.9.999949)) THEN
         write(FORM,'(1X,F6.4)') NUMBER
      ELSEIF ((NUMBER.GE.9.999949).AND.(NUMBER.LT.99.99949)) THEN
         write(FORM,'(1X,F6.3)') NUMBER
      ELSEIF ((NUMBER.GE.99.99949).AND.(NUMBER.LT.999.9949)) THEN
         write(FORM,'(1X,F6.2)') NUMBER
      ELSEIF ((NUMBER.GE.999.9949).AND.(NUMBER.LT.9999.949)) THEN
         write(FORM,'(1X,F6.1)') NUMBER
      ELSEIF ((NUMBER.GE.9999.949).AND.(NUMBER.LT.99999.49)) THEN
         write(FORM,'(1X,F6.0)') NUMBER
      ELSEIF ((NUMBER.GE.99999.49).AND.(NUMBER.LT.9.9949E9)) THEN
         write(FORM,'(1P,E7.2E1)') NUMBER
      ELSEIF (NUMBER.GE.9.9949E9) THEN
         write(FORM,'(1P,E7.1E2)') NUMBER
        
      ENDIF
      
      END
