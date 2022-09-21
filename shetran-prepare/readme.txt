210922
******


1) NFM storage and Forest. If CATCHNAME_NFM_forest.asc or CATCHNAME_NFM_storage.asc is present then changes are made to the input files. If the files are not present the changes are not made. For CATCHNAME_NFM_forest.asc vegetations types 3-12 added to etd file (it is assumed that there are two types before a generic veg type and an urban type). for CATCHNAME_NFM_storage.asc then strickler types 3-12 added to ocd file with negtive values for extra storage. (it is assumed that there are two types before a generic veg type and an urban type).

2) catch issue with diagonal cell in catchment mask. The diagonal cell is removed

3) increase allow max discharge

4) change soil depths to make faster. Only up to 100m depth used to be up to 380m

5) Strickler lake=10, strickler river=50




to compile:
gfortran prepare.f xmlparse.f90 readshetranxml.f90 -o shetran-prepare

110920
******
moves to gfrotran compiler
a) remove pause
b) long lines
c) split file


220420
******

mb.txt to mb.csv. Do not produce smd file


260516
******
correct error in reading soil details

261015
******
put rdf in code
errors messages in read xml
change base dir add       USE DFLIB, ONLY : SPLITPATHQQ      


010815
******
split soil layers and types


220515
******

increase soil array sizes to 40000 and 50000 for kilham example

085015
******

allow more than 999 rainfall stations change this line  9209 FORMAT(200I6)
 3 formatting statements: 9208,9121,9122


210415 version 2.2.7
******

for bigger catchments change the following from 200 to 500 in prepare.f
L296,L297,L316,L317,L320,L325

correct error in lake reading change 'true' to .true.


040315
******

decrease outlet link by 1m to solve dano problem

add regular timestep and increasing timestep value

260215
******

add regular outlet timestep
rename
allow no data for lake map
add correct header to river network map

120914
******

array sizes for soils 1000 to 2000


040714
******

make strickler overland flow depend on vegetation type. Change in xml file


020514
******

did not remove sinks in mean DEM - now included was WRONG. moved to later in the code and check for adjacent rivers channels. +2.0 is to account for the banks



210114 version 2.2.4
****** 

lakes mask and snow


060114  version 2.2.3snow
******

Add tmaxfile,tminfile,snowddf to readshetranxml

change rundata file,vsd file (snow), frd file (add snow) and add snow (smd) file.




131213 version 2.2.3
******

Reduce the Strickler coefficient in lakes from 20.0 to 3.0

adeed stricklerriv and stricklerlake parameters

added linkstr variable and strlinkew and strlinkns variables


did not remove sinks in mean DEM - now included



150113

chnage writing of FR43 and FR46

should be prepare.2.2.0f-liz as changed the following


151112
******
2.2.0 try sorting out problem with diagonals in remove sink

Added new lines so that if a diagonal drop the sideways element is reduced. Seems to work OK and sort out 130712 problem

031212
******
problem at outlet with location of outlet link if d"direction" is the wrong way. Added values to the elevations so there is no accumulation of water. Need to check it works with a NS link

240113
******

visulisation file problem. changed so it works between 10 and 999 in x and y directions


080313
******

uses mean and min dem files

changes visulisation output

170313
******

time varying cstcap

190413
******
visulisation file problem. changed so it works between 1 and 999 in x and y directions