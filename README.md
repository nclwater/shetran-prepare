# shetran-prepare
Executables to convert parameter and map data to Shetran input files format

## Features
- Three different exeuctables. A standard one, one including snow and one including sediment
- This uses the xml library file which contains the parameter values and calls to the map data
- There is map data for the Mean DEM, Minimum DEM, soil, land, precipitation stations, PET stations
- There is precipitation and PET time series data
- After running the executable the full set of Shetran input files are produced together with a controlling rundata file

## Installation
 
- Download any of the executables and the corresponding example data

## Usage

- Can be run on the command line or using the corresponding start executable (which also runs Shetran)
- the command line format is the executable name followed by the xml file name
- e.g if both execuutable and xml library file are in the same folder  type "shetran-prepare-2.2.10.exe Foston_Beck_at_Foston_MillLibraryFile.xml"


