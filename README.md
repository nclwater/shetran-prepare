# SHETRAN-Prepare
Executables to convert parameter and map data to Shetran input files format

## Features
- This uses the xml library file which contains the parameter values and calls to the map data
- There is map data for the Mean DEM, Minimum DEM, soil, land, precipitation stations, PET stations
- There is precipitation and PET time series data
- After running the executable the full set of Shetran input files are produced together with a controlling rundata file

## Installation
 
- Download any of the executables and the corresponding example data

## Usage

- Can be run on the command line or using the corresponding start executable (which also runs Shetran)
- the command line format is the executable name followed by the xml file name
- e.g if both execuutable and xml library file are in the same folder  type "shetran-prepare.exe Foston_Beck_at_Foston_MillLibraryFile.xml"

## Updates 23022026

- compiled with Intel ifort 2021 compiler on VS 2019

- It is backwards compatiable. All old xml files should still work (apart from the issue with rain and potential evaporation map files (see below).

- It now correctly reads the xml file. so it searches for the start and end tag and takes the data between the tags. This means that the order of the lines is not important and optional lines can be omitted.
- The compulsory lines that are needed are the following:
```
<CatchmentName>Foston_Beck_at_Foston_Mill_simple</CatchmentName>
<MaskFileName>maskAsciiFoston_Beck_at_Foston_Mill.txt</MaskFileName>
<DEMMeanFileName>demAsciiFoston_Beck_at_Foston_Mill.txt</DEMMeanFileName> 
<StartYear>1980</StartYear>
<StartMonth>1</StartMonth>
<StartDay>1</StartDay>
<EndYear>1981</EndYear>
<EndMonth>10</EndMonth>
<EndDay>10</EndDay>
```
- Previously for rain and potential evaporation map files the station number could be used, these were ordered and assigned values in numerical order. This has been changed. Map files for soils, land-use, precipitation and potential evaporation are directly put into Shetran with the values in the map files (numbering starts from 1 not 0). 
The veg Type numbers within the vegetationDetail tag in the XML file should correspond with those in the vegetation map.
The soil category numbers within the SoilDetail tag in the XML file should correspond with those in the SoilMap.
The column numbers into the PrecipitationTimeSeriesData should correspond with those in the PrecipMap. The first column is for locations with a 1 in the PrecipMap. The second column is for locations with a 2 in the PrecipMap etc.
The column numbers into the EvaporationTimeSeriesData should correspond with those in the PeMap. The first column is for locations with a 1 in the PeMap. The second column is for locations with a 2 in the PeMap etc.
- All arrays are dynamically allocated and so there are not limits on catchment size
- If the extra files with addtional storage and forest are used (34008-NFM-storage-and-woodland example) these can now work with any number of exisiting vegetation types. 10 addtional vegetationtypes are added. It is assumed that each grid square has a seperate preciptation and PET category.
- There is an option to read preciptation and potential evaporation time series data with the first column as dates (there must be a single header data row at the start of these files) e.g
```
1980-01-01T00:00:00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
```

The date must be iso 8601 format
To use this option the following line must be present in the XML file
```
<MeteorologicalDataIncludeDate>True</MeteorologicalDataIncludeDate> <!--Does the precipitation and potential evaporation data includes a first column ocntaining dates?-->
```

- The following addtional files are produced: input_CATCHNAME_element_number.asc, input_CATCHNAME_river_location.txt and input_CATCHNAME_river_network.asc



