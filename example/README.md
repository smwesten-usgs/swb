# Example description and annotated control file

The example presented here runs with the executable from March 2016. The first three lines of the logfile read:
> Soil Water Balance Code version 1.2 BETA (geographic transformations enabled)
> 
> Git branch and commit hash:  master (  51b330e )
> 
> Compiled on Mar 16 2016  13:43:05

In order to make this example work on your machine, you'll need to either download some Daymet climate data mosaics, or substitute some other source for tmax, tmin, and precipitation data. Running the batch file `make_output_directories.bat` will create the output and image directory structures discussed in the [official documentation](https://pubs.er.usgs.gov/publication/tm6A31).

This example demonstrates the use of the following new(er) SWB features not discussed in the official documentation:

 * gridded precipitation and temperature data from national mosaic of Daymet data (netCDF4 format)
 * treatment of missing values in gridded climate data
 * conversion of units in climate data set from mm/degrees C to inches/degrees F
 * use of wildcards in specification of input grids
 * mixing input grids of various projections and extents, reprojecting on-the-fly 

The remainder of this document will provide annotation of the control file statements included in the file `example_swb_control_file.ctl`.

The first section of the control file defines the number of gridcells, the coordinates of the lower left-hand corner, the gridcell resolution, and an optional *projection definition* needed for making use of datasets of varying geographic projections. *If all of your input grids match the initial SWB grid definition as defined in this first section, then `BASE_PROJECTION_DEFINITION` is not needed.*

In this example, the SWB grid resolution may be changed by simply commenting out one SWB `GRID` definition and uncommenting an alternative `GRID` definition. Projection information may be found [here](http://spatialreference.org); PROJ4 strings are listed for many common projection systems. In our example we're using UTM16, with units of **feet**. *Note that changing the base project grid resolution is only possible if a* `BASE_PROJECTION_DEFINITION` *control file statement is present.*

    #      ncol  nrow XLL     YLL              resolution
    #GRID  520   695  1092610 16670600         100.0
    #GRID  208   278  1092610 16670600         250.0
    GRID  104   139  1092610 16670600         500.0
    BASE_PROJECTION_DEFINITION +proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=us-ft +no_defs
    GRID_LENGTH_UNITS FEET

Next we have a block of control file directives that control whether certain types of output is generated during a run. `SUPPRESS_SCREEN_OUTPUT` prevents SWB from printing a detailed summary of all water budget components to the screen for each day of simulation. `SUPPRESS_INTEGRATED_OUTPUT` is obsolete and doesn't do much of anything. `SUPPRESS_DAILY_FILES` prevents the *.csv files containing water balance summary information from being written. `SUPPRESS_DISLIN_MESSAGES` attempts to silence the DISLIN graphics library in the event minor errors are encountered while creating summary plots. Last, the `RLE_MULTIPLIER` should be left alone; it controls how many significant figures are retained during data compression. 

    SUPPRESS_SCREEN_OUTPUT
    #SUPPRESS_INTEGRATED_OUTPUT
    #SUPPRESS_DAILY_FILES
    SUPPRESS_DISLIN_MESSAGES
    RLE_MULTIPLIER 10000

Gridded precipitation and air temperature data may be supplied in the form of Arc/Info or Surfer ASCII grids, or as netCDF files. These gridded files are often supplied as a series of files, one for each year, month, or even day covering the simulation period. SWB can be given a filename template to specify the naming convention of these input grids. Wildcards similar to those used in Linux date/time functions are used to indicate substitutions to be made when forming a filename for SWB to use. Inserting one or more zeroes between the wildcard identifier (%) and the wildcard name will enforce the use of leading zeroes in filenames. The table below summarizes these wildcards:

Wildcard       | Meaning | Example Filenames | SWB Template
---------------|---------|------------------|--------------
%y or %Y       | year    | prcp\_1926.asc, prcp\_1927.asc, prcp\_1928.asc | prcp\_%y.asc
%m             | month   | prcp\_1\_1926.asc, prcp\_2\_1926.asc, prcp\_3\_1926.asc | prcp\_%m\_%y.asc
%d             | day     | prcp\_1\_1\_1926.asc, prcp\_1\_2\_1926.asc, prcp\_1\_3\_1926.asc | prcp\_%m\_%d\_%y.asc
0              | leading zero | prcp\_01\_01\_1926.asc, prcp\_01\_02\_1926.asc, prcp\_01\_03\_1926.asc | prcp\_%0m\_%0d\_%y.asc
%B             | month abbreviation, uppercase | prcp\_JAN\_1926.asc, prcp\_FEB\_1926.asc, prcp\_MAR\_1926.asc | prcp\_%B\_%y.asc
%b             | month abbreviation, lowercase | prcp\_jan\_1926.asc, prcp\_feb\_1926.asc, prcp\_mar\_1926.asc | prcp\_%b\_%y.asc
%#             | sequence number, 1-365, reset to 1 at the start of each calendar year | prcp\_1\_1926.asc, prcp\_2\_1926.asc, prcp\_3\_1926.asc | prcp\_%#\_%y.asc


    PRECIPITATION NETCDF  F:\SMWData2\COMMON_CLIMATE\Daymet_2015\prcp_%Y.nc4
    PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
    PRECIPITATION_CONVERSION_FACTOR 0.03936996
    PRECIPITATION_MISSING_VALUES_CODE -32768
    PRECIPITATION_MISSING_VALUES_OPERATOR <=
    PRECIPITATION_MISSING_VALUES_ACTION MEAN

Many other control file statements are available to inform SWB of conversion factors that need to be applied or to define actions to be taken in the event of missing data values within the gridded dataset. These control file statements are listed in the section below.

    TEMPERATURE NETCDF F:\SMWData2\COMMON_CLIMATE\Daymet_2015\tmax_%Y.nc4  F:\SMWData2\COMMON_CLIMATE\Daymet_2015\tmin_%Y.nc4
    TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
    TMAX_MISSING_VALUES_CODE -128
    TMAX_MISSING_VALUES_OPERATOR <=
    TMAX_MISSING_VALUES_ACTION MEAN
    TMAX_SCALE_FACTOR 1.8
    TMAX_ADD_OFFSET 32

    TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
    TMIN_MISSING_VALUES_CODE -128
    TMIN_MISSING_VALUES_OPERATOR <=
    TMIN_MISSING_VALUES_ACTION MEAN
    TMIN_SCALE_FACTOR 1.8
    TMIN_ADD_OFFSET 32
    OUTPUT_GRID_SUFFIX asc

    GROWING_SEASON 133 268 TRUE
    INITIAL_ABSTRACTION_METHOD HAWKINS
    INITIAL_FROZEN_GROUND_INDEX CONSTANT 100.0
    UPPER_LIMIT_CFGI 83.
    LOWER_LIMIT_CFGI 55.

An experimental method for determining the length of the growing season is available by specifying `GROWING SEASON GDD`. In this method, the growing season is assumed to begin once the number of growing degree-days exceeds some threshold. The growing season is considered to end when the minimum daily air temperature falls below a killing frost threshold. Both of these thresholds may be changed. The growing season GDD threshold is set by means of the `GROWING_SEASON_STARTING_GDD` directive; the killing frost directive is set by means of the `GROWING_SEASON_KILLING_FROST` directive. If you wish to try this option out, it is strongly recommended that you also plot the daily growing season status ( `OUTPUT_OPTIONS GROWING_SEASON` )over time to make sure that the code is doing what you think it should.

    FLOW_DIRECTION ARC_GRID input\NED_10m_D8_Flow_Dir.asc
    FLOW_DIRECTION_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m
 
    SOIL_GROUP ARC_GRID input\gSSURGO_HSG_cd_subset_4_soils.asc
    SOIL_GROUP_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m
 
    LAND_USE ARC_GRID input\NLCD_2011_subset.asc
    LANDUSE_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m 
 
    OPEN_WATER_LAND_USE 11
 
    LAND_USE_LOOKUP_TABLE std_input\LU_lookup_example.txt
 
    WATER_CAPACITY ARC_GRID input\gSSURGO_AWC_cd_subset.asc
    WATER_CAPACITY_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m
 
    SM T-M EQUATIONS
    INITIAL_SOIL_MOISTURE CONSTANT 100
    INITIAL_SNOW_COVER CONSTANT 2
    RUNOFF C-N DOWNHILL
    ET HARGREAVES 43.5 49.38
 
The Hargraves-Samani method for estimating potential ET requires minimum and maximum latitudes to be supplied for the project area; if a `BASE_PROJECTION_DEFINITION` is supplied, the grid extents and projection information will be used to calculate these values and override the user-supplied values.  

    DISLIN_PARAMETERS RECHARGE
    SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
    SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
    SET_Z_AXIS_RANGE ANNUAL 0 40. 5.0
    Z_AXIS_TITLE POTENTIAL RECHARGE, IN INCHES
  
    DISLIN_PARAMETERS GROWING_SEASON
    SET_Z_AXIS_RANGE DAILY 0 1 0.5
    SET_Z_AXIS_RANGE MONTHLY 0 1. 0.5
    SET_Z_AXIS_RANGE ANNUAL 0 1. 0.5
    Z_AXIS_TITLE GROWING SEASON, ON/OFF

    #                                  <daily> <monthly> <annual>  
    OUTPUT_OPTIONS RECHARGE             NONE     NONE      BOTH
    OUTPUT_OPTIONS GROSS_PRECIP         NONE     NONE      PLOT
    OUTPUT_OPTIONS AVG_TEMP             NONE     NONE      NONE
    OUTPUT_OPTIONS MAX_TEMP             NONE     NONE      NONE
    OUTPUT_OPTIONS MIN_TEMP             NONE     NONE      NONE
    OUTPUT_OPTIONS SM_APWL              NONE     NONE      NONE
    OUTPUT_OPTIONS SNOWCOVER            NONE     NONE      NONE
    OUTPUT_OPTIONS INTERCEPTION         NONE     NONE      BOTH
    OUTPUT_OPTIONS RUNOFF_OUTSIDE       NONE     NONE      BOTH
    OUTPUT_OPTIONS ACT_ET               NONE     NONE      BOTH
    OUTPUT_OPTIONS REFERENCE_ET         NONE     NONE      NONE
    OUTPUT_OPTIONS GROWING_SEASON       NONE     NONE      NONE

    OUTPUT_FORMAT ARC_GRID
    SOLVE_NO_TS_FILE 1980 1981
    EOJ

## Description of some control file directives 

This section provides a partial list of the control file statements understood by SWB, version 1.x, particularly those statements not included in the original report. 

### Project setup and grid specification

In a departure from the published Techniques and Methods report ([https://pubs.er.usgs.gov/publication/tm6A31](https://pubs.er.usgs.gov/publication/tm6A31)), the project grid may now be defined in either of the ways shown below. Basically the upper right-hand coordinates may be left out of the definition control statement; SWB will calculate these corner values given the other coordinate and gridcell information provided.


`GRID *NX* *NY* *X0* *Y0* *Cell_size*`

-or-

`GRID *NX* *NY* *X0* *Y0* *X1* *Y1* *Cell_size*`

### Method for estimating retained soil moisture

The original code consulted digitized copies of Thorthwaite and Mather's original soil-moisture-retention tables in the course of performing the water balance for each cell. The use of these tables can lead to the SWB program complaining of mass balance errors, particularly when soil moisture levels drop to extremely low levels. This is most likely due to "irreversibility" in the calculation process; the program consults the tables twice: once to update the accumulated potential water loss, and again to back-calculate a current soil moisture value.

As an alternative, SWB now summarizes the tables in the form of two equations, which allows the soil-moisture to be updated without the complaints of mass balance errors. To use the equations rather than the tables, change the directive `SM T-M tablename` to `SM T-M EQUATIONS`.

### Specification of geographic projection

The current version of SWB has been linked to a code called PROJ4, which was originally developed by a researcher at USGS, and is now developed and maintained by volunteers as an open source software project. PROJ4 allows for coordinates to be converted between projections, and is used by SWB to convert arbitrary coordinates to the projection of the SWB project grid.

It is important to note that SWB now has two modes of operation regarding gridfiles: 

1. Original behavior: no "PROJECTION" control file statements included in the control file. All grids are assumed to be in the same geographic projection, possess identical extents, and have the same gridcell sizes.
2. Internal reprojection: a `BASE_PROJECTION_DEFINITION` is given. Each gridfile that also has a PROJECTION definition statement will have its coordinates reprojected to the BASE projection; gridcell values are taken from the cells nearest the SWB project gridcell.

If internal reprojection of datasets is desired, the gridfiles supplied to SWB will ideally extend several gridcells in all directions beyond the SWB project boundaries. The table below gives the PROJ4 strings for several common projections. More projection information may be found at [http://spatialreference.org](http://spatialreference.org), although it is probably preferable to use only PROJ4 information for projections published by ESRI or EPSG.  

Projection name                                                | EPSG Code |  PROJ.4 string
---------------------------------------------------------------|:---------:|-----------------------------------------
USA Contiguous Albers Equal Area Conic USGS version, meters  |  7301     | +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs 
United States National Atlas Equal Area, meters                 |  2163     | +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs
Lambert Conformal Conic (as defined for DayMet)  | ???  | +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
Wisconsin Transverse Mercator / NAD83 | 3070 | +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m


### Gridded datasets

For each of the three major climate datasets (precipitation, minimum and maximum air temperature), a standard set of suffixes may be added to the dataset name to control how SWB treats the dataset. The list of suffixes understood by SWB is given in the table below:

| Suffix                             | Argument         | Description                             | Default |
|------------------------------------|------------------|-----------------------------------------|-----|
| _SCALE_FACTOR                      |  *real value*    | amount to multiply raw grid value by prior to use | 1.0 |
| _ADD_OFFSET                        |  *real value*    | amount to add to the raw grid value following application of the scale factor, if any | 0.0 |
| _NETCDF_X_VAR                      |  *string*        | name of the variable to be used as the "x" axis | x |
| _NETCDF_Y_VAR                      |  *string*        | name of the variable to be used as the "y" axis | y |
| _NETCDF_Z_VAR                      |  *string*        | name of the variable to be used as the "z" (value) axis | prcp |
| _NETCDF_TIME_VAR                   |  *string*        | name of the variable to be used as the "time" axis | time |
| _NETCDF_VARIABLE_ORDER             |  "xyt or txy"    | description of the order in which the gridded data were written | tyx |
| _NETCDF_FLIP_VERTICAL               |  **none**        | if present, all gridded data will be "flipped" around the horizontal axis (data at the top of the grid become data at the bottom of the grid and vice versa). | NA |
| _NETCDF_FLIP_HORIZONTAL            |  **none**        | if present, all gridded data will be "flipped" around the vertical axis (data at the left of the grid become data at the right of the grid and vice versa) |  |
| _NETCDF_MAKE_LOCAL_ARCHIVE         |  |  |
| _PROJECTION_DEFINITION             |  | PROJ.4 string describing the geographic projection of the dataset |  |
| _MINIMUM_ALLOWED_VALUE             | *real value* | ceiling to be applied to the data; data above this value will be reset to this amount | |
| _MAXIMUM_ALLOWED_VALUE             | *real value* | floor to be applied to the data; data beneath this value will be reset to this amount   
| _MISSING_VALUES_CODE               | *real or integer value* | value |
| _MISSING_VALUES_OPERATOR           | "<", "<=", ">", ">=" | trigger missing values action if the data value meets this condition |
| _MISSING_VALUES_ACTION             | "mean" or "zero" | "mean" will substitute the mean value calculated over the remaining valid cells; "zero" will substitute a value of 0.0 in place of missing values

