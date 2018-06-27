# SWB Model annotated control file
#
# This test case covers a grid centered on the Black Earth Creek
# watershed in south-central Wisconsin. Atmospheric data for this
# case is derived from the climate station at the Dane County
# Regional Airport.
#
#--------------------------------------------------------------------------
# MODEL DOMAIN DEFINITION
#
# Definition of the model domain.  Units of meters are assumed.
# All subsequent input grids must match the specified model domain exactly.
#
#              Lower LH Corner     Upper RH Corner                   Grid
#            |_________________________| |_________________________| Cell
#     NX  NY  X0            Y0            X1            Y1           Size
GRID 340 316  528502.5299   274871.6534   562502.5299   306471.6534  100.0
#*******************************************************************
# LENGTH UNITS
#
# Must specify whether grid coordinate are given in METERS or FEET.
# This affects conversion of values from inches to acre-ft.

GRID_LENGTH_UNITS METERS
#*******************************************************************
# OUTPUT CONTROL

# If running SWB in batch mode, it may be desirable to turn off the
# daily mass balance summary that is normally printed to the screen.

# Screen output and DISLIN mesages may be supressed by uncommenting
# the directives below.

# TURN OFF SCREEN OUTPUT?
SUPPRESS_SCREEN_OUTPUT

# TURN OFF INTEGRATED OUTPUT (i.e. use external SWBREAD program after run completion)
#SUPPRESS_INTEGRATED_OUTPUT

# TURN OFF SUPPLEMENTAL MASS BALANCE / DAILY REPORT FILES?
#SUPPRESS_DAILY_FILES

# TURN OFF DISLIN MESSAGES
#SUPPRESS_DISLIN_MESSAGES
#*******************************************************************
# GROWING SEASON
#
# Define 1) beginning and 2) ending Julian day of growing season;
#        and 3) flag indicating whether or not the problem is in the
#        Northern hemisphere (possible values: TRUE / FALSE)
#
#        The growing season defines only the timespan within which
#        interception terms will be calculated.
#
GROWING_SEASON 133 268 TRUE
#*******************************************************************
# If you have access to a terminal program such as "rxvt",
# the SWB model can provide screen output with color coding for positive
# and negative values. (possible values: TRUE / FALSE)
#
# rxvt is a package that may be installed as an option along with
# the Cygwin Un*x emulation package (www.cygwin.com).
#
ANSI_COLORS FALSE
#*******************************************************************
# In order to conserve disk space, real values are converted to
# integer values, and the resulting data stream is compressed using
# a simple run-length encoding (RLE) scheme. A larger value for the
# RLE multiplier preserves more of the real data value and lowers the
# amount of data compression that takes place. However, a value for
# the RLE_MULTIPLIER that is too large may result in an INTEGER OVERFLOW
# error, in which the converted data value exceeds the storage capacity
# for the integer data structure. In this case, the RLE_MULTIPLIER
# must be lowered in order that the maximum integer value to be stored
# stays within the limitations of the data type. For a 4-byte integer,
# the converted real value must stay inside the range from
# -2,147,483,647 and +2,147,483,647. A real value of 22,000, with a
# RLE_MULTIPLIER value of 100,000 will result in an INTEGER OVERFLOW
# condition; the resulting integer value of 2,200,000,000
# exceeds the maximum that can be represented by a 4-byte integer.
#
RLE_MULTIPLIER 10000
#*******************************************************************
# PRECIPITATION
#
# Choose option for precipitation input. Options are:
#  1) PRECIPITATION SINGLE_STATION
#  2) PRECIPITATION ARC_GRID file_prefix
#  3) PRECIPITATION SURFER file_prefix
#
# If precipitation is input using grids, the model assumes that a
# separate grid file exists for each day of simulation.  The naming
# convention for the precipitation files is:
#
#  prefix_yyyy_mm_dd.suffix
#
# For example, a series of grids for the first few days of 1990 would be
# names "precip_1990_01_01.asc", "precip_1990_01_02.asc", etc.
#
PRECIPITATION SINGLE_STATION
#PRECIPITATION ARC_GRID precip\precip
#*******************************************************************
# TEMPERATURE
#
# Choose option for temperature input. Options are:
#  1) TEMPERATURE SINGLE_STATION
#  2) TEMPERATURE ARC_GRID TMAXprefix TMINprefix
#  3) TEMPERATURE SURFER TMAXprefix TMINprefix
#
# If temperature is input using grids, the model assumes that a
# separate grid file exists for each day of simulation.  The naming
# convention for the temperature files is:
#
#  TMAXprefix_yyyy_mm_dd.suffix TMINprefix_yyyy_mm_dd.suffix
#
# For example, a series of grids for the first few days of 1990 would be
# names "tmin_1990_01_01.asc", "tmin_1990_01_02.asc", etc.
#
TEMPERATURE SINGLE_STATION
#TEMPERATURE ARC_GRID precip\tmax precip\tmin
#*******************************************************************
# OUTPUT GRID FILENAME SUFFIX
#
# Set the output grid filename suffix with the OUTPUT_GRID_SUFFIX
# option.  This applies only to annual and monthly output grids.
# Daily grids have the filename pattern filename.###, where ###
# is the Julian day of the simulation
#
OUTPUT_GRID_SUFFIX asc
#*******************************************************************
# INITIAL ABSTRACTION METHOD
#
# The method for calculating the initial abstraction within the
# SCS curve number runoff procedure may be specified in two ways:
#
# 1) TR-55: Ia is assumed equal to 0.2 * S
# 2) Hawkins (2002): Ia is assumed equal to 0.05 * S
#
# If the Hawkins method is used, curve numbers are adjusted
# as per Equation 9 of Hawkins (2002).  Net effect should be to
# increase runoff for smaller precip events. This method has been
# suggested to be more appropriate to long-term simulation model applications.
#
#INITIAL_ABSTRACTION_METHOD TR55
INITIAL_ABSTRACTION_METHOD HAWKINS
#*******************************************************************
# INITIAL CONTINUOUS FROZEN GROUND INDEX
#
# assume that ground is initially frozen, "frozen" >= 83
#
INITIAL_FROZEN_GROUND_INDEX CONSTANT 100.0
#*******************************************************************
# FROZEN GROUND THRESHOLD CFGI VALUE
#
# Use this option to set a different value defining the boundary
# between "unfrozen" and "frozen" ground.  Literature value is 83.
# For example, for a CFGI < 83, the ground is considered unfrozen;
# with a CFGI >= 83, the ground is considered frozen.
#
# When frozen ground conditions exist, the curve numbers are uniformly
# assumed to reflect antecedant runoff condition III (i.e. increased
# proportion of runoff for a given amount of precipitation).
#
# The default value, if no other value is specified, is 9999.
#
# NOTE! BY DEFAULT the FROZEN GROUND INDEX is OFF (9999)
#
UPPER_LIMIT_CFGI 83.
LOWER_LIMIT_CFGI 55.
#*******************************************************************
# NOTE on GRID SPECIFICATION:
# The format for the following input grid specifications is:
# DIRECTIVE OPTION FILENAME
# where the "directive" is a key word that identifies the intended use for
# the model input grid, "option" is either ARC_GRID or SURFER, depending
# on the format of the input grid file, and "filename" is the local
# directory name plus file name of the input grid
# (e.g. input\soil_group.asc)
# The local directory specification may be omitted if the program executable
# and the input grid files share the same directory name
#*******************************************************************
# FLOW DIRECTION
#
# The user must use the ARCINFO "flowdirection" command to generate this
# grid from a DEM. The number within each cell indicates the direction to
# which surface runoff is routed from that cell.
#  1 in a cell indicates that runoff from this cell will be routed to the
#      cell to the right
#  4 in a cell indicates that runoff from this cell will be routed to
#       the cell below
#  16 in a cell indicates that runoff from this cell will be routed to
#      the cell to the left, etc ...
#
#                           32      64      128
#                           16     center   1
#                           8       4       2
#
#  A number other than the eight listed above designates a closed depression.
#
FLOW_DIRECTION ARC_GRID input\flow_direction.asc
#*******************************************************************
# SOIL GROUP
#
# SCS Curve Number Hydrologic Soil Groups: The Soil Conservation Service (SCS)
# has categorized every soil within the United States into one of four
# hydrologic soil groups based on its infiltration capacity (A - D)
# (input to the model as 1 - 4).
# "A" soils have a high minimum infiltration capacity and subsequently a low
# overland flow potential while "D" soils have a very low infiltration
# capacity and subsequently a high overland flow potential. The user must
# use the SCS soil surveys to look up the soil group for all soils within
# the model area, and use ARCINFO or ArcView to assign and generate an
# input grid.  If the user does not have access to the SCS soil survey or
# a soil has not been previously assigned to a group, infiltration data can
# be used to assign the soil to a hydrologic soil group:
#
#                               Soil Group A: > 0.76 cm/h
#                               Soil Group B: 0.38 - 0.76 cm/h
#                               Soil Group C: 0.13 - 0.38 cm/h
#                               Soil Group D: < 0.13 cm/h
#
SOIL_GROUP ARC_GRID input\soils_hyd_grp.asc
#*******************************************************************
# LAND USE/COVER CLASSIFICATION
#
# The model uses land use information, together with
# the soil available water capacity information, to calculate surface
# runoff and assign a maximum soil moisture holding capacity for each
# grid cell. THIS VERSION OF THE MODEL CAN HANDLE ANY ARBITRARY LAND USE
# CLASSIFICATION METHOD, AS LONG AS THE ACCOMPANYING LAND USE LOOKUP TABLE
# CONTAINS CURVE NUMBER, INTERCEPTION, AND ROOTING DEPTH DATA FOR EACH
# LAND USE TYPE CONTAINED IN THE GRID.
#
LAND_USE ARC_GRID input\land_cover.asc
#*******************************************************************
# SPECIFY OPEN WATER LAND USE
#
# This option forces the cells of the given land use to be treated
# as open water cells.  In these cells, recharge is *NOT* calculated,
# nor is flow routing or soil-moisture accounting performed.  Water is
# either allowed to leave these cells as actual ET, or assumed to leave
# the grid flow out of grid via surface water features.
#
#OPEN_WATER_LAND_USE 11
#*******************************************************************
#
# Land Use LOOKUP table:
#
# The first line of this file must begin with:
# NUM_LANDUSE_TYPES ##
# where ## is the number of land use types contained in the table.
#
# The remainder of the file is a tab-delimited text file having one line
# for each land use specified within the land use grid.
# Data items must be specified as follows for each line (separated by a tab):
# Column Number                Description
# -------------                -----------
# 1 Land use code              Integer value corresponding to the integer values
#                              contained in the land use ARC ASCII grid.
#
# 2 Land use description       Not used by model; for use by user to document
#                              the description of the land use corresponding to
#                              the integer land use code.
#
# 3 Assumed impervious area    Not used by model; for use by user to document
#                              assumed impervious area associated
#                              with the land use code.
#
# 4-7* SCS base curve numbers  SCS base curve numbers for hydrologic soil
#                              groups A-D, respectively. The curve numbers
#                              are those associated with antecedent runoff
#                              condition II. A curve number must
#                              be specified for each soil type.
#
# 8-11* Maximum infiltration rates   Maximum infiltration rates (inches/day)
#                                    for each soil type.
#
# 12,13 Interception storage values  Interception storage values for growing
#                                    season and non-growing season.
#
# 14-17* Depth of root zone    Root zone depth, in FEET, for each soil group A-D.
#
# 18,19  Reference             Not used by model; for use by users in
#                              documenting the sources of information
#                              placed into the table
#
# * Column numbering will obviously change if more than 4 soil types are used.
#
LAND_USE_LOOKUP_TABLE std_input\LU_lookup_WISCLAND_w_forested_hillslope.txt
#*******************************************************************
# BASE SOIL WATER CAPACITY
#
# The model uses soil information, together with land cover information,
# to calculate surface runoff and assign a maximum soil moisture holding
# capacity to each grid cell. Soil classifications, which include the
# requisite available water capacity or textural information, are typically
# available through the state soil conservation service.
#
# Each soil type or soil series within the model area must be assigned a
# soil available water capacity. If available water capacity data are not
# available, the user can use soil texture to assign a value (see table
# below from Thornthwaite). ARCINFO or ArcView is used to
# code and generate the ascii input file.
#
#           SOIL TEXTURE                 AVAILABLE WATER CAPACITY (in / ft)
#               sand                                1.20
#               loamy sand                          1.40
#               sandy loam                          1.60
#               fine sandy loam                     1.80
#               very fine sandy loam                2.00
#               loam                                2.20
#               silt loam                           2.40
#               silt                                2.55
#               sandy clay loam                     2.70
#               silty clay loam                     2.85
#               clay loam                           3.00
#               sandy clay                          3.20
#               silty clay                          3.40
#               clay                                3.60
#
WATER_CAPACITY ARC_GRID input\soils_awc.asc
#*******************************************************************
# ADJUSTED WATER CAPACITY
#
# The model will calculate the total available water capacity from
# the base soil water capacity grid and the land use grid, using the
# rooting depth functions as specified in the land use lookup table.
#
# Alternatively, the adjusted water capacity may be calculated external
# to the model and read in as an ASCII grid.  If this is done, internal
# calculation of the rooting depth and resulting adjusted water capacity is
# disabled in the model.
#
#ADJUSTED_WATER_CAPACITY ARC_GRID input\MAX_SM_STORAGE.grd
#*******************************************************************
# SOIL MOISTURE ACCOUNTING METHOD
#
# The model currently only contains one soil-moisture accounting
# calculation option: Thornthwaite-Mather (1948, 1957).
#
# The Thornthwaite-Mather soil moisture retention tables are included
# in the standard table "soil-moisture-retention-extended.grd"
#
# If the DRIPPS_COMPATIBLE option is TRUE, then the Thornthwaite-Mather
# tables are ignored, and the polynomials developed by Wes Dripps
# (based on the same tables) are used instead.
#
SM T-M std_input\soil-moisture-retention-extended.grd
#*******************************************************************
# INITIAL SOIL MOISTURE
#
# If CONSTANT, initial soil moisture is specified as a PERCENTAGE saturation
# of the available water capacity.
#
# If an ASCII GRID FILE, initial soil moisture is specified in INCHES of water.
#
INITIAL_SOIL_MOISTURE CONSTANT 100
#INITIAL_SOIL_MOISTURE ARC_GRID input\LakeMI_future_ANNUAL_initial_pct_sm.asc
#*******************************************************************
# INITIAL SNOW COVER
#
# Initial snow cover is specified as an equivalent moisture value.
# This may be specified as a single constant value
# or as an ASCII grid file.
#
INITIAL_SNOW_COVER CONSTANT 0
#INITIAL_SNOW_COVER ARC_GRID input\LakeMI_future_ANNUAL_initial_snow_cover.asc
#*******************************************************************
# SOLUTION METHOD
#
# Two solution methods are available for the routing of surface water
# through the model domain. The "ITERATIVE" method closely resembles
# Wes Dripps' original solution method, wherein water is iteratively moved
# across the entire grid until all water has either infiltrated or
# left the grid via surface flow.
#
# The "DOWNHILL" method was developed by Vic Kelson, and involves
# a pre-simulation step whereby the model grid cells are sorted in an
# upstream to downstream fashion.  Thereafter, runoff is calculated once
# for the entire model domain, proceeding from the upstream cells to the
# downstream cells. The DOWNHILL option is preferred.
#
#RUNOFF C-N ITERATIVE
RUNOFF C-N DOWNHILL
#RUNOFF C-N NO_ROUTING
#*******************************************************************
# ITERATIVE METHOD TOLERANCE
#
# The iterative method sometimes fails to converge for exceedingly
# small solution tolerances (i.e. < 1E-6 change in calculated
# runoff in a cell from one iteration to the next).  This option is
# offered as a way to force convergence.  The default value is 1E-12.
#
#ITERATIVE_METHOD_TOLERANCE 1.0E-4
#*******************************************************************
# EVAPOTRANSPIRATION METHOD
#
# The model implements several different methods for estimating ET,
# including:
# 1) Thornthwaite-Mather (program option: "T-M" latitude  )
# 2) Jensen-Haise (program option: "J-H" latitude albedo a_s b_s )
# 3) Blaney-Criddle (program option: "B-C" latitude )
# 4) Turc (program option: "TURC" latitude albedo a_s b_s )
# 5) Hargreaves (program option: "HARGREAVES" southerly lat northerly lat)

#ET HARGREAVES 42.89 43.24
#ET TURC 43 0.23 0.25 0.75
ET J-H 43 0.23 0.25 0.5
#ET T-M 43
#*******************************************************************
# PLOTTING CUSTOMIZATION
#
# This version of the SWB model allows very limited assignment of
# DISLIN plotting parameters for the generation of images.
# See http://www.mps.mpg.de/dislin/ for more information about this
# package.
#
# If no customizations are specified, default values will be used.
#
DISLIN_PARAMETERS RECHARGE
SET_Z_AXIS_RANGE DAILY 0 1.5 0.1
SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
SET_Z_AXIS_RANGE ANNUAL 0 20 2.
Z_AXIS_TITLE RECHARGE, IN INCHES
#
DISLIN_PARAMETERS ACT_ET
SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 40. 5.0
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE ACTUAL ET, IN INCHES
#
DISLIN_PARAMETERS POT_ET
SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 45. 5.
#SET_DEVICE WMF
#SET_FONT Courier New Italic
Z_AXIS_TITLE POTENTIAL ET, IN INCHES
#
#DISLIN_PARAMETERS RUNOFF_OUTSIDE
#SET_Z_AXIS_RANGE DAILY 0 5. 0.5
#SET_Z_AXIS_RANGE MONTHLY 0 12. 0.5
#SET_Z_AXIS_RANGE ANNUAL 0 25. 5.
#Z_AXIS_TITLE RUNOFF OUT OF GRID, IN INCHES
#
#DISLIN_PARAMETERS SNOWCOVER
#SET_Z_AXIS_RANGE DAILY 0 12. 0.5
#Z_AXIS_TITLE SNOW COVER, IN INCHES (WATER EQUIVALENT)
#
DISLIN_PARAMETERS SM_APWL
SET_Z_AXIS_RANGE DAILY -20. 0. 2.0
Z_AXIS_TITLE ACCUMULATED POTENTIAL WATER LOSS, IN INCHES
SET_COLOR_TABLE RRAIN
#*******************************************************************
# OUTPUT OPTIONS

# The SWB code can generate image and ARCGIS/Surfer output at the
# daily, monthly, or annual timescale. This section allows the user to
# specify exactly what output should be generated for each of 24
# internal variables at each of the three timescales.

# Format for specifying output options is:
# "OUTPUT_OPTIONS variable_name daily_option monthly_option annual_option",
# where the possible values for each option are:
# NONE, GRAPH (or PLOT), GRID, or BOTH

OUTPUT_OPTIONS RECHARGE NONE NONE PLOT
OUTPUT_OPTIONS SM_APWL NONE NONE NONE
OUTPUT_OPTIONS SNOWCOVER NONE NONE NONE
OUTPUT_OPTIONS INTERCEPTION NONE NONE PLOT
OUTPUT_OPTIONS RUNOFF_OUTSIDE NONE NONE PLOT
OUTPUT_OPTIONS ACT_ET NONE NONE NONE
OUTPUT_OPTIONS POT_ET NONE NONE PLOT
#*******************************************************************
# OUTPUT GRID FILE FORMAT
#
# Next line specifies output grid format: ARC_GRID or SURFER
#
OUTPUT_FORMAT ARC_GRID
#*******************************************************************
# BEGIN SOLUTION
#
# The time series file contains daily values with the following space or
# tab-delimited fields:
#
# 1) Month
# 2) Day
# 3) Year
# 4) Mean Air Temperature (F)
# 5) Precipitation (in)
# 6) Mean Relative Humidity (%)
# 7) Maximum Air Temperature TMAX (F)
# 8) Minimum Air Temperature (F)
# 9) Mean Wind Velocity (m/sec)
# 10)Minimum Relative Humidity (%)
# 11) Percent of Possible Sunshine (%)
#
# Any fields without data should be filled in with a "-99999"
#
SOLVE climate\MSN_1989.txt
SOLVE climate\MSN_1990.txt
#SOLVE climate\MSN_1991.txt
#SOLVE climate\MSN_1992.txt
#SOLVE climate\MSN_1993.txt
#SOLVE climate\MSN_1994.txt
#SOLVE climate\MSN_1995.txt
#SOLVE climate\MSN_1996.txt
#SOLVE climate\MSN_1997.txt
#SOLVE climate\MSN_1998.txt
#SOLVE climate\MSN_1999.txt
#SOLVE climate\MSN_2000.txt
#
EOJ
#
