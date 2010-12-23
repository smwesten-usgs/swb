#*******************************************************************
# Set up model grid
#
GRID 248 162 1066860 283660 2376300 1139020 5280
#
GRID_LENGTH_UNITS FEET
#
#*******************************************************************
# Growing season
#
#
GROWING_SEASON 105 244 TRUE
#
ANSI_COLORS FALSE
#
RLE_MULTIPLIER 10000
#*******************************************************************
# PRECIPITATION INPUT TYPE
# EITHER SINGLE FILE OR DAILY GRIDS
#
PRECIPITATION ARC_GRID climate\precip\ppt
MINIMUM_VALID_PRECIP_VALUE 1.0E-3
#
#*******************************************************************
# TEMPERATURE INPUT TYPE
# EITHER SINGLE FILE OR DAILY GRIDS
#
TEMPERATURE ARC_GRID climate\temp\max\tmax climate\temp\min\tmin
#
#*******************************************************************
# OUTPUT GRID SUFFIX
#
OUTPUT_GRID_SUFFIX asc
#
#*******************************************************************
# INITIAL ABSTRACTION METHOD
#
INITIAL_ABSTRACTION_METHOD TR55
#
INITIAL_FROZEN_GROUND_INDEX CONSTANT 100.0
#
UPPER_LIMIT_CFGI 90.
#
LOWER_LIMIT_CFGI 65.
#
#*******************************************************************
# FLOW DIRECTION GRID
#
FLOW_DIRECTION ARC_GRID input\flow_dir.asc
#
#*******************************************************************
# SOIL HYDROLOGIC GROUP GRID
#
SOIL_GROUP ARC_GRID input\soil_hg.asc
#
#*******************************************************************
# LANDUSE GRID
#
LAND_USE ARC_GRID input\lu_00.asc
#
OPEN_WATER_LAND_USE 200
#
LAND_USE_LOOKUP_TABLE std_input\LU_lookup.txt
#
IRRIGATION_LOOKUP_TABLE std_input\IRRIGATION_lookup.txt
#*******************************************************************
# AWC GRID
#
WATER_CAPACITY ARC_GRID input\soil_awc.asc
#
SM T-M std_input\soil-moisture-retention-extended.grd
#
#*******************************************************************
# INITIAL SOIL MOISTURE CONTENT
#
INITIAL_SOIL_MOISTURE ARC_GRID output\future\final_pct_sm_1999.asc
#
#*******************************************************************
# INITIAL SNOW COVER
#
INITIAL_SNOW_COVER ARC_GRID output\future\final_snow_cover_1999.asc
#
#*******************************************************************
# SOLUTION METHOD
#
RUNOFF C-N DOWNHILL
#
#*******************************************************************
# EVAPOTRANSPIRATION METHOD
#
ET HARGREAVES 40.6 43
#
#*******************************************************************
# PLOTTING CUSTOMIZATION
#
DISLIN_PARAMETERS RECHARGE
SET_Z_AXIS_RANGE DAILY 0 1.5 0.1
SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
SET_Z_AXIS_RANGE ANNUAL 0 16 2.
Z_AXIS_TITLE RECHARGE, IN INCHES
#
DISLIN_PARAMETERS REJECTED_RECHARGE
SET_Z_AXIS_RANGE DAILY 0 1.5 0.1
SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
SET_Z_AXIS_RANGE ANNUAL 0 16 2.
Z_AXIS_TITLE REJECTED RECHARGE, IN INCHES
#
DISLIN_PARAMETERS ACT_ET
SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 40. 5.0
Z_AXIS_TITLE ACTUAL ET, IN INCHES
#
DISLIN_PARAMETERS POT_ET
SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 45. 5.0
Z_AXIS_TITLE POTENTIAL ET, IN INCHES
#
DISLIN_PARAMETERS RUNOFF_OUTSIDE
SET_Z_AXIS_RANGE DAILY 0 5. 0.5
SET_Z_AXIS_RANGE MONTHLY 0 12. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 25. 5.0
Z_AXIS_TITLE RUNOFF OUT OF GRID, IN INCHES
#
DISLIN_PARAMETERS SNOWCOVER
SET_Z_AXIS_RANGE DAILY 0 5. 0.5
SET_Z_AXIS_RANGE MONTHLY 0 12. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 25. 5.0
Z_AXIS_TITLE RUNOFF OUT OF GRID, IN INCHES
#
DISLIN_PARAMETERS IRRIGATION_AMOUNT
SET_Z_AXIS_RANGE DAILY 0 5. 0.5
SET_Z_AXIS_RANGE MONTHLY 0 12. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 16.0 2.0
Z_AXIS_TITLE IRRIGATION AMOUNT, IN INCHES
#
OUTPUT_OPTIONS RECHARGE NONE NONE BOTH
OUTPUT_OPTIONS REJECTED_RECHARGE NONE NONE NONE
OUTPUT_OPTIONS SM_APWL NONE NONE NONE
OUTPUT_OPTIONS SNOWCOVER NONE NONE NONE
OUTPUT_OPTIONS RUNOFF_OUTSIDE NONE NONE NONE
OUTPUT_OPTIONS ACT_ET NONE NONE NONE
OUTPUT_OPTIONS POT_ET NONE NONE NONE
OUTPUT_OPTIONS POT_ET NONE NONE NONE
OUTPUT_OPTIONS IRRIGATION_AMOUNT NONE NONE BOTH
#*******************************************************************
# OUTPUT GRID FILE FORMAT
# Next line specifies output grid format: ARC_GRID or SURFER
#
OUTPUT_FORMAT ARC_GRID
#
SOLVE_NO_TS_DATA 2000 2000
#
EOJ
#
