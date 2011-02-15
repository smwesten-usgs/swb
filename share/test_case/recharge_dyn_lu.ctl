# Poynette test data set
#
# ==> THIS IS DESIGNED TO PRODUCE THE BASELINE OUTPUT AGAINST WHICH
#     REGRESSION TESTS MAY BE RUN
#
# Grid domain
GRID 60 56 562524.92647059 319109.55882353 567924.92647059 324149.5882353 90.0
GRID_LENGTH_UNITS METERS
#
# Growing season is from 4/15 to 10/15 in the Northern Hemisphere (if the
# "T" at the end is "F", it assumes that the specified Julian day period is
# the dormant season.
GROWING_SEASON 133 268 TRUE
#
# Grids of hydrological coverages
FLOW_DIRECTION ARC_GRID input\fl_dir.grd
SOIL_GROUP ARC_GRID input\soil_group.grd
LAND_USE DYNAMIC ARC_GRID input\land_cov
LAND_USE_LOOKUP_TABLE std_input\LU_lookup_Dripps_02-2011.txt
WATER_CAPACITY ARC_GRID input\water_cap.grd
#
# Initial conditions
INITIAL_SOIL_MOISTURE CONSTANT 100
INITIAL_SNOW_COVER CONSTANT 0
#
# Choose an ET model (Thornthwaite-Mather at 43 degrees latitude)
ET T-M 43.0
#
# Choose a soil-moisture model. The file "soil-moisture-retention.grd" is
# Wes' table of Thornthwaite soil-moisture data in SURFER format with missing
# values; it ships with the code.
SM T-M std_input\soil-moisture-retention-extended.grd
#
#
# Sources of water in water mass balance equation
#
OUTPUT_OPTIONS GROSS_PRECIP NONE NONE NONE
OUTPUT_OPTIONS NET_PRECIP NONE NONE NONE
OUTPUT_OPTIONS SNOWMELT NONE NONE BOTH
OUTPUT_OPTIONS INFLOW NONE NONE BOTH
#
# Sinks of water in water mass balance equation
#
OUTPUT_OPTIONS INTERCEPTION NONE NONE BOTH
OUTPUT_OPTIONS OUTFLOW NONE NONE BOTH
OUTPUT_OPTIONS RUNOFF_OUTSIDE NONE NONE BOTH
OUTPUT_OPTIONS CHG_IN_SOIL_MOIS NONE NONE GRID
OUTPUT_OPTIONS ACT_ET NONE NONE GRID
OUTPUT_OPTIONS RECHARGE NONE NONE BOTH
OUTPUT_OPTIONS REJECTED_RECHARGE NONE NONE BOTH
#
# Variables giving extra information
#
OUTPUT_OPTIONS SOIL_MOISTURE NONE NONE GRID
OUTPUT_OPTIONS SNOWFALL NONE NONE GRID
OUTPUT_OPTIONS SNOWCOVER NONE NONE GRID
OUTPUT_OPTIONS CFGI NONE NONE GRID
#
#
SUPPRESS_SCREEN_OUTPUT
# Run the model. Read the data from the "test" directory, put
# the output in the "test" directory, put the initial conditions for a future
# run in the "future" directory, and write 'em all in SURFER format.
OUTPUT_FORMAT ARC_GRID
SOLVE climate\MSN_1989.txt
SOLVE climate\MSN_1990.txt
SOLVE climate\MSN_1991.txt
SOLVE climate\MSN_1992.txt
SOLVE climate\MSN_1993.txt
SOLVE climate\MSN_1994.txt
SOLVE climate\MSN_1995.txt
#SOLVE climate\MSN_1996.txt
#SOLVE climate\MSN_1997.txt
#SOLVE climate\MSN_1998.txt
#SOLVE climate\MSN_1999.txt
#SOLVE climate\MSN_2000.txt
# That's the end of the job. Have a nice day.
EOJ
