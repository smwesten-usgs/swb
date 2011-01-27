
add_executable(swb
            et_blaney_criddle.F90
            et_hargreaves.F90
            et_jensen_haise.F90
            et_thornthwaite_mather.F90
            et_turc.F90
            graph.F90
            grid.F90
            irrigation.F90
            main.F90
            meteorological_functions.F90
            model.F90
            netcdf_support.F90
            RLE.F90
            runoff_curve_number.F90
            sm_thornthwaite_mather.F90
            snow.F90
            stats.F90
            types.F90)

add_executable(swbread
            graph.F90
            grid.F90
            RLE.F90
            stats.F90
            swbread.F90
            types.F90)

add_executable(swbstats
            graph.F90
            grid.F90
            RLE.F90
            stats.F90
            swbstats.F90
            types.F90)

set_target_properties(swb
            PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

#set_target_properties(swb
#            PROPERTIES COMPILE_DEFINITIONS NONE)

set_target_properties(swbread swbstats
            PROPERTIES COMPILE_DEFINITIONS GRAPHICS_SUPPORT)

target_link_libraries(swb ${EXTRA_LIBS})
