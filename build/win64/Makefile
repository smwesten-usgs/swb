# Makefile created by mkmf.pl $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 

CPPDEFS = -DNETCDF_SUPPORT -DGRAPHICS_SUPPORT -DTHORNTHWAITE_MATHER_TABLE -DIRRIGATION


include makefile_header.txt


.DEFAULT:
	-echo $@ does not exist.
all: swb.exe
RLE.o: ../../src/RLE.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/RLE.F90
control.o: ../../src/control.F90 types.o model.o grid.o stats.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/control.F90
et_blaney_criddle.o: ../../src/et_blaney_criddle.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_blaney_criddle.F90
et_crop_coefficents.o: ../../src/et_crop_coefficents.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_crop_coefficents.F90
et_hargreaves.o: ../../src/et_hargreaves.F90 types.o stats.o meteorological_functions.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_hargreaves.F90
et_jensen_haise.o: ../../src/et_jensen_haise.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_jensen_haise.F90
et_thornthwaite_mather.o: ../../src/et_thornthwaite_mather.F90 types.o stats.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_thornthwaite_mather.F90
et_turc.o: ../../src/et_turc.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/et_turc.F90
graph.o: ../../src/graph.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/graph.F90
grid.o: ../../src/grid.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/grid.F90
irrigation.o: ../../src/irrigation.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/irrigation.F90
main.o: ../../src/main.F90 types.o control.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/main.F90
meteorological_functions.o: ../../src/meteorological_functions.F90 types.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/meteorological_functions.F90
model.o: ../../src/model.F90 types.o grid.o stats.o runoff_curve_number.o et_thornthwaite_mather.o et_turc.o et_hargreaves.o et_jensen_haise.o et_blaney_criddle.o sm_thornthwaite_mather.o snow.o irrigation.o netcdf_support.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/model.F90
netcdf_support.o: ../../src/netcdf_support.F90 types.o grid.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/netcdf_support.F90
runoff_curve_number.o: ../../src/runoff_curve_number.F90 types.o grid.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/runoff_curve_number.F90
sm_thornthwaite_mather.o: ../../src/sm_thornthwaite_mather.F90 types.o grid.o stats.o RLE.o netcdf_support.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/sm_thornthwaite_mather.F90
snow.o: ../../src/snow.F90 types.o meteorological_functions.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/snow.F90
stats.o: ../../src/stats.F90 types.o grid.o graph.o RLE.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/stats.F90
swb_python_interface.o: ../../src/swb_python_interface.F90 types.o grid.o stats.o runoff_curve_number.o et_thornthwaite_mather.o et_turc.o et_hargreaves.o et_jensen_haise.o et_blaney_criddle.o sm_thornthwaite_mather.o snow.o irrigation.o netcdf_support.o control.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/swb_python_interface.F90
swbread.o: ../../src/swbread.F90 types.o graph.o stats.o grid.o RLE.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/swbread.F90
swbstats.o: ../../src/swbstats.F90 types.o graph.o grid.o stats.o RLE.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/swbstats.F90
types.o: ../../src/types.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	../../src/types.F90
./et_turc.F90: ../../src/et_turc.F90
	cp ../../src/et_turc.F90 .
./swb_python_interface.F90: ../../src/swb_python_interface.F90
	cp ../../src/swb_python_interface.F90 .
./irrigation.F90: ../../src/irrigation.F90
	cp ../../src/irrigation.F90 .
./grid.F90: ../../src/grid.F90
	cp ../../src/grid.F90 .
./et_crop_coefficents.F90: ../../src/et_crop_coefficents.F90
	cp ../../src/et_crop_coefficents.F90 .
./et_blaney_criddle.F90: ../../src/et_blaney_criddle.F90
	cp ../../src/et_blaney_criddle.F90 .
./meteorological_functions.F90: ../../src/meteorological_functions.F90
	cp ../../src/meteorological_functions.F90 .
./et_thornthwaite_mather.F90: ../../src/et_thornthwaite_mather.F90
	cp ../../src/et_thornthwaite_mather.F90 .
./swbread.F90: ../../src/swbread.F90
	cp ../../src/swbread.F90 .
./et_hargreaves.F90: ../../src/et_hargreaves.F90
	cp ../../src/et_hargreaves.F90 .
./stats.F90: ../../src/stats.F90
	cp ../../src/stats.F90 .
./RLE.F90: ../../src/RLE.F90
	cp ../../src/RLE.F90 .
./netcdf_support.F90: ../../src/netcdf_support.F90
	cp ../../src/netcdf_support.F90 .
./control.F90: ../../src/control.F90
	cp ../../src/control.F90 .
./model.F90: ../../src/model.F90
	cp ../../src/model.F90 .
./types.F90: ../../src/types.F90
	cp ../../src/types.F90 .
./et_jensen_haise.F90: ../../src/et_jensen_haise.F90
	cp ../../src/et_jensen_haise.F90 .
./swbstats.F90: ../../src/swbstats.F90
	cp ../../src/swbstats.F90 .
./runoff_curve_number.F90: ../../src/runoff_curve_number.F90
	cp ../../src/runoff_curve_number.F90 .
./graph.F90: ../../src/graph.F90
	cp ../../src/graph.F90 .
./sm_thornthwaite_mather.F90: ../../src/sm_thornthwaite_mather.F90
	cp ../../src/sm_thornthwaite_mather.F90 .
./snow.F90: ../../src/snow.F90
	cp ../../src/snow.F90 .
./main.F90: ../../src/main.F90
	cp ../../src/main.F90 .
SRC = ../../src/irrigation.F90 ../../src/grid.F90 ../../src/graph.F90 ../../src/et_turc.F90 ../../src/types.F90 ../../src/stats.F90 ../../src/main.F90 ../../src/et_jensen_haise.F90 ../../src/sm_thornthwaite_mather.F90 ../../src/et_thornthwaite_mather.F90 ../../src/swbstats.F90 ../../src/netcdf_support.F90 ../../src/model.F90 ../../src/et_blaney_criddle.F90 ../../src/et_crop_coefficents.F90 ../../src/swbread.F90 ../../src/swb_python_interface.F90 ../../src/control.F90 ../../src/RLE.F90 ../../src/meteorological_functions.F90 ../../src/runoff_curve_number.F90 ../../src/snow.F90 ../../src/et_hargreaves.F90
OBJ = irrigation.o grid.o graph.o et_turc.o types.o stats.o main.o et_jensen_haise.o sm_thornthwaite_mather.o et_thornthwaite_mather.o swbstats.o netcdf_support.o model.o et_blaney_criddle.o et_crop_coefficents.o swbread.o swb_python_interface.o control.o RLE.o meteorological_functions.o runoff_curve_number.o snow.o et_hargreaves.o
OFF = ../../src/et_turc.F90 ../../src/swb_python_interface.F90 ../../src/irrigation.F90 ../../src/grid.F90 ../../src/et_crop_coefficents.F90 ../../src/et_blaney_criddle.F90 ../../src/meteorological_functions.F90 ../../src/et_thornthwaite_mather.F90 ../../src/swbread.F90 ../../src/et_hargreaves.F90 ../../src/stats.F90 ../../src/RLE.F90 ../../src/netcdf_support.F90 ../../src/control.F90 ../../src/model.F90 ../../src/types.F90 ../../src/et_jensen_haise.F90 ../../src/swbstats.F90 ../../src/runoff_curve_number.F90 ../../src/graph.F90 ../../src/sm_thornthwaite_mather.F90 ../../src/snow.F90 ../../src/main.F90
clean: neat
	-rm -f .swb.exe.cppdefs $(OBJ) swb.exe
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
swb.exe: $(OBJ) 
	$(LD) $(OBJ) -o swb.exe  $(LDFLAGS)
