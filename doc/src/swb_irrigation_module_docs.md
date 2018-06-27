# Soil-Water-Balance Model Irrigation Module: FAO-56 Modes {#irrigation_module_docs}


The irrigation module must be used with one of four permutations of the FAO-56 methodology. These four modes of operation are enabled by including one of the following in the swb control file:


    FAO56 CROP_COEFFICIENTS_ONE_FACTOR_STANDARD
    FAO56 CROP_COEFFICIENTS_TWO_FACTOR_STANDARD
    FAO56 CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD
    FAO56 CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD

The table below summarizes the effect that each of the four modes has on the calculation of irrigation amounts.

FAO-56 calculation mode   |   Bare soil evaporation calculated?  |  Water stress calculated?   |  Thornthwaite-Mather soil-moisture retention tables consulted?
--------------------------|--------------------------------------|-----------------------------|---------------------------------------------
CROP_COEFFICIENTS_ONE_FACTOR_STANDARD     | no                   |  no                         | yes
CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD  | no                   |  yes                        | no
CROP_COEFFICIENTS_TWO_FACTOR_STANDARD     | yes                  |  no                         | yes
CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD  | yes                  |  yes                        | no 


In the following sections, each of the four swb modes of implementing the FAO-56 calculations is compared to a standard test case that was prepared and run through the FAO's CROPWAT irrigation analysis software (http://www.fao.org/nr/water/infores_databases_cropwat.html).

______________________________________________________________________________

1. SINGLE COEFFICIENT UNDER STANDARD CONDITIONS
-----------------------

By 'single coefficient', we mean that evaporation from bare soil is not evaluated separately; the potential evapotranspiration to which the soil is subjected is estimated as the 'crop evapotranspiration', the product of the grass reference evapotranspiration \f$(E{T_{\rm{o}}})\f$ and a crop coefficient \f$({K_c})\f$:


\f[
E{T_c} = {K_c}E{T_{\rm{o}}}
\f]

By 'standard conditions', we mean that the crop evapotranspiration is not limited by plant stress due to water availability. In swb, this means the actual amount of water withdrawn from the soil column beneath a cell is detemined by consulting the Thornthwaite-Mather (1957) soil moisture-retention tables. The following figures compare the swb calculations (single coefficient, standard conditions) to results obtained from FAO's CROPWAT.



![Plot of FAO-56 calculations, Single coefficient, standard](../../src/images/single_factor_standard.png) 


For the comparison in this example, swb is being run with the Hargreaves-Samani (1985) reference ET option. This option is in reasonably close agreement with the reference ET calculated by the CROPWAT program. swb can, of course, be run with a number of other reference and potential ET calculation options; the Hargreaves-Samani (1985) is the closest to the FAO Penman-Montieth method, and is recommended in FAO-56 (Allen and others, 1998) as the method of choice in situations where only air temperature data are available.

![Plot of crop evapotranspiration: SWB compared to CROPWAT](../../src/images/crop_ET__SWB_vs_CROPWAT.png) 

The plots of reference and crop ET are very similar for the remaining FAO-56 calculation options and are not shown in the remaining comparison plots.

______________________________________________________________________________

2. SINGLE COEFFICIENT UNDER NONSTANDARD CONDITIONS
-----------------------

The calculation under 'nonstandard' conditions includes the transpiration-limiting effects of soil water stress on plants. When this option is invoked in swb, the Thornthwaite-Mather soil moisture retention tables are **not** consulted. Rather, the crop evapotranspiration amount is adjusted (\f$E{T_{c,adj}}\f$) by incorporation of a water stress factor (\f${K_s}\f$) whose value may range between 0.0 and 1.0:

\f[
E{T_{c,adj}} = {K_s}{K_c}E{T_{\rm{o}}}
\f]

\f${K_s}\f$ is defined by the doil moisture deficit relative to two soil moisture amounts: the Readily Available Water (RAW) and Total Available Water (TAW) amounts. At soil moisture deficits less than the RAW amount, it is assumed that plants have adequate available moisture for growth; plants are assumed to be under no water stress. The value of \f${K_s}\f$ is one under these conditions.

Once soil moisture deficit increases beyond the RAW amount, \f${K_s}\f$ decreases linearly , reaching a value of zero as the soil moisture deficit approaches the TAW value.

Total Available Water (\f$TAW\f$) is defined as the **maximum** amount of water that can be present within the root zone, and is calculated in swb as:

\f[TAW = AWC \cdot (root\;depth)\f]

where:

&nbsp;&nbsp;\f$AWC\f$ is the Available Water Capacity, in inches per foot, and   
&nbsp;&nbsp; \f$root\;depth\f$ is the current rooting depth of vegetation in feet
  
Readily Available Water (\f$RAW\f$) is defined as the amount of water that can be withdrawn by a plant from soil moisture storage without the plant suffering water stress. \f$RAW\f$ may be defined as some fraction of the Total Available Water:

\f[
RAW = p \cdot TAW
\f]

where:

&nbsp;&nbsp; \f$p\f$ is the fraction of Total Available Water (\f$TAW\f$) that can be removed from soil moisture storage before a plant begins suffering from water stress. \f$p\f$ is called the "plant stress depletion fraction" in the swb irrigation lookup table.
  
The figure below, taken from FAO-56 (Allen and others, 1998), shows how the water stress factor changes with changing soil moisture deficit amounts.

![Source: Allen and others (1998), figure 42](FAO56_figs/Fig_42.png)

*Source: Allen and others (1998), figure 42*

When swb is run on the irrigation test case with the `FAO56 CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD` option enabled, the timing and amount of irrigation water closely match the output from FAO's CROPWAT program:
  

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

______________________________________________________________________________

3. DUAL COEFFICIENT UNDER STANDARD CONDITIONS
-----------------------

The dual coefficient version of the FAO-56 methodology splits the crop coefficient into two coefficients, one to account for soil evaporation, and the other to account for crop transpiration. This modification to the methodology is recommended when more frequent irrigation applications are likely, and the irrigation scheduling is to be done on a daily basis (Allen and others, 1998). Since swb is already performing soil water balance calculations on a daily basis, the dual-coefficient methodology is probably the preferred method when simulating land subject to frequent irrigation. The dual coefficient method for calculating crop evapotranspiration is shown below:

\f[
E{T_c} = \left( {{K_{cb}} + {K_e}} \right) \cdot E{T_o}
\f]

where:

\f$E{T_c}\f$ is the crop evapotranspiration in inches

\f${{K_{cb}}}\f$ is the "basal" crop coefficient, defined as the ratio of crop evapotranspiration (\f$E{T_c}\f$) to reference evapotranspiration (\f$E{T_o}\f$) *when the soil surface is dry*. 

\f$E{T_e}\f$ is the soil evaporation coefficient describing the evaporation component of \f$E{T_c}\f$

When the soil moisture approaches field capacity, bare soil evaporation is assumed to be a maximum. As the soil dries, the amount of bare soil evaporation tapers down to zero. In FAO-56 methodology, bare soil evaporation is attenuated by an "evaporation reduction coefficient" whose value can range from 0 to 1. 

The overall crop evapotranspiration amount is limited to the value \f${K_{c,\max }}\f$, which specifies the maximum value of \f${K_c}\f$ following rain or irrigation. Thus, the bare soil evaporation amount cannot be greater than \f${{K_{c,\max }} - {K_{cb}}}\f$, as attenuated by the evaporation reduction coefficient:

\f[
{K_e} = {K_r} \cdot \left( {{K_{c,\max }} - {K_{cb}}} \right)
\f]

where:  
\f${K_r}\f$ is the evaporation reduction coefficient  
\f${{K_{c,\max }}}\f$ is the maximum value that \f${K_c}\f$ may take when soil moisture values approach field capacity  
\f${{K_{cb}}}\f$ is the basal crop coefficient as described above  

As is true in the case of the single coefficient approach, 'standard conditions' mean that the crop evapotranspiration is not limited by plant stress due to water availability. In swb, this means the actual amount of water withdrawn from the soil column beneath a cell is detemined by consulting the Thornthwaite-Mather (1957) soil moisture-retention tables. The following figures compare the swb calculations (dual coefficient, standard conditions) to results obtained from FAO's CROPWAT.


![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 




______________________________________________________________________________

4. DUAL COEFFICIENT UNDER NONSTANDARD CONDITIONS
-----------------------

The last option for application of the FAO-56 methodology uses the dual-coefficient approach discussed in the last section. Further, this option **disables** the Thornthwaite-Mather soil moisture retention tables and **enables** the reduction of crop evaportanspiration due to water stress. 

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


### References

Allen, R.G., Pereira, L.S., Raes, D. and Smith, M., 1998, Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56: Rome, Food and Agriculture Organization of the United Nations, <http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents>.
