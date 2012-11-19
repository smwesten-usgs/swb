Soil-Water-Balance Model Irrigation Module: Introduction
========================================================

The Soil-Water-Balance Model (SWB) is designed to estimate potential groundwater recharge by tallying inputs and withdrawals from a soil element and calculating the mass balance of water for that element. Additions of water to a soil element that are in excess of the field capacity of the soil are considered to be potential recharge.

The irrigation module takes the current soil moisture deficit (i.e. `field capacity - current soil moisture`) and compares it to a predefined maximum allowable deficit. If the current soil moisture deficit exceeds this user-defined maximum, irrigation water is supplied to the cell in order to erase the soil moisture deficit. In other words, the soil moisture in the cell is brought back up to field capacity, and the amount of water supplied is tracked as irrigation water demand.

The methodologies published in FAO-56 (Allen and others, 1998) have been adopted and incorporated into the code in order to allow for the simulation of irrigation within SWB. 

In order to direct swb to conduct irrigation calculations, a second lookup table must be supplied to SWB, and the `ENABLE_IRRIGATION` directive included in the control file, as shown in the example snippet below:


```
IRRIGATION_LOOKUP_TABLE std_input\IRRIGATION_lookup_new_format.txt
ENABLE_IRRIGATION
```
In addition, the irrigation module must be used with one of four permutations of the FAO-56 methodology. These four modes of operation are enabled by including one of the following in the swb control file:

```
FAO56 CROP_COEFFICIENTS_ONE_FACTOR_STANDARD
FAO56 CROP_COEFFICIENTS_TWO_FACTOR_STANDARD
FAO56 CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD
FAO56 CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD
```
The effects of each of these modes of operation are discussed here  <http:swb_irrigation_module_docs.html>.

## IRRIGATION LOOKUP TABLE

The irrigation lookup table contains about 30 additional pieces of information for each landuse or vegetation type. Reasonable values for most of the fields within the irrigation table for common crops may be found in Allen and others (1998). Although not required, it is generally a good idea to have a line for each landuse present in the SWB lookup table also present in the SWB irrigation table. *Any landuse present in the landuse lookup table but absent in the irrigation table will be assigned default values, which may or may not be reasonable for the vegetation type represented by the landuse!*

The table presented below is in a format similar to that required by SWB for the irrigation lookup table. It could be converted for use with SWB by inserting comment delimiters ("#") at the beginning of the lines containing the headings, and by saving the file as a "tab-delimited" file within a program such as Excel. **NOTE** that if one uses Excel as a tool to prepare tab-delimited files for SWB, it often inserts extraneous tab characters at the end of lines and at the end of the file, and also places quotes ("") around certain text strings. Both of these will confuse SWB. Check the exported file with a good text editor before supplying it to SWB!


<!-- Table generated in R 2.15.2 by googleVis 0.3.3 package -->
<!-- Fri Nov 16 13:58:00 2012 -->


<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
 
// jsData 
function gvisDataTableID1848b5071f7 ()
{
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 7,
"Irrigated corn or soybeans",
4.9,
0.15,
1.1,
1,
0.15,
"5/10",
30,
40,
50,
50,
"DOY",
0.196,
0.295,
0.393,
0.472,
0.354,
0.669,
0.906,
1.063,
0.5,
50,
130,
0.5,
"5/15",
"9/5",
1,
1,
0.75 
],
[
 8,
"Irrigated corn or soybeans",
4.9,
0.15,
1.1,
1,
0.15,
"130",
30,
40,
50,
50,
"DOY",
0.196,
0.295,
0.393,
0.472,
0.354,
0.669,
0.906,
1.063,
0.5,
50,
130,
1,
"5/15",
"9/5",
1,
1,
0.75 
],
[
 41,
"Forested",
30,
1,
1,
1,
1,
"130",
90,
90,
90,
90,
"DOY",
0.196,
0.295,
0.393,
0.472,
0.354,
0.669,
0.906,
1.063,
0.3,
50,
130,
1,
"5/1",
"9/5",
1,
0.85,
0.75 
] 
];
data.addColumn('number','Landuse Code');
data.addColumn('string','Description');
data.addColumn('number','Max Crop Height (ft)');
data.addColumn('number','Kcb init');
data.addColumn('number','Kcb mid');
data.addColumn('number','Kcb late');
data.addColumn('number','Kcb min');
data.addColumn('string','Planting Date (DOY or GDD)');
data.addColumn('number','L initial');
data.addColumn('number','L development');
data.addColumn('number','L mid');
data.addColumn('number','L late');
data.addColumn('string','Measure of growth (DAY or GDD)');
data.addColumn('number','REW soils grp A');
data.addColumn('number','REW soils grp B');
data.addColumn('number','REW soils grp C');
data.addColumn('number','REW soils grp D');
data.addColumn('number','TEW soils grp A');
data.addColumn('number','TEW soils grp B');
data.addColumn('number','TEW soils grp C');
data.addColumn('number','TEW soils grp D');
data.addColumn('number','Plant Stress Depletion Fraction (p)');
data.addColumn('number','GDD Base Temperature');
data.addColumn('number','GDD Max Temperature');
data.addColumn('number','Max Allowable Depletion');
data.addColumn('string','Irrigation Start Date');
data.addColumn('string','Irrigation End Date');
data.addColumn('number','Fraction of Irrigation from Groundwater');
data.addColumn('number','Fractional Irrigation Efficiency GW');
data.addColumn('number','Fractional Irrigation Efficiency SW');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID1848b5071f7() {
  var data = gvisDataTableID1848b5071f7();
  var options = {};
options["allowHtml"] = true;
options["width"] =   1400;
options["height"] =    300;
options["page"] = "enable";

     var chart = new google.visualization.Table(
       document.getElementById('TableID1848b5071f7')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart 
function displayChartTableID1848b5071f7()
{
  google.load("visualization", "1", { packages:["table"] }); 
  google.setOnLoadCallback(drawChartTableID1848b5071f7);
}
 
// jsChart 
displayChartTableID1848b5071f7()
 
<!-- jsFooter -->  
//-->
</script>
 
<!-- divChart -->
  
<div id="TableID1848b5071f7"
  style="width: 1400px; height: 300px;">
</div>



### NOTES REGARDING THE CONSTRUCTION OF THE IRRIGATION TABLE

#### Crop development stages: Kc and L

The FAO-56 method defines four distinct stages of plant growth. The crop coefficient is assumed constant during the initial and mid-season stages. Kc during the crop development stage is interpolated between the values of Kc defined for the initial and mid-season stages; Kc during the late season is interpolated between the values of Kc defined for the mid-season and late season.  

SWB constructs a crop coefficient function (as depicted in the figure below) on the basis of the Kc values supplied for initial, mid, and end of the growing season, as well as the planting date and length of each growth stage.

![Source: Allen and others (1998), figure 25](FAO56_figs/Fig_25.png)  
*Modified from figure 25, Allen and others (1998).*

The definition of the length of each growth stage may be provided either as days or in the form of growing degree-days (GDD). The planting "date" may be provided as a day of year (DOY), as a month and day value (mm/dd), or as the minimum number of growing degree-days that must pass prior to assumed planting. The Kc function for each vegetation type must be provided in consistent units; day of year and GDD cannot be mixed! SWB expects the string "DOY or "GDD" to be present in the "Measure of growth" column so that the numbers are interpreted correctly.

**NOTE** that Kcb values ('basal' crop coefficients) used with the dual-stage crop coefficient approach are generally somewhat lower than the somewhat more standard Kc values used with the single-coefficient approach. See table 12 (Allen and others, 1998) for typical 'standard' Kc values used in the single-coefficient approach; see table 17 (Allen and others, 1998) for typical Kcb (basal) crop coefficient values used in the dual-coefficient approach.

#### Bare soil evaporation: REW and TEW

The next set of fields in the irrigation lookup table define the readily evaporable water (REW) and total evaporable water (TEW) needed to implement the dual-coefficient FAO-56 approach. These coefficients may be ignored if a single-coefficient approach is used in SWB.

**NOTE** that the number of fields with REW values and the number of fields with TEW values must be equal to the number of hydrologic soil groups used in the model.

Typical values for the REW and TEW are given in table 19 of Allen and others (1998). The table below is adapted from it.

<!-- Table generated in R 2.15.2 by googleVis 0.3.3 package -->
<!-- Fri Nov 16 11:52:19 2012 -->


<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
 
// jsData 
function gvisDataTableID19f81f477fe ()
{
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "Sand",
"0.6 to 1.32",
"0.079 to 0.28",
"0.24 to 0.47" 
],
[
 "Loamy sand",
"0.72 to 1.44",
"0.157 to 0.31",
"0.35 to 0.55" 
],
[
 "Sandy loam",
"1.32 to 1.8",
"0.236 to 0.39",
"0.59 to 0.79" 
],
[
 "Loam",
"1.56 to 2.16",
"0.315 to 0.39",
"0.63 to 0.87" 
],
[
 "Silt loam",
"1.56 to 2.28",
"0.315 to 0.43",
"0.71 to 0.98" 
],
[
 "Silt",
"1.92 to 2.4",
"0.315 to 0.43",
"0.87 to 1.02" 
],
[
 "Silt clay loam",
"1.56 to 2.16",
"0.315 to 0.43",
"0.87 to 1.06" 
],
[
 "Silty clay",
"1.56 to 2.28",
"0.315 to 0.47",
"0.87 to 1.10" 
],
[
 "Clay",
"1.44 to 2.4",
"0.315 to 0.47",
"0.87 to 1.14" 
] 
];
data.addColumn('string','Soil Type');
data.addColumn('string','Available Water Content (in/ft)');
data.addColumn('string','Readily Evaporable Water (in)');
data.addColumn('string','Total Evaporable Water (in)');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID19f81f477fe() {
  var data = gvisDataTableID19f81f477fe();
  var options = {};
options["allowHtml"] = true;
options["width"] =   1200;
options["height"] =    300;
options["page"] = "enable";

     var chart = new google.visualization.Table(
       document.getElementById('TableID19f81f477fe')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart 
function displayChartTableID19f81f477fe()
{
  google.load("visualization", "1", { packages:["table"] }); 
  google.setOnLoadCallback(drawChartTableID19f81f477fe);
}
 
// jsChart 
displayChartTableID19f81f477fe()
 
<!-- jsFooter -->  
//-->
</script>
 
<!-- divChart -->
  
<div id="TableID19f81f477fe"
  style="width: 1200px; height: 300px;">
</div>


*Table adapted from table 19, Allen and others (1998)*

#### Plant stress depletion fraction (p)

The next field value to be defined is the plant stress depletion fraction. This defines the amount of soil moisture that may be depleted (i.e. the readily available water, RAW) before the plant begins to exhibit water stress. The plant stress depletion fraction relates the RAW to the total available water (TAW) within the root zone:

\[RAW = p \cdot TAW\]

#### Growing Degree-Day BASE and MAXIMUM temperature

The next two fields define the base and maximum temperatures for use in calculating and updating the growing degree-day value for a *cell*. There is really no *standard* growing degree-day. The base and maximum temperatures used in the tabulation obviously influence the calculated GDD. For corn, base and maximum values of 50 degrees F and 130 degrees F, respectively, are often used.

#### Maximum allowable depletion (MAD) fraction

The maximum allowable depletion value determines the amount of soil moisture deficit that can be tolerated before irrigation is triggered. The value may range from 0. (irrigate every day) to 1.0 (never irrigate). Once irrigation is indicated, SWB applies water to each affected grid cell until the soil moisture has returned to field capacity.

#### Irrigation start and end dates

The next two fields define absolute limits on when simulated irrigation may take place. For example, even though climatic and soil moisture conditions might indicate a need for late season irrigation, this can be circumvented by setting an irrigation end date to prevent this from occurring.

Irrigation start and end dates may be specified as a month and day (MM/DD).

#### Irrigation source and efficiency

SWB can track the assumed source of irrigation water and the efficiency of the irrigation systems. The column "fraction of irrigation from groundwater" allows for partitioning of irrigation source waters between a "groundwater" source and a "surface water" source.

The irrigation efficiency fractions (at this time - November 2012) do *not* increase the amount of water applied, but rather increase the amount of water required at the source.

In other words, if 1.0 inches of water were required in a grid cell to restore soil moisture to field capacity, the amount of source water required for an 85% efficient irrigation system would be 1.0 / .085 = 1.18 inches. The difference between the source requirement and the amount applied (0.18 inches in this case) is assumed lost somewhere in the process of delivery. This may need to be rethought in future versions.

### References

Allen, R.G., Pereira, L.S., Raes, D. and Smith, M., 1998, Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56: Rome, Food and Agriculture Organization of the United Nations, <http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents>.
