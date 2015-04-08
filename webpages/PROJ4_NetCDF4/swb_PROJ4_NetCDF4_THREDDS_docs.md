Soil-Water-Balance Model: geographic transformations
========================================================




As of version 1.2, SWB is able to accept gridded data  whose coordinates are expressed in any well-defined projection. The 

The table below summarizes the effect that each of the four modes has on the calculation of irrigation amounts.

<!-- Table generated in R 3.0.1 by googleVis 0.4.3 package -->
<!-- Mon Jul 08 11:17:44 2013 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataTableID224c7f74030 () {
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
],
[
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
],
[
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
],
[
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
],
[
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
],
[
 "USA Contiguous Albers Equal Area conic USGS version, meters",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
] 
];
data.addColumn('string','Projection name');
data.addColumn('string','PROJ4 string');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID224c7f74030() {
  var data = gvisDataTableID224c7f74030();
  var options = {};
options["allowHtml"] = true;

     var chart = new google.visualization.Table(
       document.getElementById('TableID224c7f74030')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  var chartid = "table";

  // Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
  var i, newPackage = true;
  for (i = 0; newPackage && i < pkgs.length; i++) {
    if (pkgs[i] === chartid)
      newPackage = false;
  }
  if (newPackage)
    pkgs.push(chartid);

  // Add the drawChart function to the global list of callbacks
  callbacks.push(drawChartTableID224c7f74030);
})();
function displayChartTableID224c7f74030() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
    var pkgCount = pkgs.length;
    google.load("visualization", "1", { packages:pkgs, callback: function() {
      if (pkgCount != pkgs.length) {
        // Race condition where another setTimeout call snuck in after us; if
        // that call added a package, we must not shift its callback
        return;
      }
      while (callbacks.length > 0)
        callbacks.shift()();
    } });
  }, 100);
}
 
// jsFooter
 </script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartTableID224c7f74030"></script>
 
<!-- divChart -->
  
<div id="TableID224c7f74030"
  style="width: 600px; height: 500px;">
</div>
<!-- Table generated in R 3.0.1 by googleVis 0.4.3 package -->
<!-- Mon Jul 08 11:17:44 2013 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataTableID224c2ee25f79 () {
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "+a",
"Semimajor radius of the ellipsoid axis" 
],
[
 "+alpha",
"? Used with Oblique Mercator and possibly a few others" 
],
[
 "+axis",
"Axis orientation (new in 4.8.0)" 
],
[
 "+b",
"Semiminor radius of the ellipsoid axis" 
],
[
 "+datum",
"Datum name" 
],
[
 "+ellps",
"Ellipsoid name" 
],
[
 "+k",
"Scaling factor (old name)" 
],
[
 "+k_0",
"Scaling factor (new name)" 
],
[
 "+lat_0",
"Latitude of origin" 
],
[
 "+lat_1",
"Latitude of first standard parallel" 
],
[
 "+lat_2",
"Latitude of second standard parallel" 
],
[
 "+lat_ts",
"Latitude of true scale" 
],
[
 "+lon_0",
"Central meridian" 
],
[
 "+lonc",
"? Longitude used with Oblique Mercator and possibly a few others" 
],
[
 "+lon_wrap",
"Center longitude to use for wrapping" 
],
[
 "+nadgrids",
"Filename of NTv2 grid file to use for datum transforms" 
],
[
 "+no_defs",
"Don't use the /usr/share/proj/proj_def.dat defaults file" 
],
[
 "+over",
"Allow longitude output outside -180 to 180 range, disables wrapping" 
],
[
 "+pm",
"Alternate prime meridian" 
],
[
 "+proj",
"Projection name" 
],
[
 "+south",
"Denotes southern hemisphere UTM zone" 
],
[
 "+to_meter",
"Multiplier to convert map units to 1.0m" 
],
[
 "+towgs84",
"3 or 7 term datum transform parameters" 
],
[
 "+units",
"meters, US survey feet, etc." 
],
[
 "+vto_meter",
"vertical conversion to meters." 
],
[
 "+vunits",
"vertical units." 
],
[
 "+x_0",
"False easting" 
],
[
 "+y_0",
"False northing" 
],
[
 "+zone",
"UTM zone" 
] 
];
data.addColumn('string','PROJ4 directive');
data.addColumn('string','Notes');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID224c2ee25f79() {
  var data = gvisDataTableID224c2ee25f79();
  var options = {};
options["allowHtml"] = true;

     var chart = new google.visualization.Table(
       document.getElementById('TableID224c2ee25f79')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  var chartid = "table";

  // Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
  var i, newPackage = true;
  for (i = 0; newPackage && i < pkgs.length; i++) {
    if (pkgs[i] === chartid)
      newPackage = false;
  }
  if (newPackage)
    pkgs.push(chartid);

  // Add the drawChart function to the global list of callbacks
  callbacks.push(drawChartTableID224c2ee25f79);
})();
function displayChartTableID224c2ee25f79() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
    var pkgCount = pkgs.length;
    google.load("visualization", "1", { packages:pkgs, callback: function() {
      if (pkgCount != pkgs.length) {
        // Race condition where another setTimeout call snuck in after us; if
        // that call added a package, we must not shift its callback
        return;
      }
      while (callbacks.length > 0)
        callbacks.shift()();
    } });
  }, 100);
}
 
// jsFooter
 </script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartTableID224c2ee25f79"></script>
 
<!-- divChart -->
  
<div id="TableID224c2ee25f79"
  style="width: 600px; height: 500px;">
</div>
<!-- Table generated in R 3.0.1 by googleVis 0.4.3 package -->
<!-- Mon Jul 08 11:17:44 2013 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataTableID224cece578d () {
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "BASE_PROJECTION_DEFINITION",
"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
] 
];
data.addColumn('string','SWB directive');
data.addColumn('string','value');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID224cece578d() {
  var data = gvisDataTableID224cece578d();
  var options = {};
options["allowHtml"] = true;
options["width"] =   1200;
options["height"] =    200;
options["page"] = "enable";

     var chart = new google.visualization.Table(
       document.getElementById('TableID224cece578d')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  var chartid = "table";

  // Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
  var i, newPackage = true;
  for (i = 0; newPackage && i < pkgs.length; i++) {
    if (pkgs[i] === chartid)
      newPackage = false;
  }
  if (newPackage)
    pkgs.push(chartid);

  // Add the drawChart function to the global list of callbacks
  callbacks.push(drawChartTableID224cece578d);
})();
function displayChartTableID224cece578d() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
    var pkgCount = pkgs.length;
    google.load("visualization", "1", { packages:pkgs, callback: function() {
      if (pkgCount != pkgs.length) {
        // Race condition where another setTimeout call snuck in after us; if
        // that call added a package, we must not shift its callback
        return;
      }
      while (callbacks.length > 0)
        callbacks.shift()();
    } });
  }, 100);
}
 
// jsFooter
 </script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartTableID224cece578d"></script>
 
<!-- divChart -->
  
<div id="TableID224cece578d"
  style="width: 1200px; height: 200px;">
</div>


