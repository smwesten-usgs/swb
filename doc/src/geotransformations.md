# Soil-Water-Balance Model: geographic transformations {#geotransformations}


As of version 1.2, SWB is able to accept gridded data whose coordinates are expressed in any well-defined projection. A code called PROJ.4 has been linked to the SWB code, allowing coordinate transformations to be made on input grids. 

Projection name                                                | EPSG Code |  PROJ.4 string
---------------------------------------------------------------|:---------:|-----------------------------------------
USA Contiguous Albers Equal Area Conic\n USGS version, meters  |  7301     | +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs 
United State National Atlas Equal Area, meters                 |  2163     | +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs