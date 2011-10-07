require(RNetCDF)

nc_header <- function(nc) {

# create NEW NetCDF output file
nc.out<-create.nc(nc$netcdf.filename)

att.put.nc(nc.out, "NC_GLOBAL", "title", "NC_CHAR", nc$title_txt)
att.put.nc(nc.out, "NC_GLOBAL", "institution", "NC_CHAR", nc$institution_txt)
att.put.nc(nc.out, "NC_GLOBAL", "Conventions", "NC_CHAR", "CF-1.4")
att.put.nc(nc.out, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

# define NetCDF DIMENSIONS
dim.def.nc(nc.out,"y",nc$n_rows)       # Y
dim.def.nc(nc.out,"x",nc$n_cols)       # X
dim.def.nc(nc.out,"time",unlim=TRUE)   # Time

# define projected coordinates
var.def.nc(nc.out,"y", "NC_DOUBLE", "y")
att.put.nc(nc.out, "y", "units", "NC_CHAR", nc$len_units)
att.put.nc(nc.out, "y", "axis", "NC_CHAR", "Y")
att.put.nc(nc.out, "y", "standard_name", "NC_CHAR", "projection_y_coordinate")
att.put.nc(nc.out, "y", "long_name", "NC_CHAR", "y coordinate of projection")
att.put.nc(nc.out, "y", "grid_spacing", "NC_CHAR", as.character(nc$cell_size))

var.def.nc(nc.out,"x", "NC_DOUBLE", "x")
att.put.nc(nc.out, "x", "units", "NC_CHAR", nc$len_units)
att.put.nc(nc.out, "x", "axis", "NC_CHAR", "X")
att.put.nc(nc.out, "x", "standard_name", "NC_CHAR", "projection_x_coordinate")
att.put.nc(nc.out, "x", "long_name", "NC_CHAR", "x coordinate of projection")
att.put.nc(nc.out, "x", "grid_spacing", "NC_CHAR", as.character(nc$cell_size))

# define latitude and longitude coordinates
var.def.nc(nc.out,"lat", "NC_DOUBLE", c("x","y"))
att.put.nc(nc.out, "lat", "units", "NC_CHAR", "degrees_north")
att.put.nc(nc.out, "lat", "standard_name", "NC_CHAR", "latitude")
att.put.nc(nc.out, "lat", "long_name", "NC_CHAR", "latitude")
var.def.nc(nc.out,"lon", "NC_DOUBLE", c("x","y"))
att.put.nc(nc.out, "lon", "units", "NC_CHAR", "degrees_east")
att.put.nc(nc.out, "lon", "standard_name", "NC_CHAR", "longitude")
att.put.nc(nc.out, "lon", "long_name", "NC_CHAR", "longitude")

var.def.nc(nc.out,"time", "NC_DOUBLE", "time")
att.put.nc(nc.out, "time", "units", "NC_CHAR", nc$origin_txt)
att.put.nc(nc.out, "time", "calendar", "NC_CHAR", nc$calendar_txt)
att.put.nc(nc.out, "time", "start_day", "NC_DOUBLE", nc$jul_startday)
att.put.nc(nc.out, "time", "end_day", "NC_DOUBLE", nc$jul_endday)

# ** GRID MAPPING STUFF NEEDS TO BE CHANGED FOR USE WITH A DIFFERENT
# PROJECTION AND COORDINATE SYSTEM!! **
var.def.nc(nc.out,"wtm", "NC_CHAR", NA)
att.put.nc(nc.out, "wtm", "grid_mapping_name", "NC_CHAR", "transverse_mercator")
att.put.nc(nc.out, "wtm", "scale_factor_at_central_meridian", "NC_DOUBLE", 0.9996)
att.put.nc(nc.out, "wtm", "longitude_of_central_meridian", "NC_DOUBLE", -90.00)
att.put.nc(nc.out, "wtm", "latitude_of_projection_origin", "NC_DOUBLE", 0.0)
att.put.nc(nc.out, "wtm", "false_easting", "NC_DOUBLE", 520000)
att.put.nc(nc.out, "wtm", "false_northing", "NC_DOUBLE", -4480000)

var.put.nc(nc.out,"x",nc$x0)
var.put.nc(nc.out,"y",nc$y0)

var.put.nc(nc.out,"lon",nc$lon)
var.put.nc(nc.out,"lat",nc$lat)

  if(nc$dtype=="PRCP") {
    var.def.nc(nc.out, "prcp", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "prcp", "standard_name", "NC_CHAR", "precipitation")    
    att.put.nc(nc.out, "prcp", "long_name", "NC_CHAR", "Precipitation")
    att.put.nc(nc.out, "prcp", "units", "NC_CHAR", "inches")
    att.put.nc(nc.out, "prcp", "scale_factor", "NC_DOUBLE", 1./100.)
    att.put.nc(nc.out, "prcp", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "prcp", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "prcp", "missing_value", "NC_SHORT", -999)        
    att.put.nc(nc.out, "prcp", "grid_mapping", "NC_CHAR", "wtm")        
    
  } else if (nc$dtype=="TMAX") {
  
    var.def.nc(nc.out, "tmax", "NC_SHORT", c("x","y", "time"))
    att.put.nc(nc.out, "tmax", "standard_name", "NC_CHAR", "max_daily_temperature")
    att.put.nc(nc.out, "tmax", "long_name", "NC_CHAR", "Max. Daily Air Temperature")
    att.put.nc(nc.out, "tmax", "units", "NC_CHAR", "degrees Fahrenheit")
    att.put.nc(nc.out, "tmax", "scale_factor", "NC_DOUBLE", 1./100.)
    att.put.nc(nc.out, "tmax", "add_offset", "NC_DOUBLE", -100.0)    
    att.put.nc(nc.out, "tmax", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "tmax", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "tmax", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="TMIN") {
  
    var.def.nc(nc.out, "tmin", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "tmin", "standard_name", "NC_CHAR", "min_daily_temperature")    
    att.put.nc(nc.out, "tmin", "long_name", "NC_CHAR", "Min. Daily Air Temperature")
    att.put.nc(nc.out, "tmin", "units", "NC_CHAR", "degrees Fahrenheit")
    att.put.nc(nc.out, "tmin", "scale_factor", "NC_DOUBLE", 1./100.)
    att.put.nc(nc.out, "tmin", "add_offset", "NC_DOUBLE", -100.0)    
    att.put.nc(nc.out, "tmin", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "tmin", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "tmin", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="SNOW") {
  
    var.def.nc(nc.out, "snow", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "snow", "standard_name", "NC_CHAR", "daily_snowfall")    
    att.put.nc(nc.out, "snow", "long_name", "NC_CHAR", "Daily Snowfall Amount")
    att.put.nc(nc.out, "snow", "units", "NC_CHAR", "inches")
    att.put.nc(nc.out, "snow", "scale_factor", "NC_DOUBLE", 1./10.)
    att.put.nc(nc.out, "snow", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "snow", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "snow", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "snow", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="SNWD") {
  
    var.def.nc(nc.out, "snwd", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "snwd", "standard_name", "NC_CHAR", "snow_depth")    
    att.put.nc(nc.out, "snwd", "long_name", "NC_CHAR", "Snow Depth")
    att.put.nc(nc.out, "snwd", "units", "NC_CHAR", "inches")
    att.put.nc(nc.out, "snwd", "scale_factor", "NC_DOUBLE", 1./10.)
    att.put.nc(nc.out, "snwd", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "snwd", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "snwd", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "snwd", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="AWND") {
  
    var.def.nc(nc.out, "awnd", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "awnd", "standard_name", "NC_CHAR", "avg_daily_wind_spd")    
    att.put.nc(nc.out, "awnd", "long_name", "NC_CHAR", "Average Daily Wind Speed")
    att.put.nc(nc.out, "awnd", "units", "NC_CHAR", "miles per hour")
    att.put.nc(nc.out, "awnd", "scale_factor", "NC_DOUBLE", 1./10.)
    att.put.nc(nc.out, "awnd", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "awnd", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "awnd", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "awnd", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="RWND") {
  
    var.def.nc(nc.out, "rwnd", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "rwnd", "standard_name", "NC_CHAR", "resultant_daily_wind_spd")    
    att.put.nc(nc.out, "rwnd", "long_name", "NC_CHAR", "Resultant Daily Wind Speed")
    att.put.nc(nc.out, "rwnd", "units", "NC_CHAR", "miles per hour")
    att.put.nc(nc.out, "rwnd", "scale_factor", "NC_DOUBLE", 1./10.)
    att.put.nc(nc.out, "rwnd", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "rwnd", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "rwnd", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "rwnd", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="WTEQ") {
  
    var.def.nc(nc.out, "wteq", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "wteq", "standard_name", "NC_CHAR", "water_equiv_snwd")    
    att.put.nc(nc.out, "wteq", "long_name", "NC_CHAR", "Water Equivalent of Snow Depth")
    att.put.nc(nc.out, "wteq", "units", "NC_CHAR", "inches")
    att.put.nc(nc.out, "wteq", "scale_factor", "NC_DOUBLE", 1./10.)
    att.put.nc(nc.out, "wteq", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "wteq", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "wteq", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "wteq", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="PSUN") {
  
    var.def.nc(nc.out, "psun", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "psun", "standard_name", "NC_CHAR", "percent_possible_sun")    
    att.put.nc(nc.out, "psun", "long_name", "NC_CHAR", "Percent of Possible Sunshine")
    att.put.nc(nc.out, "psun", "units", "NC_CHAR", "percent")
    att.put.nc(nc.out, "psun", "scale_factor", "NC_DOUBLE", 1.)
    att.put.nc(nc.out, "psun", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "psun", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "psun", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "psun", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="MXRH") {
  
    var.def.nc(nc.out, "mxrh", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "mxrh", "standard_name", "NC_CHAR", "max_rel_humidity")    
    att.put.nc(nc.out, "mxrh", "long_name", "NC_CHAR", "Maximum Relative Humidity")
    att.put.nc(nc.out, "mxrh", "units", "NC_CHAR", "percent")
    att.put.nc(nc.out, "mxrh", "scale_factor", "NC_DOUBLE", 1.)
    att.put.nc(nc.out, "mxrh", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "mxrh", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "mxrh", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "mxrh", "grid_mapping", "NC_CHAR", "wtm")        

  } else if(nc$dtype=="MNRH") {
  
    var.def.nc(nc.out, "mnrh", "NC_SHORT", c("x","y","time"))
    att.put.nc(nc.out, "mnrh", "standard_name", "NC_CHAR", "min_rel_humidity")    
    att.put.nc(nc.out, "mnrh", "long_name", "NC_CHAR", "Minimum Relative Humidity")
    att.put.nc(nc.out, "mnrh", "units", "NC_CHAR", "percent")
    att.put.nc(nc.out, "mnrh", "scale_factor", "NC_DOUBLE", 1.)
    att.put.nc(nc.out, "mnrh", "add_offset", "NC_DOUBLE", 0.0)    
    att.put.nc(nc.out, "mnrh", "coordinates", "NC_CHAR", "y x")    
    att.put.nc(nc.out, "mnrh", "missing_value", "NC_SHORT", -999)            
    att.put.nc(nc.out, "mnrh", "grid_mapping", "NC_CHAR", "wtm")        
    
  } else {
    stop("Unknown data type referenced in script\n")
  }

  return(nc.out)

}
