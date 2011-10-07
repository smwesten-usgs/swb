rm(list=ls())

# Look for "** CUSTOMIZABLE stuff here" comments; make changes as necessary...

# load all required libraries
library(Hmisc)
library(chron)
library(sp)
library(maptools)
library(rgdal)
library(fields)

# store current working directory for use later in script
basedir <- getwd()

# read in command-line arguments
arguments <- commandArgs(trailingOnly=TRUE)

grep_txt <- arguments[1]

# "USE_EXISTING" argument will cause script to attempt to skip the 
# data munging step and jump directly to the interpolation step.
# existing data will have been created with a previous run of the script.
if(length(arguments)>1 & "USE_EXISTING" %in% arguments) {
  use.existing <- TRUE
} else {
  use.existing <- FALSE
}  

if(length(arguments) == 0) stop(paste("\n\nMust supply the desired element",
    " (i.e. PRCP, TMAX, TMIN, SNOW, SNWD)\n\n"))

# set defaults depending on meteorological element being munged/interpolated
if(grep_txt=="PRCP") {
    title_txt <- "TPS-Interpolated precipitation"
    nc.name <- "prcp"
    tsproc.abbr <- "pr"
    existing.data<-"prcp_data.r"
    swb.name <- "PRECIP"
} else if(grep_txt=="TMAX") {
    title_txt <- "TPS-Interpolated max. air temperature"    
    nc.name <- "tmax"
    tsproc.abbr <- "tx"
    existing.data<-"tmax_data.r"    
    swb.name <- "TMAX"    
} else if(grep_txt=="TMIN") {
    title_txt <- "TPS-Interpolated min. air temperature"        
    nc.name <- "tmin"
    tsproc.abbr <- "tn"
    existing.data<-"tmin_data.r"    
    swb.name <- "TMIN"    
} else if(grep_txt=="SNOW") {
    title_txt <- "TPS-Interpolated daily snowfall"        
    nc.name <- "snow"
    tsproc.abbr <- "sf"
    existing.data<-"snow_data.r"    
    swb.name <- "SNOWFALL"    
} else if(grep_txt=="SNWD") {
    title_txt <- "TPS-Interpolated snow depth"        
    nc.name <- "snwd"
    tsproc.abbr <- "sd"
    existing.data<-"snwd_data.r"    
    swb.name <- "SNOWCOVER"    
} else if(grep_txt=="AWND") {
    title_txt <- "TPS-Interpolated average wind speed"        
    nc.name <- "awnd"
    tsproc.abbr <- "wd"
    existing.data<-"awnd_data.r"    
    swb.name <- "NONE"    
} else if(grep_txt=="RWND") {
    title_txt <- "TPS-Interpolated resultant wind speed"        
    nc.name <- "rwnd"
    tsproc.abbr <- "rw"
    existing.data<-"rwnd_data.r"    
    swb.name <- "NONE"    
} else if(grep_txt=="MNRH") {
    title_txt <- "TPS-Interpolated min relative humidity"        
    nc.name <- "mnrh"
    tsproc.abbr <- "rhn"
    existing.data<-"mnrh_data.r"    
    swb.name <- "NONE"    
} else if(grep_txt=="MXRH") {
    title_txt <- "TPS-Interpolated max relative humidity"        
    nc.name <- "mxrh"
    tsproc.abbr <- "rhx"
    existing.data<-"mxrh_data.r"    
    swb.name <- "NONE"        
} else if(grep_txt=="PSUN") {
    title_txt <- "TPS-Interpolated pct of possible sunshine"        
    nc.name <- "psun"
    tsproc.abbr <- "ps"
    existing.data<-"psun_data.r"    
    swb.name <- "NONE"        
} else if(grep_txt=="WTEQ") {
    title_txt <- "TPS-Interpolated water equivalent of snow"        
    nc.name <- "wteq"
    tsproc.abbr <- "we"
    existing.data<-"wteq_data.r"    
    swb.name <- "NONE"        
} else {
    stop(paste("\n\nYou supplied an unrecognized meteorological element. Must be one of:",
      "\n  PRCP\n  TMAX\n  TMIN\n  MNRH\n  MXRH\n  PSUN\n  SNOW\n  SNWD\n",
      " WTEQ\n  AWND\n  RWND\n\n"))
}          

# include the script that actually writes out to the NetCDF file
source("fxn_write_NetCDF.r")

# if the data munging step has already been done, we'll jump right into the
# interpolations step
if(use.existing==TRUE) {
  load(existing.data)
  use.existing <- TRUE
}  

#-------------------------------------------------------------------------------
# DEFINE SOME FUNCTIONS
#-------------------------------------------------------------------------------

# function to convert a chron object to a text string ( mm/dd/yyyy)
chron2datetxt <- function(x) {

  mm <- as.numeric(months(x))
  dd <- days(x)
  yy <- as.character(years(x))
  
  datetxt <- sprintf("%02i/%02i/%s",mm,dd,yy)
  
  return(datetxt)

}

# function to trim whitespace
trimws <- function (x, left = TRUE, right = TRUE) 
{
    res <- x
    if (left) 
        res <- sub("^[[:space:]]+", "", res)
    if (right) 
        res <- sub("[[:space:]]+$", "", res)
    return(res)
}

# function written to take the cumbersome row-oriented format used by NCDC
# and convert that data into a data frame
rows2columns <- function(temp, values, dates, flag1, flag2) {
  # transpose, unlist, and vectorize the values and dates
  temp.values<-as.vector(unlist(t(temp[,values])))
  temp.dates<-as.vector(dates(unlist(t(temp[,dates]))))
  temp.times<-as.vector(times(unlist(t(temp[,dates]))))  
  temp.month<-months(temp.dates)
  temp.day<-days(temp.dates)    
  temp.year<-years(temp.dates)    
  temp.hours<-hours(temp.times)
  temp.minutes<-minutes(temp.times)
  temp.seconds<-as.vector(seconds(unlist(t(temp[,dates]))))    
  # transpose, unlist, and vectorize the values and dates
  temp.flag1<-as.vector(unlist(t(temp[,flag1])))
  temp.flag2<-as.vector(unlist(t(temp[,flag2])))

  # create new data frame with the transposed, unlisted, vectorized values
  dat<-data.frame(values=as.numeric(as.character(temp.values)),
     dates=temp.dates,
     month=as.numeric(temp.month),     
     day=as.numeric(as.character(temp.day)),          
     year=as.numeric(as.character(temp.year)),          
     hours=as.numeric(as.character(temp.hours)),
     minutes=as.numeric(as.character(temp.minutes)),
     seconds=as.numeric(as.character(temp.seconds)),          
     flag1=temp.flag1, flag2=temp.flag2)
  # remove anything else coded as "missing"
  dat <- subset(dat,dat$flag1 != "M") 
  # remove anything else coded as "accumulated"
  dat <- subset(dat,dat$flag1 != "A") 
  dat$frac <- dat$hours / 23.

  return(dat)   
}    

# given a y-coordinate, the lower and upper y coordinates
# associated with the grid domain, return the
# corresponding row number
get_row <- function(yll,yur,n.rows,ywtm) {

  rownum <- trunc(n.rows * (ywtm - yll) / (yur - yll))
  return(rownum)

}

# given an x-coordinate, the left and right x coordinates
# associated with the grid domain, return the
# corresponding column number
get_col <- function(xll,xur,n.cols,xwtm) {

  colnum <- trunc(n.cols * (xwtm - xll) / (xur - xll))
  return(colnum)

}

#-------------------------------------------------------------------------------
# END of FUNCTION DEFINITIONS
#-------------------------------------------------------------------------------

# ** CUSTOMIZABLE stuff here
setwd(basedir)
start.date.txt <- "01/01/2000"
start.date<-chron(start.date.txt)
start_year <- as.character(years(start.date))
end.date.txt <- "12/31/2000"
end.date<-chron(end.date.txt)
end_year <- as.character(years(end.date))

jul.origin<-c(1,1,as.numeric(start_year))
origin_txt <- paste("days since ",start_year,"-01-01 00:00:00",sep="")
jul.startday<-julian(1,1,as.numeric(start_year),origin=jul.origin)
jul.endday<-julian(12,31,as.numeric(end_year),origin=jul.origin)

num.decimals <- c(2,1,1,1,1,1,1,1,1,1,1)

# define some file names
data_fname <- "COOP_Data.txt"
station_fname <- "stations.txt"

# number of stations to pick at random for interpolations
# set to negative number to disable; this was useful in testing the script
num.stations <- -999

# ** lambda value is used in the fields thin-plate spline routine to control the level of smoothing
# ** CUSTOMIZABLE stuff here
lambda <- 3.5e-4

# define projection parameters for WTM 83/91
# (any other user of this script will need to substitute the projection parameters for
# their area of interest below...)
# ** CUSTOMIZABLE stuff here
proj4_WTM8391<-c("+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000")
proj4_LL<-"+proj=latlong +ellps=GRS80 +datum=NAD83"
proj4_WGS84<-"+proj=latlong +datum=WGS84"

# Define the dimensions of the OUTPUT grid system.
# This must match EXACTLY with the grid system used in the
# subsequent SWB runs
# ** CUSTOMIZABLE stuff here
n.cols<- 340
n.rows<- 322
n.cols.out<- 340
n.rows.out<- 322
cell.size<- 1600
cell.size.out <- 1600.0
nodata.value<- -99999.00
xll.corner<- 227200. 
yll.corner<- 220800. 

# calculate the corner coordinates given the grid parameters above
xur.corner<- xll.corner + (n.cols)*cell.size 
yur.corner<- yll.corner + (n.rows)*cell.size 

x0 <- seq(xll.corner+0.5*cell.size,by=cell.size,length.out=n.cols)
y0 <- seq(yll.corner+0.5*cell.size,by=cell.size,length.out=n.rows)
x.out <- seq(xll.corner+0.5*cell.size.out,length.out=n.cols.out,by=cell.size.out)
y.out <- seq(yll.corner+0.5*cell.size.out,length.out=n.rows.out,by=cell.size.out)

# create the data structure needed by the SpatialPoints routine
coords<-expand.grid(x=x.out,y=y.out)
gridpts.WTM.sp <- SpatialPoints(coords=coords, proj4string=CRS(proj4_WTM8391))

# here we create a geographic (unprojected) set of coordinates corresponding to the
# output grid locations
gridpts.LL.sp <- spTransform(gridpts.WTM.sp,CRS(proj4_WGS84))

netcdf_filename <- paste(grep_txt,"_",n.rows.out,"_",n.cols.out,"_",
    start_year,"-",end_year,".nc",sep="")

# more munging to convert the grid coordinates into matrix form
lon=matrix(nrow=n.rows.out,ncol=n.cols.out,data=coordinates(gridpts.LL.sp)[,1],byrow=TRUE)
lat=matrix(nrow=n.rows.out,ncol=n.cols.out,data=coordinates(gridpts.LL.sp)[,2],byrow=TRUE)

# create the list of items needed to create the NetCDF header
# ** CUSTOMIZABLE stuff here
nc <- list(netcdf.filename=netcdf_filename,
            n_rows=n.rows.out,
            n_cols=n.cols.out,
            dtype=grep_txt,
            lon=t(lon),
            lat=t(lat),
            x0=x.out,
            y0=y.out,
            cell_size=cell.size.out,
            len_units="meters",
            dtype=grep_txt,
            title_txt=title_txt,
            institution_txt="U.S. Geological Survey",
            origin_txt=origin_txt,
            calendar_txt="gregorian",
            jul_startday=jul.startday,
            jul_endday=jul.endday)

# create a data type to hold the subsequently generated field names
daily.txt<-character(31*4)

# generate field names for the NCDC daily output file
for(i in 1:31) {
  k<- (i-1) * 4
  daily.txt[k + 1]<-sprintf("%s%i", "MMDDYYYY",i)
  daily.txt[k + 2]<-sprintf("%s%i", "VAL",i)
  daily.txt[k + 3]<-sprintf("%s%i", "FLAG1.",i)
  daily.txt[k + 4]<-sprintf("%s%i", "FLAG2.",i)
}

# assemble field names for the NCDC daily output file
header.txt<-c("DATA.ORIGIN","COOPID","WBAN.STA.NUM","STA.NAME",
  "DIVISION.NUMBER","MET.ELEMENT","MET.UNITS","YEAR.MONTH", daily.txt )

# make the call to actually write out the NetCDF header
nc.out <- nc_header(nc)

if(use.existing==FALSE) {

# read in shapefile for Wisconsin counties
# ** CUSTOMIZABLE stuff here
wi.counties.sp<-readShapePoly("ctyppoly.shp",proj4string=CRS(proj4_WTM8391))

# read in station data
stations<-read.delim(station_fname,header=TRUE,stringsAsFactors=FALSE)
colnames(stations) <- c("COOPID","WBANID","STATION.NAME","COUNTRY",
  "STATE","COUNTY","CLIMATE.DIVISION","LAT.DEG","LAT.MIN","LON.DEG","LON.MIN","ELEVATION")
stations$unique.id<-make.unique(as.character(stations$COOPID))
cat("Read in data for",nrow(stations),"stations\n")

# *** using some unix magic to identify the stations that have the data we're looking for ***
if(.Platform$OS.type == "unix" ) {  # use single quote '
  coop_id_subset <- system(paste("grep",grep_txt,data_fname,"| gawk -F, \'{ print $2}\' | uniq "), intern=TRUE)
} else {  # use double quote
  coop_id_subset <- shell(paste("grep \"",grep_txt,"\" ",data_fname," | gawk -F, \"{ print $2}\" | uniq ",sep=""), intern=TRUE)
}

stations <- subset(stations, stations$COOPID %in% coop_id_subset)
cat("Created a subset of",nrow(stations),"stations that have data type",grep_txt,"\n")

###  ###  ###  ### ###  ###  ###  ### ###  ###  ###  ### ###  ###  ###  ###
## for TESTING the script ONLY!!
# create a smaller subset of stations for faster runtimes
if(num.stations > 0) {
  stations<- subset(stations,stations$COOPID %in% sample(stations$COOPID,num.stations))
}  
###  ###  ###  ### ###  ###  ###  ### ###  ###  ###  ### ###  ###  ###  ###
# calculate coordinates in decimal degrees
stations$LAT.DD <- as.numeric(stations$LAT.DEG) + (as.numeric(stations$LAT.MIN) / 60.)
stations$LON.DD <- as.numeric(stations$LON.DEG) - (as.numeric(stations$LON.MIN) / 60.)
# if no lat-long present, discard
stations<-subset(stations,!is.na(LAT.DD))
stations<-subset(stations,!is.na(LON.DD))

# now create the needed data structures to create a
# SpatialPointsDataFrame for the stations we will include in the interpolations
coords<-data.frame(x=stations$LON.DD,y=stations$LAT.DD)
stations.LL.sp <- SpatialPointsDataFrame(coords=coords, proj4string=CRS(proj4_LL),
   data=stations)

# here we create a WTM 83/91 version of the station location
stations.WTM.sp <- spTransform(stations.LL.sp,CRS(proj4_WTM8391))
stations.WTM.sp$x <- as.data.frame(coordinates(stations.WTM.sp))$x
stations.WTM.sp$y <- as.data.frame(coordinates(stations.WTM.sp))$y

# define the type and size of some of the data frame elements
stations.WTM.sp$value<-numeric(nrow(stations.WTM.sp))
stations.WTM.sp$numrec<-numeric(nrow(stations.WTM.sp))
stations.WTM.sp$tobs<-numeric(nrow(stations.WTM.sp))

# generate an explicit list of valid dates between the starting and ending dates
date.vals<-seq(start.date,end.date)

# set up empty data frames that possess all valid dates within date range
dat.all<-data.frame(dates=date.vals)

temp_fname <- paste(nc.name,"_temp.dat",sep="")

##########################################################
# BEGIN LOOP:  iterate over all STATIONS in station file
##########################################################
for(k in 1:nrow(stations.WTM.sp)) {

  output.file <- paste(stations.WTM.sp$STATION.NAME[k],".txt",sep="")

  cat(paste(stations.WTM.sp$STATION.NAME[k],"\n"))

  # *** using grep to pare down file before bringing it into R ***
if(.Platform$OS.type == "unix" ) {  # use single quote '
  system(paste("grep",stations.WTM.sp$STATION.NAME[k],data_fname," > ",temp_fname), intern=TRUE)
} else {
  shell(paste("grep \"",stations.WTM.sp$STATION.NAME[k],"\" ",data_fname," > ",temp_fname,sep=""), intern=TRUE)
}

  # read in a copy of the GREPPED data corresponding to the current station
  w<-read.csv(temp_fname,header=FALSE,stringsAsFactors=FALSE)

  # assign column names to the GREPPED input NCDC file
  colnames(w)<-header.txt

  # rip the YEAR.MONTH column into separate YEAR and MONTH values
  w$year<-substr(w$YEAR.MONTH,1,4)
  w$month<-substr(w$YEAR.MONTH,5,6)

  ## the following 4 statements return INDEX values used later on in the script
  ## to create a data file from the NCDC formatted text file

  # determine which columns hold MONTH-DAY-YEAR info
  dates<-agrep("MMDDYYYY",colnames(w))
  # determine which columns hold the values we're interested in
  values<-agrep("VAL",colnames(w))
  # determine which columns hold the data flags
  flag1<-values+1
  flag2<-values+2

  # iterate over date columns; convert each value to a chron object
  for(i in dates) {
  
    datetxt=paste(w$month,substr(w[[i]],1,nchar(w[[i]])-2),w$year,sep="/")
    timevals <- substr(w[[i]],nchar(w[[i]])-1,nchar(w[[i]]))
#    timevals[timevals<0 | timevals > 23] <- 18

    timetxt <- ifelse(timevals>=0 & timevals <=23,
       paste(timevals,":00:00",sep=""),
       "23:59:59")

#    w[[i]]<-chron(dates=paste(w$month,substr(w[[i]],1,nchar(w[[i]])-2),w$year,sep="/"),
#                  times=paste(substr(w[[i]],nchar(w[[i]])-1,nchar(w[[i]])),
#                        ":00:00",sep=""))

     w[[i]] <- ifelse(timetxt=="",
                 chron(dates=datetxt),
                 chron(dates=datetxt, times=timetxt))

  }

  # for mapping the stations, assume the default TIME of OBSERVATION is "NA"
  stations.WTM.sp$tobs[k] <- NA  

  # pare down the input text based on the METEOROLGIC ELEMENT we're after
  temp.noaa<-w[agrep(grep_txt,w$MET.ELEMENT),]
  # eliminate duplicate YEAR-MONTH entries
  temp.noaa<-subset(temp.noaa,!duplicated(temp.noaa$YEAR.MONTH))

  # if we still have any records at this point, make a data frame
  if(nrow(temp.noaa) > 0) {
    temp.df<-rows2columns(temp.noaa, values=values, dates=dates, flag1=flag1,
             flag2=flag2)    
    
      # perform basic data transformations based on the METEOROLGIC ELEMENT
      if(grep_txt == "PRCP") {
         temp.df <- subset(temp.df,values >= 0 & values < 2000)  # eliminate negative values or precip values > 20 inches per day
         temp.df <- subset(temp.df,!(flag1 %in% c("M","A")))  # eliminate values with "missing" or "accumulated" flags
         temp.df$values <- temp.df$values / 100.  # NWS stores precip in hundredths of inches

      } else if(grep_txt == "SNWD") {
         temp.df <- subset(temp.df,values >= 0 & values <= 100)  # this may need to be increased if we're in Houghton, MI!

      } else if(grep_txt == "SNOW") {
         temp.df <- subset(temp.df,values >= 0 & values <= 200)
         temp.df$values <- temp.df$values / 10.  # NWS stores SNOW in TENTHS of inches       

      } else if(grep_txt %in% c("TMIN","TMAX")) {
         temp.df <- subset(temp.df,values > -150 & values < 150)

      } else if(grep_txt %in% c("AWND","RWND")) {
         temp.df <- subset(temp.df,values >= 0 & values < 800)
         temp.df$values <- temp.df$values / 10.  # NWS stores WIND SPEED in tenths of MPH       

      } else if(grep_txt %in% c("PSUN","MNRH","MXRH")) {
         temp.df <- subset(temp.df,values >= 0 & values <= 100)

      } else if(grep_txt %in% c("WTEQ")) {
         temp.df <- subset(temp.df,values >= 0 & values <= 40)

      } else {                                
         stop("Unknown data element supplied.  Stopping.\n\n")
      }  
     
      # set the TIME of OBSERVATION to the median value recorded in file
      stations.WTM.sp$tobs[k] <- median(temp.df$hours,na.rm=TRUE)

      # start with fresh data frame; ALL dates present
      temp_alldates.df<-data.frame(dates=date.vals)  
  
      # copy values where they exist
      temp_alldates.df <- merge(temp_alldates.df,temp.df,by.x="dates",by.y="dates",all=TRUE)

      # attempt a crude time of observation correction
      if(grep_txt %in% c("PRCP","SNOW","SNWD")) {
  
        temp_alldates.df$interp_val <- c(temp_alldates.df$value[1:nrow(temp_alldates.df)-1]
                        * (temp_alldates.df$frac[1:nrow(temp_alldates.df)-1]) +
                       temp_alldates.df$value[2:nrow(temp_alldates.df)] 
                       * (1.0 - temp_alldates.df$frac[1:nrow(temp_alldates.df)-1]),
                       temp_alldates.df$value[nrow(temp_alldates.df)] 
                       * temp_alldates.df$frac[nrow(temp_alldates.df)])
      } else {
         temp_alldates.df$interp_val <- temp_alldates.df$value
      }                       
                     
      dat.all<-merge(dat.all,temp_alldates.df[,c("dates","interp_val")],
                   by.x="dates",by.y="dates",all=TRUE,fill=NA)
      n <- length(colnames(dat.all))
      colnames(dat.all)[n]<-stations$unique.id[k]
  
  }  
}
##########################################################
#### END LOOP:  iterate over all STATIONS in station file
##########################################################

dat.all$year<-as.character(years(dat.all$date))
dat.all$month<-as.character(months(dat.all$date))
dat.all$day<-as.character(days(dat.all$date))
dat.all$value<-numeric(nrow(dat.all))

### PLOT UP a map of all stations; thematic elements are TOBS and NUMREC
pdf(file=paste(existing.data,".pdf",sep=""),width=8,height=8)

# tally the number of records
for(k in stations.WTM.sp$unique.id) {
  stations.WTM.sp$numrec[stations.WTM.sp$unique.id==k]<-length(na.omit(dat.all[,which(colnames(dat.all) %in% k)]))
}  

plot(wi.counties.sp,bg="grey85")
colorpal <- colorRampPalette(c("darkgreen","yellow","red"))
ncols <- length(sort(unique(stations.WTM.sp$tobs)))
if(ncols > 0) {
  palette(colorpal(max(3,ncols)))
  points(stations.WTM.sp,col=stations.WTM.sp$tobs,
    cex=ifelse(is.na(stations.WTM.sp$numrec),0.75,stations.WTM.sp$numrec*0.75/nrow(dat.all)+0.75),
    pch = ifelse(is.na(stations.WTM.sp$numrec),"x",stations.WTM.sp$tobs))
  legend("topright", legend=sort(unique(stations.WTM.sp$tobs)),
    pch=sort(unique(stations.WTM.sp$tobs)),
    col=sort(unique(stations.WTM.sp$tobs)))
} else {
  legend("center",title="NO VALID DATA FOUND",cex=2,
         title.col="red")
}    
dev.off()

save.image(existing.data)

}

################################################################################
################################################################################
################################################################################
## Begin interpolation routine
################################################################################
################################################################################
################################################################################

ndays <- end.date - start.date + 1

for(i in 1:ndays) {

     # peel off today's point data and stick into a temporary data frame
     today <- start.date + i - 1
     cat("processing: ",as.character(today),"\n")         

     # get NUMERIC values for today's date...leave out YEAR, MONTH DAY, etc.     
     todays_data <- dat.all[dat.all$date==today,(!colnames(dat.all) %in% 
         c("dates","year","month","day","value"))]

     # peel off today's point data and stick into a temporary data frame
     temppts <- data.frame(val=as.numeric(todays_data), unique.id=names(todays_data))
     
     # doing the following merge to yield a table with the STATION.NAME attribute present
     pts.mrg <- merge(x=stations.WTM.sp,y=temppts,by.x="unique.id",by.y="unique.id",all.x=TRUE)
     pts <- data.frame(x=pts.mrg$x,y=pts.mrg$y,z=pts.mrg$val)
     # eliminate "NA" values
     pts <- subset(pts,!is.na(pts$z))

################################################################################
## BEGIN INTERPOLATION
################################################################################
    if(nrow(pts) ==0) {
    
      # create a set of dummy points, all with "missing data" value
      pts <- data.frame(x=pts.mrg$x,y=pts.mrg$y,z=pts.mrg$val)    
      pts$z <- -999
    
    } else if(nrow(pts)<5) {
    
      # create a dummy set of data, all with mean data value
      meanval <- mean(pts$z)
      pts <- data.frame(x=pts.mrg$x,y=pts.mrg$y,z=pts.mrg$val)    
      pts$z <- meanval      
    
    }

    # fit a thin-plate spline to the data; for precip, fit to the square root of the data
    if(grep_txt %in% c("PRCP","SNOW","SNWD")) {
      fit<-Tps(cbind(pts$x,pts$y),sqrt(pts$z))
    } else {
      fit<-Tps(cbind(pts$x,pts$y),pts$z)
    }  

    # create a list of the coordinates for which we want predictions at
    grid.l<-list(x=x0,y=y0)
  
    if(lambda >= 0) {
      outp<-predict.surface(fit,grid.list=grid.l,extrap=T, lambda = lambda)
    } else {
        # allow smoothing parameter to be calculated by general cross-variance minimization
      outp<-predict.surface(fit,grid.list=grid.l,extrap=T)  
    }  

    if(grep_txt %in% c("PRCP","SNOW","SNWD")) {
       outp$z <- outp$z^2
    } 
                   
    #force interpolated grid values to remain within original data values
    outp$z[outp$z<min(pts$z)]<-min(pts$z)       
    # if the interpolation has returned a miniscule PRECIP value, call it zero!
    if(grep_txt %in% c("PRCP","SNOW")) {
      outp$z[outp$z<0.003]<-0
    }

    # enforce a cap: interpolated value cannot be bigger than any of the observed values
    outp$z[outp$z>max(pts$z)]<-max(pts$z)
       
    # put current TIME value out to NetCDF file  
    var.put.nc(nc.out, "time", jul.startday + i -1,start=i)
  
    ## WRITE data out to NetCDF file....
    if(grep_txt=="PRCP") {                                      #apply a SCALE FACTOR
      var.put.nc(nc.out, nc.name, as.integer(outp$z*100),start=c(1,1,i),
        count=c(n.cols.out,n.rows.out,1))
    } else if(grep_txt %in% c("SNOW","SNWD","AWND","RWND")) {   #apply a SCALE FACTOR
      var.put.nc(nc.out, nc.name, as.integer(outp$z*10),start=c(1,1,i),
        count=c(n.cols.out,n.rows.out,1))
    } else if(grep_txt %in% c("TMIN","TMAX")) {                 # apply an OFFSET
      var.put.nc(nc.out, nc.name, as.integer((outp$z+100)*100),start=c(1,1,i),
        count=c(n.cols.out,n.rows.out,1))
    } else {                                                    # output with no OFFSET or SCALE FACTOR
      var.put.nc(nc.out, nc.name, as.integer(outp$z),start=c(1,1,i),
        count=c(n.cols.out,n.rows.out,1))        
    }      
  
}  

close.nc(nc.out)
