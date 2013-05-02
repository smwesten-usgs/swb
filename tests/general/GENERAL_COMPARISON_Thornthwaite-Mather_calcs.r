myargs <- commandArgs(trailingOnly = TRUE)

#setwd("d:/SMWData/Source_Code/swb/tests/general")
source("Thornthwaite_Mather_cell_class.R")

TOLERANCE <- 0.01

swb_exe <- myargs[1]

swb_ctl <- "recharge_single_cell.ctl"

my_control_file <- c(
  "GRID 1 1 2165054. 255067. 161.1",
  "GRID_LENGTH_UNITS FEET",
  "GROWING_SEASON 133 268 T",
  "SUPPRESS_SCREEN_OUTPUT",
  "OUTPUT_FUTURE_PATH ",
  "OUTPUT_PATH ",
  "INITIAL_ABSTRACTION_METHOD HAWKINS",
  "PRECIPITATION SINGLE_STATION",
  "TEMPERATURE SINGLE_STATION",
  "FLOW_DIRECTION CONSTANT 1",
  "SOIL_GROUP CONSTANT 1",
  "LAND_USE CONSTANT 1",
  "LAND_USE_LOOKUP_TABLE LU_LOOKUP_single_cell.txt",
  "WATER_CAPACITY CONSTANT 3.0",
  "INITIAL_SOIL_MOISTURE CONSTANT 100",
  "INITIAL_SNOW_COVER CONSTANT 0",
  "RUNOFF C-N DOWNHILL",
  "ET T-M 43",
  "SM T-M soil-moisture-retention-extended.grd",
  "SOLVE Coshocton_Climate_1999.txt test_ future_ T T",
  "EOJ")

writeLines(text=my_control_file,
           con=swb_ctl)

# first run SWB
retval <- shell(cmd=paste(swb_exe,swb_ctl, " > swb_command_line_echo.txt", sep=" "),
                intern=TRUE,
                ignore.stdout=FALSE)


climate_1999 <- read.table("Coshocton_climate_1999.txt", header=FALSE, skip=1)
colnames(climate_1999) <- c("Month","Day","Year","TMean","Precip","RHMean","TMax","TMin","WindVel","RHMin","PctSun")
climate_1999$Date <- as.Date(paste(climate_1999$Year,climate_1999$Month,climate_1999$Day,sep="-"))

# now run the equivalent procedures in R
mean_monthly_temps <- monthly_temps(climate_1999)

mean_monthly_temps_C <- FtoC(mean_monthly_temps)

i <- TM_i(mean_monthly_temps_C)
I <- TM_I(i)
a <- TM_a(I)

df <- climate_1999

mycel <- cellclass(RootingDepth=2,
                   AWC=3,
                   latitude=43.,
                   grow_start=133,
                   grow_stop=268)

mycel$initialize(sm_init=6,
                 snowcover_init=0,
                 max_int_growing=0.05,
                 max_int_nongrowing=0.)

mycel$init_tm(df)

mycel$opendaily("simple_TM_calculation.csv")

for (n in 1:nrow(df)) {
  
  mycel$updateprecip(grossprecip=df$Precip[n],
                     tmax=df$TMax[n],
                     tmin=df$TMin[n])
  
  mycel$processprecip(n)
  
  mycel$processet(df$Month[n], df$Day[n], df$Year[n] )
  
  mycel$massbalance()
  
  mycel$dumpdaily(df$Date[n])
  
}


v_in <- read.csv("SWB_daily_MEAN_values.csv", header=TRUE, as.is=TRUE, skip=2)
v_base <- read.csv(mycel$filename, header=TRUE, as.is=TRUE)


v_in$Date<-as.Date(v_in$Date,format="%m/%d/%Y")
v_in$Year <- format(v_in$Date, format="%Y")

v_base$Date<-as.Date(v_base$Date,format="%m/%d/%Y")
v_base$Year <- format(v_base$Date, format="%Y")  

# ensure vector lengths are the same, assuming the date ranges are comparable
v_base <- v_base[v_base$Date %in% v_in$Date, ]

colnames(v_base)[colnames(v_base) == "REFERENCE_ET"] <- "POT_ET"
colnames(v_in)[colnames(v_in) == "REFERENCE_ET"] <- "POT_ET"

lTEST <- TRUE

for (i in sort(unique(v_in$Year))) {

  vb <- subset(v_in,v_in$Year==i)  # swb results
  vbb <- subset(v_base,v_base$Year==i)  # R script results
  
  if (nrow(vbb) < nrow(vb)) next
  
  for (j in 2:(ncol(vb)-1)) {
    
    y<-vb[ ,j ]  # swb results
    
    if ( any(colnames(vbb)==colnames(vb)[j] ) ) {
      x<-as.vector( unlist( vbb[which(colnames(vbb)==colnames(vb)[j] ) ]  ) )
      
      diffvals <- abs(y) - abs(x)
      
      cat(paste(colnames(vb)[j]),":", signif(max(diffvals), digits=3), "\n", sep="")      
      
      if (any(diffvals > TOLERANCE ) ) {
      
        indices <- which(diffvals > TOLERANCE)
        for (index in indices) {
          cat(colnames(vb)[j],":", format(vb[index,"Date"], "%m-%d-%Y"),
              y[index], x[index],"  rpd: ",rpd(y[index],x[index]),"\n")
        }
        
        lTEST <- FALSE
      }
      
    }
    
  }
  
}

if( lTEST == TRUE ) {
  cat("PASS")    
} else {
  cat("FAIL")  
}
