setwd("D:\\SMWData\\Source_Code\\swb\\share\\R-scripts")

library(chron)

cellclass <- function(RootingDepth, AWC, latitude, grow_start, grow_stop) {
  
  cel = list(
    rooting_depth=RootingDepth,
    awc=AWC,
    smc=RootingDepth * AWC,
    latitude=latitude,
    grow_start=grow_start,
    grow_stop=grow_stop,
    sm=0.,
    grossprcp=0.,
    prcp=0.,
    potet=0.,
    actet=0.,
    tmax=0.,
    tmin=0.,
    tmean=0.,
    snowcover=0.,
    interception=0.,
    apwl=0.,
    recharge=0.    
    
    )
  
  cel$initialize = function(sm_init, snowcover_init, max_int_nongrowing, max_int_growing) {
    assign('sm',sm_init, envir=cel)
    assign('snowcover',snowcover_init, envir=cel)
    assign('potentialinterception_nongrow', max_int_nongrowing, envir=cel)
    assign('potentialinterception_grow', max_int_growing, envir=cel)    
  }

  cel$init_tm = function(climate_df) {
  
    cel$mean_monthly_temps <- monthly_temps(climate_df)
    
    cel$mean_monthly_temps_C <- FtoC(cel$mean_monthly_temps)
    
    cel$i <- TM_i(cel$mean_monthly_temps_C)
    cel$I <- TM_I(cel$i)
    cel$a <- TM_a(cel$I)
    cel$daysinmonth <- getdaysinmonth(climate_df$Year[1])
    
  }
  
  cel$updateprecip <- function(grossprecip, tmax, tmin) {
    cel$grossprcp <- grossprecip
    #assign('grossprcp', grossprecip, envir=cel)
    assign('tmax', tmax, envir=cel)
    assign('tmin', tmin, envir=cel)
    assign('tmean', mean(c(tmax,tmin)), envir=cel)    
  }
  
  cel$processprecip <- function(doy) {
    
    rFREEZING <- 32.
    rMELT_INDEX <- 1.5 * 5/9 / 25.4    # 1.5 mm/degC
    
    lFREEZING <- ifelse (cel$tmean - (cel$tmax-cel$tmin) / 3.0 <= rFREEZING, TRUE, FALSE)
    
    if(doy < cel$grow_start || doy >= cel$grow_stop ) {
      netrainfall <- cel$grossprcp - cel$potentialinterception_nongrow
    } else {
      netrainfall <- cel$grossprcp - cel$potentialinterception_grow
    }  
    
    if ( netrainfall < 0. ) netrainfall <- 0.
    
    assign('interception', cel$grossprcp - netrainfall, envir=cel)
    assign('netrainfall', netrainfall, envir=cel)
    
    if (lFREEZING ) {
      
      cel$snowcover <- cel$snowcover + netrainfall
      cel$netrainfall <- 0.
    } else {
      cel$netrainfall <- netrainfall
    } 


    potentialmelt <- ifelse ( cel$tmean > rFREEZING, rMELT_INDEX * ( cel$tmax - rFREEZING ), 0.) 
    
    if ( cel$snowcover > potentialmelt ) {
      cel$snowmelt <- potentialmelt
      cel$snowcover <- cel$snowcover - cel$snowmelt
    } else {   # not enough snowcover to satisfy the amount that *could* melt
      cel$snowmelt <- cel$snowcover
      cel$snowcover <- 0.
    }
    
  }
    
  cel$massbalance <- function() {
    
    APWL_CAP <- -40.69
    
    cel$netinfil <- cel$netrainfall + cel$snowmelt
    cel$pminuspe <- cel$netinfil - cel$potet
    
    cel$old_sm <- cel$sm
    
    if (cel$pminuspe < 0.) {
      
      cel$apwl <- max(APWL_CAP,(cel$apwl + cel$pminuspe))
      cel$old_sm <- cel$sm
      cel$sm <- calc_SoilMoisture(cel$smc, cel$apwl)
      cel$deltasm <- cel$sm - cel$old_sm
      
      if (cel$deltasm > abs(cel$pminuspe)) {
        cel$sm <- cel$old_sm + cel$pminuspe
        cel$deltasm <- cel$sm - cel$old_sm        
      }
      
      cel$actet <- min(cel$netinfil + abs(cel$deltasm), cel$potet)
      cel$deficit <- cel$potet - cel$actet
      cel$surplus <- 0.
      
    } else {   # precip *EXCEEDS* Potential ET
      
      cel$deficit <- 0.
      cel$surplus <- max(0., cel$sm + cel$pminuspe - cel$smc)
      
      cel$sm <- min(cel$smc, (cel$sm + cel$pminuspe) )
      cel$deltasm <- cel$sm - cel$old_sm
      cel$actet <- cel$potet
      cel$apwl <- calc_APWL(cel$smc, cel$sm) 
      
    }  
    
    cel$recharge <- cel$surplus    
    
  }  
  
  cel$processet <- function(month, day, year, latitude) {
    
    lFREEZING = 32.
    
    TM_e <- TM_e(AirTemp=FtoC(cel$tmean), HeatIndex_I=cel$I, TM_a=cel$a)
    cel$potet <- TM_ea(month, day, year,cel$daysinmonth,cel$latitude,TM_e) / 25.4
    
    if(cel$tmean < lFREEZING) cel$potet <- 0.
    
  }

  cel$opendaily <- function(filename) {
    
    header <- paste("Date","MIN_TEMP","MAX_TEMP","AVG_TEMP","GROSS_PRECIP","INTERCEPTION","SNOWCOVER","SNOWMELT","NET_RAINFALL",
                    "NET_INFIL","P_MINUS_PE","POT_ET","ACT_ET","CHG_IN_SOIL_MOIS","SOIL_MOISTURE", "SM_SURPLUS","SM_DEFICIT",
                    "SM_APWL","RECHARGE", sep=",")
    
    cel$filename <- filename
    write(x=header, file=filename)
    
  }
  
  cel$dumpdaily <- function(currentdate) {
    
    SIGDIGITS <- 5
    
    outputtext <- paste(format(currentdate, "%m/%d/%Y"),
          signif(cel$tmin, digits=SIGDIGITS),                        
          signif(cel$tmax, digits=SIGDIGITS),                        
          signif(cel$tmean, digits=SIGDIGITS),                        
          signif(cel$grossprcp, digits=SIGDIGITS),
          signif(cel$interception, digits=SIGDIGITS),
          signif(cel$snowcover, digits=SIGDIGITS),                        
          signif(cel$snowmelt, digits=SIGDIGITS),                                                
          signif(cel$netrainfall, digits=SIGDIGITS),
          signif(cel$netinfil, digits=SIGDIGITS),
          signif(cel$pminuspe, digits=SIGDIGITS),
          signif(cel$potet, digits=SIGDIGITS),
          signif(cel$actet, digits=SIGDIGITS),
          signif(cel$deltasm, digits=SIGDIGITS),                        
          signif(cel$sm, digits=SIGDIGITS),
          signif(cel$surplus, digits=SIGDIGITS),
          signif(cel$deficit, digits=SIGDIGITS),
          signif(cel$apwl, digits=SIGDIGITS),                        
          signif(cel$recharge, digits=SIGDIGITS), sep=",")
    
    write(x=outputtext, file=cel$filename, append=TRUE)    
    
  }
  
  cel$get <- function(itemname) {
    
    retval <- get(itemname, envir=cel)
    return(retval)
  }
  
  cel <- list2env(cel)
  class(cel) <- "model_cell"
  
  return(cel)
  
}


TM_ea <- function(Month, Day, Year, daysinmonth, Latitude, TM_e) {
  
  h <- get_daylight_hours(Month, Day, Year, Latitude)
  numdaysinmonth <- daysinmonth[Month]
  TM_ea <- TM_e * (1/numdaysinmonth) * (h/12)
  return(TM_ea)
  
}

getdaysinmonth <- function(StartYear, EndYear=StartYear) {
  
  dateseq <- seq.dates(from=paste("01","01",StartYear,sep="/"),
                       to=paste("12","31",EndYear,sep="/"), by="days" )
  
  monthlengths <- as.numeric(by(dateseq, INDICES=list(months(dateseq),years(dateseq)), FUN=length))
  return(monthlengths)
  
}

get_daylight_hours <- function(Month, Day, Year, Latitude) {

  Latitude <- Latitude * pi / 180
  
  frac_year <- fractional_year(Month, Day, Year)
  solar_decl <- solar_declination(frac_year)
  sunset_ang <- sunset_angle(Latitude, solar_decl)
  daylight_hrs <- daylight_hours(sunset_ang)
      
  return(daylight_hrs)  
  
}

daylight_hours <- function(Omega_s) {
  
  N <- 24. / pi * Omega_s
  cat(N,"\n")
  return(N)
  
}

sunset_angle <- function(Latitude, Delta) {
  
  Omega_s <- acos( - tan(Latitude) * tan(Delta) )
  return(Omega_s)
  
}

fractional_year <- function( Month, Day, Year ) {
  
  FirstDay <- as.Date(paste(Year,"01","01", sep="-"))
  LastDay <- as.Date(paste(Year,"12","31", sep="-"))
  Today <- as.Date(paste(Year, Month, Day, sep="-"))
  DaysInYear <- as.numeric(LastDay - FirstDay + 1)
  
  DOY <- 2 * pi * ( as.numeric(Today - FirstDay) + 1) / (DaysInYear)
  return(DOY)
  
}

day_of_year <- function( Month, Day, Year ) {
  
  FirstDay <- as.Date(paste(Year,"01","01", sep="-"))
  Today <- as.Date(paste(Year, Month, Day, sep="-"))
    
  DOY <- ( as.numeric(Today - FirstDay) + 1)
  return(DOY)
  
}

solar_declination <- function(fractional_year_radians) {
  
  gam <- fractional_year_radians
  
  DOY <- gam / (2 * pi) * 365.25
  
  Delta1 <- 6.918E-3 - 3.99912E-1 * cos(gam) + 7.0257E-2 * sin(gam) - 6.758E-3 * cos(2*gam)
             + 9.07E-4 * sin(2*gam) - 2.697E-3 * cos(3*gam) + 1.48E-3 * sin(3*gam)
      
  Delta3 <- 0.409 * sin ( gam - 1.39)
  
  return(Delta1)
  
  
}


FtoC <- function(T_Fahrenheit) {
  
  result <- (T_Fahrenheit - 32) * 5/9
  return(result)
  
}

CtoF <- function(T_Celcius) {
  
  result <- T_Celcius * 9/5 + 32
  
}

calc_APWL <- function(MaxSoilMoisture_inches, CurrentSoilMoisture_inches) {

  rTM_slope_term <- 0.478769194198665
  rTM_exp_term <- -1.03678439421169
      
APWL <- -( log10(MaxSoilMoisture_inches ) - log10(CurrentSoilMoisture_inches)) /
           ( rTM_slope_term * MaxSoilMoisture_inches^rTM_exp_term )

#  dpValue = -( log10(REAL(rSWC,kind=T_DBL)) - log10(dpSoilStorage)) / &
#    ( rTM_slope_term * REAL(rSWC,kind=T_DBL)**rTM_exp_term )
  
  
return (APWL)

}

calc_SoilMoisture <- function(MaxSoilMoisture_inches, AccumulatedPotentialWaterLoss_inches) {
  
  rTM_slope_term <- 0.478769194198665
  rTM_exp_term <- -1.03678439421169
  
  APWL <- AccumulatedPotentialWaterLoss_inches
  SWC <- MaxSoilMoisture_inches
  
  CurrentSoilMoisture_inches <- 10^( log10(SWC) - 
                                ( abs(APWL) * rTM_slope_term * SWC^rTM_exp_term  ) )
  
  return(CurrentSoilMoisture_inches)
  
}

TM_I <- function(TM_i) {
  
  result <- sum(TM_i)
  return(result)
  
}

TM_i <- function(MeanMonthlyAirTemp) {
  
  result <- ifelse(MeanMonthlyAirTemp > 0.,(MeanMonthlyAirTemp / 5.0)^1.514, 0.)
  return(result)
  
}

TM_a <- function(HeatIndex_I) {
  
  I <- HeatIndex_I
  
  result <- 6.75E-7 * I^3 - 7.71E-5 * I^2 + 1.7921E-2 * I + 0.49239
  return(result)
    
}

TM_e_hi_temp <- function(AirTemp) {
  
  # These values come from Fig 13, Thornthwaite and Mather (1948)
  temp <- seq(26.5,38,0.5)
  pe <- c(13.5,13.95,14.37,14.78,15.17,15.54,15.89,16.21,16.52,16.80,17.07,17.31,
          17.53,17.72,17.9,18.05,18.18,18.29,18.37,18.43,18.47,18.49,18.5,18.50)
  
  result_list <- approx(x=temp, y=pe, xout=AirTemp)
  result <- as.numeric(unlist(result_list[["y"]]))
  return(result)  
}

# UNADJUSTED Potential ET
TM_e <- function(AirTemp, HeatIndex_I, TM_a) {
  
  t <- AirTemp
  Temp_degC <- FtoC(AirTemp)
  I <- HeatIndex_I
  a <- TM_a
  
  if (Temp_degC < 26.5) {
    result <- ifelse(t > 0, 16. * (10*t / I)^a, 0.)
  } else {
    result <- TM_e_hi_temp(Temp_degC)
  }
  
  
  return(result)
  
}

monthly_temps <- function(climate_df) { 

  mean_monthly_temps <- as.vector(by(climate_df$TMean, INDICES=as.factor(climate_df$Month), FUN=mean))
  
}


#######################################################################################################

climate_1999 <- read.table("Coshocton_climate_1999.txt", header=FALSE, skip=1)
colnames(climate_1999) <- c("Month","Day","Year","TMean","Precip","RHMean","TMax","TMin","WindVel","RHMin","PctSun")
climate_1999$Date <- as.Date(paste(climate_1999$Year,climate_1999$Month,climate_1999$Day,sep="-"))


mean_monthly_temps <- monthly_temps(climate_1999)

mean_monthly_temps_C <- FtoC(mean_monthly_temps)

i <- TM_i(mean_monthly_temps_C)
I <- TM_I(i)
a <- TM_a(I)

#e <- TM_e(AirTemp=FtoC(climate_1999$TMean), HeatIndex_I=I, TM_a=a)
#daysinmonth <- getdaysinmonth(1999)
#ea <- TM_ea(climate_1999$Month,climate_1999$Day,climate_1999$Year,daysinmonth,40.1,e)


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

table <- read.csv(mycel$filename, header=TRUE)