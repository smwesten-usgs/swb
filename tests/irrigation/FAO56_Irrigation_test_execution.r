myargs <- commandArgs(trailingOnly = TRUE)
swb_exe <- myargs[1]

# constants for testing purposes
#swb_exe <- "d:\\DOS\\swb.exe"
#setwd("D:\\SMWData\\Source_Code\\swb\\build\\win_x64\\gfortran\\tests\\irrigation")

nash_sutcliffe <- function(cw, swb)  {
  
  numerator <- sum((cw - swb)^2)
  avg <- mean(cw)
  denominator <- sum((cw - avg)^2)
  
  E <- 1.0 - numerator / denominator
  
  return(E)
  
}

rpd <- function(arg1, arg2) {
  
  res <- abs(arg1 - arg2) / mean(c(arg1,arg2)) * 100
  
  return(res)
  
}

swb_ctl <- "recharge_single_factor_nonstandard.ctl"

version <- R.version

windows_sys <- length( grep( "mingw", version$os) ) > 0

if ( windows_sys ) {
# cleanup
retval <- system2(command="cleanup.bat", 
                  stderr="stderr.txt",
                  stdout="stdout.txt",
                  wait=TRUE)

} else {

  retval <- system2(command="./cleanup.sh", 
                    stderr="stderr.txt",
                    stdout="stdout.txt",
                    wait=TRUE)  
}

retval <- system2(command=swb_exe,
                  args=swb_ctl,
                  stderr="SWB_stderr.txt",
                  stdout="SWB_stdout.txt",
                  wait=TRUE)

#Sys.sleep(2)

pdf(file="FAO56_Irrigation_Test__R_Plots.pdf", width=11, height=8.5)

sm <- read.table("SOIL_MOISTURE_2_2.ssf", 
                 colClasses=c("character","character","character","numeric"))

colnames(sm) <- c("row_col", "date", "time", "value")
sm$date <- as.Date(sm$date, format="%m/%d/%Y")

refet <- read.table("REFERENCE_ET_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(refet) <- c("row_col", "date", "time", "value")
refet$date <- as.Date(refet$date, format="%m/%d/%Y")

refet_adj <- read.table("REFERENCE_ET_ADJ_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(refet_adj) <- c("row_col", "date", "time", "value")
refet_adj$date <- as.Date(refet$date, format="%m/%d/%Y")

cropet <- read.table("CROP_ET_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(cropet) <- c("row_col", "date", "time", "value")
cropet$date <- as.Date(cropet$date, format="%m/%d/%Y")

rootdpth <- read.table("ROOTING_DEPTH_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(rootdpth) <- c("row_col", "date", "time", "value")
rootdpth$date <- as.Date(rootdpth$date, format="%m/%d/%Y")

cropcoef <- read.table("CROP_COEFFICIENT_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(cropcoef) <- c("row_col", "date", "time", "value")
cropcoef$date <- as.Date(cropcoef$date, format="%m/%d/%Y")

baresoilevap <- read.table("BARE_SOIL_EVAP_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(baresoilevap) <- c("row_col", "date", "time", "value")
baresoilevap$date <- as.Date(baresoilevap$date, format="%m/%d/%Y")

irr <- read.table("IRRIGATION_FROM_GW_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(irr) <- c("row_col", "date", "time", "value")
irr$date <- as.Date(irr$date, format="%m/%d/%Y")

gross_precip <- read.table("GROSS_PRECIP_2_2.ssf", colClasses=c("character","character","character","numeric"))
colnames(gross_precip) <- c("row_col", "date", "time", "value")
gross_precip$date <- as.Date(irr$date, format="%m/%d/%Y")

cw_eto <- read.csv("Daily_ET0_CROPWAT_1990.csv", header=TRUE)
cw_eto$date <- as.Date(cw_eto$date, format="%m/%d/%Y")
cw_eto$eto_in_day <- cw_eto$eto_mm_day / 25.4

cw_msb <- read.csv("Mass_Balance_CROPWAT_1990.csv", header=TRUE)
cw_msb$date <- as.Date(cw_msb$date, format="%m/%d/%Y")
cw_msb$rain_in <- cw_msb$rain_mm / 25.4
cw_msb$gross_irr_in <- cw_msb$gross_irr_mm / 25.4
cw_msb$eta_in_day <- cw_msb$eta_mm_day / 25.4
cw_msb$deficit_in <- cw_msb$deficit_mm / 25.4
cw_msb$sm <- sm$value[sm$date == as.Date("1990-05-10")] - cw_msb$deficit_in 

with(cw_msb, plot(date, gross_irr_in, pch=21, main="GROSS IRRIGATION AMOUNT"))
with(irr, lines(date, value, col="green"))
legend("topright",legend=c("CROPWAT","SWB"), lty=c(-1,1),
       pch=c(21,-1), col=c("black","green"), inset=c(0.02,0.02))

with(cw_msb, plot(date, eta_in_day, main="CROP ET", pch=21))
with(cropet, lines(date, value, col="blue"))
legend("topright",legend=c("CROPWAT","SWB"), lty=c(-1,1),
       pch=c(21,-1), col=c("black","blue"), inset=c(0.02,0.02))

with(rootdpth, plot(date, value, col="brown", type="l", ylim=c(0, 1.8),
                    main="EFFECTIVE_ROOTING DEPTH (FEET)"))

with(cropcoef, plot(date, value, col="orange", type="l", main="CROP COEFFICIENT (Kcb)"))

with(cw_msb, plot(date, sm, pch=21, main="SOIL MOISTURE"))
with(sm, lines(date, value, col="red"))
legend("bottomright",legend=c("CROPWAT","SWB"), lty=c(-1,1),
       pch=c(21,-1), col=c("black","red"), inset=c(0.02,0.02))

with(cw_msb, plot(date, rain_in, pch=21, main="PRECIPITATION"))
with(gross_precip, lines(date, value, col="blue"))
legend("topright",legend=c("CROPWAT","SWB"), lty=c(-1,1),
       pch=c(21,-1), col=c("black","blue"), inset=c(0.02,0.02))

with(cw_eto, plot(date, eto_in_day, pch=21, main="REFERENCE ETo"))
with(refet, lines(date, value, col="purple"))
legend("topright",legend=c("CROPWAT","SWB"), lty=c(-1,1),
       pch=c(21,-1), col=c("black","purple"), inset=c(0.02,0.02))

cor_cropet <- cor(cw_msb$eta_in_day,subset(cropet,date%in%cw_msb$date)$value)
cor_sm <- cor(cw_msb$sm,subset(sm,date%in%cw_msb$date)$value)

ns_sm <- nash_sutcliffe(cw=cw_msb$sm,
                        swb=subset(sm,date %in% cw_msb$date)$value)
ns_cropet <- nash_sutcliffe(cw=cw_msb$eta_in_day,
                            swb=subset(cropet,date %in% cw_msb$date)$value)
ns_eto <- nash_sutcliffe(cw=cw_eto$eto_in_day,
                         swb=subset(refet,date %in% cw_eto$date)$value)
ns_irr <- nash_sutcliffe(cw=cw_msb$gross_irr_in,
                         swb=subset(irr,date %in% cw_msb$date)$value)

sum_irr_swb <- sum(subset(irr,date %in% cw_msb$date)$value)
sum_irr_cw <- sum(cw_msb$gross_irr_in)

rpd_irr <- rpd(sum_irr_swb, sum_irr_cw)

dev.off()

# reference ET and crop ET are easier to get right; the soil moisture depends on
# several other factors, so we'll accept a lower NS for it.
lTEST <- ns_sm > 0.75 & ns_eto > 0.98 & ns_cropet > 0.98 & rpd_irr < 10.

if( lTEST == TRUE ) {
  cat("PASS")    
} else {
  cat("FAIL")  
}

