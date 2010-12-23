# erase all current SPlus data objects - start with a clean slate...
rm(list=ls())

library(svDialogs)
library(maptools)

filters = c("ARC ASCII Grid File", "ARC ASCII Grid File", "All files","*.grd","*.asc","*.*")

#define pathname for the R_Scripts directory
path.to.Rscripts<-"D:/SMWData/R_Scripts/"

path.to.animations<-"D:/R_scratch/"

#source("D:/SMWData/R_Scripts/matrix_manip.r")
source("D:/SMWData/R_Scripts/image_legend.r")

#pathname to results generated with Wes Dripps' VB code
pathname.wes<-"d:/SMWData/source_code/dripps/recharge/VB_test_cases/bec/results"

#pathname to results generated with Vic's fortran code
pathname.vic<-"d:/SMWData/source_code/dripps/recharge/test_cases/bec"

pathname.wes<-guiDlgDir("Select the directory containing files from Wes", dir=pathname.wes)

pathname.vic<-guiDlgDir("Select the directory containing files from Vic", dir=pathname.vic)

#create vector of GRID file names
gridfiles.wes<-guiDlgOpen("Select files from Wes' model run",defaultDir = pathname.wes,multi=TRUE,
   filters=filters)

#create vector of GRID file names
gridfiles.vic<-guiDlgOpen("Select files from Vic's model run",defaultDir = pathname.vic,multi=TRUE,
   filters=filters)

#create variable to hold the number of textfiles (or number of sites) to be processed
num.grids.wes<-length(gridfiles.wes)

#create variable to hold the number of textfiles (or number of sites) to be processed
num.grids.vic<-length(gridfiles.vic)

if (num.grids.vic == 1) {
  filenum.vic<-1
} else {
  filenum.vic<-as.numeric(substring(gridfiles.vic,
   regexpr("[0-9]",gridfiles.vic,perl=T),
   regexpr("[0-9]$",gridfiles.vic,perl=T)))
}

if(num.grids.wes == 1) {
  filenum.wes<-1
} else {  
  filenum.wes<-as.numeric(substring(gridfiles.wes,
   regexpr("[0-9]",gridfiles.wes,perl=T),
   regexpr("[.]",gridfiles.wes,perl=T)-1))

}

if (num.grids.vic != num.grids.wes) stop("unequal number of grids for comparison...")

prefix.wes<-guiDlgInput("Enter prefix for Wes' grid objects")

prefix.vic<-guiDlgInput("Enter prefix for Vic's grid objects",default=prefix.wes)


for (i in 1:num.grids.wes) {

  index<-match(i,filenum.wes)
  tempgrid <- readAsciiGrid(gridfiles.wes[index], as.image=TRUE)
  assign(paste(prefix.wes,".wes.",i,sep=""),tempgrid)

  g.min<-min(tempgrid$z)
  g.max<-max(tempgrid$z)
  g.mean<-mean(tempgrid$z)
  cat("file",i,"of",num.grids.wes,":",gridfiles.wes[index],
    " ==>   min:",g.min," max:",g.max," mean:",g.mean,"\n")
  flush.console()

}

for (i in 1:num.grids.vic) {

  index<-match(i,filenum.vic)
  tempgrid <- readAsciiGrid(gridfiles.vic[index],as.image=TRUE)
  assign(paste(prefix.wes,".vic.",i,sep=""),tempgrid)
  
  g.min<-min(tempgrid$z)
  g.max<-max(tempgrid$z)
  g.mean<-mean(tempgrid$z)

  cat("file",i,"of",num.grids.vic,":",gridfiles.vic[index],
    " ==>   min:",g.min," max:",g.max," mean:",g.mean,"\n")
  flush.console()

}
