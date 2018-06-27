library(date)
#
#  This script MUST be run AFTER the "read_ASCII_grid_generic.r" script
#  is run....this script processes the output of the "read_ASCII_grid" script.
#
             
source("D:/SMWData/R_Scripts/matrix_manip.r")
source("D:/SMWData/R_Scripts/image_legend.r")

guiSetStyle.tcltk(style = "large", ask = FALSE)

jpeg.filename<-guiDlgInput("Enter prefix for output jpg files",default="Rplot_")

start.mo<-guiDlgInput("Enter starting month of simulation (1-12)",default=1)
start.day<-guiDlgInput("Enter starting day of simulation (1-31)",default=1)
start.year<-guiDlgInput("Enter starting year of simulation (1900-2100)",default=1999)

title.text<-guiDlgInput("Enter text for the plot title",
  default="Black Earth Creek Test Case - Year 1999 RECHARGE Estimates")
  
scale.min<-as.numeric(guiDlgInput("MINIMUM scale value",default=-1.0))  
scale.max<-as.numeric(guiDlgInput("MAXIMUM scale value",default=1.0))  
  
pathname.vic.input<-guiDlgDir(title="Select the directory that contains the ASCII land use grid",
  dir=pathname.vic) 
  
descriptor.text<-guiDlgInput("Enter text description of data type",
  default="RECHARGE (inches)")

setwd(path.to.animations)   

land.cover <- readAsciiGrid(paste(pathname.vic.input,"/land_cov.grd",sep=""), as.image=TRUE)

dim(land.cover$z)<-NULL

land.cover.percent<-aggregate(land.cover$z,list(land.cover=land.cover$z),FUN=length)
land.cover.percent$pct.area<-formatC(land.cover.percent$x/length(land.cover$z)*100,digits=1,
   width=3,format="f")
land.cover.percent$x<-NULL   

#unlink("Rplot*.jpg")

jpeg(filename = paste(jpeg.filename,"%04d.jpg",sep=""), width = 800, height = 600,
          pointsize = 12, bg = "white", res = NA,quality=100)
          
#pdf(file = "Rplot%04d.pdf", width = 10.666, height = 8.0,
#          pointsize = 12, bg = "white",onefile=FALSE)

#red to green to blue
colorTable<-c(rainbow(n=10,start=0,end=1/6),rgb(1,1,1),rainbow(n=10,start=2/6,end=5/6))

par("mar" = c(3, 3, 6, 9) + 0.1) # wide righthand margin

near.zero<-1e-4

sum.mean.wes<-0
sum.mean.vic<-0

i<-1
          
for (i in 1:num.grids.vic) {

  date.string<-as.character(date.mmddyyyy(mdy.date(as.numeric(start.mo),
    as.numeric(start.day),as.numeric(start.year))+(i-1)))
  diffgrid=eval(parse(text=paste(prefix.vic,".vic.",i,"$z - ",prefix.wes,".wes.",i,"$z",sep="")))
  vic.list<-eval(parse(text=paste(prefix.vic,".vic.",i,"$z",sep="")))
  dim(vic.list)<-NULL
  vic.min<-min(vic.list)
  vic.max<-max(vic.list)
  vic.mean<-mean(vic.list)
  vic.sd<-sd(vic.list)
  vic.median<-median(vic.list)
  sum.mean.vic<-sum.mean.vic + vic.mean

  wes.list<-eval(parse(text=paste(prefix.wes,".wes.",i,"$z",sep="")))
  dim(wes.list)<-NULL
  wes.min<-min(wes.list)
  wes.max<-max(wes.list)
  wes.mean<-mean(wes.list)
  wes.sd<-sd(wes.list)  
  wes.median<-median(wes.list)
  sum.mean.wes<-sum.mean.wes + wes.mean

#  assign(paste("rech.vic.minus.wes.",i,sep=""),diffgrid)
  
   g.min<-min(diffgrid)
   g.max<-max(diffgrid)
   g.mean<-mean(diffgrid)  

     par(family="sans", font=1)
     image.matrix(diffgrid,col=colorTable,axes=FALSE,zlim=c(scale.min,scale.max))
     box()
     image.scale(c(scale.max:scale.min),col=colorTable,digits=2,size=12,labels="range")
     title(main = list(title.text, cex=1.3,
                       col="red", font=3),line=4)
           
    dim(diffgrid)<-NULL

    g.sd<-sd(diffgrid)
    g.median<-median(diffgrid)

    mtext(sprintf("JULIAN DAY: %i  (%s)\nDIFFERENCE: min: %8.2f   max: %8.2f   mean: %8.2f   median: %8.2f",
        i,date.string,g.min,g.max,g.mean,g.median),side=1,line=1)
      
    mtext(sprintf(
     "VIC: min: %8.2f   max: %8.2f   mean: %8.2f   median: %8.2f\nWES: min: %8.2f   max: %8.2f   mean: %8.2f   median: %8.2f",
         vic.min,vic.max,vic.mean,vic.median,wes.min,wes.max,wes.mean,wes.median),line=1)
    
    diff.min<-aggregate(diffgrid,list(land.cover=land.cover$z),FUN=min)
    diff.min$x<-formatC(diff.min$x,format="f",digits=2,width=6)
    colnames(diff.min)<-c("land.cover","diff.min")

    diff.mean<-aggregate(diffgrid,list(land.cover=land.cover$z),FUN=mean)
    diff.mean$x<-formatC(diff.mean$x,format="f",digits=2,width=6)
    colnames(diff.mean)<-c("land.cover","diff.mean")

    diff.max<-aggregate(diffgrid,list(land.cover=land.cover$z),FUN=max)
    diff.max$x<-formatC(diff.max$x,format="f",digits=2,width=6)
    colnames(diff.max)<-c("land.cover","diff.max")
    txt<-capture.output(merge(merge(merge(land.cover.percent,diff.min,by="land.cover"),
                    diff.mean,by="land.cover"),
                    diff.max,by="land.cover"))

    par(family="mono", font=2)
    text(100,70,paste(txt,collapse="\n"),cex=0.65)

cat("file",i,"of",num.grids.vic,": DIFFERENCE",
  " ==>   min:",g.min," max:",g.max," mean:",g.mean,"\n")
flush.console()

}

dev.off()

cat(descriptor.text,": Wes = ",sum.mean.wes,"   Vic = ",sum.mean.vic,"\n\n")
                               
