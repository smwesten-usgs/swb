rm(list=ls())

myfilters <- matrix(data=c("CSV file (*.csv)", "*.csv", "All files (*.*)", "*.*"), nrow=2, ncol=2,
                    byrow=TRUE)

msb_filename <- choose.files(default="SWB_daily_mass_balance_report.csv", 
                             caption="Choose the daily mass balance report file you wish to plot",
                             multi=FALSE,
                             filters=myfilters,
                             index=1)

setwd( dirname(msb_filename) )

titleTxt <- winDialogString(message="Enter the title you wish to use in the plots",
                            default="Mass Balance Plot")
cex.main=1.0

mb_in<-read.csv(msb_filename, header=TRUE, as.is=TRUE, row.names=NULL)

# appears to be an error in read.csv regarding row names...kludge to get around this
colnames(mb_in)<-colnames(mb_in)[-1]
mb_in<-mb_in[-ncol(mb_in)]

mb_in$Date<-as.Date(mb_in$Date,format="%m/%d/%Y")

pdf(file = "SWB_mass_balance_plot.pdf",width = 11, height = 8.5)
layout(matrix(c(1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6), nrow=16 , ncol=1, byrow = TRUE))

for (i in sort(unique(mb_in$Year))) {

mb<-subset(mb_in,mb_in$Year==i)

par(mar = c(0,0,2,0)) 
plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
u <- par("usr")
text(1,u[4],labels = paste(titleTxt, ": ",i, sep=""),col = "blue",pos = 1, cex=2)

y<-as.vector(c(mb$Mean.Max.Temp,rev(mb$Mean.Min.Temp)))
x<-as.vector(c(mb$Date,rev(mb$Date)))

ylim.min <- min(mb$Mean.Min.Temp)
ylim.max <- max(mb$Mean.Max.Temp)
ylim.max <- ylim.max + 0.025 * (ylim.max - ylim.min)
ylim.min <- ylim.min - 0.025 * (ylim.max - ylim.min)

par(mar=c(3,6,3,3))
plot(Mean.Min.Temp ~ Date, data=mb,xlab="",ylab="TEMPERATURE, \nIN DEGREES FAHRENHEIT",
    col="navyblue",type="n", ylim=c(ylim.min,ylim.max),cex.main=cex.main,
    font.lab=2, tcl=0.3)
polygon(x,y,col="grey80", border=NA)
axis.Date(mb$Date,side=3,labels=F, tcl=0.3)
axis(side=4,at=axTicks(side=2),labels=F,tcl=0.3)
abline(h=32.0,col="black")
box(lwd=1.5)

# now plot INPUTS
par(mar=c(3,6,1,3))
y0 <- mb$Gross.Precip
y1 <- mb$Net.Rainfall
y2 <- mb$Snowmelt + y1
#y3 <- mb$Irrigation + y2
y3 <- 0 + y2

x<-as.vector(c(mb$Date,rev(mb$Date)))
ylim.min<-0.
ylim.max<-max(c(y0,y1,y2,y3))
ylim.max<-ylim.max + 0.025 * (ylim.max - ylim.min)
ylim.min<-ylim.min - 0.025 * (ylim.max - ylim.min)

plot(Mean.Min.Temp ~ Date, data=mb,xlab="",ylab="INPUTS, \nIN ACRE-FEET",
    col="navyblue",type="n", ylim=c(ylim.min,ylim.max),cex.main=cex.main,
    font.lab=2, tcl=0.3)
polygon(x,c(y0,rep(0,length(y0))),col="green", border=NA)
polygon(x,c(y1,rep(0,length(y1))),col="lightgreen", border=NA)
polygon(x,c(y2,rev(y1)),col="dodgerblue", border=NA)
polygon(x,c(y3,rev(y2)),col="mediumpurple", border=NA)
axis.Date(mb$Date,side=3,labels=F, tcl=0.3)
axis(side=4,at=axTicks(side=2),labels=F,tcl=0.3)
legend("topright",legend=c("GROSS PRECIP","NET RAINFALL","SNOWMELT","IRRIGATION"),
  fill=c("green","lightgreen","dodgerblue","mediumpurple"),inset=c(0.02,0.05),cex=0.8,lty=NULL)

abline(h=0)
box(lwd=1.5)


# now plot storage
par(mar=c(3,6,1,3))
y1<-mb$TOTAL.Soil.Moisture.Storage
y2<-mb$TOTAL.Surface.Storage..snow. + y1
x<-as.vector(c(mb$Date,rev(mb$Date)))
ylim.min<-min(c(y1,y2))
ylim.max<-max(c(y1,y2))
ylim.max<-ylim.max + 0.025 * (ylim.max - ylim.min)
ylim.min<-ylim.min - 0.025 * (ylim.max - ylim.min)

plot(Mean.Min.Temp ~ Date, data=mb,xlab="",ylab="STORAGE, \nIN ACRE-FEET",
    col="navyblue",type="n", ylim=c(ylim.min,ylim.max),cex.main=cex.main,
    font.lab=2, tcl=0.3)
polygon(x,c(y1,rep(0,length(y1))),col="wheat3", border=NA)
polygon(x,c(y2,rev(y1)),col="dodgerblue", border=NA)
axis.Date(mb$Date,side=3,labels=F, tcl=0.3)
axis(side=4,at=axTicks(side=2),labels=F,tcl=0.3)
legend("top",legend=c("SOIL MOISTURE","SNOW"),
  fill=c("wheat3","dodgerblue"),inset=c(0.02,0.05),cex=0.8,lty=NULL)

box(lwd=1.5)

# now plot AET
par(mar=c(3,6,1,3))
y1<-mb$Actual.Evapotranspiration
x<-as.vector(c(mb$Date,rev(mb$Date)))
ylim.min<-min(y1)
ylim.max<-max(y1)
ylim.max<-ylim.max + 0.025 * (ylim.max - ylim.min)
ylim.min<-ylim.min - 0.025 * (ylim.max - ylim.min)

plot(Mean.Min.Temp ~ Date, data=mb,xlab="",ylab="ACTUAL ET, \nIN ACRE-FEET",
    col="navyblue",type="n", ylim=c(ylim.max,ylim.min),cex.main=cex.main,
    font.lab=2, tcl=0.3)
polygon(x,c(y1,rep(0,length(y1))),col="yellow2", border=NA)
axis.Date(mb$Date,side=3,labels=F, tcl=0.3)
axis(side=4,at=axTicks(side=2),labels=F,tcl=0.3)
abline(h=0)
box(lwd=1.5)

par(mar=c(4,6,1,3))
y1<-mb$Recharge
y2<-mb$Rejected.Recharge + y1
y3<-mb$Surface.Flow.Out.of.Grid + y2
x<-as.vector(c(mb$Date,rev(mb$Date)))
ylim.min<-min(c(y1,y2,y3))
ylim.max<-max(c(y1,y2,y3))
ylim.max<-ylim.max + 0.025 * (ylim.max - ylim.min)
ylim.min<-ylim.min - 0.025 * (ylim.max - ylim.min)

plot(Mean.Min.Temp ~ Date, data=mb,xlab="",ylab="FLOW OUT, \nIN ACRE-FEET",
    col="navyblue",type="n", ylim=c(ylim.max,ylim.min),cex.main=cex.main,
    font.lab=2, tcl=0.3)
polygon(x,c(y1,rep(0,length(y1))),col="green", border=NA)
polygon(x,c(y2,rev(y1)),col="red", border=NA)
polygon(x,c(y3,rev(y2)),col="navy", border=NA)
axis.Date(mb$Date,side=3,labels=F, tcl=0.3)
axis(side=4,at=axTicks(side=2),labels=F,tcl=0.3)
abline(h=0)
legend("bottomright",legend=c("RECHARGE","REJECTED RECHARGE","SURFACE FLOW OUT OF GRID"),
  fill=c("green","red","navy"),inset=c(0.02,0.05),cex=0.8,lty=NULL)
box(lwd=1.5)

}

dev.off()

