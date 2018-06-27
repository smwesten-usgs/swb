
get_new_data <- TRUE

if (get_new_data) {
  
  myfilters <- matrix(data=c("CSV file (*.csv)", "*.csv", "All files (*.*)", "*.*"), nrow=2, ncol=2,
                      byrow=TRUE)
  
  val_filename <- choose.files(default="SWB_daily_MEAN_values.csv", 
                               caption="Choose the daily values report file you wish to plot",
                               multi=FALSE,
                               filters=myfilters,
                               index=1)
  
  val_filename_base <- choose.files(default="SWB_daily_MEAN_values.csv", 
                                    caption="Choose the daily values report file you wish to compare against",
                                    multi=FALSE,
                                    filters=myfilters,
                                    index=1)
  
  setwd( dirname(val_filename) )
  
  titleTxt <- winDialogString(message="Enter the title you wish to use in the plots",
                              default="SWB daily values comparisons")
  cex.main=1.0
  
  v_in<-read.csv(val_filename, header=TRUE, as.is=TRUE, row.names=NULL, skip=2)
  v_base<-read.csv(val_filename_base, header=TRUE, as.is=TRUE, row.names=NULL, skip=2)
  
  v_in$Date<-as.Date(v_in$Date,format="%m/%d/%Y")
  v_in$Year <- format(v_in$Date, format="%Y")
  
  v_base$Date<-as.Date(v_base$Date,format="%m/%d/%Y")
  v_base$Year <- format(v_base$Date, format="%Y")  
  
  # ensure vector lengths are the same, assuming the date ranges are comparable
  v_base <- v_base[v_base$Date %in% v_in$Date, ]
  
}

pdf(file = "SWB_mass_balance_comparison_plot.pdf",width = 11, height = 8.5)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), nrow=4 , ncol=4, byrow = TRUE))

for (i in sort(unique(v_in$Year))) {

  vb <- subset(v_in,v_in$Year==i)
  vbb <- subset(v_base,v_base$Year==i)  
  
  par(mar = c(0,0,2,0)) 
  plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
  u <- par("usr")
  text(1,u[4],labels = paste(titleTxt, ": ",i, sep=""),col = "blue",pos = 1, cex=2)
  
  for (j in 2:ncol(vb)) {
    
    y<-vb[ ,j ]
    
    if ( any(colnames(vbb)==colnames(vb)[j] ) ) {
      x<-as.vector( unlist( vbb[which(colnames(vbb)==colnames(vb)[j] ) ]  ) )
      title_txt <- colnames(vb)[j]
      
      par(mar=c(3,6,3,3))
      plot(y~x, xlab="",ylab="", main=title_txt,col="blue")
      abline(b=1,a=0,col="red")
      box(lwd=1.5)
      
    }
    
  }
  
}

dev.off()

