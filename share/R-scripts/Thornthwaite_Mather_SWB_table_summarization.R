setwd("D:\\SMWData\\Source_Code\\swb\\share\\R-scripts")

gen <- function(x, const, fac1, fac2, fac3, fac4) {
  
  pred <- const + fac1*x + fac2*x^2 + fac3*x^3 + fac4*x^4
  return(pred)
}


TM_table_2 <- read.table("Thornthwaite_Mather_1957__Table_2.dat", as.is=TRUE, header=TRUE)
TM_2 <- data.frame(Temp_degC=numeric(),I=numeric())

mycolnames <- colnames(TM_table_2)[2:ncol(TM_table_2)]
x<- as.numeric(gsub(pattern="T", replacement="", x=mycolnames) )
y<- TM_table_2[ ,1]

for (i in seq(1,10)) {
  Temp_degC <- y + x[i]/10
  I <- TM_table_2[ ,i+1]
  
  temp_df <- data.frame(Temp_degC=Temp_degC,I=I)
  TM_2 <- rbind(TM_2, temp_df)
  
  
}

# sort in ascending order
TM_2C <- TM_2[order(TM_2[ ,1]), ]
TM_2C_new <- data.frame(Temp_degC=seq(32,140), I=numeric(length(seq(32,140))))
TM_2F <- data.frame(Temp_degF=TM_2C$Temp_degC*9/5+32, I=TM_2C$I)
TM_2F_new <- data.frame(Temp_degF=seq(32,140), I=numeric(length(seq(32,140))))

nls.control(maxiter=3000, printEval=TRUE, warnOnly=TRUE)
TM_2F_opt <- nls( I ~ gen(Temp_degF, const, fac1, fac2, fac3, fac4), 
                  data=TM_2F, start=list(const=0., fac1=0.5, fac2=0.3, fac3=0.1, fac4=0.05),
                  control=list(maxiter=3000, tol=1.e-8, printEval=TRUE, 
                               minFactor=1.e-10, warnOnly=TRUE))




mycolnames <- colnames(TM_table_4)[2:ncol(TM_table_4)]

y<- TM_table_4[ ,1] *9/5 + 32
x<- as.numeric(gsub(pattern="I", replacement="", x=mycolnames) )


TM_table_4 <- read.table("Thornthwaite_Mather_1957__Table_4.dat", as.is=TRUE, header=TRUE)
TM4_mat <- t(as.matrix(TM_table_4[ , -1])) / 25.4

#filled.contour(x, y, sm.mat, axes=T,xlab="ACCUMULATED POTENTIAL WATER LOSS, IN INCHES",
#   ylab="MAXIMUM SOIL-WATER CAPACITY, IN INCHES")
image(x=x, y=y, z=TM4_mat, xlab="THORNTHWAITE-MATHER \"I\" VALUE",
      ylab="AIR TEMPERATURE, IN DEGREES FAHRENHEIT",col=rev(rainbow(32,start=1/6,end=4/6)),
      main="POTENTIAL EVAPOTRANSPIRATION, IN INCHES")
contour(x,y,TM4_mat,axes=F,add=T,levels=seq(0.02,0.3,0.02))
box()

plot(x=c(20,140), y=c(0,30), type="n")
with(TM_2F, points(I ~ Temp_degF, cex=0.7))
lines(TM_2F_opt$m$predict(newdata=TM_2F_new) ~ seq(32,140), type="l", col="red")
