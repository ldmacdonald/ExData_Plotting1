readdata <- function(){
  data <- read.table("household_power_consumption.txt", sep=";",header=TRUE)
  df <- data.frame(data)
  df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")
  df <- subset(df, Date >= as.Date('2007-02-01'))
  df <- subset(df, Date <= as.Date('2007-02-02'))
  
  df
}

plot3 <- function(df=NULL){
  library(plyr)
  if(is.null(df)) { 
    df <- readdata() 
  }
  #df$Date <- weekdays(df$Date)
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
  df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
  df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
  sub1 <-ddply(df, c("Date"), summarize, Sub_metering_1 = sum(Sub_metering_1))
  sub2 <-ddply(df, c("Date"), summarize, Sub_metering_2 = sum(Sub_metering_2))
  sub3 <-ddply(df, c("Date"), summarize, Sub_metering_3 = sum(Sub_metering_3))
  
  png(filename="plot3.png", width=480, height=480, units="px",bg="white")
  plot(sub1$Sub_metering_1, type = "n", ylab="Energy sub metering",ylim=c(0,40),xlab="",
       , yaxt="n",xaxt="n")
  lines(sub1$Sub_metering_1, col="black")
  lines(sub2$Sub_metering_2, col="red")
  lines(sub3$Sub_metering_3, col="blue")
  legend("topright",col=c("black","red","blue"),lty=1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         cex=0.8, xjust=1 )
  axis(2,at=c(0,10,20,30))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
  dev.off()
}