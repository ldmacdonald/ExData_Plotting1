readdata <- function(){
  data <- read.table("household_power_consumption.txt", sep=";",header=TRUE)
  df <- data.frame(data)
  df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")
  df <- subset(df, Date >= as.Date('2007-02-01'))
  df <- subset(df, Date <= as.Date('2007-02-02'))
  
  df
}

plot2 <- function(df){
  library(plyr)
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  r <-ddply(df, c("Date"),summarize, Global_active_power = sum(Global_active_power))
  
  plot(r$Global_active_power, type = "n", ylab="Global Active Power (kilowats)",ylim=c(0,8), xlab="",
       yaxt="n",xaxt="n")
  lines(r$Global_active_power)
  axis(2,at=c(0,2,4,6))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
}

plot3 <- function(df){
  library(plyr)
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
  df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
  df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
  sub1 <-ddply(df, c("Date"), summarize, Sub_metering_1 = sum(Sub_metering_1))
  sub2 <-ddply(df, c("Date"), summarize, Sub_metering_2 = sum(Sub_metering_2))
  sub3 <-ddply(df, c("Date"), summarize, Sub_metering_3 = sum(Sub_metering_3))
  
  plot(sub1$Sub_metering_1, type = "n", ylab="Energy sub metering",ylim=c(0,40),xlab="",
       , yaxt="n",xaxt="n")
  lines(sub1$Sub_metering_1, col="black")
  lines(sub2$Sub_metering_2, col="red")
  lines(sub3$Sub_metering_3, col="blue")
  legend("topright",col=c("black","red","blue"),lty=1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         cex=0.8, xjust=1,bty="n" )
  axis(2,at=c(0,10,20,30))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
}

plot4a <- function(df){
  library(plyr)
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  df$Voltage <- as.numeric(as.character(df$Voltage))
  #r <-ddply(df, c("Date"),summarize, Voltage = sum(as.numeric(as.character(df$Voltage))))
  plot(as.vector(df$Voltage), ylab="Voltage",xlab="datetime",type="n",xaxt="n",yaxt="n",ylim=c(232,248))
  lines(df$Voltage, col="black")
  axis(2,at=c(234,238,242,246))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
  r
}

plot4b <- function(df){
  library(plyr)
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  
  df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
  r <-ddply(df, c("Date"),summarize, Global_reactive_power = sum(Global_reactive_power))
  
  plot(r$Global_reactive_power, type = "n", ylab="Global_reactive_power",ylim=c(0,.5), xlab="datetime",
       yaxt="n",xaxt="n")
  lines(r$Global_reactive_power)
  axis(2,at=c(0,.1,.2,.3,.4,.5), labels=c("0.0","0.1","0.2","0.3","0.4","0.5"))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
}

plot4 <- function(df=NULL){
  if(is.null(df)) { 
    df <- readdata() 
  }

  png(filename="plot4.png", width=480, height=480, units="px",bg="white")
  par(mfrow=c(2,2))
  plot2(df)
  plot4a(df)
  plot3(df)
  plot4b(df)
  dev.off()
}