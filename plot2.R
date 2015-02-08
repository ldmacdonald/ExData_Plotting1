readdata <- function(){
  data <- read.table("household_power_consumption.txt", sep=";",header=TRUE)
  df <- data.frame(data)
  df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")
  df <- subset(df, Date >= as.Date('2007-02-01'))
  df <- subset(df, Date <= as.Date('2007-02-02'))
  
  df
}

plot2 <- function(df=NULL){
  library(plyr)
  if(is.null(df)) { 
    df <- readdata() 
  }
  df$Date <- paste(df$Date, df$Time)
  df$Date <- strftime(df$Date, "%Y-%m-%d %H:%M:%OS")
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  r <-ddply(df, c("Date"),summarize, Global_active_power = sum(Global_active_power))
  
  png(filename="plot2.png", width=480, height=480, units="px",bg="white")
  plot(r$Global_active_power, type = "n", ylab="Global Active Power (kilowats)",ylim=c(0,8), xlab="",
       yaxt="n",xaxt="n")
  lines(r$Global_active_power)
  axis(2,at=c(0,2,4,6))
  axis(1,at=c(0,1440,2880) ,labels=c("Thu","Fri","Sat"))
  dev.off()
}