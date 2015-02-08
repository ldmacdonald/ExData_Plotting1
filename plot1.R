readdata <- function(){
  data <- read.table("household_power_consumption.txt", sep=";",header=TRUE)
  df <- data.frame(data)
  df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")
  df <- subset(df, Date >= as.Date('2007-02-01'))
  df <- subset(df, Date <= as.Date('2007-02-02'))

  df
}

plot1 <- function(df=NULL){
  library(plyr)
  if(is.null(df)) { 
    df <- readdata() 
  }
  #par(mar=c(4,4,2,2))
  png(filename="plot1.png", width=480, height=480, units="px",bg="white")
  hist(as.vector(as.numeric(as.character(df$Global_active_power))), xlab="Global Active Power (kilowats)", ylab="Frequency", 
          ylim=c(0,1200), xlim=c(0,6),main="Global Active Power", col="red")
  dev.off()
  
}