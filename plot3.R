plot3 <- function()
{
  
  # Initialize path values
  dataFile <- "household_power_consumption.txt"
  zipFilename <- 'exdata_data_household_power_consumption.zip'
  
  # Check if zipfile has been already 1) dowloaded and 2)unzipped : iIF NOT,download & unzip 
  #  zip file exists? IF NOT, downlod it
  if (!file.exists(dataFile)) 
  {
    if(!file.exists(zipFilename)) 
    { 
      zipFileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" 
      download.file(zipFileUrl, zipFilename)
    }
    unzip(zipFilename)
  }
  
  # Reading data from source file & subsetting in the period 2007-02-01/02 
  
  powerDataSource   <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
  powerDataSource$Date <- as.Date(powerDataSource$Date, format("%d/%m/%Y"))
  powerDatasourceSub <- subset(powerDataSource, subset = (Date >= "2007-02-01" & Date <="2007-02-02"))
  powerDatasourceSub <- powerDatasourceSub[complete.cases(powerDatasourceSub),]
  
  ### Creating Plot 3 - Energy sub_metering vs Datetime
  # assign (Y) variables 
  subMet1 <- as.numeric(powerDatasourceSub$Sub_metering_1)
  subMet2 <- as.numeric(powerDatasourceSub$Sub_metering_2)
  subMet3 <- as.numeric(powerDatasourceSub$Sub_metering_3)
  
  # paste $Date & $time and converting in date/time format to create Datetime variable
  
  xDateTime <- paste(powerDatasourceSub$Date, powerDatasourceSub$Time, sep = " ")
  powerDatasourceSub$DateTime <- as.POSIXct((xDateTime))
  
  # plotting subMetering_1 line black / subMetering_2 line red / subMetering_3 line blue
  with (powerDatasourceSub, 
        {
          plot(powerDatasourceSub$DateTime, subMet1,  type =  "l", xlab ="", ylab="Energy sub metering")
          lines(powerDatasourceSub$DateTime, subMet2, col="red")
          lines(powerDatasourceSub$DateTime, subMet3, col="blue")
        }
  )
  # adding a legend top right
  legend("topright", col = c("black","red","blue"), legend = c("sub_metering 1", "sub_metering 2","sub_metering 3"),lty = 1,lwd=2 ) 
 
   # saving into .png file
  dev.copy(png,file="plot3.png", width=480, height=480)
  dev.off()
  

}
