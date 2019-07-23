plot2 <- function()
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
  
  # Creating Plot 2 - global Active Power vs datatime
  # paste $Date & $time and converting in date/time format to create Datetime variable
  globalActivePower <- as.numeric(powerDatasourceSub$Global_active_power)
  xDateTime <- paste(powerDatasourceSub$Date, powerDatasourceSub$Time, sep = " ")
  powerDatasourceSub$DateTime <- as.POSIXct((xDateTime))
  
  # plotting 
  plot(powerDatasourceSub$DateTime, globalActivePower, xlab = "", ylab = "Global Active Power(kilowatt)", type = "l") 
  
  # saving into .png file
  dev.copy(png,file="plot2.png",width=480, height=480)
  dev.off()
  
  
}

  