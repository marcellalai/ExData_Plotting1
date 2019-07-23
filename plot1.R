plot1 <- function()
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
  
  # Reading data from source file & subsettiing in the period 2007-02-01/02 
  
  powerDataSource   <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
  powerDataSource$Date <- as.Date(powerDataSource$Date, format("%d/%m/%Y"))
  powerDatasourceSub <- subset(powerDataSource, subset = (Date >= "2007-02-01" & Date <="2007-02-02"))
  powerDatasourceSub <- powerDatasourceSub[complete.cases(powerDatasourceSub),]
  
  # Creating Plot 1 -> histogram of Global Active Power
  globalActivePower <- as.numeric(powerDatasourceSub$Global_active_power)
  hist(globalActivePower, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatt)")
  
  # Copying plot into a .png file & closing device
  dev.copy(png,file="plot1.png", width=480, height=480)
  dev.off()
  rm(powerDataSource)
}
