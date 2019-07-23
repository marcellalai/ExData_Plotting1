plot4 <- function()
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
  
  # retrieving variables to explore with graphics
  globalActivePower <- as.numeric(powerDatasourceSub$Global_active_power)
  subMet1 <- as.numeric(powerDatasourceSub$Sub_metering_1)
  subMet2 <- as.numeric(powerDatasourceSub$Sub_metering_2)
  subMet3 <- as.numeric(powerDatasourceSub$Sub_metering_3)
  voltage <- as.numeric(powerDatasourceSub$Voltage)
  globalReactivePower <- as.numeric(powerDatasourceSub$Global_reactive_power)
  
  # paste $Date & $time and converting in date/time format to create Datetime variable
  xDateTime <- paste(powerDatasourceSub$Date, powerDatasourceSub$Time, sep = " ")
  powerDatasourceSub$DateTime <- as.POSIXct((xDateTime))
  
  ###### Creating multiPlot ( 4 )
  
  # setting global graphs  paramas (2x2 )
  par(mfrow = c(2,2), mar = c(4,4,2,2))
  
  # plotting graph1
  plot(powerDatasourceSub$DateTime, globalActivePower, ylab = "Global Active Power(kilowatt)", type = "l", xlab="")
  
  # plotting graph2
  plot(powerDatasourceSub$DateTime, voltage, xlab = "Datetime", ylab = "Voltage", type = "l")
  
  # plotting graph3 (SubMet 1/2/3 vs datetime) with legend
  with (powerDatasourceSub, 
        {
          plot(powerDatasourceSub$DateTime, subMet1,  type =  "l", xlab ="", ylab="Energy sub metering")
          lines(powerDatasourceSub$DateTime, subMet2, col="red")
          lines(powerDatasourceSub$DateTime, subMet3, col="blue")
        }
  )
  legend("topright", c("sub_metering 1", "sub_metering 2","sub_metering 3"), 
         lwd = 2.5 ,lty = ,  bty = "n", cex = 0.5, col = c("black","red","blue") )
  
  
  # plotting graph4 : Global_Reactive_Power Vs datetime
  plot(powerDatasourceSub$DateTime, globalReactivePower, ylab = "Global_Reactive_Power", xlab = "Datetime", type = "l")
  
  # Copying plot into a .png file & closing device
  dev.copy(png,file="plot4.png",width=480, height=480)
  dev.off()
  
}