
# Load handful libraries
# Last library should be lubridate to take advantage of their date handling capabilities.
library(data.table)
library(lubridate)

# Get data
file <- c("household_power_consumption.txt")

fileGet <- function(x) {
       dtIn <- fread( x, na.strings=c("NA", "", "?") )
       return(dtIn)
}
datosIn <- fileGet(file)


# Filter needed data
dateA <- c("1/2/2007")
dateB <- c("2/2/2007")
filtData <- function(dateA, dateB) {
      dtGood <- datosIn[Date==dateA | Date==dateB,]
      return(dtGood)  
}
datosGood <- filtData(dateA, dateB)  
  

# Add weekdays to data.table
datosGoodWeek <- datosGood[, WeekDay:=wday(dmy(Date), label=TRUE, abbr=TRUE) ]


#Plot One
png("plot1.png", width=480, height=480)

hist(
       as.numeric(datosGoodWeek$Global_active_power), col="red"
      ,main="Global Active Power", xlab="Global Active Power (kilowatts)"
    )

dev.off()
