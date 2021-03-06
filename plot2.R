
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
# And includes in the data.frame the weekdays
dateA <- c("1/2/2007")
dateB <- c("2/2/2007")
filtData <- function(dateA, dateB) {
  dtGood <- datosIn[Date==dateA | Date==dateB,]
  dtGoodWeek <- dtGood[, WeekDay:=wday(dmy(Date), label=TRUE, abbr=TRUE) ]
  return(dtGoodWeek)  
}
datosGoodWeek <- filtData(dateA, dateB)  


#Plot Two
png("plot2.png", width=480, height=480)

par(pty="s")
plot(
    as.numeric(datosGoodWeek$Global_active_power)
   ,axes=F
   ,type="l"
   ,ylab="Global Active Power (kilowatts)"
   ,xlab=""
)
val.axes <- seq.int(1, dim(datosGoodWeek)[1], length.out=3)
axis(1, at=val.axes, labels=datosGoodWeek$WeekDay[val.axes])
axis(2)
box()

dev.off()
