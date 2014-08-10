
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


#Plot Four 
png("plot4.png", width=480, height=480)

par(mfrow=c(2,2), pty="s", oma=c(2,2,1,1))
#----------
# plot 1
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
#----------

#----------
# plot 2
plot(
  as.numeric(datosGoodWeek$Voltage)
  ,axes=F
  ,type="l"
  ,ylab="Voltage"
  ,xlab="datetime"
)
val.axes <- seq.int(1, dim(datosGoodWeek)[1], length.out=3)
axis(1, at=val.axes, labels=datosGoodWeek$WeekDay[val.axes])
axis(2)
box()
#----------

#----------
# plot 3
plot(
  as.numeric(datosGoodWeek$Sub_metering_1)
  ,axes=F
  ,type="l"
  ,col="black"
  ,ylab="Energy sub metering"
  ,xlab=""
)
lines(as.numeric(datosGoodWeek$Sub_metering_2), col="red")
lines(as.numeric(datosGoodWeek$Sub_metering_3), col="blue")
val.axes <- seq.int(1, dim(datosGoodWeek)[1], length.out=3)
axis(1, at=val.axes, labels=datosGoodWeek$WeekDay[val.axes])
axis(2)
legend(
        "topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3")
       ,col=c("black","red","blue"), lty=1, cex=0.8, bty="n"
       )
box()
#----------

#----------
# plot 4
plot(
  as.numeric(datosGoodWeek$Global_reactive_power)
  ,axes=F
  ,type="l"
  ,ylab="Global_reactive_power"
  ,xlab="datetime"
)
val.axes <- seq.int(1, dim(datosGoodWeek)[1], length.out=3)
axis(1, at=val.axes, labels=datosGoodWeek$WeekDay[val.axes])
axis(2)
box()
#----------


dev.off()
