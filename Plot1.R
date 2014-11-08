load.file <- function (txtfile = "household_power_consumption.txt") {
    # download and unzip file
    url <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipfile <- "exdata-data-Fhousehold_power_consumption.zip"
    download.file(url, zipfile, mode="wb")
    unzip(zipfile, txtfile)
}    

get.data <- function (txtfile = "household_power_consumption.txt") {
    consumption <- read.csv(txtfile, sep = ";", na.strings = '?',
                            colClasses = c(rep("character", 2), rep("numeric", 7)))
    # select rows w/ the dates 2007-02-01 and 2007-02-02
    data <- consumption[consumption[,"Date"] == "1/2/2007" | consumption[,"Date"] == "2/2/2007",]
    rm(consumption)
    # convert the columns Date and Time in to Posix format and remove the ununsed columns
    data <- within(data, datetime <- as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
    data <- subset(data, select = -c(Date, Time))
    data
}

open.graphics.device <- function(x = 1, y = 1) {
    # reset graphics device
    png(file = "plot1.png", width=480, height=480)
    par(mfrow = c(x, y))
}

draw.plot1 <- function(data) {
    # build plot
    hist(data$Global_active_power, col = "red", main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)")
}

close.graphics.device <- function() {
    # close graphics device
    dev.off()
}

load.file()
data <- get.data()
open.graphics.device()
draw.plot1(data)
close.graphics.device()
