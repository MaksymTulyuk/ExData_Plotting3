load.data <- function () {
    # download file
    consumption <- read.csv("household_power_consumption.txt", sep = ";", na.strings = '?',
                            colClasses = c(rep("character", 2), rep("numeric", 7)))
    # select rows w/ the dates 2007-02-01 and 2007-02-02
    data <- consumption[consumption[,"Date"] == "1/2/2007" | consumption[,"Date"] == "2/2/2007",]
    rm(consumption)
    # convert the columns Date and Time in to Posix format and remove the ununsed columns
    data <- within(data, datetime <- as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
    data <- subset(data, select = -c(Date, Time))
    data
}

draw.plot2 <- function(data) {
    # build plot
    with(data, plot(datetime, Global_active_power, type = "l", xlab = "",
                    ylab = "Global Active Power (kilowatts)"))
}

save.png <- function() {
    # save png file and close graphics device
    dev.copy(png, filename = "plot2.png")
    dev.off()
}

data <- load.data()
draw.plot2(data)
save.png()
