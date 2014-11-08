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

reset.graphics.device <- function(x = 2, y = 2) {
    # reset graphics device
    par(mfrow = c(x, y))
}

draw.plot1 <- function(data) {
    # build plot
    with(data, plot(datetime, Global_active_power, type = "l", xlab = ""))
}

draw.plot2 <- function(data) {
    # build plot
    with(data, plot(datetime, Voltage, type = "l"))
}

draw.plot3 <- function(data) {
    # build plot
    with(data, plot(datetime, Sub_metering_1, type = "l", xlab = "", ylab = ""))
    par(new = TRUE)
    with(data, plot(datetime, Sub_metering_2, type = "l", col = "red", xlab = "",
                    ylab = "", ylim = range(Sub_metering_1)))
    par(new = TRUE)
    with(data, plot(datetime, Sub_metering_3, type = "l", col = "blue", xlab = "",
                    ylab = "Energy sub metering", ylim = range(Sub_metering_1)))
    par(new = TRUE)
    legend("topright", lty = 1, bty = "n",
           col = c("black", "red", "blue"), legend = names(data)[5:7])
}

draw.plot4 <- function(data) {
    # build plot
    with(data, plot(datetime, Global_reactive_power, type = "l"))
}

save.png <- function() {
    # save png file and close graphics device
    dev.copy(png, filename = "plot4.png")
    dev.off()
}

data <- load.data()
reset.graphics.device()
draw.plot1(data)
draw.plot2(data)
draw.plot3(data)
draw.plot4(data)
save.png()
