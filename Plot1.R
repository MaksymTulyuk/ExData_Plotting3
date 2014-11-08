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

data <- load.data()
# build histogram
hist(data$Global_active_power, col = "red", main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)")
# save png file
dev.copy(png, filename = "plot1.png")
dev.off()
