## Plot 5
## Emissions from motor vehicle sources in Baltimore City, 1999-2008

# How to reproduce research
# 1. Download this script
# 2. Download https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# 3. Unzip xdata%2Fdata%2FNEI_data.zip
# 4. Run the code in the command line: R --no-environ CMD BATCH plot5.R

getBaltimore <- function(data) {
    # select data for Baltimore
    data[data$fips == "24510",]
}

getDataWithMotor <- function(merged) {
    # get column names from EI.Sector has a word "On-Road"
    motorEISectorNames <- grep("On-Road", names(summary(merged$EI.Sector)), value = TRUE)
    motorVehicles <- subset(merged, EI.Sector %in% motorEISectorNames,
                          select = c(Emissions, year, EI.Sector, SCC.Level.One:SCC.Level.Four))
    motorVehicles
}

openGraphicsDevice <- function(file, x = 480, y = 480) {
    # reset graphics device
    png(file, width = x, height = y)
}

drawPlotAndRegressionLine <- function(data) {
    # plot total emission per year
    with(data, plot(year, Emissions, pch = 19, ylim = c(0, max(Emissions)/2)))
    
    # regression line
    model <- lm(Emissions ~ year, data)
    abline(model, col = 'red', lwd = 2)
    
    # legend
    par(new = TRUE)
    legend("topright", lty = 1, lwd = 2, col = "red", legend = "Regression line")
}

closeGraphicsDevice <- function(label) {
    title(main = label, outer = FALSE)
    dev.off();
}

# load and merge data sheets
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
merged <- merge(NEI, SCC)
# let's save memory by deleting big data
rm(NEI)
rm(SCC)

# get motor related data
Baltimore <- getBaltimore(merged)
MotorInBaltimore <- getDataWithMotor(Baltimore)

# open file to save plot
openGraphicsDevice("plot5.png")
# clean up data and draw plot with it
drawPlotAndRegressionLine(MotorInBaltimore)
# set title and close PNG file
closeGraphicsDevice("Motor emissions in the Baltimore City, 1999-2008")
