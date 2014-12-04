## Plot 6
## Emissions from motor vehicle sources in Baltimore City and LA, 1999-2008

# How to reproduce research
# 1. Download this script
# 2. Download https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# 3. Unzip xdata%2Fdata%2FNEI_data.zip
# 4. Run the code in the command line: R --no-environ CMD BATCH plot6.R

getBaltimore <- function(data) {
    # select data for Baltimore
    data[data$fips == "24510",]
}

getLA <- function(data) {
    # select data for LA
    data[data$fips == "06037",]
}

getDataWithMotor <- function(merged) {
    # get column names from EI.Sector has a word "On-Road"
    motorEISectorNames <- grep("On-Road", names(summary(merged$EI.Sector)), value = TRUE)
    motorVehicles <- subset(merged, EI.Sector %in% motorEISectorNames,
                            select = c(Emissions, year, EI.Sector, SCC.Level.One:SCC.Level.Four))
    motorVehicles
}

openGraphicsDevice <- function(file, x = 800, y = 600) {
    # reset graphics device
    png(file, width = x, height = y)
    par(mfrow=c(2, 2), oma = c(0, 0, 2, 0))
}

drawPlotAndRegressionLine <- function(data, title, yscale) {
    # draw plot total emission per year
    with(data, plot(year, Emissions, pch = 19, main = title, ylim = yscale))
    
    # add grid
    grid()
    
    # add regression line
    model <- lm(Emissions ~ year, data)
    abline(model, col = 'red', lwd = 2)

    # add legend
    par(new = TRUE)
    legend("top", lty = 1, lwd = 2, col = "red", legend = "Regression line")
}

closeGraphicsDevice <- function(label) {
    title(main = label, outer = TRUE)
    dev.off();
}

# load and merge data sheets
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
merged <- merge(NEI, SCC)
# let's save memory by deleting big data
rm(NEI)
rm(SCC)

# get motor related data in Baltimore and LA
Baltimore <- getBaltimore(merged)
MotorInBaltimore <- getDataWithMotor(Baltimore)
LA <- getLA(merged)
MotorInLA <- getDataWithMotor(LA)

# open file to save plot
openGraphicsDevice("plot6.png")
# draw plot with it for Baltimore
drawPlotAndRegressionLine(MotorInBaltimore, "Baltimore (scaled to max_level/2)", c(0, max(MotorInBaltimore$Emissions)/2))
# draw plot with it for LA with LA scale
drawPlotAndRegressionLine(MotorInLA, "Los Angeles (scaled to max_level/2)", c(0, max(MotorInLA$Emissions)/2))
# draw plot with it for Baltimore
drawPlotAndRegressionLine(MotorInBaltimore, "Baltimore (scaled to regression line)", c(0, 5))
# draw plot with it for LA with LA scale
drawPlotAndRegressionLine(MotorInLA, "Los Angeles (scaled to regression line)", c(10, 30))
# set title and close PNG file
closeGraphicsDevice("Compare motor emissions, 1999-2008")
