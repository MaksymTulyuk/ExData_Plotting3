## Plot 4
## emissions from coal combustion-related sources in the US, 1999-2008

# How to reproduce research
# 1. Download this script
# 2. Download https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# 3. Unzip xdata%2Fdata%2FNEI_data.zip
# 4. Run the code in the command line: R --no-environ CMD BATCH plot4.R

getDataWithCoal <- function(merged) {
    # get column names from SCC.Level.Three has a word "Coal" and not "Mining"
    coalSCCLevelNames <- grep("Coal", names(summary(merged$SCC.Level.Three)), value = TRUE)
    coalSCCLevelNames <- grep("Mining", coalSCCLevelNames, value = TRUE, invert = TRUE)
    # get column names from EI.Sector has a word "Coal"
    coalEISectorNames <- grep("Coal", names(summary(merged$EI.Sector)), value = TRUE)
    coalCombust <- subset(merged, EI.Sector %in% coalEISectorNames | SCC.Level.Three %in% coalSCCLevelNames,
                           select = c(Emissions, year, EI.Sector, SCC.Level.One:SCC.Level.Four))
    coalCombust
}

openGraphicsDevice <- function(file, x = 640, y = 480) {
    # reset graphics device
    png(file, width = x, height = y)
    par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
}

drawTwoPlots <- function(coalCombust, sumCoalCombust) {
    # draw case-by-case and total emission plots
    with(coalCombust, plot(year, Emissions, pch = 20, main = "Emission (case-by-case)"))
    with(sumCoalCombust, plot(year, Emissions, pch = 20, main = "Total Emission",
                              type = "l", col = 'red', lwd = 2))
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

# get coal related data and summary coal data
coalCombust <- getDataWithCoal(merged)
sumCoalCombust <- aggregate(Emissions ~ year, data = coalCombust, sum)

# open PNG file to save plots
openGraphicsDevice("plot4.png")
# draw plots
drawTwoPlots(coalCombust, sumCoalCombust)
# close the PNG file
closeGraphicsDevice("Coal combustion-related emission in the US, 1999-2008")
