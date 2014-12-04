## Plot 1
## Total emissions in the United States from 1999 to 2008

# How to reproduce research
# 1. Download this script
# 2. Download https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# 3. Unzip xdata%2Fdata%2FNEI_data.zip
# 4. Run the code in the command line: R --no-environ CMD BATCH plot1.R

getEmissionYear <- function(data) {    
    # total emission per each year
    aggregate(Emissions ~ year, data = data, sum)
}

openGraphicsDevice <- function(file) {
    # reset graphics device
    png(file)
}

drawPlotAndRegressionLine <- function(data) {
    # plot total emission per year
    with(data, plot(year, Emissions, pch = 19))
    
    # regression line
    model <- lm(Emissions ~ year, data)
    abline(model, lwd = 2)

    # legend
    par(new = TRUE)
    legend("topright", lty = 1, lwd = 2, legend = "Regression line")
}

closeGraphicsDevice <- function(label) {
    title(main = label)
    dev.off();
}

# load data sheet
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

# count emission per year in the US
EmissionPerYear <- getEmissionYear(NEI)

# open file to save plot
openGraphicsDevice("plot1.png")
drawPlotAndRegressionLine(EmissionPerYear)
# set title and close PNG file
closeGraphicsDevice("Total emissions in the US, 1999-2008")
