## Plot 3
## Emissions per type (point, nonpoint, onroad, nonroad) in the Baltimore City, Maryland ("24510") from 1999 to 2008

# How to reproduce research
# 1. Download this script
# 2. Download https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# 3. Unzip xdata%2Fdata%2FNEI_data.zip
# 4. Run the code in the command line: R --no-environ CMD BATCH plot3.R

library("ggplot2")

getBaltimore <- function(data) {
    # select data for Baltimore
    data[data$fips == "24510",]
}

factoriseColumns <- function(data) {
    # change type from str to factor for ggplot2
    data[c(1,2,3,5)] <- lapply(data[c(1,2,3,5)], as.factor)
    data
}

# load data sheet
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

# clean up data and draw plot with it
Baltimore <- getBaltimore(NEI)
Baltimore <- factoriseColumns(Baltimore)

# draw 4 plots
plot <- qplot(year, Emissions, data = Baltimore, main ="Emissions per type in the Baltimore City, 1999-2008") +
    # with different colours and scales (one per each type of emisson)
    aes(colour = factor(type)) + facet_wrap(~type, scales="free_y") +
    # and with summary line on each plot
    stat_summary(fun.y = sum, geom="line")
ggsave("plot3.png", plot, width=8, height=8, dpi=120)
