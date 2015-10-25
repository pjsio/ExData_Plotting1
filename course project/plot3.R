require(ggplot2)
require(data.table)
require(dplyr)

## This first line will likely take a few seconds. Be patient!
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", './temp.zip')
NEI <- readRDS(unzip("temp.zip", "summarySCC_PM25.rds"))
SCC <- readRDS(unzip('temp.zip', 'Source_Classification_Code.rds'))
closeAllConnections()

## If R Crashes while reading, manually download and unzip the file

NEI <- data.table(NEI)
SCC <- data.table(SCC)

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999–2008
# for Baltimore City? Which have seen increases in emissions from 1999–2008?
# Use the ggplot2 plotting system to make a plot answer this question.

NEI$type <- as.factor(NEI$type)
by_type <- NEI %>%
  filter(fips =='24510') %>%
  group_by(year,type)%>%
  summarise(sum(Emissions))
colnames(by_type)[3] <- 'Emission_sum'

png(file = 'plot3.png', width = 480, height = 480)
p <- ggplot(by_type, aes(x=year, y = Emission_sum, color = type)) +
  geom_line()+
  geom_point() + ggtitle('Total Emissions Split by Type') + theme(plot.title = element_text(face='bold'))
p
dev.off()

## ANSWER: All types except POINT seem to decrease steadily over the years.
##         POINT sees a spike in 2005, but decreases afterwards.