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

# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.
Baltimore <- NEI %>%
  filter(fips =='24510') %>%
  group_by(year)%>%
  summarise(sum(Emissions))

png(file = 'plot2.png', width = 480, height = 480)
plot(Baltimore, type = 'l', lwd =3, col='red', main = 'Baltimore Total PM2.5')
dev.off()

## ANSWER: Overall, we see a drop from 1999 to 2008. However, there is a notable spike in 2005.