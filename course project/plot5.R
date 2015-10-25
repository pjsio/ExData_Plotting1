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


# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
Car_codes <- SCC %>%
  filter(grepl('Vehicle+', SCC.Level.Two))

Cars <- NEI %>%
  filter(Car_codes$SCC %in% SCC) %>%
  filter(fips =='24510') %>%
  group_by(year) %>%
  summarise(sum(Emissions))

colnames(Cars)[2] <- 'Emission_sum'

png(file = 'plot5.png', width = 480, height = 480)
g <- ggplot(Cars, aes(year, Emission_sum))
p <- g + geom_point(size=5) + geom_line(linetype =2, alpha='0.3', size=2) + ggtitle('Baltimore Motor Vehicle Emissions') + theme(plot.title = element_text(face='bold'))
p
dev.off()


## ANSWER: Emissions have dropped dramatically from 1998 to 2002,
##         briefly increased towards 2005 and then slumped to lower than 2002 levels by 2008.