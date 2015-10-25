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


# Across the United States, how have emissions from coal combustion-related sources
# changed from 1999–2008?

## Filter first for combustion source
## Then filter by coal (there are other non-combustible sources that emit Coal-related particles)
Coal_codes <- SCC %>%
  filter(grepl('ombustion+', SCC.Level.One)) %>%
  filter(grepl('Coal+', SCC.Level.Four))

Coal <- NEI %>%
  filter(Coal_codes$SCC %in% SCC) %>%
  group_by(year) %>%
  summarise(sum(Emissions))
colnames(Coal)[2] <- 'Emission_sum'

png(file = 'plot4.png', width = 480, height = 480)
qplot(year, Emission_sum, data= Coal, geom=c('line'), main = 'Coal Combustion Emssion')
dev.off()


## ANSWER: Emissions from coal combustion-related sources seem to have decreased steadily
##         throughout the years