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

# Compare emissions from motor vehicle sources in Baltimore City with emissions from
# motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

Car_codes <- SCC %>%
  filter(grepl('Vehicle+', SCC.Level.Two))

Cars <- NEI %>%
  filter(Car_codes$SCC %in% SCC) %>%
  filter(fips == '06037'| fips == '24510') 

Cars$fips <- as.factor(Cars$fips)
levels(Cars$fips) <- c('LA County', 'Baltimore')

cargroup <- Cars %>%
  group_by(year, fips) %>%
  summarise(sum(Emissions))

colnames(cargroup)[3] <- 'Emission_sum'

png(file = 'plot6.png', width = 480, height = 480)
g <- ggplot(cargroup, aes(year, Emission_sum, color = fips))
p <- g + geom_line(linetype = 1, alpha='0.5', size =3) + geom_point(size= 5) + ggtitle('Motor Vehicle Emissions By City') + theme(plot.title = element_text(face='bold'))
p
dev.off()

## ANSWER: From the graph, we can see that LA County has had greater fluctuations in
##         motor vehicle emissions.
