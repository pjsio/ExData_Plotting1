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

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all
# sources for each of the years 1999, 2002, 2005, and 2008.
Yearly <- NEI %>%
  group_by(year) %>%
  summarise(sum(Emissions))

png(file = 'plot1.png', width = 480, height = 480)
plot(Yearly, type = 'l', lwd= 5, col='blue', main = 'Total Emissions ')
dev.off()

## ANSWER: Yes, it has decreased (indicated by the line)



# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.
Baltimore <- NEI %>%
  filter(fips =='24510') %>%
  group_by(year)%>%
  summarise(sum(Emissions))

png(file = 'plot2.png', width = 480, height = 480)
plot(Baltimore, type = 'l', lwd =3, col='red')
dev.off()

## ANSWER: Overall, we see a drop from 1999 to 2008. However, there is a notable spike in 2005.






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
ggplot(by_type, aes(x=year, y = Emission_sum, color = type)) +
  geom_line()+
  geom_point()
dev.off()

## ANSWER: All types except POINT seem to decrease steadily over the years.
##         POINT sees a spike in 2005, but decreases afterwards.






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
qplot(year, Emission_sum, data= Coal, geom=c('line'))
dev.off()


## ANSWER: Emissions from coal combustion-related sources seem to have decreased steadily
##         throughout the yeras






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
p <- g + geom_point(size=5) + geom_line(linetype =2, alpha='0.3', size=2)
p
dev.off()


## ANSWER: Yes. Emissions have dropped dramatically from 1998 to 2002,
##         briefly increased towards 2005 and then slumped to lower than 2002 levels by 2008.



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
p <- g + geom_line(linetype = 1, alpha='0.5', size =3) + geom_point(size= 5)
p
dev.off()

## ANSWER: From the graph, we can see that LA County has had greater fluctuations in
##         motor vehicle emissions.
