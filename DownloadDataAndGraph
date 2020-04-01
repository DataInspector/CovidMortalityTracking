library(readxl)
library(dplyr)
library(httr)
library(ggplot2)
library(utils)

date <- format(Sys.time(), "%Y-%m-%d")
# date <- '2020-03-29'

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",date, ".xlsx", sep = "")
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
world <- read_excel(tf)

# GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
# world <- read.csv(tf)

world.total <- aggregate(world$deaths, by = list(world$geoId, world$countriesAndTerritories), FUN = sum)

selected <- filter(world, geoId %in% c('UK','IT','NL','FR','DE','US','ES'))

selectedByDay <- setNames(aggregate(selected$deaths, by = list(selected$dateRep, selected$geoId, selected$countriesAndTerritories), FUN = sum), c('Date','CountryID','CountryName','Deaths'))

selectedByDay$CountryName <- gsub('_', ' ' ,selectedByDay$CountryName)

selectedByDay <- selectedByDay %>%
  group_by(CountryID) %>%
  mutate(cumulativeDeaths = cumsum(Deaths))

selectedByDay <- filter(selectedByDay, cumulativeDeaths > 19)

selectedByDay <- selectedByDay %>% 
  group_by(CountryID) %>% 
  mutate(sequenceDay = dplyr::row_number())

selectedByDay <- filter(selectedByDay, sequenceDay <= 40)

ggplot(selectedByDay, aes(sequenceDay, cumulativeDeaths, group = CountryName)) +
  geom_line(aes(color = CountryName),size=1.25) + 
  scale_y_continuous(trans='log10') +
  theme_classic() + 
  theme(legend.position='bottom') + 
  labs(title = 'Cumulative Deaths by day', subtitle = paste('Produced on ', date), caption = 'Source: www.ecdc.europa.eu') +
  xlab(paste('Days since 20 Deaths')) + 
  ylab('Cumulative Deaths') + 
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 16))

# https://www.worldometers.info/world-population/population-by-country/
# population <- data.frame(CountryID = c('UK','CN','US','DE','FR','IT','ES'), population = c(67886011, 1439323776, 331002651, 83783942, 65273511, 60461826, 46754778))

selected <- filter(world, geoId %in% c('UK','IT','NL','FR','DE','US','ES'))

# selected <- world

selectedByDay2 <- setNames(aggregate(selected$deaths, by = list(selected$dateRep, selected$geoId, selected$countriesAndTerritories, selected$popData2018), FUN = sum), c('Date','CountryID','CountryName','population','Deaths'))

selectedByDay2$CountryName <- gsub('_', ' ' ,selectedByDay2$CountryName)

# selectedByDay2[522,4] <- 53

selectedByDay2 <- selectedByDay2 %>%
  group_by(CountryID) %>%
  mutate(cumulativeDeaths = cumsum(Deaths))

#selectedByDay2 <- merge(selectedByDay2, population)

selectedByDay2$DeathsPerK <- selectedByDay2$cumulativeDeaths / (selectedByDay2$population / 100000)

selectedByDay2 <- filter(selectedByDay2, DeathsPerK > 0.05)

selectedByDay2 <- selectedByDay2 %>% 
  group_by(CountryID) %>% 
  mutate(sequenceDay = dplyr::row_number())

selectedByDay2 <- filter(selectedByDay2, sequenceDay <= max(selectedByDay2[selectedByDay2$CountryID!='CN',]$sequenceDay)+2)

ggplot(selectedByDay2, aes(sequenceDay, DeathsPerK, group = CountryName)) +
  geom_line(aes(color = CountryName),size=1.25) + 
  scale_y_continuous(trans='log10') +
  theme_classic() +
  labs(title = 'Cumulative Deaths per 100k Population by day', subtitle = paste0('Produced on ', date), caption = 'Source: www.ecdc.europa.eu') +
  xlab(paste('Days since 0.05 per 100k')) + 
  ylab('Cumulative Deaths per 100k Population') + 
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 16))
