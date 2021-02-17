#Streamlined workflow

#always load libraries first
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(rgeos)
library(gifski)
library(gganimate)
library(transformr)
library(raster)
library(rgdal)
library(stringdist)

#Read in all hymenoptera data
hymenoptera <- read_csv("../records-2020-10-29.csv")
hymenoptera <- Filter(function(x)!all(is.na(x)), hymenoptera)
hymenoptera <- hymenoptera %>%
  janitor::clean_names()
names(hymenoptera)
#Rename columns to 'lat' and 'lon' to make them more readable
hymenoptera <- hymenoptera %>%
  rename(lat = decimal_latitude_processed,
         lon = decimal_longitude_processed,
         year = year_processed)

head(hymenoptera$event_date)

event_colnames <- c("event_year", "event_month", "event_day")
event_split <- str_split_fixed(hymenoptera$event_date, "-", 3)
colnames(event_split) <- event_colnames

hymenoptera <- cbind(hymenoptera, event_split)

hymenoptera$year2020 <- ifelse(hymenoptera$event_year == 2020, TRUE, FALSE)

#create map

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020), alpha=0.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(name = "Record Year", labels = c("Before 2020", "2020"))

#Let's try rounding things to integers
hymenoptera$lat_int <- round(hymenoptera$lat, digits = 1)
hymenoptera$lon_int <- round(hymenoptera$lon, digits = 1)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=lon_int, y=lat_int, colour=year2020), alpha=0.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(name = "Record Year", labels = c("Before 2020", "2020"))

#Now land use data
gb2019 <- raster("../gb2019lcm20m.tif")
str(gb2019)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=lon_int, y=lat_int, colour=year2020), alpha=0.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(name = "Record Year", labels = c("Before 2020", "2020"))