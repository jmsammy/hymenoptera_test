#always load libraries first
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

#Read in all hymenoptera data
hymenoptera <- read_csv("../records-2020-10-29.csv")
hymenoptera <- Filter(function(x)!all(is.na(x)), hymenoptera)

#Rename columns to 'lat' and 'lon' to make them more readable
hymenoptera <- hymenoptera %>%
  rename(lat = `decimalLatitude processed`,
         lon = `decimalLongitude processed`)

#split into 2020 and not 2020

hymenoptera$year2020 <- ifelse(hymenoptera$`year processed` == 2020, TRUE, FALSE)

#filter out 2020 data
hymenoptera_2020 <- hymenoptera %>%
  filter(`year processed` == "2020" | `year processed` == '2019')

#Create map for viewing
locations_sf <- st_as_sf(hymenoptera_2020, coords = c("lon", "lat"), crs = 4326)

mapview(locations_sf)

#Produce map for putting points on
britain <- get_stamenmap(center = c(-2.1, 54.1), zoom=5, maptype="terrain")

britain <- get_stamenmap(bbox = c(left = -11.733, bottom = 49.582, right =2.285, top = 61.186), zoom=5, maptype="toner")

ggmap(britain) +
  geom_point(data = hymenoptera_2020 %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020), alpha=0.5, size=1)
