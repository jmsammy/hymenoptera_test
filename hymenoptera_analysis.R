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

#filter out 2020 data
hymenoptera_2020 <- hymenoptera %>%
  filter(`year processed` == "2020")

#Create map for viewing
locations_sf <- st_as_sf(hymenoptera_2020, coords = c("lon", "lat"), crs = 4326)

mapview(locations_sf)
