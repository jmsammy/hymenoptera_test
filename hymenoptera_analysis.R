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


#Read in all hymenoptera data
hymenoptera <- read_csv("../records-2020-10-29.csv")
hymenoptera <- Filter(function(x)!all(is.na(x)), hymenoptera)

#Rename columns to 'lat' and 'lon' to make them more readable
hymenoptera <- hymenoptera %>%
  rename(lat = `decimalLatitude processed`,
         lon = `decimalLongitude processed`,
         year = `year processed`)

#split into 2020 and not 2020

hymenoptera$year2020 <- ifelse(hymenoptera$year == 2020, TRUE, FALSE)

#filter out 2020 data
hymenoptera_2020 <- hymenoptera %>%
  filter(year == "2020" | year == '2019')

#Create map for viewing
locations_sf <- st_as_sf(hymenoptera_2020, coords = c("lon", "lat"), crs = 4326)

mapview(locations_sf)

#Produce map for putting points on
britain <- get_stamenmap(center = c(-2.1, 54.1), zoom=5, maptype="terrain")

britain <- get_stamenmap(bbox = c(left = -11.733, bottom = 49.582, right =2.285, top = 61.186), zoom=5, maptype="terrain")

ggmap(britain) +
  geom_point(data = hymenoptera_2020 %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020), alpha=0.5, size=1)

#We now have a map, but it's a little bit untidy. Is there another package we can use to do this better? I would be happy just to have plain lines that I can eventually superimpose locations of cities on.

#We've got some libraries installed that will allow us to use a world map, so let's do that.

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_point(data = hymenoptera_2020 %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020), alpha=0.5, size=1)

#I like this! It's nice and simple - much simpler than what I was using for ggmap.

#I want to try making a contour map
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_density_2d(data = hymenoptera_2020 %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020))

#This works, maybe I should try using gganimate to look at the change in distribution through the years

p <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_point(data = hymenoptera_2020 %>%
                    arrange(year2020), aes(x=lon, y=lat, colour=year2020))

anim <- p +
  transition_states(year2020,
                  transition_length = 2,
                  state_length = 1)
anim
