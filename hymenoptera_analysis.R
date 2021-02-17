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

test <- data.frame(hymenoptera$event_date, hymenoptera$event_year, hymenoptera$event_month, hymenoptera$event_day)

#split into 2020 and not 2020

hymenoptera$year2020 <- ifelse(hymenoptera$event_year == 2020, TRUE, FALSE)

hymenoptera_month <- hymenoptera %>%
  group_by(scientific_name_processed, event_year, event_month, year2020) %>%
  count(name = "n")

hymenoptera_species <- hymenoptera %>%
  group_by(scientific_name_processed) %>%
  count(name = "n")

top_species <- hymenoptera_species %>%
  filter(n > 500)

top_hymenoptera_month <- hymenoptera_month %>%
  filter(scientific_name_processed %in% top_species$scientific_name_processed)

ggplot(top_hymenoptera_month, aes(event_month, n, 
                              fill = scientific_name_processed))+
  geom_boxplot()

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
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020), alpha=0.5)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(name = "Record Year", labels = c("Before 2020", "2020"))

#I like this! It's nice and simple - much simpler than what I was using for ggmap.

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


#Read in land use data.
gb2019 <- raster("../gb2019lcm20m.tif")
plot(gb2019)
gb2019_pts <- rasterToPoints(gb2019, spatial = TRUE)
# Then to a 'conventional' dataframe
gb2019_df  <- data.frame(gb2019_pts)
rm(gb2019_pts)

#I want to try making a contour map
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_density_2d(data = hymenoptera_2020 %>%
               arrange(year2020), aes(x=lon, y=lat, colour=year2020))

#Contour map with full data?
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_density2d(data = hymenoptera, aes(x=lon, y=lat, group=year))

#I can't figure out why it thinks there are non-positive values in here... 
#Also this error: Error in seq_len(n) : argument must be coercible to non-negative integer
#I don't understand what to do here.

#This works, maybe I should try using gganimate to look at the change in distribution through the years

p <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-11.733, 2.285), ylim = c(49.582, 61.186), expand = FALSE)+
  geom_point(data = hymenoptera, aes(x=lon, y=lat, colour=as.factor(year)))+
  theme(legend.position = "none")+
  transition_time(year)+
  labs(title = "Year: {as.integer(frame_time)}")

animate(p)