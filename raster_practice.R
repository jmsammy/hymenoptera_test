library(sp)
library(sf)
library(rgdal)
library(raster)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rasterVis)

gb2019 <- raster("../gb2019lcm20m_wgs84.tif")
gb2019
gb2019b1 <- raster("../gb2019lcm20m_wgs84.tif", band=1)
gb2019b2 <- raster("../gb2019lcm20m_wgs84.tif", band=2)
plot(gb2019b1)
plot(gb2019b2)

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

world <- ne_countries(scale = "medium", returnclass = "sf")

gplot(gb2019b1) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=lon, y=lat), alpha=0.1)+
  coord_quickmap() +
  ggtitle("UK Land Use") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text


