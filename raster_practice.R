library(sp)
library(sf)
library(rgdal)
library(raster)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rasterVis)

gb2019 <- raster("../201925m_wgs.tif")
gb2019
gb2019b1 <- raster("../201925m_wgs.tif", band=1)
gb2019b2 <- raster("../201925m_wgs.tif", band=2)
gb2019b3 <- raster("../201925m_wgs.tif", band=3)
plot(gb2019b1)
plot(gb2019b2)
plot(gb2019b3)

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

col <- cbind(hymenoptera$lon, hymenoptera$lat)

test <- raster::extract(gb2019b1, col)
hymenoptera$land_use <- test

dictionary <- function(df, dict, old_col, new_col, dict_refcol, dict_defcol){
  for(i in 1:length(df[[old_col]])){
    df[[new_col]][i] <- dict[[dict_defcol]][dict[[dict_refcol]]==df[[old_col]][i]]
  }
  return(df)
}

landuse_dict <- readxl::read_excel("landuse_dict.xlsx")

hymenoptera <- dictionary(df = hymenoptera, 
           dict =landuse_dict, 
           old_col = "land_use", 
           new_col = "land_use_names", 
           dict_refcol = "num_ref", 
           dict_defcol = "land_cover_class")
