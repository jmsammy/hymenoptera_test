library(sp)
library(sf)
library(rgdal)
library(raster)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rasterVis)
library(rnrfa)
library(sparta)

#Dictionary function for later use
dictionary <- function(df, dict, old_col, new_col, dict_refcol, dict_defcol){
  for(i in 1:length(df[[old_col]])){
    df[[new_col]][i] <- dict[[dict_defcol]][dict[[dict_refcol]]==df[[old_col]][i]]
  }
  return(df)
}

gb2019 <- raster("../gb2019lcm25m.tif")
gb2019
gb2019b1 <- raster("../gb2019lcm25m.tif", band=1)
gb2019b2 <- raster("../gb2019lcm25m.tif", band=2)
gb2019b3 <- raster("../gb2019lcm25m.tif", band=3)
plot(gb2019b1)
plot(gb2019b2)
plot(gb2019b3)
res(gb2019b1)
gb20191k <- aggregate(gb2019b1, fact = 4, fun = modal)
gb2019prop <- aggregate(gb2019b1, fact=4, fun = rs_prop_2)
plot(gb2019prop)
plot(gb20191k)
area(gb20191k)
gb2019_ha <- stack(gb20191k, gb2019prop)

writeRaster(x = gb2019_ha,
            
            # where your file will go - update with your file path!
            
            filename="gb2019_ha.tif", 	
            format = "GTiff") 
#I can now read in the tif without having to make it afresh every time.

gb20191k <- raster("gb2019_ha.tif", band=1) 
gb2019prop <- raster("gb2019_ha.tif", band=2) 

hist(gb2019prop)
sr2019 <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

#####
world2015 <- raster("../anthromes2015AD.asc")
plot(world2015)
e <- extent(-11.733, 2.285, 49.582, 61.186)
anthgb2015 <- crop(world2015, e)
plot(anthgb2015)
area(anthgb2015)
crs(anthgb2015) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#convert crs to same as 2019
proj_anthgb2015 <- projectRaster(anthgb2015, crs = sr2019)
area(proj_anthgb2015)
plot(proj_anthgb2015)
anthgb20151k <- disaggregate(proj_anthgb2015, fact=c(52, 92), fun=modal)
area(anthgb20151k)
plot(anthgb20151k)

#Read in all hymenoptera data
hymenoptera <- read_csv("../records_processed.csv")
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

#remove NAs
levels(as.factor(hymenoptera$state_province_processed))

hymenoptera <- hymenoptera %>%
  filter(nchar(grid_reference) >= 8)
foo <- osg_parse(hymenoptera$grid_reference[1])

hymenoptera$easting <- NA
hymenoptera$northing <- NA

hymenoptera <- hymenoptera %>%
  filter(!is.na(grid_reference)) %>%
  filter(!state_province_processed == "Northern Ireland")

for(i in (1:length(hymenoptera$grid_reference))){
  foo <- osg_parse(hymenoptera$grid_reference[i])
  df <- data.frame(foo)
  hymenoptera$easting[i] <- foo$easting
  hymenoptera$northing[i] <- foo$northing
}
rm(foo)

hymenoptera$easting <- floor(hymenoptera$easting/100)*100 # ensure x precision is exactly 100m, referenced to left
hymenoptera$northing <- floor(hymenoptera$northing/100)*100 # ensure y precision is exactly 100m, referenced to bottom

hymenoptera$event_date <- as.Date(hymenoptera$event_date)
str(hymenoptera$event_date)
unique_Months <- format(hymenoptera$event_date, "%B_%Y")

hymenoptera_nona <- hymenoptera %>%
  filter(!is.na(scientific_name_processed)) %>%
  filter(!is.na(plan_no)) %>%
  filter(!is.na(event_date)) %>%
  filter(!is.na(event_year)) %>%
  filter(!is.na(event_month))

hymenoptera_subset  <- siteSelection(taxa = hymenoptera_nona$scientific_name_processed,
                               site = hymenoptera_nona$plan_no,
                               time_period = hymenoptera_nona$event_date,
                               minL=2)

system.time({
  occ_out <- occDetModel(taxa = hymenoptera_nona$scientific_name_processed,
                         site = hymenoptera_nona$plan_no,
                         survey = hymenoptera_nona$event_date,
                         write_results = FALSE,
                         n_iterations = 200,
                         burnin = 15,
                         n_chains = 3,
                         thinning = 3,
                         seed = 123)
})

library(snowfall)

# I have 12 logical processors on my PC so I set cpus to 11 (n-1)
# when I initialise the cluster
sfInit(parallel = TRUE, cpus = 11)

# Export my data to the cluster
sfExport('formattedOccData')

# I create a function that takes a species name and runs my model
occ_mod_function <- function(taxa_name){
  
  library(sparta)
  
  occ_out <- occDetModel(taxa = taxa_name,
                         site = hymenoptera$plan_no,
                         survey = unique_Months,
                         write_results = FALSE,
                         n_iterations = 200,
                         burnin = 15,
                         n_chains = 3,
                         thinning = 3,
                         seed = 123)
} 

# I then run this in parallel

system.time({
  taxa <- levels(as.factor(hymenoptera$scientific_name_processed))
  para_out <- sfClusterApplyLB(taxa, occ_mod_function)
})
# Name each element of this output by the species
for(i in  1:length(para_out)) names(para_out)[i] <- para_out[[i]]$SPP_NAM

# Stop the cluster
sfStop()

gplot(gb20191k) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  geom_count(data = hymenoptera %>%
               arrange(year2020), aes(x=easting, y=northing), alpha=0.1)+
  coord_quickmap() +
  ggtitle("UK land use vs distribution of hymenoptera") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

col <- cbind(hymenoptera$easting+50, hymenoptera$northing+50) # Add 50 so that the point being extracted is in the centre of the hectare.

land_use <- raster::extract(gb20191k, col)
hymenoptera$land_use <- land_use

#Use dictionary function for land use data

landuse_dict <- readxl::read_excel("landuse_dict.xlsx")

hymenoptera <- dictionary(df = hymenoptera, 
           dict =landuse_dict, 
           old_col = "land_use", 
           new_col = "land_use_names", 
           dict_refcol = "num_ref", 
           dict_defcol = "land_cover_class")

ggplot(hymenoptera, aes(x=land_use, fill=scientific_name_processed))+
  geom_histogram()+
  theme(legend.position = "none")

#Use dictionary function for anthrome data
col <- cbind(hymenoptera$easting, hymenoptera$northing)

anth <- raster::extract(proj_anthgb2015, col)
hymenoptera$anthrome <- anth

anthrome_dict <- readxl::read_excel("anthrome_dict.xlsx")

hymenoptera <- dictionary(df = hymenoptera, 
                          dict =anthrome_dict, 
                          old_col = "anthrome", 
                          new_col = "anthrome_names", 
                          dict_refcol = "num_ref", 
                          dict_defcol = "anthrome_class")

ggplot(hymenoptera, aes(x=anthrome, fill=scientific_name_processed))+
  geom_histogram()+
  theme(legend.position = "none")

