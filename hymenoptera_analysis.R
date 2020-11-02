library(tidyverse)

hymenoptera <- read_csv("../records-2020-10-29.csv")
hymenoptera <- Filter(function(x)!all(is.na(x)), hymenoptera)

hymenoptera_2020 <- hymenoptera %>%
  filter(`year processed` == "2020")

