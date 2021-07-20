library(sf)
library(tidyverse)
library(stars)
library(broom)

paths <- list.files(path = "data-raw/Mammals", pattern = ".shp$",recursive = TRUE, full.names = TRUE )

mammals <- paths %>%
  purrr::map(function(i){
    Mammals <- sf::st_read(i)
    return(Mammals)
  }) %>% purrr::reduce(rbind)  %>% sf::st_centroid(of_largest_polygon = TRUE)

#ensure one point per species (i.e., avoid code breaking if there are more than two points per species)

mammals <- mammals %>%
  group_by(scientific) %>%
  slice(1)

mammalsdistance <- mammals %>%
  sf::st_distance()
colnames(mammalsdistance) <- mammals$scientific
rownames(mammalsdistance) <- mammals$scientific

mammalsdistance <- mammalsdistance %>%
  as.dist()
mammalsdistance <- mammalsdistance %>%
  broom::tidy(upper = TRUE ) %>%
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  rename(geo.distance = distance)

mammalsdistance_median <- mammalsdistance %>% group_by(item1) %>%
  summarise(geo.distance = median(geo.distance))
readr::write_csv(mammalsdistance_median, "data-raw/mammals_geo_distance.csv" )
