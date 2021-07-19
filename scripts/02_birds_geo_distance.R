library(sf)
library(tidyverse)
library(stars)
library(broom)

paths <- list.files(path = "data-raw", pattern = ".shp$",recursive = TRUE, full.names = TRUE )

birds <- paths %>%
  purrr::map(function(i){
    Birds <- sf::st_read(i)
    return(Birds)
  }) %>% purrr::reduce(rbind)  %>% sf::st_centroid(of_largest_polygon = TRUE)
gc()

birds <- paths %>%
  purrr::map(function(i){
  Birds <- sf::st_read(i)
  BirdsCentroid <- Birds %>% sf::st_centroid(of_largest_polygon = TRUE)
  gc()
  return(BirdsCentroid)
}) %>% purrr::reduce(rbind)

#ensure one point per species

birds <- birds %>%
  group_by(SCINAME) %>%
  slice(1)

birdsdistance <- birds %>%
  sf::st_distance()
colnames(birdsdistance) <- birds$SCINAME
rownames(birdsdistance) <- birds$SCINAME

birdsdistance <- birdsdistance %>%
  as.dist()

birdsdistance <- birdsdistance %>%
  broom::tidy(upper = TRUE) %>%
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  rename(geo.distance = distance)
readr::write_csv(birdsdistance, "data-raw/birds_geo_distance.csv" )
