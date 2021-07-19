library(tidyverse)

paths <- list.files(path = "data-raw", pattern = "Birds_\\d.shp$",recursive = TRUE, full.names = TRUE )

birds <- paths %>%
  purrr::map(function(i){
    Birds <- sf::st_read(i)
    return(Birds)
  }) %>% purrr::reduce(rbind)

PCA <- list.files(path = "data-raw/PCAWorld/", ".tif", full.names = TRUE) %>%
  stars::read_stars()
st_crs(PCA) <- "+proj=longlat +datum=WGS84"
st_crs(birds) <- st_crs(PCA)



library(furrr)
library(future)
plan(multiprocess)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)
safely_max <- purrr::possibly(.f = max_density, otherwise = NULL, quiet = FALSE)

birds_centroids <- birds %>%
  # warning with the ram
  # test run  first 100 rows
  slice(1:10) %>%
  split(., .$SCINAME) %>%
  #furrr::future_map(.x = ., .y = names(.), function(i, j) sf::st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) %>%
                group_by(species) %>%
                summarise(PCA1 = safely_max(PCA1.tif),
                          PCA2 = safely_max(PCA2.tif),
                          PCA3 = safely_max(PCA3.tif)) ) %>% purrr::reduce(rbind)


birds_dist_env_matrix <- birds_centroids %>%
  na.exclude() %>%
  select(-species) %>%
  dist() %>%
  as.matrix()
rownames(birds_dist_env_matrix) <- birds_centroids$species
colnames(birds_dist_env_matrix) <- birds_centroids$species


birds_dist_env <- birds_dist_env_matrix %>%
  as.dist(upper = TRUE) %>%
  broom::tidy(upper = TRUE) %>%
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  rename(enviromental.distance = distance)


birdsdistance_median <- birds_dist_env %>% group_by(item1) %>%
  summarise(enviromental.distance = median(enviromental.distance))

readr::write_csv(birdsdistance_median, "data-raw/birds_env_distance.csv" )
