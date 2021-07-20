library(tidyverse)

paths <- list.files(path = "data-raw/Mammals", pattern = ".shp$",recursive = TRUE, full.names = TRUE )

mammals <- paths %>%
  purrr::map(function(i){
    mammals <- sf::st_read(i)
    return(mammals)
  }) %>% purrr::reduce(rbind)

PCA <- list.files(path = "data-raw/PCAWorld/", ".tif", full.names = TRUE) %>%
  stars::read_stars()
st_crs(PCA) <- "+proj=longlat +datum=WGS84"
st_crs(mammals) <- st_crs(PCA)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)
safely_max <- purrr::possibly(.f = max_density, otherwise = NULL, quiet = FALSE)

mammals_centroids <- mammals %>%
  # warning with the ram
  #test run first 100 rows
  #slice(1:8) %>%
  split(., .$scientific) %>%
  #furrr::future_map(.x = ., .y = names(.), function(i, j) sf::st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) %>%
                group_by(species) %>%
                summarise(PCA1 = safely_max(PCA1.tif),
                          PCA2 = safely_max(PCA2.tif),
                          PCA3 = safely_max(PCA3.tif)) ) %>% purrr::reduce(rbind)


mammals_dist_env_matrix <- mammals_centroids %>%
  na.exclude() %>%
  select(-species) %>%
  dist() %>%
  as.matrix()
rownames(mammals_dist_env_matrix) <- mammals_centroids$species
colnames(mammals_dist_env_matrix) <- mammals_centroids$species


mammals_dist_env <- mammals_dist_env_matrix %>%
  as.dist(upper = TRUE) %>%
  broom::tidy(upper = TRUE) %>%
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  rename(enviromental.distance = distance)


mammalsdistance_median <- mammals_dist_env %>% group_by(item1) %>%
  summarise(enviromental.distance = median(enviromental.distance))

readr::write_csv(mammalsdistance_median, "data-raw/mammals_env_distance.csv" )

