

bird_env_geo_phy <- list.files(path = getwd() , pattern = "birds.*distance.csv$", recursive = TRUE, full.names = TRUE) %>%
  purrr::map(readr::read_csv) %>%
  reduce(left_join) %>%
  na.exclude()

mammals_env_geo_phy <- list.files(path = getwd() , pattern = "mammals.*distance.csv$", recursive = TRUE, full.names = TRUE) %>%
  purrr::map(readr::read_csv) %>%
  reduce(left_join) %>%
  na.exclude()


write_csv(bird_env_geo_phy, "data-raw/birdsdistance.csv")
write_csv(mammals_env_geo_phy, "data-raw/mammalsdistance.csv")

# note
# the standarized version of this data frames are used in modelling with ecointeraction package
# ecointeraction::birdsdistance
# ecointeraction::batsdistance
# ecointeraction::mammalsdistance

