library(tidyverse)
library(ecointeraction)
library(taxize)


max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}


plasmodium_results <- readr::read_rds("data-raw/bird_plasmodium_1000_sims.rds") %>%
  purrr::map( ~ .x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))



wnv_results <- readr::read_rds("data-raw/bird_wnv_1000_sims.rds") %>%
  purrr::map( ~ .x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))



coronavirus_results <- readr::read_rds("data-raw/bat_coronavirus_1000_sims.rds") %>%
  purrr::map( ~ .x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))



top_10_susceptible_plasmodium <- plasmodium_results %>%
  arrange(desc(susceptible)) %>%
  head(10)


top_10_susceptible_wnv <- wnv_results %>%
  arrange(desc(susceptible)) %>%
  head(10)

top_10_susceptible_coronavirus <- coronavirus_results %>%
  arrange(desc(susceptible)) %>%
  head(10)

top_10_incidence_plasmodium <- plasmodium_results %>%
  left_join(birdsplasmodiumrelictum) %>%
  arrange(desc(incidence)) %>%
  head(10)

top_10_incidence_wnv <- wnv_results %>%
  left_join(ecointeraction::birdswnv) %>%
  arrange(desc(incidence)) %>%
  head(10)

top_10_incidence_coronavirus <- coronavirus_results %>%
  left_join(ecointeraction::batscoronavirus) %>%
  arrange(desc(incidence)) %>%
  head(10)


tax_plasmodium_susceptible <- taxize::tax_name(
  top_10_susceptible_plasmodium$species,
  c("family", "order")
)

tax_plasmodium_incidence <- taxize::tax_name(
  top_10_incidence_plasmodium$species,
  c("family", "order")
)


tax_wnv_susceptible <- taxize::tax_name(
  top_10_susceptible_wnv$species,
  c("family", "order")
)

tax_wnv_incidence <- taxize::tax_name(
  top_10_incidence_wnv$species,
  c("family", "order")
)


tax_coronavirus_susceptible <- taxize::tax_name(
  top_10_susceptible_coronavirus$species,
  c("family", "order")
)

tax_coronavirus_incidence <- taxize::tax_name(
  top_10_incidence_coronavirus$species,
  c("family", "order")
)


top_10_susceptible_plasmodium <- tax_plasmodium %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_susceptible_plasmodium) %>%
  left_join(ecointeraction::birdsplasmodiumrelictum)

top_10_incidence_plasmodium <- ecointeraction::birdsplasmodiumrelictum %>%
  left_join(plasmodium_results) %>% na.exclude() %>%
  arrange(desc(incidence)) %>%
  head(10)


top_10_incidence_plasmodium <- tax_plasmodium_incidence %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_incidence_plasmodium) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))

top_10_susceptible_plasmodium <- tax_plasmodium_susceptible %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_susceptible_plasmodium) %>%
  left_join(ecointeraction::birdsplasmodiumrelictum) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))

top_10_incidence_wnv <- tax_wnv_incidence %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_incidence_wnv) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))

top_10_susceptible_wnv <- tax_wnv_susceptible %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_susceptible_wnv) %>%
  left_join(ecointeraction::birdswnv) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))

top_10_incidence_coronavirus <- tax_coronavirus_incidence %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_incidence_coronavirus) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))

top_10_susceptible_coronavirus <- tax_coronavirus_susceptible %>%
  rename(species =  query) %>% dplyr::select(-db) %>%
  left_join(top_10_susceptible_coronavirus) %>%
  left_join(ecointeraction::batscoronavirus) %>%
  dplyr::select(order, family, species, susceptible, incidence) %>%
  mutate(susceptible = round(susceptible, 3))


write_csv(top_10_incidence_plasmodium, "data-raw/top_10_incidence_plasmodium.csv")
write_csv(top_10_susceptible_plasmodium, "data-raw/top_10_susceptible_plasmodium.csv")
write_csv(top_10_incidence_wnv, "data-raw/top_10_incidence_wnv.csv")
write_csv(top_10_susceptible_wnv, "data-raw/top_10_susceptible_wnv.csv")
write_csv(top_10_incidence_coronavirus, "data-raw/top_10_incidence_coronavirus.csv")
write_csv(top_10_susceptible_coronavirus, "data-raw/top_10_susceptible_coronavirus.csv")


