library(tidyverse)
library(sf)
library(ecointeraction)
library(MASS)


# top 10 species
top_10_incidence_plasmodium <- read_csv(top_10_incidence_plasmodium, "data-raw/top_10_incidence_plasmodium.csv")
top_10_susceptible_plasmodium <- read_csv(, "data-raw/top_10_susceptible_plasmodium.csv")
top_10_incidence_wnv <- read_csv("data-raw/top_10_incidence_wnv.csv")
top_10_susceptible_wnv <- read_csv(, "data-raw/top_10_susceptible_wnv.csv")
top_10_incidence_coronavirus <- read_csv("data-raw/top_10_incidence_coronavirus.csv")
top_10_susceptible_coronavirus <- read_csv("data-raw/top_10_susceptible_coronavirus.csv")


###############################################
# plasmodium top incidence environmental plots #
###############################################

birds1 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds1/")
birds2 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds2/")
birds3 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds3/")
birds4 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds4/")


birds1 <-  birds1 %>%
  dplyr::filter(SCINAME %in% top_10_incidence_plasmodium$species[1:6])

birds2 <-  birds2 %>%
  filter(SCINAME %in% top_10_incidence_plasmodium$species[1:6])


birds3 <-  birds3 %>%
  filter(SCINAME %in% top_10_incidence_plasmodium$species[1:6])

birds4 <-  birds4 %>%
  filter(SCINAME %in% top_10_incidence_plasmodium$species[1:6])

plasmodium_incidence <- list(birds1, birds2, birds3, birds4) %>% reduce(rbind) %>% rename(scientific = SCINAME)


PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(plasmodium_incidence)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

library(tictoc)
library(furrr)
library(future)
plan(multiprocess)


tic()

plasmodium_top6_incidence <- plasmodium_incidence %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )

toc()

# write_rds(bats_env, "data-raw/bats_env.rds")

plasmodium_incidence_env <- plasmodium_top6_incidence %>%
  purrr::reduce(bind_rows)

plasmodium_incidence_env_sample <- plasmodium_incidence_env %>%
  group_by(species) %>%
  sample_n(500)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

plasmodium_incidence_env_summarize <- plasmodium_incidence_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

plasmodium_incidence_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = plasmodium_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = plasmodium_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = plasmodium_incidence_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(plasmodium_incidence_env_plot, "data-raw/plasmodium_incidence_env_plot.rds")
png("data-raw/plasmodium_incidence_env_plot.png", width = 480*2, height = 480*2)
plasmodium_incidence_env_plot
dev.off()


###############################################
# plasmodium top susceptible enviromental plots #
###############################################

birds1 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds1/")
birds2 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds2/")
birds3 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds3/")
birds4 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds4/")


birds1 <-  birds1 %>%
  dplyr::filter(SCINAME %in% top_10_susceptible_plasmodium$species[1:6])

birds2 <-  birds2 %>%
  filter(SCINAME %in% top_10_susceptible_plasmodium$species[1:6])


birds3 <-  birds3 %>%
  filter(SCINAME %in% top_10_susceptible_plasmodium$species[1:6])

birds4 <-  birds4 %>%
  filter(SCINAME %in% top_10_susceptible_plasmodium$species[1:6])

plasmodium_susceptible <- list(birds1, birds2, birds3, birds4) %>% reduce(rbind) %>% rename(scientific = SCINAME)


PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(plasmodium_susceptible)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

library(tictoc)
library(furrr)
library(future)
plan(multiprocess)


tic()

plasmodium_top6_susceptible <- plasmodium_susceptible %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )

toc()

# write_rds(bats_env, "data-raw/bats_env.rds")

plasmodium_susceptible_env <- plasmodium_top6_susceptible %>%
  purrr::reduce(bind_rows)

plasmodium_susceptible_env_sample <- plasmodium_susceptible_env %>%
  group_by(species) %>%
  sample_n(1000)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

plasmodium_susceptible_env_summarize <- plasmodium_susceptible_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

plasmodium_susceptible_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = plasmodium_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = plasmodium_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = plasmodium_susceptible_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  #ggtitle("Enviromental space") +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(plasmodium_susceptible_env_plot, "data-raw/plasmodium_susceptible_env_plot.rds")
png("data-raw/plasmodium_susceptible_env_plot.png", width = 480*2, height = 480*2)
plasmodium_susceptible_env_plot
dev.off()


### wnv inicdence
birds1 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds1/")
birds2 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds2/")
birds3 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds3/")
birds4 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds4/")


birds1 <-  birds1 %>%
  dplyr::filter(SCINAME %in% top_10_incidence_wnv$species[1:6])

birds2 <-  birds2 %>%
  filter(SCINAME %in% top_10_incidence_wnv$species[1:6])


birds3 <-  birds3 %>%
  filter(SCINAME %in% top_10_incidence_wnv$species[1:6])

birds4 <-  birds4 %>%
  filter(SCINAME %in% top_10_incidence_wnv$species[1:6])

wnv_incidence <- list(birds1, birds2, birds3, birds4) %>% reduce(rbind) %>% rename(scientific = SCINAME)


PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(wnv_incidence)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

wnv_top6_incidence <- wnv_incidence %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )


# write_rds(bats_env, "data-raw/bats_env.rds")

wnv_incidence_env <- wnv_top6_incidence %>%
  purrr::reduce(bind_rows)

wnv_incidence_env_sample <- wnv_incidence_env %>%
  group_by(species) %>%
  sample_n(500)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

wnv_incidence_env_summarize <- wnv_incidence_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

wnv_incidence_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = wnv_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = wnv_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = wnv_incidence_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  #ggtitle("Enviromental space") +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(wnv_incidence_env_plot, "data-raw/wnv_incidence_env_plot.rds")
png("data-raw/wnv_incidence_env_plot.png", width = 480*2, height = 480*2)
wnv_incidence_env_plot
dev.off()


### wnv susceptible
birds1 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds1/")
birds2 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds2/")
birds3 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds3/")
birds4 <- sf::read_sf("/home/alrobles/maps/shapes/birds/birds4/")


birds1 <-  birds1 %>%
  dplyr::filter(SCINAME %in% top_10_susceptible_wnv$species[1:6])

birds2 <-  birds2 %>%
  filter(SCINAME %in% top_10_susceptible_wnv$species[1:6])


birds3 <-  birds3 %>%
  filter(SCINAME %in% top_10_susceptible_wnv$species[1:6])

birds4 <-  birds4 %>%
  filter(SCINAME %in% top_10_susceptible_wnv$species[1:6])

wnv_susceptible <- list(birds1, birds2, birds3, birds4) %>% reduce(rbind) %>% rename(scientific = SCINAME)


PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(wnv_susceptible)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

wnv_top6_susceptible <- wnv_susceptible %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )


# write_rds(bats_env, "data-raw/bats_env.rds")

wnv_susceptible_env <- wnv_top6_susceptible %>%
  purrr::reduce(bind_rows)

wnv_susceptible_env_sample <- wnv_susceptible_env %>%
  group_by(species) %>%
  sample_n(500)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

wnv_susceptible_env_summarize <- wnv_susceptible_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

wnv_susceptible_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = wnv_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = wnv_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = wnv_susceptible_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  #ggtitle("Enviromental space") +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(wnv_susceptible_env_plot, "data-raw/wnv_susceptible_env_plot.rds")
png("data-raw/wnv_susceptible_env_plot.png", width = 480*2, height = 480*2)
wnv_susceptible_env_plot
dev.off()





### wnv inicdence
mammals <- sf::read_sf("/home/alrobles/maps/shapes/mammals/")


coronavirus_incidence <-  mammals %>%
  dplyr::filter(scientific %in% top_10_incidence_coronavirus$species[1:6])

PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(coronavirus_incidence)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

coronavirus_top6_incidence <- coronavirus_incidence %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )


# write_rds(bats_env, "data-raw/bats_env.rds")

coronavirus_incidence_env <- coronavirus_top6_incidence %>%
  purrr::reduce(bind_rows)

coronavirus_incidence_env_sample <- coronavirus_incidence_env %>%
  group_by(species) %>%
  sample_n(500)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

coronavirus_incidence_env_summarize <- coronavirus_incidence_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

coronavirus_incidence_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = coronavirus_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = coronavirus_incidence_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = coronavirus_incidence_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  #ggtitle("Enviromental space") +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(coronavirus_incidence_env_plot, "data-raw/coronavirus_incidence_env_plot.rds")
png("data-raw/coronavirus_incidence_env_plot.png", width = 480*2, height = 480*2)
coronavirus_incidence_env_plot
dev.off()

### wnv susceptible
mammals <- sf::read_sf("/home/alrobles/maps/shapes/mammals/")


coronavirus_susceptible <-  mammals %>%
  dplyr::filter(scientific %in% top_10_susceptible_coronavirus$species[1:6])

PCA <- list.files(path = "/home/alrobles/worldclim/PCA/", ".tif", full.names = TRUE) %>%
  stars::read_stars()

st_crs(PCA) <- st_crs(coronavirus_susceptible)
#bat_crops <- st_crop(PCA, bats_shapes, crop = FALSE)

safely_st_crop <- purrr::possibly(.f = st_crop, otherwise = NULL, quiet = FALSE)

coronavirus_top6_susceptible <- coronavirus_susceptible %>%
  split(., .$scientific) %>%
  #furrr::future_map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
  purrr::map2(.x = ., .y = names(.), function(i, j) safely_st_crop(PCA, i, crop = TRUE) %>%
                as_tibble() %>%
                na.exclude() %>%
                mutate(species = j) )


# write_rds(bats_env, "data-raw/bats_env.rds")

coronavirus_susceptible_env <- coronavirus_top6_susceptible %>%
  purrr::reduce(bind_rows)

coronavirus_susceptible_env_sample <- coronavirus_susceptible_env %>%
  group_by(species) %>%
  sample_n(500)
# ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#   stat_ellipse(geom = "polygon")

PCA_sample <- PCA %>% as_tibble() %>%
  na.exclude() %>%
  sample_frac(0.03)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

coronavirus_susceptible_env_summarize <- coronavirus_susceptible_env %>%
  group_by(species) %>%
  summarise(PCA1 = max_density(PCA1.tif),
            PCA2 = max_density(PCA2.tif),
            PCA3 = max_density(PCA3.tif))
#"#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

coronavirus_susceptible_env_plot <- PCA_sample %>%
  ggplot() +
  geom_point(aes(PCA1.tif, PCA2.tif), size = 0.1, col = "#BBBBBB")  +
  geom_point(data = coronavirus_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), size = 0.1, alpha = 0.4) +
  scale_alpha_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values=c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  stat_ellipse(data = coronavirus_susceptible_env_sample,  aes(PCA1.tif, PCA2.tif, fill = species, col = species), geom = "polygon", alpha = 0.6, size = 1.3) +
  geom_point(data = coronavirus_susceptible_env_summarize, aes(PCA1, PCA2,  shape = species ), col = "black", size = 10) +
  theme_classic() +
  #ggtitle("Enviromental space") +
  xlab("PC1") +
  xlim(-6, 10) +
  ylab("PC2") +
  ylim(-5, 21) +
  #ggtitle("Species environmental centroids") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        legend.title=element_text(size = 30, face = "bold"),
        legend.text=element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.text = element_text(face = "bold.italic"))
readr::write_rds(coronavirus_susceptible_env_plot, "data-raw/coronavirus_susceptible_env_plot.rds")
png("data-raw/coronavirus_susceptible_env_plot.png", width = 480*2, height = 480*2)
coronavirus_susceptible_env_plot
dev.off()
