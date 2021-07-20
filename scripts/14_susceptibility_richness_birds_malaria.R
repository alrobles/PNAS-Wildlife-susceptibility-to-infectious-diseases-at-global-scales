library(raster)
library(fasterize)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stars)
library(spatstat)
library(maptools)
library(tidyverse)
library(CoordinateCleaner)
library(stars)
library(ggspatial)
library(maps)
library(cowplot)

birds_plasmodium <- readr::read_rds("data-raw/bird_plasmodium_1000_sims.rds")

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}


birds_predicted <- birds_plasmodium %>%
  purrr::map(function(x) x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))


prediction <-  birds_predicted %>%
  filter(susceptible > 0.5)

birds1 <- sf::read_sf("data-raw/Birds_1/") %>%
  filter(SCINAME %in% birds_predicted$species)

birds2 <- sf::read_sf("data-raw/Birds_2/") %>%
  filter(SCINAME %in% birds_predicted$species)

birds3 <- sf::read_sf("data-raw/Birds_3/") %>%
  filter(SCINAME %in% birds_predicted$species)

birds4 <- sf::read_sf("data-raw/Birds_4/") %>%
  filter(SCINAME %in% birds_predicted$species)

birds <-list(birds1, birds2, birds3, birds4) %>% reduce(rbind)
birds <- birds %>% rename(species = SCINAME)
birds <- birds %>% left_join(prediction)
birds <- birds %>% na.exclude()


world <- ne_countries(scale = "medium", returnclass = "sf")
#resolution 1 degree
r <- raster(world, res = 1)
crs(r) <- crs(birds)
rcount_malaria <- fasterize(birds, r, field = "susceptible", fun = "count", background = 0)
rcount_malaria <- mask(rcount_malaria, world)
writeRaster(x = rcount_malaria, filename = "data-raw/susceptibility_richness_bird_malaria.asc")

plasmodium <- readxl::read_excel("data-raw/MalAvi_7133898_coordinatesOK_ALN_Angel.xlsx") %>% janitor::clean_names()
plasmodium <- plasmodium %>%
  dplyr::filter(lineage_name %in% c("SGS1", "GRW04", "GRW11", "LZFUS01", "PHCOL01") ) %>%
  dplyr::select(species, longitude_decimal_degree, latitude_decimal_degree_20)  %>%
  dplyr::rename(longitude = longitude_decimal_degree, latitude = latitude_decimal_degree_20) %>%
  dplyr::distinct() %>%
  na.exclude()


plasmodium_pts <-  SpatialPoints(plasmodium[ ,2:3])
plasmodium_sf <- sf::st_as_sf(plasmodium[ , c(1, 2:3)], coords = c("longitude", "latitude"))
sf::st_crs(plasmodium_sf) <- crs(rcount_malaria)

plasmodium_ppp <- as.ppp.SpatialPoints(plasmodium_pts)
richness_malaria <- as.im(rcount_malaria)

world <- ne_countries(scale = "medium", returnclass = "sf")
names(rcount_malaria) <- "richness"
richness_stars_malaria <- rcount_malaria %>% stars::st_as_stars(na.rm = TRUE)


m1_malaria <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = richness_stars_malaria) +
  geom_sf(data = plasmodium_sf) +
  #theme_classic() +
  #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  scale_fill_viridis_c(
    alpha = 0.7,
    begin = 0,
    end = 1,
    direction = 1,
    option = "D",
    aesthetics = "fill",
    na.value = NA
  ) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Bird species richness susceptible to avian malaria ") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))


# png(filename = "data-raw/bird_plasmodium_richness_pts.png", height = 480, width = 640)
m1_malaria
# dev.off()


p <- plasmodium_pts[ -c(which(is.na(raster::extract(rcount_malaria, plasmodium_pts))) ), ]
plasmodium_pts <- plasmodium_pts[ -c(which(is.na(raster::extract(rcount_malaria, plasmodium_pts))) ), ]
plasmodium_ppp <- as.ppp.SpatialPoints(p)
Window(plasmodium_ppp) <- Window(richness_malaria)

#Likelihood-ratio test
PPM1 <- ppm(plasmodium_ppp ~ richness_malaria)
PPM0 <- ppm(plasmodium_ppp ~ 1)

plasmodium_anova <- anova(PPM0, PPM1, test="LRT")
plasmodium_anova %>% as_tibble

rho_richness_1 <- rhohat(plasmodium_ppp, richness_malaria,  method = "ratio")
rho_richness_2 <- rhohat(plasmodium_ppp, richness_malaria,  method = "reweight")
rho_richness_3 <- rhohat(plasmodium_ppp, richness_malaria,  method = "transform")

pred_1 <- predict(rho_richness_1)
pred_2 <- predict(rho_richness_2)
pred_3 <- predict(rho_richness_3)

p1_malaria <- ~plot(rho_richness_2, main = "", col = "#D55E00" )
p2_malaria <-  ~plot(pred_2,  main="Estimated intensity avian malaria point patterns in birds \n as a function of richness", legend = "bottom")

pred_malaria <- raster(pred_2)
names(pred_malaria) <- "rho(richness)"
pred_malaria <- mask(pred_malaria, world)
crs(pred_malaria) <-  crs(rcount)
pred_malaria_stars <- pred_malaria %>% stars::st_as_stars(na.rm = TRUE)

pred_malaria <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = pred_malaria_stars) +
  scale_fill_viridis_c(
    alpha = 0.7,
    begin = 0,
    end = 1,
    direction = 1,
    option = "A",
    aesthetics = "fill",
    na.value = NA
  ) +
  ggtitle("Estimated intensity of avian malaria point patterns in birds", subtitle = "instensity as function of susceptible species richness") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18)) +
  labs(fill = bquote(rho)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))


p_hor_malaria <- plot_grid(p1_malaria, p2_malaria,  nrow = 1)
plot_grid(m1_malaria ,p_hor_malaria,  nrow = 2)

library(grid)
library(ggmap)
p_bottom_malaria <-
  ggdraw() +
  draw_plot(p1_malaria, x = 0.12, y = 0.15, width = 0.24, height =  0.45) +
  ggdraw(pred_malaria)

png("data-raw/bird_malaria_richness.png", width = 1200, height = 1200)
plot_grid(m1_malaria, p_bottom_malaria, nrow = 2, ncol = 1)
dev.off()

tiff("data-raw/bird_malaria_richness.tif", width = 1600, height = 1200)
plot_grid(m1_malaria, p_bottom_malaria, nrow = 2, ncol = 1)
dev.off()
