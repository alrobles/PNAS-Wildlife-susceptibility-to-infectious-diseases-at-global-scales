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


birds_wnv <- readr::read_rds("data-raw/bird_wnv_1000_sims.rds")

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}
#
#
birds_predicted <- birds_wnv %>%
  purrr::map(function(x) x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))

prediction <-  birds_predicted %>%
  filter(susceptible > 0.5) %>%
  arrange(desc(susceptible)) %>%
  dplyr::select(species, susceptible)

birds1 <- sf::read_sf("data-raw/Birds_1/") %>%
  filter(SCINAME %in% prediction$species)

birds2 <- sf::read_sf("data-raw/Birds_2/") %>%
  filter(SCINAME %in% prediction$species)

birds3 <- sf::read_sf("data-raw/Birds_3/") %>%
  filter(SCINAME %in% prediction$species)

birds4 <- sf::read_sf("data-raw/Birds_4/") %>%
  filter(SCINAME %in% prediction$species)


birds <-list(birds1, birds2, birds3, birds4) %>% reduce(rbind)

birds <- birds %>% rename(species = SCINAME)
birds <- birds %>% na.exclude()

world <- ne_countries(scale = "medium", returnclass = "sf")
r <- raster(world, res = 1)
crs(r) <- crs(birds)
rcount_wnv <- fasterize(birds, r, field = "susceptible", fun = "count", background = 0)
rcount_wnv <- mask(rcount_wnv, world)

wnv <- readxl::read_excel("data-raw/Tolsa2018Birds-WNV.xls")  %>% janitor::clean_names()
wnv <- wnv %>%
  dplyr::select(specie, longitud, latitud)  %>%
  dplyr::distinct() %>%
  na.exclude() %>%
  dplyr::rename(species = specie, longitude = longitud, latitude = latitud)

wnv_pts <-  SpatialPoints(wnv[ ,2:3])
#crs(corona_pts) <-crs(rcount)
wnv_sf <- sf::st_as_sf(wnv[ , c(1, 2:3)], coords = c("longitude", "latitude"))
sf::st_crs(wnv_sf) <- crs(rcount_wnv)

#wnv_pts <- SpatialPointsDataFrame((wnv_expand[ ,2:3]), data = wnv_expand[ , 5])
wnv_ppp <- as.ppp.SpatialPoints(wnv_pts)
richness_wnv <- as.im(rcount_wnv)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
names(rcount_wnv) <- "richness"
richness_stars_wnv <- rcount_wnv %>% stars::st_as_stars(na.rm = TRUE)

m1_wnv <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = richness_stars_wnv) +
  geom_sf(data = wnv_sf) +
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
  ggtitle("Bird species richness susceptible to WNV ") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))




# png(filename = "/home/alrobles/Imágenes/presentacion2020/bat_richness_100.png", height = 480, width = 640)
m1_wnv
# dev.off()


#
# plot(corona_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
# crs(corona_pts) <- crs(rcount)
# plot(rcount)
# points(corona_pts, )
p <- wnv_pts[ -c(which(is.na(raster::extract(rcount_wnv, wnv_pts))) ), ]
#raster::extract(rcount, p)
wnv_pts <- wnv_pts[ -c(which(is.na(raster::extract(rcount_wnv, wnv_pts))) ), ]
#wnv_ppp <- as.ppp.SpatialPoints(wnv_pts)
wnv_ppp <- as.ppp.SpatialPoints(p)
Window(wnv_ppp) <- Window(richness_wnv)

#Likelihood-ratio test
PPM1 <- ppm(wnv_ppp ~ richness_wnv)
PPM0 <- ppm(wnv_ppp ~ 1)

wnv_anova <- anova(PPM0, PPM1, test="LRT")
wnv_anova %>% as_tibble

#rho_richness_1 <- rhohat(wnv_ppp, richness_wnv,  method = "ratio")
rho_richness_wnv <- rhohat(wnv_ppp, richness_wnv,  method = "reweight")
#rho_richness_3 <- rhohat(wnv_ppp, richness_wnv,  method = "transform")

#pred_1 <- predict(rho_richness_1)
pred_rho_wnv <- predict(rho_richness_wnv)
#pred_3 <- predict(rho_richness_3)
#
# p1_wnv <- ~plot(rho_richness_2, main = "Intensity of Wes Nile virus given richness susceptibility", col = "#D55E00" )
# p2_wnv <-  ~plot(pred_2,  main="Estimated intensity of Wes Nile virus in birds \n as a function of richness (covariate)")

p1_wnv <- ~plot(rho_richness_wnv, main = "", col = "#D55E00" )
p2_wnv <-  ~plot(pred_rho_wnv,  main="Estimated intensity avian West Niles virus point patterns in birds \n as a function of richness", legend = "bottom")

pred_wnv <- raster(pred_rho_wnv)
names(pred_wnv) <- "rho(richness)"
pred_wnv <- mask(pred_wnv, world)
crs(pred_wnv) <-  crs(rcount)
pred_wnv_stars <- pred_wnv %>% stars::st_as_stars(na.rm = TRUE)

pred_wnv <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = pred_wnv_stars) +
  #geom_sf(data = plasmodium_sf) +
  #theme_classic() +
  #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  scale_fill_viridis_c(
    alpha = 0.7,
    begin = 0,
    end = 1,
    direction = 1,
    option = "A",
    aesthetics = "fill",
    na.value = NA
  ) +
  #xlab("Longitude") + ylab("Latitude") +
  ggtitle("Estimated intensity of avian wnv point patterns in birds", subtitle = "instensity as function of susceptible species richness") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18)) +
  labs(fill = bquote(rho)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))



p_hor_wnv <- plot_grid(p1_wnv, p2_wnv,  nrow = 1)
plot_grid(m1_wnv ,p_hor_wnv,  nrow = 2)

ecointeraction::batsdistance
ecointeraction::birdsdistance
ecointeraction::mammalsdistance

library(grid)
library(ggmap)
p_bottom_wnv <- ggdraw(pred_wnv) +
  draw_plot(p1_wnv, x = 0.12, y = 0.15, width = 0.24, height =  0.45)

# p_bottom_wnv <- ggdraw(p2_wnv, clip = "on") + theme_cowplot(font_size = 10) +
#    draw_plot(p1_wnv, x = 0.1, y = 0.15, width = 0.15, height =  0.4)

png("data-raw/bird_wnv_richness.png", width = 1200, height = 1200)
plot_grid(m1_wnv, p_bottom_wnv, nrow = 2, ncol = 1)
dev.off()

tiff("data-raw/bird_wnv_richness.png", width = 1600, height = 1200)
plot_grid(m1_wnv, p_bottom_wnv, nrow = 2, ncol = 1)
dev.off()
