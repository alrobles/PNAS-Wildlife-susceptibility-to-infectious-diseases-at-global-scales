library(raster)
library(fasterize)
library(tidyverse)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(stars)
library(spatstat)
library(maptools)
library(tidyverse)
library("CoordinateCleaner")
library(stars)
library("ggspatial")
library(maps)
library(cowplot)

bat_coronavirus <- readr::read_rds("data-raw/bat_coronavirus_1000_sims.rds")

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}


bats_predicted <- bat_coronavirus %>%
  purrr::map(function(x) x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))


mammals <- sf::read_sf("data-raw/Mammals/terrestrial_mammals_diss_scientific.shp") %>%
  rename(SCIENTIFIC = scientific)

prediction <-  bats_predicted %>%
  filter(susceptible > 0.5)

bats <-  mammals %>% inner_join(prediction, by = c("SCIENTIFIC" = "species"))
bats <- bats %>% rename(species = SCIENTIFIC)
bats <- bats %>% dplyr::select(species, susceptible, geometry)
# dir.create("/home/alrobles/maps/shapes/susceptibility/coronavirus")
# sf::st_write(bats, "/home/alrobles/maps/shapes/susceptibility/coronavirus/bats_coronavirus_susceptibility.shp")

world <- ne_countries(scale = "medium", returnclass = "sf")
r <- raster(world, res = 2)
crs(r) <- crs(bats)
rcount_bats <- fasterize(bats, r, field = "susceptible", fun = "count", background = 0)
rcount_bats <- mask(rcount_bats, world)

writeRaster(rcount_bats, "data-raw/susceptibility_richness_bats_coronavirus.asc")

corona <- read_csv("data-raw/bat_coronavirus_DB.csv")  %>% dplyr::select(c(2, 11, 12)) %>% distinct() %>% na.exclude() %>%
  rename(species = from_bat)
corona_pts <-  SpatialPoints(corona[ ,2:3])
#crs(corona_pts) <-crs(rcount)
corona_sf <- sf::st_as_sf(corona[ , c(1, 2:3)], coords = c("longitud", "latitude"))
sf::st_crs(corona_sf) <- crs(rcount_bats)

#wnv_pts <- SpatialPointsDataFrame((wnv_expand[ ,2:3]), data = wnv_expand[ , 5])
corona_ppp <- as.ppp.SpatialPoints(corona_pts )
richness_coronavirus <- as.im(rcount_bats)

#corona_rescale <- rescale(corona_ppp, unitname = "grades")

world <- ne_countries(scale = "medium", returnclass = "sf")
names(rcount_bats) <- "richness"
richness_stars_coronavirus <- rcount_bats %>% stars::st_as_stars(na.rm = TRUE)


m1_coronavirus <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = richness_stars_coronavirus) +
  geom_sf(data = corona_sf) +
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
  ggtitle("Bats species richness susceptible to coronavirus ") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))


# png(filename = "/home/alrobles/Imágenes/presentacion2020/bat_richness_100.png", height = 480, width = 640)
m1_coronavirus
# dev.off()


#
# plot(corona_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
# crs(corona_pts) <- crs(rcount)
# plot(rcount)
# points(corona_pts, )

p <- corona_pts[ -c(which(is.na(raster::extract(rcount_bats, corona_pts))) ), ]
#raster::extract(rcount, p)
#corona_pts <- corona_pts[ -c(which(is.na(raster::extract(rcount_bats, corona_pts))) ), ]
#plasmodium_ppp <- as.ppp.SpatialPoints(plasmodium_pts)
corona_ppp <- as.ppp.SpatialPoints(p)
Window(corona_ppp) <- Window(richness_coronavirus)
# corona_pts <- corona_pts[ -c(which(is.na(raster::extract(rcount, corona_pts) )) ), ]
# corona_ppp <- as.ppp.SpatialPoints(corona_pts)
# Window(corona_ppp) <- Window(richness_coronavirus)

#Likelihood-ratio test
PPM1 <- ppm(corona_ppp ~ richness_coronavirus)
PPM0 <- ppm(corona_ppp ~ 1)

corona_anova <- anova(PPM0, PPM1, test="LRT")
corona_anova %>% as_tibble

rho_richness_1 <- rhohat(corona_ppp, richness_coronavirus,  method = "ratio")
rho_richness_2 <- rhohat(corona_ppp, richness_coronavirus,  method = "reweight")
rho_richness_3 <- rhohat(corona_ppp, richness_coronavirus,  method = "transform")

pred_1 <- predict(rho_richness_1)
pred_2 <- predict(rho_richness_2)
pred_3 <- predict(rho_richness_3)

p1_coronavirus <- ~plot(rho_richness_2, main = "", col = "#D55E00" )
p2_coronavirus <-  ~plot(pred_2,  main="Estimated intensity coronavirus point patterns in bats \n as a function of richness", legend = "bottom")

pred_coronavirus <- raster(pred_2)
names(pred_coronavirus) <- "rho(richness)"
pred_coronavirus <- mask(pred_coronavirus, world)
crs(pred_coronavirus) <-  crs(rcount)
pred_coronavirus_stars <- pred_coronavirus %>% stars::st_as_stars(na.rm = TRUE)

pred_coronavirus <- ggplot(data = world) +
  theme_void() +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1, "in") ,width = unit(1, "in"),
                         pad_x = unit(0.5, "in"), pad_y = unit(4.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_stars(data = pred_coronavirus_stars) +
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
  ggtitle("Estimated intensity of coronavirus point patterns in bats", subtitle = "instensity as function of susceptible species richness") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18)) +
  labs(fill = bquote(rho)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(face = "bold", size = 18))



p_hor_coronavirus <- plot_grid(p1_coronavirus, p2_coronavirus,  nrow = 1)
# plot_grid(m1_coronavirus ,p_hor_coronavirus,  nrow = 2)

library(grid)
library(ggmap)
p_bottom_coronavirus <-
  ggdraw(pred_coronavirus) +
  draw_plot(p1_coronavirus, x = 0.12, y = 0.15, width = 0.24, height =  0.45)
# p_bottom_coronavirus <- ggdraw(p2_coronavirus, clip = "on") + theme_cowplot(font_size = 10) +
#    draw_plot(p1_coronavirus, x = 0.1, y = 0.15, width = 0.15, height =  0.4)

png("data-raw/bat_coronavirus_richness.png", width = 1200, height = 1200)
plot_grid(m1_coronavirus, p_bottom_coronavirus, nrow = 2, ncol = 1)
dev.off()

tiff("data-raw/bat_coronavirus_richness.tif", width = 1200, height = 1200)
plot_grid(m1_coronavirus, p_bottom_coronavirus, nrow = 2, ncol = 1)
dev.off()


