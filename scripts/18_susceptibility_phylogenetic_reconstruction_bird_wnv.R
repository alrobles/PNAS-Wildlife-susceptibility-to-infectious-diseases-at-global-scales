library(tidyverse)
library(phytools)
library(RColorBrewer)

max_density <- function(x){
  dens <- density(x)
  head(dens$x[which.max(dens$y)], 1)
}

#get results from models
list_results <- read_rds("data-raw/bird_wnv_1000_sims.rds")

wnv_prediction <- list_results %>%
  purrr::map( ~ .x[[5]]) %>% reduce(bind_rows) %>%
  group_by(species) %>%
  summarise(susceptible = max_density(susceptible))


#read consensus tree
birds_tree <- read.tree("data-raw/birds_reviewed_tree.tre")
birds_tree <- force.ultrametric(birds_tree)

birds_tree_tip_names <- birds_tree$tip.label %>%
  enframe %>%
  mutate(species = str_replace(value, "_", " ")) %>%
  left_join(wnv_prediction) %>% na.exclude()

birds_tree <- keep.tip(birds_tree, birds_tree_tip_names$value)
#get susceptible according to tree
susceptible <- setNames(birds_tree_tip_names$susceptible, birds_tree_tip_names$value)

#plot tree with Ramp Palette
{
  png(file = "data-raw/wnv_susceptible_tree.png",
      units="in", width=5, height=5, res=300)
  obj <- contMap(birds_tree, susceptible, fsize = c(0.00001, 1), lwd = 2, outline = FALSE)
  n <- length(obj$cols)
  obj$cols[1:n] <- colorRampPalette(c("#999999", "lightblue", "yellow", "orange", "red"),  space="Lab")(n)
  plot.contMap(obj, lwd = 2, type = "fan",  fsize = c(0.00001, 4), outline = FALSE, legend = FALSE )

  add.color.bar(200, obj$cols, title="Susceptibility",
                lims=c(0.202, 0.9), digits=3, direction="upwards",
                subtitle="", lwd=15, x=250, y=-120, prompt=FALSE)

  dev.off()
}
