library(phytools)
list.files(pattern = ".tre")
mammals <- read.tree("data-raw/mammals_reviewed_tree.tre")

MammalsDistanceMatrix <- mammals %>% cophenetic.phylo()
MammalsTable <- MammalsDistanceMatrix %>%
  as.dist(upper = TRUE) %>%
  broom::tidy()

MammalsTable <- MammalsTable %>%
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  rename(phylo.distance = distance) %>%
  na.exclude()
MammalsTable <- MammalsTable %>%
  dplyr::mutate_at(c("item1", "item2"), function(x) str_replace(x, "_", " " ))
MammalsTable <- MammalsTable %>%
  group_by(item1) %>%
  summarise(phylo.distance = median(phylo.distance))

readr::write_csv(MammalsTable,"data-raw/mammals_phylo_distance.csv" )
