library(phytools)
list.files(pattern = ".tre")
#reading a sample of 1000 trees from birdtree.org
birds <- read.nexus("data-raw/output.nex")
#taking a sample for replication purpose
sp_sample <- sample(birds[1]$tree_1663$tip.label, length(birds[1]$tree_1663$tip.label))

prune_tree <- purrr::map(birds[1:100], keep.tip, sp_sample)
class(prune_tree)<-"multiPhylo"
bird_consensus <- consensus.edges(prune_tree, method = "least.squares")


BirdTable <- bird_consensus %>%
  #bird distance matrix
  cophenetic.phylo() %>%
  #convert as distance
  as.dist(upper = TRUE) %>%
  #tidy distance object
  broom::tidy() %>%
  #mutate factors as characters
  dplyr::mutate_at(c("item1", "item2"), as.character) %>%
  #rename
  rename(phylo.distance = distance) %>%
  # exclude possible nas
  na.exclude() %>%
  # rename species
  dplyr::mutate_at(c("item1", "item2"), function(x) str_replace(x, "_", " " ))

# phylogenetic distance pairs (don't run)
# readr::write_csv(BirdTable,"data-raw/birds_phylo_distance_pairs.csv")

#summarize median distance
BirdTable <- BirdTable %>%
  group_by(item1) %>%
  summarise(phylo.distance = median(phylo.distance))

readr::write_csv(BirdTable, "data-raw/birds_phylo_distance.csv")
