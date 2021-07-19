#' mammalsdistance
#'
#' A dataset containing the median phylogenetic, geographical and enviromental distances of
#' of terrestrial mammals. The raw-data has the distances for each pair of mammals.
#' The data was centered and scaled. Then was summarized by each species taking the
#' median for each distance.
#' Environmental information from WorldClim project
#' Phylogenetic information from Vertlife project
#' Geografical information from GBIF project
#'
#'@docType data
#'
#'@keywords datasets
#'
#'@format A data frame with 5109 rows and 4 variables:
#' \describe{
#'   \item{species}{Mammals species}
#'   \item{geo_distance}{Geographical distance}
#'   \item{env_distance}{Environmental distance}
#'   \item{phylo_distance}{Phylogenetic distance}
#' }
#'@example
#' data(mammalsdistance)
"mammalsdistance"
