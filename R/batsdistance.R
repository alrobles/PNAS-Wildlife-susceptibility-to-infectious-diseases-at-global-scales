#' batsdistance
#' A dataset containing the median phylogenetic, geographical and enviromental distances of bats.
#' The raw-data has the distances for each pair of bats. The data was centered and scaled.
#' Then was summarized by each species taking the median for each distance.
#' Environmental information from WorldClim project
#' Phylogenetic information from Vertlife project
#' Geografical information from GBIF project
#'
#'@docType data
#'
#'@keywords datasets
#'
#'@format A data frame with 1191 rows and 12 variables:
#' \describe{
#'   \item{species}{Bats species}
#'   \item{geo_distance}{Geographical distance}
#'   \item{env_distance}{Environmental distance}
#'   \item{phylo_distance}{Phylogenetic distance}
#' }
#'@example
#' data(batsdistance)
"batsdistance"
