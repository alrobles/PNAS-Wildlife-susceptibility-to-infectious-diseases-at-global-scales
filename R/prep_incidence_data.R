#' prep_incidence_data
#' Function to prepare incidence data with distance data to model. Takes the species
#' in the incidence data and join with a sample form the distance data.
#' This allow to get a dataset ready to model with ML framework
#' @importFrom dplyr anti_join sample_n full_join sample_frac select
#' @importFrom rlang enquo quo_name set_names
#' @param incidence A \code{data.frame} object with incidence per species
#' @param distance A \code{data.frame} with enviromental, geographical and
#' phylogenetic median distance
#' @param by A character vector of variables to join by. The distance table and
#' the incidence table must have an species column
#' @return A \code{data.frame} with data prepare to model. Includes a sample of median distance
#'  labeled as unknown class and incidence species labeled as susceptible class
#' @export
#'
#' @examples
#' library(ecointeraction)
#' birdsplasmodium %>%
#'     acummulate_incidence(group = species) %>%
#'     cutoff_incidence() %>%
#'     prep_incidence_data(distance  = birdsdistance)
prep_incidence_data <- function(incidence, distance,  by = "species" ){
  if(!( any( names(distance) == "species")  | any( names(incidence) == "species"  )) ){
    stop('distance or incidence needs an species column')
  }
  by <- rlang::enquo(by)
  by <- rlang::set_names(rlang::quo_name(by), rlang::quo_name(by))
  not_intefected <- dplyr::anti_join(distance, incidence, by = by)

  if(nrow(incidence)*6 < nrow(distance)){
    not_intefected_sample <- not_intefected %>%
      dplyr::sample_n(nrow(incidence)*6)
  }else  {
    stop('The incidence dataset is too large to predict. Please cuttoff your incidence data')
  }
  infected <- dplyr::inner_join(dplyr::select(incidence, by), distance, by = by) %>%
    dplyr::mutate(incidence = 1)
  dplyr::full_join(infected, not_intefected_sample ) %>%
    dplyr::mutate(incidence = ifelse(is.na(incidence), "unknown", "susceptible") %>% as.factor() )  %>%
    dplyr::sample_frac(1L)

}
