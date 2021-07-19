#' Cuttoff incidence datatable from interaction data frame
#' In a incidence \code{data.frame}, cutoff keeps the maximal incidence information
#' with the minimal number of species possible
#' @importFrom dplyr arrange slice pull
#' @importFrom rlang enquo :=
#' @importFrom stats na.exclude
#' @param data A \code{data.frame} object with acummulated interaction information
#' @param incidence The incidence of interaction in the data \code{data.frame}
#' @param accuracy The accuracy of the cutoff. Since the cutoff is get from second derivate,
#' the accuracy set the round number of digits to cut the second derivate. Default 5
#' @return A \code{data.frame} with the summarized incidence from an interaction \code{data.frame}. Includes the acummulated incidence for each group provided
#' @export
#'
#' @examples
#' library(ecointeraction)
#'acummulate_incidence(mammalvirus, virus, incidence) %>% cutoff_incidence
cutoff_incidence <- function(data, incidence = incidence, accuracy = 4 ){

  incidence <- rlang::enquo(incidence)
  cummulativesum <- "cummulativesum"
  id <- "id"
  id <- rlang::enquo(id)
  ratediffdiff <- "ratediffdiff"
  ratediffdiff <- rlang::enquo(ratediffdiff)

  #data <- acummulate_incidence(mammalvirus, group = mammal_species)
  data_cummulative_rate <- cummulative_rate(data, incidence = incidence,
                                            cummulativesum = cummulativesum,  accuracy = accuracy) %>%
    stats::na.exclude()
  id_min <- data_cummulative_rate %>%
    dplyr::arrange( ratediffdiff) %>%
    dplyr::slice(1) %>%
    dplyr::pull(id)

  data %>%
   dplyr::filter(id <= id_min)
}
