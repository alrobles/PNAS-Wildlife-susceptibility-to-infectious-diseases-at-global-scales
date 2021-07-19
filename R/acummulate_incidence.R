#' Acummulate incidence from interaction data frame
#' @importFrom dplyr group_by summarise arrange ungroup mutate row_number select
#' @importFrom rlang enquo :=
#' @param data A \code{data.frame} object with interaction information
#' @param group A group column in data \code{data.frame} to acummulate incidence in an interaction \code{data.frame}
#' @param incidence The incidence of interaction in the data \code{data.frame}
#'
#' @return A \code{data.frame} with the summarized incidence from an interaction \code{data.frame}. Includes the acummulated incidence for each group provided
#' @export
#'
#' @examples
#' library(ecointeraction)
#'  acummulate_incidence(mammalvirus, virus, incidence)
acummulate_incidence <- function(data, group, incidence = incidence ){
  group <- rlang::enquo(group)
  incidence <- rlang::enquo(incidence)
  cummulativesum <- "cummulativesum"
  id <- "id"
  data %>%
    dplyr::group_by(!! group) %>%
    dplyr::summarise(incidence = sum(!! incidence ) ) %>%
    dplyr::arrange(dplyr::desc(incidence)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!! cummulativesum := cumsum(incidence) ) %>%
    dplyr::mutate(!! id := dplyr::row_number()) %>%
    dplyr::select(id, dplyr::everything())
}
