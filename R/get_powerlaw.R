#' Continuous maximum likelihood estimators for fitting the power-law distribution to data.
#' This fuction is based on poweRlaw package using the methods described in Clauset et al, 2009.
#' @importFrom poweRlaw displ estimate_xmin bootstrap_p dist_rand
#' @importFrom parallel clusterExport
#' @importFrom stats time
#' @param incidence The incidence of interaction in the data \code{data.frame}
#' @param threads The number of threads used in boostrapping
#' @param seed Set the seed for replication
#' @param no_of_sims Set the seed number of replications in bootstrap
#' @return A \code{list} with the power law distribution fitted to incidence data and de p-value of the test
#' @examples
#' \dontrun{
#' #' library(ecointeraction)
#' data(birdsplasmodiumrelictum)
#' get_powerlaw(birdsplasmodiumrelictum)
#' }
#' @export
get_powerlaw <- function(incidence, threads = 1, seed = 123, no_of_sims=1000){
  if( is.data.frame(incidence) ){
    incidence = incidence$incidence
  }
  if(is.numeric(incidence)){
    #Ensure decressing counting
    incidence <- sort(incidence, decreasing = TRUE)
    pl = poweRlaw::displ$new(incidence)
    est = poweRlaw::estimate_xmin(pl)
    pl$setXmin(est)
    bs_pl <- poweRlaw::bootstrap_p(m = pl, no_of_sims=no_of_sims, threads=threads, seed = seed)
    return(list(pl = pl, est = est, bs_pl = bs_pl))
  } else{
    stop("the vector is an numeric vector. Can't calculate distribution")
  }
}
