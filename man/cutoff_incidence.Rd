% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cutoff_incidence.R
\name{cutoff_incidence}
\alias{cutoff_incidence}
\title{Cuttoff incidence datatable from interaction data frame
In a incidence \code{data.frame}, cutoff keeps the maximal incidence information
with the minimal number of species possible}
\usage{
cutoff_incidence(data, incidence = incidence, accuracy = 4)
}
\arguments{
\item{data}{A \code{data.frame} object with acummulated interaction information}

\item{incidence}{The incidence of interaction in the data \code{data.frame}}

\item{accuracy}{The accuracy of the cutoff. Since the cutoff is get from second derivate,
the accuracy set the round number of digits to cut the second derivate. Default 5}
}
\value{
A \code{data.frame} with the summarized incidence from an interaction \code{data.frame}. Includes the acummulated incidence for each group provided
}
\description{
Cuttoff incidence datatable from interaction data frame
In a incidence \code{data.frame}, cutoff keeps the maximal incidence information
with the minimal number of species possible
}
\examples{
library(ecointeraction)
acummulate_incidence(mammalvirus, virus, incidence) \%>\% cutoff_incidence
}
