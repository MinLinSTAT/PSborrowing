% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_posterior.R
\name{find_posterior}
\alias{find_posterior}
\title{Calculate Posterior Values for Summary Data}
\usage{
find_posterior(summ, alpha, power = FALSE)
}
\arguments{
\item{summ}{A data frame containing the summary data. The data frame must include
a \code{stratum} column indicating group stratifications.}

\item{alpha}{A data frame containing alpha values, also stratified by \code{stratum}.}

\item{power}{Whether calculates the power for a design problem. Currently only calculate \eqn{\Pi(\Delta>0|D)}.}
}
\value{
A tibble containing posterior values for each \code{stratum} and associated
variables. The \code{stratum} and \code{variable} columns are converted to factors.
}
\description{
This function calculates posterior values for a given summary data frame (\code{summ})
using alpha values (\code{alpha}). It splits the input data by \code{stratum}, computes
posteriors for each group using a helper function \code{.get_posterior}, and returns
a combined tibble with the results.
}
\details{
\itemize{
\item The \code{summ} and \code{alpha} data frames are split by the \code{stratum} column.
\item The helper function \code{.get_posterior} is applied to corresponding subsets of
\code{summ} and \code{alpha} to calculate posterior values.
\item The results are combined into a single tibble and formatted with \code{stratum} and
\code{variable} columns as factors.
}
}
