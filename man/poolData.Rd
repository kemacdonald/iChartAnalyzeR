% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poolData.R
\name{poolData}
\alias{poolData}
\title{Pool Data}
\usage{
poolData(iChart, dependent = "Accuracy", include_T_initial = TRUE,
  RejectFirstGap = TRUE, RejectLongestGap = TRUE, RejectRT = FALSE,
  save_results = TRUE)
}
\arguments{
\item{iChart}{A data frame in iChart format with iChart column names.}

\item{dependent}{A string indicating which dependent measure to use (Accuracy or RT).}

\item{include_T_initial}{A boolean indicating whether Target-initial trials should be included in RT computation.}

\item{RejectFirstGap}{A boolean indicating whether bad first gaps should be filtered out of the computation.}

\item{RejectLongestGap}{A boolean indicating whether bad longest gaps should be filtered out of the computation.}

\item{RejectRT}{A boolean indicating whether bad RTs should be filtered out of the computation.}

\item{save_results}{A boolean indicating whether the results should be saved to disk.}
}
\description{
\code{poolData()} computes the mean accuracy or RT for each each participant and condition.
}
\details{
This function aggregates iChart RT and Accuracy for each trial, participant, and condition,
returning this information in a data frame.
}
\examples{
\dontrun{acc <- poolData(d, RejectFirstGap=FALSE, RejectLongestGap=FALSE, RejectRT=FALSE,
  dependent="Accuracy", paired = TRUE, save_results = TRUE)}

}
