% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_wells.R
\name{remove_wells}
\alias{remove_wells}
\title{remove_wells}
\usage{
remove_wells(
  dt,
  method = "without transformation",
  background = c("A01", "H01", "A12", "H12"),
  exclude = NULL,
  nMeasures = 14
)
}
\arguments{
\item{dt}{dataframe}

\item{method}{character}

\item{background}{character vector}

\item{exclude}{character vector}

\item{nMeasures}{integer - how many timepoints}
}
\description{
removes background and user-specified.
}
