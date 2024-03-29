% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swt.R
\name{lifeport_sumstats}
\alias{lifeport_sumstats}
\title{Summary statistics for LifePort data.}
\usage{
lifeport_sumstats(lpdat, ice_threshold = 2.5, infuse_threshold = 10)
}
\arguments{
\item{lpdat}{a list with data from lifeport_process()}

\item{ice_threshold}{threshold for ice temperature in degrees Celsius}

\item{infuse_threshold}{threshold for infuse temperature in degrees Celsius}
}
\value{
a list with LifePort data
}
\description{
Summary statistics for LifePort data.
}
