% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_mean_fish_v20220420.R
\name{calc_mean_fish}
\alias{calc_mean_fish}
\title{TCRMP Fish Summary}
\usage{
calc_mean_fish(df, data_type, sum_by)
}
\arguments{
\item{df}{a data frame containing raw fish data.}

\item{data_type}{a character value that specifies what kind of data
is contained in \code{df}.}

\item{sum_by}{a character vector containing variable(s) by which to
summarize fish data.}
}
\description{
Summarizes raw fish data collected by the US Virgin
Islands Territorial Coral Reef Monitoring Program (TCRMP).
}
\examples{
calc_mean_fish(tcrmp_fish, "Abundance",
               c("SampleYear", "Location"))

calc_mean_fish(tcrmp_fish, "Abundance", "Location")


calc_mean_fish(tcrmp_fish, "Biomass",
               c("SampleYear", "Location"))

calc_mean_fish(tcrmp_fish, "Biomass", "Location")
}
