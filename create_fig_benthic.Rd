% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_fig_benthic_v20220419.R
\name{create_fig_benthic}
\alias{create_fig_benthic}
\title{TCRMP Report Benthic Cover Summary Figures}
\usage{
create_fig_benthic(df, cover, sample_years, path)
}
\arguments{
\item{df}{a data frame containing mean benthic cover and
SEM for each benthic cover category by location for each sampling
period.}

\item{cover}{a vector of benthic cover categories for which figures
should be created.}

\item{sample_years}{a vector of years (\code{c(start, end)}) to include
in summary figures.}

\item{path}{a file pathway to where summary \code{.jpg}s should be
saved.}
}
\description{
Creates summary figures of benthic cover categories
from data collected by the US Virgin Islands Territorial Coral
Reef Monitoring Program (TCRMP) for a specified range of years.
}
\details{
The figures created match the format of those published in
the annual TCRMP report. Annual reports can be found at:
(\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
}
\examples{
\dontrun{

ben_cov <- calc_mean_cover(tcrmp_benthic,
                           c("SampleYear", "Location"))

create_fig_benthic(ben_cov, c("Coral", "OFAV", "Spo"),
                   c(2005, 2020), "C:/Users/User/Documents")
}
}
