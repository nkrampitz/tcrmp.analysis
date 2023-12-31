% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shearwater_compile_v20220419.R
\name{shearwater_compile}
\alias{shearwater_compile}
\title{Compile Shearwater Dive Log}
\usage{
shearwater_compile(path, displ_year)
}
\arguments{
\item{path}{file pathway to folder where \code{.csv} files are located
and to which the compiled file will be saved.}

\item{displ_year}{(optional) if supplied, summary statistics for the
indicated year will be displayed.}
}
\description{
Combines and cleans exported dive logs (\code{.csv}) from
the Shearwater Desktop platform and saves the compiled data as a new
file.
}
\details{
Optional statistics displayed include: dive year, dive mode (air,
nitrox, CCR), dive depth range, total number of dives, and total dive
time.
}
\examples{
\dontrun{

shearwater_compile("C:/Users/User/Documents/Logs")

shearwater_compile("C:/Users/User/Documents/Logs", 2020)
}
}
