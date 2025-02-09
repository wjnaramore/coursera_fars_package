% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{make_filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{the year of interest with 4 digits}
}
\value{
This function returns a character object of the full filename string
}
\description{
Given the year input, this function creates a filename consistent with the
naming convention used for fars csv files from US National Highway Traffic
Safety Administration's Fatality Analysis Reporting System
}
\examples{
make_filename(2013)

}
