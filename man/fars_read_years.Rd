% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Loads FARS data files for given year/s}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{a vector of years for which FARS data file will be loaded}
}
\value{
return NULL for an invalid year. For a valid input, returns list of
FARS data \code{years}
}
\description{
Loads FARS data files for given year/s
}
\examples{
  \dontrun{
    fars_read_years(c(2013, 2014))
    fars_read_years(c("2013", "2014"))
  }
}
