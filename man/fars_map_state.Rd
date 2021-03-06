% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{For a given state and given year, it draws the location of accidents}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{an integer representing state number for which the location will be
drawn}

\item{year}{a vector of years for which FARS data file will be loaded}
}
\value{
plots a map of the state with the accidents as dots on the map.
It throws an error if \code{"state.num"} is not invalid. If there hasn't been
any accident in that year, it produces a message.
}
\description{
For a given state and given year, it draws the location of accidents
}
\examples{
  \dontrun{
    fars_map_state(34, 2013)
    fars_map_state(34, 2014)
    fars_map_state(8, 2013)
  }
}
