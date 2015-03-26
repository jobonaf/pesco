\name{population}
\alias{population}
\docType{data}
\title{
Emilia-Romagna population
}
\description{
Emilia-Romagna population (in year 2010) on a grid with 1km resolution
}
\usage{data("population")}
\format{
List of 2
  \describe{
    \item{\code{coords}}{Coordinates of the 47817 grid cells (UTM 32N WGS84, in meters), as a list of x and y}
    \item{\code{data}}{Population density (people per sq.km). Zero outside Emilia-Romagna region.}
    }
}
\examples{
data(population)
str(population)
}
\keyword{datasets}
