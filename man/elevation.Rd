\name{elevation}
\alias{elevation}
\docType{data}
\title{
Emilia-Romagna topography
}
\description{
Emilia-Romagna topography on a grid with 1km resolution
}
\usage{data("elevation")}
\format{
List of 2
  \describe{
    \item{\code{coords}}{Coordinates of the 47817 grid cells (UTM 32N WGS84, in meters), as a list of x and y}
    \item{\code{data}}{Elevation (meters above mean sea level). \code{NA} outside Emilia-Romagna region.}
    }
}
\examples{
data(elevation)
str(elevation)
}
\keyword{datasets}
