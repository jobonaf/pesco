\name{emissions}
\alias{emissions}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Emissions of PM10 and NO2 in Emilia-Romagna
}
\description{
Proxies of the emission densities of PM10 and NO2 in Emilia-Romagna (in year 2010) disaggregated on a grid with 1km resolution
}
\usage{data("emissions")}
\format{
List of 6 elements:
  \describe{
    \item{\code{PM10.summer}}{Emissions of PM10 in summer}
    \item{\code{PM10.winter}}{Emissions of PM10 in winter}
    \item{\code{PM10.annual}}{Annual emissions of PM10}
    \item{\code{NO2.summer}}{Emissions of NO2 in summer}
    \item{\code{NO2.winter}}{Emissions of NO2 in winter}
    \item{\code{NO2.annual}}{Annual emissions of NO2}
    }
Each of the 6 elements is a list of 2:
  \describe{
    \item{\code{coords}}{Coordinates of the 47817 grid cells (UTM 32N WGS84, in meters), as a list of x and y}
    \item{\code{data}}{Emissions data. Zero outside Emilia-Romagna region.}
    }
}
\examples{
data(emissions)
str(emissions)
}
\keyword{datasets}
