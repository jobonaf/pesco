\name{PM10.ctm}
\alias{PM10.ctm}
\docType{data}
\title{
Concentrations of PM10 simulated by a Chemistry-Transport Model
}
\description{
Hourly concentrations of PM10 simulated by the Chemistry-Transport Model CHIMERE at the ground level
}
\usage{data("PM10.ctm")}
\format{
List of 3
  \describe{
    \item{\code{coords}}{Coordinates of the grid cells (UTM 32N WGS84, in meters), as a list of two numeric matrices [1:128, 1:82], x and y}
    \item{\code{time}}{Time, vector of 24 POSIXct}
    \item{\code{data}}{Concentration in microgram per cubic meter, numeric array [1:128, 1:82, 1:24]}
    }
}
\references{
Stortini, M., et al. "Long-term simulation and validation of ozone and aerosol in the Po Valley." Developments in Environmental Science 6 (2007): 768-770.

Bessagnet, B., et al. "Aerosol modeling with CHIMERE - preliminary evaluation at the continental scale." Atmospheric Environment 38.18 (2004): 2803-2817.
}
\examples{
data(PM10.ctm)
str(PM10.ctm)
}
\keyword{datasets}
