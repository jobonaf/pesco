\name{PM10.obs}
\alias{PM10.obs}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Observed PM10 data in Emilia-Romagna
}
\description{
Daily concentrations of PM10 measured by the monitoring stations in Emilia-Romagna
}
\usage{data("PM10.obs")}
\format{
  A data frame in the long-table format with 760 daily observations on the following 9 variables.
  \describe{
    \item{\code{Time}}{sampling time as POSIXct}
    \item{\code{PM10}}{a numeric vector with PM10 concentrations in microgram per cubic meter}
    \item{\code{Name}}{a factor with the names of the monitoring stations}
    \item{\code{Municipality}}{a factor with the names of the municipalities}
    \item{\code{Code}}{a factor with the codes of the stations}
    \item{\code{Lat}}{latitudes as numeric vector}
    \item{\code{Lon}}{longitudes as numeric vector}
    \item{\code{Elev}}{elevations as numeric vector}
    \item{\code{Type}}{a numeric vector identifying the station type}
  }
}
\examples{
data(PM10.obs)
str(PM10.obs)
}
\keyword{datasets}
