\name{NO2.obs}
\alias{NO2.obs}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Observed NO2 data in Emilia-Romagna
}
\description{
Hourly concentrations of NO2 measured by the monitoring stations in Emilia-Romagna
}
\usage{data("NO2.obs")}
\format{
  A data frame in the long-table format with 236520 hourly observations on the following 9 variables.
  \describe{
    \item{\code{Time}}{sampling time as POSIXct}
    \item{\code{NO2}}{a numeric vector with NO2 concentrations in microgram per cubic meter}
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
data(NO2.obs)
str(NO2.obs)
}
\keyword{datasets}
