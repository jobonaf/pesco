\name{geo.functions}
\alias{ll2utm}
\alias{ll2utm.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to convert coordinates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to convert coordinates
}
\usage{
ll2utm     (rlat, rlon, iz=32)
ll2utm.grid(lat, lon, round=-2, iz=32)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{rlat}{latitude}
\item{rlon}{longitude}
\item{lat}{latitude (numeric vector)}
\item{lon}{longitude (numeric vector)}
\item{iz}{UTM zone}
\item{round}{rounding on the UTM target coordinates}
}
\details{
\code{ll2utm} converts geographical coordinates (latitude/longitude) in UTM WGS84, while \code{ll2utm.grid} does the same on vectors of coordinates, applying some rounding, supposed they are on a regular grid in the target UTM system
}
