\name{prepare.functions}
\alias{prepare.ctm}
\alias{prepare.emis}
\alias{prepare.elev}
\alias{prepare.obs}
\alias{prepare.day}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to prepare input for \code{kriging}
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to prepare input for \code{kriging}
}
\usage{
prepare.day(day, obs.daily, ctm.daily,
            emis.winter, emis.summer,
            elev = NULL, verbose = FALSE)                        

prepare.ctm(ctm.daily, day,
            x.pnt, y.pnt, x.grd, y.grd,
            conc.min = 10^-6)
prepare.emis(emis.winter, emis.summer, day,
             x.pnt, y.pnt, x.grd = NULL, y.grd = NULL)
prepare.elev(elev, x.pnt, y.pnt, 
             z.pnt = rep(NA,length(x.pnt)),
             x.grd = NULL, y.grd = NULL)   
prepare.obs(obs.daily, day, pollutant,
             conc.min = 10^-6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{day}{required day, in the format "YYYY-MM-DD"}
\item{obs.daily}{data frame with daily observations, as returned by \code{dailyObs}}
\item{ctm.daily}{CTM output, aggregated on a daily basis, as returned by \code{dailyCtm}}
\item{emis.winter}{winter emissions, in a list of 2 elements:
 \describe{
 \item{coords}{coordinates, a list of 2 numeric vectors x and y}
 \item{data}{numeric vector}}}
\item{emis.summer}{summer emissions, in a list of 2 elements:
 \describe{
 \item{coords}{coordinates, a list of 2 numeric vectors x and y}
 \item{data}{numeric vector}}}
\item{elev}{elevation, in a list of 2 elements:
 \describe{
 \item{coords}{coordinates, a list of 2 numeric vectors x and y}
 \item{data}{numeric vector}}}
\item{pollutant}{name of the column with pollutant concentations}
\item{verbose}{logical; if \code{TRUE} some messages are given}
\item{x.pnt}{numeric vector of x coordinates of the stations}
\item{y.pnt}{numeric vector of y coordinates of the stations}
\item{z.pnt}{numeric vector of elevation of the stations}
\item{x.grd}{numeric vector of x coordinates of the grid cells}
\item{y.grd}{numeric vector of y coordinates of the grid cells}
\item{conc.min}{minimum concentration; if a concentration is less than this value, then it is set equal to it}
}
\details{
To prepare the required input for the function \code{kriging}, you need only the function \code{prepare.day}, which includes all the functions \code{prepare.ctm}, \code{prepare.emis}, \code{prepare.elev} and \code{prepare.obs}.
}
