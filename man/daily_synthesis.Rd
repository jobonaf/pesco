\name{daily_synthesis}
\alias{dailyStat}
\alias{dailyObs}
\alias{dailyCtm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Daily AQ indicators
}
\description{
Functions to calculate daily Air Quality indicators
}
\usage{
dailyCtm(data, statistic) 
dailyObs(data, statistic, pollutant, 
         Time="Time", Code="Code",
         others=c("Name","Municipality",
                  "Lat","Lon","Elev","Type"))
         
dailyStat(x, time, statistic, necess=0.75) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{input hourly data. For \code{dailyObs}, a data frame; for \code{dailyCtm}, a list of 3 elements:
 \describe{
 \item{coords}{coordinates, in a list of 2
  \describe{
  \item{x}{numeric matrix \code{[nx,ny]}}
  \item{y}{numeric matrix \code{[nx,ny]}}
  }
 }
 \item{time}{vector of \code{nt} POSIXct}
 \item{data}{concentration values in a 3 dimensions array \code{[nx,ny,nt]}}
 }
 }
\item{statistic}{daily statistic to be used; possible values are "mean", "max" and "max8h" (daily maximum of the 8hr running mean)}
\item{pollutant}{name of the column with pollutant concentations}
\item{Time}{name of the column with time}
\item{Code}{name of the column with station's code}
\item{others}{vector of the names of the columns with station's static attributes}
\item{x}{numeric vector of hourly data}
\item{time}{vector of date-times as POSIXct}
\item{necess}{fraction of valid hourly data needed in a day}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
