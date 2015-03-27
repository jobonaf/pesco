\name{aqstat.functions}
\alias{stat.period}
\alias{stat.period2}
\alias{which.period}
\alias{stat.window}
\alias{exc.period}
\alias{shift}
\alias{mean.window}
\alias{detect.event}
\alias{aot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to calculate Air Quality indicators
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to calculate simple statistical Air Quality indicators, also for legal purposes.
}
\usage{
stat.period (x, period, necess,       FUN = mean)
stat.period2(x, period, nmax.missing, FUN = mean)
which.period(x, period, necess,       FUN = which.max)
exc.period  (x, period, necess,       threshold)

stat.window(x, window, necess, FUN = mean)
mean.window(x, k, necess)

detect.event(x, threshold)
aot(x, hr, threshold = 80, estimate = T, 
    hr.min = 8, hr.max = 19)

shift(x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of the concentration values
}
  \item{period}{
vector, with the same length as \code{x}, to distinguish different periods (e.g. days, months)
}
  \item{window}{
numerical vectors with two elements; defines the running window, e.g. \code{c(-7,0)} is the 8 hours window for 8hr running mean of ozone or carbon monoxide
}
  \item{necess}{
if >1, number of valid data needed in each time period. If <1, fraction of data needed in each time period.
}
  \item{nmax.missing}{
number of missing data accepted in each time period
}
  \item{FUN}{
the function to be applied
}
  \item{threshold}{
threshold
}
  \item{k}{
in \code{shift}, the number of timesteps you want to shift \code{x}; in \code{mean.window}, the width of the window
}
  \item{hr}{numerical vector of the hours (with the same length as \code{x})}
  \item{estimate}{logical. IF \code{TRUE} the AOT is corrected according to the EU legislation in order to take into account the number of missing values}
  \item{hr.min}{first hour of the timerange over which AOT is calculated}
  \item{hr.max}{last hour of the timerange over which AOT is calculated}
}
\details{
The functions \code{stat.period} and \code{stat.period2} apply the function \code{FUN} over defined time periods, with different approaches in handling missing data. The function \code{which.period} is similar to \code{stat.period}, but you can use it for functions (such as \code{which.min} or \code{which.max} ) which do not accept the argument \code{na.rm}.

Instead \code{stat.window}  operates on a floating window, and calls \code{shift}  that moves the time series forward or backward in time. The function \code{mean.window} do the same and is more efficient, but limited to the moving average.

The function \code{exc.period}  counts exceedances of a given threshold.
Instead \code{detect.event}  returns an array containing the date and time of the exceedances and their duration (expressed in number of timestep).

\code{aot} calculates Accumulated exposure Over Threshold
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
