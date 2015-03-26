\name{time.functions}
\alias{Hour}
\alias{Month}
\alias{Year}
\alias{Ymd}
\alias{Ym}
\alias{YQ}
\alias{Ndays}
\alias{Nmonths}
\alias{Ndays.in.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to manage time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to manage time (objects of class \code{POSIXct}).
}
\usage{
Hour   (x, tz = "Africa/Algiers")
Month  (x, tz = "Africa/Algiers")
Year   (x, tz = "Africa/Algiers")
Ymd    (x, tz = "Africa/Algiers")
Ym     (x, tz = "Africa/Algiers")
YQ     (x, tz = "Africa/Algiers")
Ndays  (x, tz = "Africa/Algiers")
Nmonths(x, tz = "Africa/Algiers")

Ndays.in.year(year, tz = "Africa/Algiers")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of class \code{POSIXct}
}
  \item{year}{
year (numeric)
}
  \item{tz}{
%%     ~~Describe \code{tz} here~~
timezone
}
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

\seealso{
\code{\link{POSIXct}}
}
