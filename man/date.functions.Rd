\name{date.functions}
\alias{it2en.date}
\alias{en2it.date}
\alias{date.lang}
\alias{date2Date}
\alias{Date2date}
\alias{seq.date}
\alias{format.dates}
\alias{ITholidays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to manage dates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to manage dates, useful to read GrADS ctl+bin files.
}
\usage{
it2en.date(date) 
en2it.date(date) 
date.lang() 

date2Date(date) 
Date2date(Date) 

seq.date(from,to,by="1 day") 
format.dates(dates) 

ITholidays(years) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{date}{date string in the format used in GrADS ctl files ('1jan2001')}
\item{Date}{object of class \code{Date}}
\item{from}{first day, in the format used in GrADS ctl files ('1jan2001')}
\item{to}{last day, in the format used in GrADS ctl files ('1jan2001')}
\item{by}{time step}
\item{dates}{character vector of dates in the format 'yyyymmdd'}
\item{years}{numerical vector of years}
}
\details{
The functions \code{it2en.date} and \code{en2it.date} translate a date, formatted as in GrADS ctl files, from Italian to English and vice versa, respectively, while \code{date.lang} checks if the system language is Italian or English.

The functions \code{date2Date} and \code{Date2date} convert a string in the format used in GrADS ctl files ('1jan2001') to an object of class \code{Date} and vice versa, respectively.

\code{seq.date} builds a sequence of dates in the format used in GrADS ctl files ('1jan2001').

\code{format.dates} prepares dates in a way which can be useful for an axis.

\code{ITholidays} returns Italian holidays for given years.
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

\seealso{
\code{\link{holiday}}, \code{\link{seq.dates}}, \code{\link{Date}}, 
}
