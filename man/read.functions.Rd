\name{read.functions}
\alias{read.ncdf.arpaer}
\alias{read.grads}
\alias{read.qaria}
\alias{read.sql}
\alias{qaria2long}
\alias{read.field}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read data as provided by Arpa-ER
}
\description{
Functions to read some data as provided by Arpa Emilia-Romagna
}
\usage{
read.qaria (file) 
read.sql   (file) 
qaria2long (datafiles, anafile, codes=NULL)

read.ncdf.arpaer(con=NULL, pollutant="pm10", lev=1, 
                 tz.in="UTC", tz.out="Africa/Algiers")
read.grads(filectl) 

read.field(file, coords.col=1:2, data.col=3, coords.fact=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{file}{name of the input file}
\item{datafiles}{vector of names of files (format 'estra_qaria', an internal standard for some people in Arpa-ER)}
\item{anafile}{file with stations metadata}
\item{codes}{vector with stations codes, if \code{NULL} it is argued from the elements of \code{datafiles}, if they are in the form /path/to/files/pollutant_stationcode.asc}
\item{con}{name of a NetCDF file, or an already open connection to NetCDF}
\item{pollutant}{pollutant code used in the NetCDF}
\item{lev}{level}
\item{tz.in}{timezone used by the CTM}
\item{tz.out}{timezone used for the observed data}
\item{filectl}{name of the GrADS ctl file}
\item{coords.col}{a vector with the numbers of the columns containing the coordinates}
\item{data.col}{a scalar with the number of the column containing the data}
\item{coords.fact}{multiplication factor for coordinates (e.g. 1000 if you need to convert kilometers to meters)}
\item{...}{optional arguments; will be passed to \code{read.table}, called by \code{read.field}}
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
\note{If your input data are not provided following internal 'standard' formats of Arpa-ER, you don't need these functions, except maybe \code{read.field}, which is quite general. Then, you'll need to write your own reading functions, suitable for your data format.

The function \code{read.grads} is deprecated, since I wrote it when I was young, and it needs external executables.}