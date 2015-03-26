\name{char.functions}
\alias{xgrep}
\alias{subwrd}
\alias{small}
\alias{capital}
\alias{Capital}
\alias{trim.leading}
\alias{trim.trailing}
\alias{trim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to manage strings
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions to manage strings.
}
\usage{
xgrep (pattern,string,where=FALSE) 
subwrd(string,pos) 
small (string) 

capital      (strings) 
Capital      (strings) 
trim.leading (strings) 
trim.trailing(strings) 
trim         (strings) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{string}{string}
\item{strings}{vector of strings}
\item{pattern}{pattern to be found}
\item{pos}{position of the word in the string}
\item{where}{logical. If \code{TRUE}, the first position of the \code{pattern} in \code{string} is returned. If \code{FALSE}, it is only checked if the \code{pattern} is included in \code{string}}
}
