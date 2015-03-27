\name{kriging}
\alias{kriging}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Trans-Gaussian kriging
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Trans-Gaussian kriging
}
\usage{
kriging(x.pnt, y.pnt, obs, model, 
        proxy.1, proxy.2 = NULL, lambda, 
        K.max.dist = 2e+05, K.min.dist = 1000, K.pairs.min = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.pnt}{vector of x coordinates of the stations}
  \item{y.pnt}{vector of y coordinates of the stations}
  \item{obs}{numeric vector of daily values observed at the stations}
  \item{model}{daily values provided by a chemistry-transport model, interpolated over the target grid}
  \item{proxy.1}{external variable, defined over the target grid}
  \item{proxy.2}{another external variable (optional), defined over the target grid}
  \item{lambda}{parameter for the Box-Cox transformation}
  \item{K.max.dist}{a numerical value defining the maximum distance for the variogram; pairs of locations separated for distance larger than this value are ignored for the variogram calculation}
  \item{K.min.dist}{a numeric value; points which are separated by a distance less than this value are considered co-located}
  \item{K.pairs.min}{an integer number defining the minimum numbers of pairs for the bins}
}

\seealso{
To properly format the input data: \code{\link{prepare.day}}
}
