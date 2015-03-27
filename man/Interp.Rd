\name{Interp}
\alias{Interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interpolates spatial data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Interpolates data from regular grid or sparse points to (another) regular grid.
}
\usage{
Interp(x, y, z, xp, yp, method = "linear", type = "points")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of x coordinates (origin)}
  \item{y}{vector of y coordinates (origin)}
  \item{z}{numeric vector of values to be interpolated}
  \item{xp}{vector of x coordinates (target)}
  \item{yp}{vector of y coordinates (target)}
  \item{method}{interpolation method; can be "linear", "spline" or "nearest"}
  \item{type}{interpolation type; can be "points" or "grid"}
}
