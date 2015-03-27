\name{boxcox}
\alias{boxcox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Box-Cox transformation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
One parameter Box-Cox transformation
}
\usage{
boxcox(x, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
numeric vector to be trasformed
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
lambda parameter, as numeric scalar
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If \eqn{\lambda = 0}, then \eqn{log(x)}.

If \eqn{\lambda = 1}, then \eqn{x}. 

Otherwise, \eqn{(x^\lambda - 1)/\lambda}.
}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
%% ~put references to the literature/web site here ~
Box, George EP, and David R. Cox. "An analysis of transformations." Journal of the Royal Statistical Society. Series B (Methodological) (1964): 211-252.
}
