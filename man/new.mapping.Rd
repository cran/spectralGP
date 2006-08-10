\name{new.mapping}
\alias{new.mapping}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Map arbitrary locations to gridpoints of spectral GP object }
\description{
  Finds the nearest gridpoint in a spectral GP representation for each
  supplied location based on Euclidean distance.
}
\usage{
new.mapping(object, locations)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{locations}{A two-column matrix-like object (vector for one-dimensional data)
    of locations of interest, for which the first column is the first
    coordinate and the second column the second coordinate.  Locations
    should lie in \eqn{(0,1)^d}, as the process representation is on a grid on \eqn{(0,1)^d}.}
}
\details{
}
\value{
  A vector for which each element is the index of the gridpoint nearest
  the location.  The indices run from 1 to \eqn{(k/2)^d} where k the number of
  gridpoints in each direction (assuming there are an equal number in
  each direction).  The indices run along the first dimension from the
  lower right corner of the space, e.g.,
  13 14 15 16
  9 10 11 12
  5 6 7 8
  1 2 3 4
}
\references{For technical details, see Paciorek, C.J. 2006. Bayesian Smoothing of Irregularly-spaced Data Using Fourier Basis Functions. Harvard University Biostatistics Working Paper Series. Working Paper 49.  \url{http://www.bepress.com/harvardbiostat/paper49}}
\author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{predict.gp}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

library(spectralGP)
loc1=runif(100)
loc2=cbind(runif(100),runif(100,0,1))
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
map1=new.mapping(gp1,loc1)
map2=new.mapping(gp2,loc2)
simulate(gp1)
simulate(gp2)
vals1=predict(gp1,mapping=map1)
vals2=predict(gp2,mapping=map2)
plot(gp1)
points(loc1,vals1)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
