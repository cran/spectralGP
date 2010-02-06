\name{xy2unit}
\alias{xy2unit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scales locations to the unit hypercube for use in spectral GP}
\description{
  Scales locations to \eqn{(0,1)^d} so that they can be related to the
  gridpoints in a spectral GP representation.  The
  \code{locations.scale} argument allows one to scale the
  \code{locations} to a separate set of locations.  E.g., if one wants
  to predict over a certain set of locations, but has a separate
  training set of locations that lie within the prediction set, one
  would use the prediction locations as the \code{locations.scale} argument.
}
\usage{
xy2unit(locations, locations.scale = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locations}{A two-column matrix-like object (vector for one-dimensional data) of
    locations to be scaled. }
  \item{locations.scale}{A two-column matrix-like object (vector for one-dimensional data) of
    locations that provides the function with the min and max
    coordinates in each direction.}
}
\details{
  One may want to use both training and prediction locations as the
  \code{locations.scale} argument to ensure that all locations of
  interest will lie in \eqn{(0,1)^d} and be able to be related to the gridpoints.
}
\value{
  A matrix (vector for one-dimensional data) of scaled locations lying
  in \eqn{(0,1)^d}.
}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}}, \code{\link{new.mapping}}}
\examples{
library(spectralGP)
gp1=gp(c(128,128),matern.specdens,c(1,4))
n=100
locs=cbind(runif(n,0.2,1.2),runif(n,-0.2,1.4))
locs.predict=cbind(runif(n,-0.4,0.8),runif(n,-0.1,1.7))
scaled.locs=xy2unit(locs,rbind(locs,locs.predict))
scaled.locs.predict=xy2unit(locs.predict,rbind(locs,locs.predict))
train.map=new.mapping(gp1,scaled.locs)
predict.map=new.mapping(gp1,scaled.locs.predict)
plot(locs,xlim=c(min(locs[,1],locs.predict[,1]),max(locs[,1],
  locs.predict[,1])),ylim=c(min(locs[,2],locs.predict[,2]),
  max(locs[,2],locs.predict[,2])))
points(locs.predict,col=2)
plot(scaled.locs,xlim=c(0,1),ylim=c(0,1))
points(scaled.locs.predict,col=2)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
