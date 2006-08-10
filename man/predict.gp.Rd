\name{predict.gp}
\alias{predict.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction from a spectral GP object}
\description{
  Produces the process values of a spectral GP object on the defined
  grid or predicts process values for a new set of inputs (domain points).
}
\usage{
predict.gp(object,newdata=NULL,mapping=NULL,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}. }
  \item{newdata}{An optional two-column matrix-like object (vector for one-dimensional data)
    of locations of interest, for which the first column is the first
    coordinate and the second column the second coordinate.  Locations
    should lie in \eqn{(0,1)^d}, as the process representation is on a grid on \eqn{(0,1)^d}.}
  \item{mapping}{Optional output of \code{new.mapping}, which creates a vector of indices mapping the prediction locations to
    their nearest gridpoints.}
  \item{\dots}{Other arguments.}
}
\details{ Does prediction for a spectral GP, either at the gridpoints or
  for locations by associating locations with the nearest gridpoint,
  depending on the arguments supplied.  If \code{newdata} and
  \code{mapping} are both NULL, then prediction is done on the grid.  If only
  \code{newdata} is supplied, the mapping is done using
  \code{new.mapping} and then the prediction is done.  If \code{mapping}
  is supplied (this should be done for computational efficiency if
  prediction at the same locations will be done repeatedly) then the
  mapping is used directly to calculate the predictions.
}
\value{
  A vector of process values (matrix for two-dimensional processes in
  which prediction on the grid is requested).
}
\references{For technical details, see Paciorek, C.J. 2006. Bayesian Smoothing of Irregularly-spaced Data Using Fourier Basis Functions. Harvard University Biostatistics Working Paper Series. Working Paper 49.  \url{http://www.bepress.com/harvardbiostat/paper49}}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{new.mapping}},\code{\link{plot.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
simulate(gp1)
simulate(gp2)
gridvals=predict(gp1)
gridvals2=predict(gp2)

loc1=runif(100)
loc2=cbind(runif(100),runif(100,0,1))
map1=new.mapping(gp1,loc1)
map2=new.mapping(gp2,loc2)
vals1=predict(gp1,mapping=map1)
vals2=predict(gp2,mapping=map2)
#equivalently:
vals1=predict(gp1,loc1)
vals2=predict(gp2,loc2)
plot(gp1)
points(loc1,vals1)

}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
