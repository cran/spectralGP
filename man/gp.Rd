\name{gp}
\alias{gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a new GP object}
\description{
  Creates a Gaussian process (GP) object based on the spectral basis
  approximation of a GP on a grid.  The advantage of this approach is that GPs
  can be simulated and sampled much more efficiently than standard GP
  representations.  E.g., GPs can be simulated on fine grids of 256X256
  locations, many more locations than can usually be predicted with
  standard computational approaches.  Currently one and two dimensional
  GPs are supported.
}
\usage{
gp(gridsize = c(64, 64), specdens = matern.specdens, specdens.param = c(1, 4))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gridsize}{ Vector (or scalar for one dimension) of number of
    gridpoints in each direction. Number of gridpoints should be a power
  of two, and it is recommended that the number be the same for each dimension.} 
  \item{specdens}{ Function (as a function or text string of the
    function name) that calculates spectral density of
    correlation function desired; function should take a vector (scalar)
    of parameter values.  See matern.specdens() for an example.}
  \item{specdens.param}{ Vector of parameters to be supplied to the
    specdens.function function.}
}
\details{
  This function produces an object of class gp.  More details on the
  spectral representation of GPs can be found in Paciorek & Ryan
  (2005).  Accounting for space in nonparametric logistic regression: A
  Bayesian spectral basis model outperforms other methods.
}
\value{
  An object of class gp.  This includes the dimension of the space, the
  spectral density information, a matrix of coefficients, the Fourier
  frequencies, and prior variances.
  \item{gridsize}{Vector (or scalar for one dimension) of number of
    gridpoints in each direction. }
  \item{d}{Dimension of the space (1 or 2).}
  \item{specdens}{Spectral density function of the correlation function
    of the GP.}
  \item{coeff}{Matrix of coefficient values (a one-column matrix for
    one-dimensional processes).}
  \item{omega}{A matrix of Fourier frequency values corresponding the
    basis functions in expand.grid() format.}
  \item{variances}{A matrix of coefficient variances.}
}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{ \code{\link{simulate.gp}},\code{\link{plot.gp}},\code{\link{propose.coeff.gp}},\code{\link{calc.variances.gp}},\code{\link{new.mapping}},\code{\link{logdensity.gp}},\code{\link{predict.gp}},\code{\link{add.blocks.gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
simulate(gp1)
simulate(gp2)
plot(gp1)
plot(gp2)

n=100
locs=cbind(runif(n,0.2,1.2),runif(n,-0.2,1.4))
locs.predict=cbind(runif(n,-0.4,0.8),runif(n,-0.1,1.7))
scaled.locs=xy2unit(locs,rbind(locs,locs.predict))
scaled.locs.predict=xy2unit(locs.predict,rbind(locs,locs.predict))
train.map=new.mapping(gp2,scaled.locs)
predict.map=new.mapping(gp2,scaled.locs.predict)
vals.train=predict(gp2,mapping=train.map)
vals.predict=predict(gp2,mapping=predict.map)

}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
