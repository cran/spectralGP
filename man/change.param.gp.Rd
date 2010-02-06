\name{change.param.gp}
\alias{change.param.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Changes correlation function parameter values of a spectral GP object}
\description{
  Changes the correlation parameter values or the (optional) variance
  parameter and  recalculates the prior
  variances of the coefficients using \code{calc.variances.gp}.
}
\usage{
\method{change.param}{gp}(object,new.specdens.param=NULL,
  new.variance.param=NULL,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{new.specdens.param}{A vector of new parameter values, matching the length
    of the original parameter vector.}
  \item{new.variance.param}{The new variance parameter value.}
  \item{\dots}{Other arguments.}
}
\details{
  This function allows the user to change the parameter values of the
  spectral GP object and recalculate the prior variances for the
  coefficients.  This is particularly useful for implementing MCMC with
  the spectral GP.
}
\value{
  The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}}, \code{\link{calc.variances.gp}}, \code{\link{logdensity.gp}}}
\examples{
library(spectralGP)
rho=1
gp1=gp(128,matern.specdens,c(rho,4))
gp2=gp(c(64,64),matern.specdens,c(rho,4),0.5)
propose.coeff(gp1)
propose.coeff(gp2)
print(logdensity(gp1))
print(logdensity(gp2))
rho=2
sigma=2.5
change.param(gp1,c(rho,4)) # change parameter values of correlation function
change.param(gp2,c(rho,4),sigma)
print(logdensity(gp1))
print(logdensity(gp2))
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
