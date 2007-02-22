\name{logdensity.gp}
\alias{logdensity.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates log prior density of a spectral GP object}
\description{
 Calculates the log prior density of a spectral GP object as the log
 prior density of the basis coefficients, based on the prior variances
 and a prior of independent Gaussians.
}
\usage{
logdensity.gp(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}. }
  \item{\dots}{Other arguments.}
}
  \details{
    The log density is calculated based on the real and imaginary
    components of the basis function coefficients, but only those
    coefficients that are not determined as the complex conjugates of
    other coefficients. The density function is that the coefficients
    are IID normal with mean zero and prior variance based on the
    spectral density and correlation parameters.
}
\value{
  The logarithm of the prior density.
}

\references{Type 'citation("spectralGP")' for references.}
\author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{calc.variances.gp}}}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
propose.coeff(gp1)
propose.coeff(gp2)
print(logdensity(gp1))
print(logdensity(gp2))
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
