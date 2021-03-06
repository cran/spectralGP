\name{simulate.gp}
\alias{simulate.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates a process realization from a spectral GP object }
\description{
  Simulates a process realization by drawing a random draw of
  coefficients from their prior distribution and updating the process values.  
}
\usage{
\method{simulate}{gp}(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{
  Modifies the \code{coeff} and \code{process} elements of the object.
}
\value{
   The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.
}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}}, \code{\link{zero.coeff.gp}}, \code{\link{propose.coeff.gp}}, \code{\link{updateprocess.gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
simulate(gp1)
simulate(gp2)
plot(gp1)
plot(gp2)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
