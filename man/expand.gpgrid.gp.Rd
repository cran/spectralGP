\name{expand.gpgrid.gp}
\alias{expand.gpgrid.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate grid locations for a spectral GP object.}
\description{
  This is a version of \code{expand.grid} that calculates the grid
  locations  for a spectral GP object. Gridpoints representing the
  part of the domain in which the periodicity of the GP emerges are omitted.
}
\usage{
\method{expand.gpgrid}{gp}(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{
  Note that this function is not named \code{expand.grid.gp} because
  \code{expand.grid} is a function, and not an S3 method.
}
\value{
  A matrix of grid locations with the first column the
    x-dimension and the second the y-dimension, or for one dimensional
    processes, a vector of grid locations.
 }
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}}, \code{\link{getgrid.gp}}, \code{\link{predict.gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
grid1=expand.gpgrid(gp1)
grid2=expand.gpgrid(gp2)
plot(grid2)
}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
