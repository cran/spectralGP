\name{getgrid.gp}
\alias{getgrid.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates the gridpoints in a spectral GP object}
\description{
  Calculates the sequence of gridpoints in each dimension for a spectral
  GP object.  Gridpoints representing the part of the domain in which
  the periodicity of the GP emerges are omitted.
}
\usage{
getgrid.gp(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{ Not meant to be used directly by the user, unless the user
  needs the unique gridpoints in each dimension. For the expanded grid
  that corresponds to the process values, with each row containing the
  two coordinates of a grid location, use \code{expand.gpgrid}.}
}
\value{
  For two dimensions, a list containing the gridpoints in each
    dimension, with the first element containing the unique 
    gridpoints in the first dimension and the second element the unique
    gridpoints in the second dimension, or for one-dimensional
    processes, a vector of gridpoints.
 }
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{getgrid.gp}},\code{\link{predict.gp}},\code{\link{expand.gpgrid.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
grid1=getgrid(gp1)
grid2=getgrid(gp2)
}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
