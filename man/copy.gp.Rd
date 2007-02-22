\name{copy.gp}
\alias{copy.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Copy a spectral GP object.}
\description{
  Creates a new copy of a spectral GP object, with new memory
  allocated for the object, or copies the elements of one spectral GP
  object to another one that is already in existence.
}
\usage{
copy.gp(object, object2 = NULL,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Spectral GP object to be copied.}
  \item{object2}{Already existing spectral GP object to which the
  elements of \code{object} should be copied.  If NULL, the function
  returns a newly-created copy of \code{object}.}
  \item{\dots}{Other arguments.}
}
\details{
  This function copies an object of class gp.  More details on the
  spectral representation of GPs can be found in Paciorek (2006). 
}
\value{
  An object of class gp.  If \code{object2} is specified, returns NULL.
}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{ \code{\link{gp}},\code{\link{is.gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(128,matern.specdens,c(0.5,4))
copy(gp1,gp2)  # gp2 is now a copy of gp1, with first parameter equal to 1
gp3=copy(gp1)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
