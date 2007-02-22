\name{updateprocess.gp}
\alias{updateprocess.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recalculate process values in a spectral GP object}
\description{
 Calculates the process values in a
  spectral GP object based on the
  current coefficient values.  The process values are calculated by
  multiplying the coefficient values by the basis matrix, which is done
  by the inverse FFT.
}
\usage{
updateprocess.gp(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{
  Modifies the \code{process} values of the object.
}
\value{
  The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{simulate.gp}},\code{\link{zero.coeff.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
propose.coeff(gp1)
gp1$coeff[1,1]=0
updateprocess(gp1)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line


