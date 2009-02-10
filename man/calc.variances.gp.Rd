\name{calc.variances.gp}
\alias{calc.variances.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates prior variances of coefficients in a spectral GP object}
\description{
  Calculates the prior variances of the spectral coefficients in a GP
  object.  The variances are based on the spectral density function
  chosen in \code{gp} and the 
  correlation function parameters supplied.
}
\usage{
\method{calc.variances}{gp}(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{
  This function is  an internal function not meant to be called by the user. 
  The prior variances for each coefficient are calculated based on the
  frequency of the corresponding basis function, the spectral density
  function, the parameters of the spectral density/correlation
  function, and the (optional) coefficient variance parameter.
  The function creates \code{variances}, a matrix of
  variances corresponding to \code{coeff},  the matrix of coefficients.
}
\value{
   The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{simulate.gp}},\code{\link{logdensity.gp}},\code{\link{change.param.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line

