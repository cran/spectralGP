\name{zero.coeff.gp}
\alias{zero.coeff.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sets coefficients to zero in a spectral GP object }
\description{
Sets coefficients to zero in a spectral GP object.  Used to zero out the
coefficients before simulating a new GP realization from the prior distribution.}
\usage{
zero.coeff.gp(object,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.}
  \item{\dots}{Other arguments.}
}
\details{
  Modifies the \code{coeff} and \code{process} components of the object.
}
\value{
  The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{simulate.gp}},\code{\link{updateprocess.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{internal}