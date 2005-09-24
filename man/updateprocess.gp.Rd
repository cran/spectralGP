\name{updateprocess.gp}
\alias{updateprocess.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recalculate process values in a spectral GP object}
\description{
  An internal function that recalculates the process values in a
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
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{simulate.gp}},\code{\link{zero.coeff.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{internal}
