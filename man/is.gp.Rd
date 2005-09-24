\name{is.gp}
\alias{is.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test if object is a spectral GP}
\description{
  Tests if the argument is a spectral GP object.}
\usage{
is.gp(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}. }
}
\details{
}
\value{
  Returns 'TRUE' if the argument is a gp, and 'FALSE' otherwise.}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
is.gp(gp1)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
