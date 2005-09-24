\name{points.gp}
\alias{points.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add points for a one-dimensional process based on a spectral GP object}
\description{
  Adds points to an existing plot.
}
\usage{
points.gp(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A GP object, created by \code{gp}.}
  \item{\dots}{Extra arguments to plotting functions.}
}
\details{
}
\value{
  No value is returned.
}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{simulate.gp}},\code{\link{getgrid.gp}},\code{\link{predict.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(c(128),matern.specdens,c(1,4))
simulate(gp1)
plot(gp1)
simulate(gp1)
points(gp1,col=2)

}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
