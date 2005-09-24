\name{names.gp}
\alias{names.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The names of the elements of a GP object}
\description{
  Gives the names of the elements of the GP object.
}
\usage{
names.gp(x,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Spectral GP object.}
  \item{\dots}{Other arguments.}
}
\details{
}
\value{ A vector of strings of the names of the elements of the GP object.
}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{ \code{\link{gp}}}
\examples{
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
print(names(gp1))
add.blocks(gp1)
print(names(gp1))

}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
