\name{names.gp}
\alias{names.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The names of the elements of a GP object}
\description{
  Gives the names of the elements of the GP object.
}
\usage{
\method{names}{gp}(x,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Spectral GP object.}
  \item{\dots}{Other arguments.}
}
\value{ A vector of strings of the names of the elements of the GP object.
}
\references{Type 'citation("spectralGP")' for references.}
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
