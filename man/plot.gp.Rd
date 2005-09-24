\name{plot.gp}
\alias{plot.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a process based on a spectral GP object}
\description{
  Makes a line plot (for one-dimensional processes) or image plot
  (two-dimensional processes) of a process represented in a spectral
  GP object.
}
\usage{
plot.gp(x, type = "l", col = terrain.colors(32), \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A GP object, created by \code{gp}.}
  \item{type}{Type of plot if process is one-dimensional, "l" for line,
    "p" for points, etc.}
  \item{col}{Color scheme for image plot if process is
    two-dimensional. E.g., topo.colors(64) is the default for
    image.plot; I prefer terrain.colors(64) as topo.colors has sharp
    color changes between adjacent bins.}
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
gp2=gp(c(256,256),matern.specdens,c(1,0.5))
simulate(gp2)
plot(gp2)

}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
