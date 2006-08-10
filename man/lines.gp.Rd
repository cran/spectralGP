\name{lines.gp}
\alias{lines.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add a line plot for a one-dimensional process based on a spectral GP object}
\description{
  Adds a line plot to an existing plot.
}
\usage{
lines.gp(x, \dots)
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
\references{For technical details, see Paciorek, C.J. 2006. Bayesian Smoothing of Irregularly-spaced Data Using Fourier Basis Functions. Harvard University Biostatistics Working Paper Series. Working Paper 49.  \url{http://www.bepress.com/harvardbiostat/paper49}}
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
lines(gp1,col=2)

}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
