\name{Hastings.coeff.gp}
\alias{Hastings.coeff.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates Hastings value of coefficients}
\description{
  Calculates Hastings value of coefficients, the logdensity of the current
  coefficients given proposal mean and variance based on a Gibbs sample
  of the form in \code{Gibbs.sample.coeff.gp}.
}
\usage{
\method{Hastings.coeff}{gp}(object, z, sig2e, meanVal=0, sdVal=1, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.} 
  \item{z}{Vector of values for \code{z}, the noisy version of the process.}
  \item{sig2e}{Noise variance component that distorts \code{z} as a
    version of the process.}
  \item{meanVal}{Optional mean value for \code{z}.}
  \item{sdVal}{Optional standard deviation value for \code{z}.}
  \item{\dots}{Other arguments.}
}
\details{
  This function can be used in an MCMC context to calculate the Hastings correction
  that may be necessary in taking a quasi-Gibbs sample of the process
  coefficients, as part of one of the algorithms of Paciorek (2006).
  The function calculates and returns the logdensity.}
\value{
    The function returns the logdensity.}
\references{Type 'citation("spectralGP")' for references.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{updateprocess.gp}}}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
