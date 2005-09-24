\name{Gibbs.sample.coeff.gp}
\alias{Gibbs.sample.coeff.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Samples new coeffients via Gibbs sampling in a spectral GP object.}
\description{
  Samples new coefficients via Gibbs sampling in a spectral GP object
  following the Gibbs sampling scheme of Wikle (2002), which involves an
  extra variance component (\code{sig2e} and a noisy version of the
  process (\code{z}).
}
\usage{
Gibbs.sample.coeff.gp(object, z, sig2e, meanVal=0, sdVal=1, \dots)
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
  This function can be used in an MCMC context to take Gibbs samples
  of the process coefficients, as part of the algorithm of Wikle
  (2002). The function modifies the GP object, updating the \code{coeff} and
  \code{process} components.}
\value{
    The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
}
\references{For details of the algorithm, see Wikle (2002) - reference info
  available under \code{help(spectralGP)}.}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}},\code{\link{updateprocess.gp}}}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
