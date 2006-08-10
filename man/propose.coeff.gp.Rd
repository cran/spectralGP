\name{propose.coeff.gp}
\alias{propose.coeff.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Proposes new coeffients in a spectral GP object.}
\description{
  Proposes new coefficients in a spectral GP object as normal deviates
  centered around the current values, with the proposal standard deviation the
  product of the supplied standard deviation(s) and the square root of
  the prior variances.  The proposal can be done for all coefficients at
  once (\code{block=0}) or for individual blocks. 
}
\usage{
propose.coeff.gp(object, block = 0, proposal.sd = 1,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}.} 
  \item{block}{The block of coefficients to be proposed, or 0 if all
    coefficients are to be proposed.}
  \item{proposal.sd}{Proposal standard deviation. This is multiplied by
    the square root of the prior variance for each coefficient to
    produce the final proposal standard deviation.}
  \item{\dots}{Other arguments.}
}
\details{
   This function can be used to simulate a GP by using
   \code{proposal.sd=1} to sample coefficients, \code{coeff}, from the
   prior or in a MCMC context to propose new coefficient
   values via the Metropolis algorithm.  The function automatically
   updates the process values in the \code{process} component of the gp
   list based on the new coefficient values.
}
\value{
   The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
}
\references{For technical details, see Paciorek, C.J. 2006. Bayesian Smoothing of Irregularly-spaced Data Using Fourier Basis Functions. Harvard University Biostatistics Working Paper Series. Working Paper 49.  \url{http://www.bepress.com/harvardbiostat/paper49}}
  \author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{calc.variances.gp}},\code{\link{simulate.gp}},\code{\link{updateprocess.gp}},\code{\link{zero.coeff.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
rho=1
gp1=gp(128,matern.specdens,c(rho,4))
gp2=gp(c(64,64),matern.specdens,c(rho,4))
propose.coeff(gp1)
propose.coeff(gp2)
plot(gp1)
plot(gp2)
prior1=logdensity(gp1)
prior2=logdensity(gp2)
add.blocks(gp1)
add.blocks(gp2)
propose.coeff(gp1,block=2,proposal.sd=0.1)
propose.coeff(gp2,block=3,proposal.sd=0.1)
priorstar1=logdensity(gp1)
priorstar2=logdensity(gp2)
plot(gp1)
plot(gp2)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
