\name{matern.specdens}
\alias{matern.specdens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Matern correlation spectral density function }
\description{
  Calculates the Matern spectral density for supplied frequencies and
  Matern correlation parameters. Spectral density is evaluated for each
  supplied frequency or pair of frequencies.  The output is generally
  used as the prior variances for spectral GP basis coefficients.}
}
\usage{
matern.specdens(omega, param, d = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{omega}{Vector or two-column matrix-like object of frequencies, with the
    first column the frequencies in the first dimension and the second
    column in the second dimension.}
  \item{param}{Vector of two Matern parameter values, first the spatial
    range and second the differentiability parameter.}
  \item{d}{Dimension of the domain.}
}
\details{
  The spectral
  density,\deqn{\frac{\Gamma(\nu+d/2)(4\nu)^\nu}{\pi^(d/2)\Gamma(\nu)(\pi \rho)^{2\nu}}\left(\frac{4\nu}{(\pi \rho)^2}+\omega^T \omega\right)^{-(\nu +d/2)}},
  corresponds to the following functional form of
  the Matern correlation function,
 \deqn{\frac{1}{\Gamma(\nu)2^{\nu-1}}\left(\frac{2\sqrt{\nu}\tau}{\rho}\right)^{\nu}\mathcal{K}_{\nu}\left(\frac{2\sqrt{\nu}\tau}{\rho}\right),} where rho is the range and nu the differentiability.  Rho is interpreted on the scale \eqn{(0,1)^d}. Nu of 0.5 is the exponential correlation, and as nu goes to infinity the correlation approaches the squared exponential (Gaussian).  Nu of 0.5 gives Gaussian processes with continuous but not differentiable sample paths, while nu of infinity gives infinitely-differentiable (and analytic) sample paths.  In the spectral GP approximation, the frequencies are a sequence of integers from 0 to half the gridsize in each dimension. 
}
\value{
  A vector of spectral density values corresponding to the supplied frequencies.}
\references{Type 'citation("spectralGP")' for references.}
\author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{calc.variances.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
dens1=matern.specdens(gp1$omega,c(1,4),d=1)
dens2=matern.specdens(gp2$omega,c(1,4),d=2)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
