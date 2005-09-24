\name{add.blocks.gp}
\alias{add.blocks.gp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adds coefficient block structure to a spectral GP object}
\description{
  Adds block structure to a GP object,
  allowing simulating and sampling (in an MCMC
  setup) from blocks of coefficients, as opposed to all the coefficients
  at once.  The size of the blocks will usually increase with increasing
 frequency as the most important coefficients are those for the low
 frequency basis functions.}
}
\usage{
add.blocks.gp(object, breaks = NULL,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A GP object, created by \code{gp}. }
  \item{breaks}{An optional vector of increasing frequency values that allow for
    binning the coefficients based on the frequencies of the basis
    functions with which they are associated. The maximum value of the
    vector should be one half the number of gridpoints in the dimension
    with the largest number of gridpoints (e.g., 64, if
    gridsize=c(32,128)).  }
  \item{\dots}{Other arguments.}
}
\details{
  The function sets up a block structure with blocks with increasing
  numbers of coefficients as the frequencies increase.  The frequency
  in question is the highest frequency of the (\eqn{omega_1,omega_2}) pair in
  the two-dimensional situation.  E.g., the default block structure is
  c(1,2,4,8,16,...), which means that the first block is the
  coefficients whose maximum frequency is 1, the second with maximum
  frequency is 2, the third with maximum frequency of 3 and 4, the
  fourth with maximum frequency between 5 and 8, etc.
}
\value{
   The function modifies the GP object, which is essentially a pointer
   (an R environment in this case), so NULL is returned.}
}
\references{For technical details, see Paciorek & Ryan (2005).
  Accounting for space in nonparametric logistic regression: A Bayesian
  spectral basis model outperforms other methods.}
\author{Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{gp}},\code{\link{propose.coeff.gp}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
add.blocks(gp1,c(1,3,7,15,31,64))
add.blocks(gp2)
propose.coeff(gp1,block=5)
plot(gp1)
propose.coeff(gp2,block=2,proposal.sd=0.1)
plot(gp2)
}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
