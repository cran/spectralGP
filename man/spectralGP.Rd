\name{spectralGP}
\alias{spectralGP}
\title{
spectralGP - tools for specifying Gaussian processes using the
computationally efficient Fourier basis
}
\description{
  SpectralGP is a collection of functions for creating Gaussian processes in 
 one and two dimensions using the Fourier basis approximation. It provides
 fast  simulation and plotting of process realizations by use of the
 FFT, allowing simulation and plotting on very dense grids.  For inference,
 it provides tools for use in setting up an MCMC: calculation of 
 coefficient variances, calculation of process density, and
 coefficient proposals. It uses R environments to store GP objects as references/pointers.

Some major methods include:
\itemize{
\item \code{gp}   Create a Gaussian process object
\item \code{simulate.gp}  Simulate a Gaussian process realization
\item \code{plot.gp}   Plot a Gaussian process
\item \code{predict.gp} Extract process values at specified domain points
}

Note that if implementing the Gibbs sampling algorithm of Wikle (2002),
when you use the FFT to calculate t(Psi)z (using Wikle's notation),
you should use the following approach in R:
\code{fft(matrix(z,fun$gridsize[1],fun$gridsize[2]),inv=FALSE)/sqrt(prod(fun$gridsize))},
where z is a vector of (noisy) process values corresponding to the
process element of the GP (\code{process}).

At some point there may be more information available at
\url{http://www.biostat.harvard.edu/~paciorek/code/spectralGP}


DISCLAIMER:

This is software for statistical research; I do not guarantee the
correctness of any function or program in this package.
authors permission.

Many thanks to Chris Wikle who first suggested I use the Fourier basis
approximation for Gaussian processes.

For more details, see
Paciorek & Ryan (2005).  Accounting for space in nonparametric logistic
regression: A Bayesian spectral basis model outperforms other methods. In
submission.
\url{http://www.biostat.harvard.edu/~paciorek/papers.html}

See also,
 Royle, J.A., and C.K. Wikle, (2004).  Efficient Statistical Mapping of
 Avian Count Data.  Ecological and Environmental Statistics , to
 appear.
 \url{http://www.stat.missouri.edu/~wikle/pub_new.html}
 
   Wikle, C.K., (2002).  Spatial modeling of count data: A case study in modelling breeding bird survey data on large spatial domains.
   In Spatial Cluster Modelling, A. Lawson and D. Denison, eds. Chapman
   and Hall, 199-209.
   \url{http://www.stat.missouri.edu/~wikle/pub_new.html}

}
\examples{
gp1=gp(128,matern.specdens,c(1,4))
gp2=gp(c(64,64),matern.specdens,c(1,4))
simulate(gp1)
simulate(gp2)
plot(gp1)
plot(gp2)
gridvals=predict(gp1)
newlocs=runif(100)
offgridvals=predict(gp1,newlocs)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
