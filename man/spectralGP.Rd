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

DISCLAIMER:

 This is software for statistical research and not for commercial
     uses. The author does not guarantee the correctness of any function
     or program in this package. Any changes to the software should not
     be made without the author's permission.

ACKNOWLEDGEMENT:
     
Many thanks to Chris Wikle who first suggested I use the Fourier basis
approximation for Gaussian processes.

REFERENCES:

For more details, type 'citation("spectralGP")' for references.

See also:

 Royle, J.A., and C.K. Wikle, (2005).  Efficient Statistical Mapping of
 Avian Count Data.  Ecological and Environmental Statistics 12:225-243.
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
