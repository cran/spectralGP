\name{lonlat2xy}
\alias{lonlat2xy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Projects lon/lat coordinates to x/y Euclidean coordinate system}
\description{
  The projection calculates, for all points, the great circle distance in the x direction to the mean longitude and in the y direction to the mean latitude, and uses these distances as the x-y coordinates of the location. This function is copied from the fields library.
}
\usage{
lonlat2xy(lnlt,miles=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lnlt}{A two-column matrix-like object of lon/lat coordinates to
    be projected, with longitude in the first column.}
  \item{miles} {Indicator of whether distances should be calculated in
    miles or kilometers (FALSE, the default).}
}
\details{
    Note that this is an ad hoc projection best used only for small
  portions of the globe.
}
\value{
  A  two-column matrix of projected x/y coordinates, with the x-coordinate in the
  first column.
}
  \author{copied from the fields library by Christopher Paciorek \email{paciorek@alumni.cmu.edu} }
\seealso{\code{\link{xy2unit}},\code{\link{new.mapping}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(spectralGP)
gp1=gp(c(128,128),matern.specdens,c(1,4))
n=100
locs=cbind(runif(n,20,80),runif(n,40,50))
locs.predict=cbind(runif(n,30,90),runif(n,38,48))
locs=lonlat2xy(locs)
locs.predict=lonlat2xy(locs.predict)
scaled.locs=xy2unit(locs,rbind(locs,locs.predict))
scaled.locs.predict=xy2unit(locs.predict,rbind(locs,locs.predict))
train.map=new.mapping(gp1,scaled.locs)
predict.map=new.mapping(gp1,scaled.locs.predict)
plot(locs,xlim=c(min(locs[,1],locs.predict[,1]),max(locs[,1],locs.predict[,1])),ylim=c(min(locs[,2],locs.predict[,2]),max(locs[,2],locs.predict[,2])))
points(locs.predict,col=2)
plot(scaled.locs,xlim=c(0,1),ylim=c(0,1))
points(scaled.locs.predict,col=2)
}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{smooth}% __ONLY ONE__keyword per line
\keyword{spatial}% __ONLY ONE__ keyword per line
