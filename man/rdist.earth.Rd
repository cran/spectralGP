\name{rdist.earth}
\alias{rdist.earth}
\title{
  Great circle distance matrix 
}
\description{
Given two sets of longitude/latitude locations computes the Great circle
(geogrpahic)  distance matrix
among all pairings. This function and help file are copied from the
fields library.
}
\usage{
rdist.earth(loc1, loc2, miles = TRUE, R = NULL)
}
\arguments{
\item{loc1}{
Matrix of first set of lon/lat coordinates first column is the
longitudes
and second is the latitudes. 
}
\item{loc2}{
Matrix of second  set of lon/lat coordinates first column is the
longitudes
and second is the latitudes. If missing x1 is used. 
}
\item{miles}{
If true distances are in statute miles if false distances in kilometers. 
}
\item{R}{
Radius to use for sphere to find spherical distances. If NULL the radius
is either in miles or kilometers depending on the values of the miles
argument. If R=1 then distances are of course in radians. 
}
}
\value{
The great circle distance matrix if nrow(x1)=m and nrow(
x2)=n then the returned matrix will be mXn. 
}
\details{
Surprisingly this  all done efficiently in S.
}
\seealso{
  rdist, exp.earth.cov 
}
\examples{
 lon.lat=cbind(runif(20,0,360),runif(20,-90,90))
 out<- rdist.earth (lon.lat)
#out is a 20X20 distance matrix	
}
\keyword{spatial}
