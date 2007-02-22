"zero.coeff.gp" <-
function(object,...){
  # zeroes out the coefficients
  object$coeff=matrix(0,nr=object$gridsize[1],nc=object$gridsize[2])
  updateprocess(object)
  return(NULL)
}
