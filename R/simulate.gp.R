"simulate.gp" <-
function(object,...){
  # simulate sample processes from prior
  if(!is.gp(object)){
    stop(" 'object' argument must be of class 'gp' ")
  }
  zero.coeff(object)  
  propose.coeff(object,block=0,proposal.sd=1)
  return(NULL)
}
