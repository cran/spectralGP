"Gibbs.sample.coeff.gp" <-
function(object,z,sig2e,meanVal=0,sdVal=1,...){
  # takes a Gibbs sample, following the approach of Wikle (2002)
  if(!is.gp(object)){
    stop(" 'object' argument must be of class 'gp' ")
  }
  
  m1=object$gridsize[1]
  m2=object$gridsize[2]

  sig2e.precMatrix=matrix(2*sdVal*sdVal/sig2e,nr=m1,nc=m2) 
  sig2e.precMatrix[1,1]=(1/2)*sig2e.precMatrix[1,1]  
  sig2e.precMatrix[(m1/2+1),1]=(1/2)*sig2e.precMatrix[(m1/2+1),1]
  if(object$d==2){
    sig2e.precMatrix[(m1/2+1),(m2/2+1)]=(1/2)*sig2e.precMatrix[(m1/2+1),(m2/2+1)]
    sig2e.precMatrix[1,(m2/2+1)]=(1/2)*sig2e.precMatrix[1,(m2/2+1)]
  }
  coeff.var=1/(1/object$variances+sig2e.precMatrix)
  coeff.mean=coeff.var*sig2e.precMatrix*fft(matrix((z-meanVal)/sdVal,nr=m1,nc=m2,byrow=FALSE), inv = FALSE)/(sqrt(m1*m2))
  object$coeff=matrix(rnorm(m1*m2,Re(coeff.mean),sqrt(c(coeff.var))),nr=m1,nc=m2)+(0+1i)*matrix(rnorm(m1*m2,Im(coeff.mean),sqrt(c(coeff.var))),nr=m1,nc=m2)
  object$coeff[1,1]=Re(object$coeff[1,1])
  object$coeff[(m1/2+1),1]=Re(object$coeff[(m1/2+1),1])
  object$coeff[m1:(m1/2+2),1]=Conj(object$coeff[2:(m1/2),1])
  if(object$d==2){
    object$coeff[1,(m2/2+1)]=Re(object$coeff[1,(m2/2+1)])
    object$coeff[(m1/2+1),(m2/2+1)]=Re(object$coeff[(m1/2+1),(m2/2+1)])
    object$coeff[1,m2:(m2/2+2)]=Conj(object$coeff[1,2:(m2/2)])
    object$coeff[(m1/2+1),m2:(m2/2+2)]=Conj(object$coeff[(m1/2+1),2:(m2/2)])
    object$coeff[m1:(m1/2+2),m2/2+1]=Conj(object$coeff[2:(m1/2),m2/2+1])
    object$coeff[m1:(m1/2+2),m2:(m2/2+2)]=Conj(object$coeff[2:(m1/2),2:(m2/2)])
    object$coeff[(m1/2):2,m2:(m2/2+2)]=Conj(object$coeff[(m1/2+2):m1,2:(m2/2)])
  }
  updateprocess(object)
  return(NULL)
}

