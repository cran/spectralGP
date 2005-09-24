"change.param.gp" <-
function(object,new.param,...){
  if(!is.gp(object)){
    stop(" 'object' argument must be of class 'gp' ")
  }
  if(length(new.param)!=length(object$specdens.param)){
    stop('New parameter vector is not of the proper length: ',length(object$specdens.param),'\n')
  } 
  object$specdens.param=new.param
  calc.variances(object)
  return(NULL)
}
