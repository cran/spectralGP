"expand.gpgrid.gp" <-
function(object,...){
  # gives implicit grid of process in the expand.grid() two column format
  if(!is.gp(object)){
    stop(" 'object' argument must be of class 'gp' ")
  }
  gr=getgrid(object)
  if(object$d==2){
    return(expand.grid(gr$x1,gr$x2))
  } else{
    return(gr)
  }
}
