"points.gp" <-
function(x,...){
  # add points for one-dimensional gp
  if(!is.gp(x)){
    stop(" 'x' argument must be of class 'gp' ")
  }
  if(x$d!=1){
    stop(" points function only works for one-dimensional processes")
  }
  points(getgrid(x),predict(x),...)
}
