#################################################################
# HELPFUNCTION TO EXTRAPOLATE SURVEY BY WEIGHT
# USING RANDOM ROUND UP OR DOWN
#

random_round <- function(x,w,group){
  
  if(sample(c(0,1),1)==1){
    xr <- ceiling(x[,unique(w)])
  }else{
    xr <- floor(x[,unique(w)])
  }
  n <- nrow(x)
  x <- x[rep(1:n,xr)]
  x[,ID_new:=sort(rep(1:xr,n))]
  
  return(cbind(x,group))
}