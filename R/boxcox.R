## Box-Cox
boxcox <- function(x,lambda) {
  if(lambda==1) {
    return(x)
  } else if(lambda==0) {
    return (log(x))
  } else {
    return((x^lambda-1)/lambda)
  }
}