## interpolation (to grid or to sparse points)
Interp <- function(x,y,z,xp,yp,
                   method="linear",
                   type="points") {
  x <- as.vector(x)
  y <- as.vector(y)
  xp <- as.vector(xp)
  yp <- as.vector(yp)
  z <- as.vector(z)
  idx <- which(!is.na(z))
  if(length(idx)<length(z)) {
    x <- x[idx]
    y <- y[idx]
    z <- z[idx]
  }
  acc.methods <- c("linear","spline","nearest")
  acc.types <- c("points","grid")
  if(type=="points") {
    FUN=interpp
    xo <- xp
    yo <- yp
  } else if(type=="grid") {
    FUN=interp
    xo <- sort(unique(xp))
    yo <- sort(unique(yp))
    max.dist <- Dist(xp[1],yp[1],xp[2],yp[2])*sqrt(2)*1.001
  } else {
    stop(paste("type '",type,"' is not supported. Please choose one of '",
               paste(acc.types,collapse="', '"),"'",sep=""))
  }
  if(method=="linear") {
    out <- FUN(x=x,y=y,z=z,xo=xo,yo=yo,linear=TRUE, extrap=FALSE)
  } else if(method=="spline") {
    out <- FUN(x=x,y=y,z=z,xo=xo,yo=yo,linear=FALSE, extrap=FALSE)
  } else if(method=="nearest") {
    k <- which.nearest(xp,yp,x,y)
    out <- list(x=xp, y=yp, z=z[k])
    if(type=="grid") {
      dd <- Dist(xp,yp,x[k],y[k])
      out$z[dd>max.dist] <- NA
    }
  } else {
    stop(paste("method '",method,"' is not supported. Please choose one of '",
               paste(acc.methods,collapse="', '"),"'",sep=""))
  }
  return(out)
}


