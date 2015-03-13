# contiene funzioni utili per statistiche di dati QA
# con valenza legale o meno

stat.period <- function(x,period,necess,FUN=mean) {
  xmean  <- suppressWarnings(tapply(X=x, INDEX=period, FUN=FUN, na.rm=T))
  xvalid <- tapply(X=!is.na(x),
                   INDEX=period, FUN=sum, na.rm=T)
  xmean[xvalid < necess] <- NA
  return(xmean)
}

which.period <- function(x,period,necess,FUN=which.max) {
  xwhich  <- suppressWarnings(tapply(X=x, INDEX=period, FUN=FUN))
  xvalid <- tapply(X=!is.na(x),
                   INDEX=period, FUN=sum, na.rm=T)
  xwhich[xvalid < necess] <- NA
  return(xwhich)
}

stat.period2 <- function(x,period,nmax.missing,FUN=mean) {
  xmean  <- suppressWarnings(tapply(X=x, INDEX=period, FUN=FUN, na.rm=T))
  xinvalid <- tapply(X=is.na(x),
                   INDEX=period, FUN=sum, na.rm=T)
  xmean[xinvalid > nmax.missing] <- NA
  return(xmean)
}

shift <- function(x,k) {
  lx <- length(x)
  if(k>0) {
    out <- c(rep(NA,k), x)[1:lx]
  }
  if(k<0) {
    out <- c(x, rep(NA,-k))[(1-k):(lx-k)]
  }
  if(k==0) {
    out <- x
  }
  return(out)
}

stat.window <- function(x,window,necess,FUN=mean) {
  X <- NULL
  for(i in window[1]:window[2]) {
    xx <- shift(x,-i)
    X <- rbind(X,xx)
  }
  xmean <- suppressWarnings(apply(X, MARGIN=2, FUN=FUN, na.rm=T))
  xvalid <- apply(X=!(is.na(X)), MARGIN=2,
                  FUN=sum, na.rm=T)
  xmean[xvalid < necess] <- NA
  return(xmean)
}

mean.window <- function(x,k,necess) {
  xmean <- runmean(x,k=k,alg="C",endrule="NA",align="right")
  xvalid <- runmean(!is.na(x),k=k,alg="C",endrule="NA",align="right")*k
  xmean[xvalid < necess] <- NA
  return(xmean)
}

exc.period <- function(x,period,necess,threshold){
  xx <- x > threshold
  out <- stat.period(xx,period=period,
                     necess=necess,FUN=sum)
  return(out)
}

detect.event <- function(x,threshold) {
  over <- (x>threshold)
  over[is.na(over)] <- F
  event.start <- over & !c(F,over)[1:length(over)]
  index.event.start <- which(event.start)
  id.event <- cumsum(event.start)*over
  n.events <- max(id.event)
  if(n.events>0) {
    event.duration <- NULL
    event.max <- NULL
    for(i in 1:n.events) {
      event.duration <- c(event.duration, sum(i==id.event))
      event.max <- c(event.max, max(x[i==id.event]))
    }
    out <- data.frame(index=index.event.start,
                      duration=event.duration,
                      max=event.max)
  } else {
    out <- NULL
  }
  return(out)
}

aot <- function(x, hr, threshold=80, estimate=T, hr.min=8, hr.max=19) {
  if(length(x)!=length(hr)) stop("x and hr must have the same length!")
  in.hr <- hr>=hr.min & hr<=hr.max
  valid <- !is.na(x)
  in.hr.valid <- in.hr&valid
  delta.positive <- pmax(x[in.hr]-threshold,0)
  Aot <- sum(delta.positive, na.rm=T)
  if(estimate) Aot <- Aot*sum(in.hr)/sum(in.hr.valid)
  PercValid <- sum(in.hr.valid)/sum(in.hr)*100
  out <- list(Aot=Aot, PercValid=PercValid)
  return(out)
}

