## daily synthesis of a single hourly time series
dailyStat <- function(x, time, statistic, necess=0.75) {
  if(length(x)!=length(time)) stop("x's and time's lengths differ!")
  if(length(unique(diff.Date(time)))!=1) stop("time is not regularly spaced!")
  acc.stats <- c("mean","max","max8h")
  if(statistic%in%acc.stats) {
    if(statistic=="mean") stat <- stat.period(x, Ymd(time), 
                                              necess=necess*24, 
                                              FUN=mean)
    if(statistic=="max")  stat <- stat.period(x, Ymd(time), 
                                              necess=necess*24, 
                                              FUN=max)
    if(statistic=="max8h")stat <- stat.period(mean.window(x, k=8, 
                                                          necess=8*necess), 
                                              Ymd(time), necess=necess*24, 
                                              FUN=max)    
  } else stop(paste("'",statistic,"' is not supported. Please choose one of '",
                    paste(acc.stats,collapse="', '"),"'",
                    sep=""))
  names(stat) <- strptime(names(stat),format="%Y%m%d")
  return(stat)
}

## daily synthesis of hourly observations (provided in "long table" format)
dailyObs <- function(data, statistic, pollutant, 
                     Time="Time", Code="Code",
                     others=c("Name","Municipality",
                              "Lat","Lon","Elev","Type")) {
  codes <- unique(data[,Code])
  flag=TRUE
  for (code in codes) {
    #print(code)
    dat <- data[data[,Code]==code,]
    ttt <- dat[,Time]
    sta <- dailyStat(x=dat[,pollutant],time=ttt,statistic,necess=0.75)
    out <- data.frame(sta, names(sta),
                      dat[rep(1,length(sta)),
                          c(Code,others)])
    colnames(out) <- c(pollutant, Time, Code, others)
    if(flag) {
      Out<-out
      flag<-FALSE
    } else {
      Out<-rbind(Out,out)
    }
  }
  return(Out)
}

## daily synthesis of CTM (provided as read by read.ncdf.arpaer)
dailyCtm <- function(data, statistic) {
  acc.stats <- c("mean","max","max8h")
  if(statistic%in%acc.stats) {
    out <- apply(X=data$data, MARGIN=c(1,2), FUN=dailyStat,
                 time=data$time, statistic=statistic)
  } else stop(paste("'",statistic,"' is not supported. Please choose one of '",
                    paste(acc.stats,collapse="', '"),"'",
                    sep=""))
  Out <- list(coords=data$coords, time=names(out), data=aperm(out,perm=c(2,3,1)))
  return(Out)
}


