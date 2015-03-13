## functions to read:
##  - observed data
##  - CTM output
##  - stations' metadata
##  - proxies

## read NetCDF as provided by Arpa Emilia-Romagna
read.ncdf.arpaer <- function(con=NULL, pollutant="pm10", lev=1, tz="UTC") {
  if(class(con)!="ncdf") {   ## existing file or connection
    nc   <- open.ncdf(con)
  } else {                    ## ncdf object
    nc <- con
  }
  lon  <- get.var.ncdf(nc,varid="lon")
  lat  <- get.var.ncdf(nc,varid="lat")
  Time <- as.POSIXct(get.var.ncdf(nc,varid="Times"), 
                     format="%Y-%m-%d_%H:%M:%S", tz=tz)
  value  <- get.var.ncdf(nc,varid=pollutant)
  if(length(dim(value))==4) {
    value <- value[,,lev,]
  } else if (length(dim(value))!=3) {
    stop(paste(pollutant,"must have dimensions X-Y-Z-T or X-Y-T"))
  }
  if (class(con)!="ncdf") {
    close.ncdf(nc)
  }
  coords <- ll2utm.grid(lat,lon)
  Coords <- list(x=matrix(coords$x,nrow=nrow(lon),ncol=ncol(lon)),
                 y=matrix(coords$y,nrow=nrow(lon),ncol=ncol(lon)))
  out <- list(coords=Coords, time=Time, data=value)
}


read.grads <- function(filectl) {
  
  # manages file name
  dum  <- strsplit(filectl,"/",fixed=TRUE)[[1]]
  file <- dum[length(dum)]
  path <- NULL
  if (length(dum)> 1) {
    for (i in 1:(length(dum)-1)) {
      path <- paste(path,dum[i],sep="/")
    } 
  }       
  nlines <-  length(scan(filectl,what="character", flush = TRUE))
  ctl <- scan(filectl,what="character",sep="\n")
  
  # reads ctl file
  ok <- 0
  for (iline in 1:length(ctl)) {
    line <- ctl[iline]
    if (xgrep("dset",small(line))) {
      dum <- subwrd(line,2)
      if ( xgrep("^",dum) ) { 
        filebin <- strsplit(dum,"^",fixed=TRUE)[[1]][2]
        if(length(path)>0) filebin <- paste(path,filebin,sep="/")
      } else {
        filebin <- dum
      }
      ok <- ok + 1 
    }
    if (xgrep("undef",small(line))) {
      rmis <- as.numeric(subwrd(line,2))
      ok <- ok + 1 
    }
    if (xgrep("xdef",small(line))) {
      nx   <- as.integer(subwrd(line,2))
      ok   <- ok + as.integer(subwrd(small(line),3)=="linear")
      xmin <- as.numeric(subwrd(line,4))
      dx   <- as.numeric(subwrd(line,5))
    }
    if (xgrep("ydef",small(line))) {
      ny   <- as.integer(subwrd(line,2))
      ok   <- ok + as.integer(subwrd(small(line),3)=="linear")
      ymin <- as.numeric(subwrd(line,4))
      dy   <- as.numeric(subwrd(line,5))
    }
    if (xgrep("zdef",small(line))) {
      nz   <- as.integer(subwrd(line,2))
      ztyp <- subwrd(line,3)
      if(ztyp=="linear") {
        zmin <- as.numeric(subwrd(line,4))
        dz   <- as.numeric(subwrd(line,5))
        ok <- ok + 1 
      } 
      if(small(ztyp)=="levels") {
        zdim <- as.numeric(subwrd(line,4:(3+nz)))
        ok <- ok + 1 
      } 
    }
    if (xgrep("tdef",small(line))) {
      nt   <- as.integer(subwrd(line,2))
      ok   <- ok + as.integer(subwrd(small(line),3)=="linear")
      tmin <- small(subwrd(line,4))
      dt   <- small(subwrd(line,5))
    }
    if (xgrep("vars",small(line))) {
      ok <- ok + 1 
      if(ok==7) {
        nv  <- as.integer(subwrd(line,2))
        var <- vector(mode="character",length=nv)
        for (ivar in 1:nv) {
          dum <- ctl[(iline+ivar)]
          var[ivar] <- subwrd(dum,1)
        }
      } else if(ok>8) {
        stop(paste("Problems with",filectl))
      }   
    }
  }
  
  # manages dimensions names
  xdim <- as.character(seq(xmin,length=nx,by=dx))
  ydim <- as.character(seq(ymin,length=ny,by=dy))
  if(ztyp=="linear") zdim <- as.character(seq(zmin,length=nz,by=dz))
  vdim <- as.character(var)
  
  # manages dates and time 
  date.fmt=c(dates="ymd", times="hms")
  tl <- nchar(tmin)
  if (tl==4) tmin <- paste("00:00z01jan",tmin,sep="")
  if (tl==7) tmin <- paste("00:00z01",tmin,sep="")
  if (tl==9) tmin <- paste("00:00z",tmin,sep="")
  if (tl==12)tmin <- paste(substr(tmin,1,2),":00",substr(tmin,3,12),sep="")
  if (date.lang()=="it") tmin <- en2it.date(tmin)
  if (date.lang()=="en") tmin <- it2en.date(tmin)
  library(chron)
  timemin <- paste(substr(tmin,1,5),":00",sep="")
  datemin <- substr(tmin,7,15)
  datemin <- as.character(as.Date(datemin,format="%d%b%Y"),format="%m/%d/%y")
  start <- chron(datemin, timemin, format = c(dates = "m/d/y", times = "h:m:s"))
  
  sl <- nchar(dt)
  steptyp <- substr(dt,(sl-1),(sl))
  nstep   <- as.integer(substr(dt,1,(sl-2)))
  if (steptyp=="yr") {
    dum <- seq.dates(start,by="years",length=(nt*nstep))
    dum <- dum[seq(1,length=nstep,by=nt)]
    dum <- chron(dum,out.format=date.fmt)
    tdim <- as.character(dum)
  } 
  if (steptyp=="mo") {
    dum <- seq.dates(start,by="months",length=(nt*nstep))
    dum <- dum[seq(1,length=nt,by=nstep)]
    dum <- chron(dum,out.format=date.fmt)
    tdim <- as.character(dum)
  } 
  if (steptyp=="dy") {tdim <- as.character(chron(start+seq(0,length=nt,by=nstep       ), out.format=date.fmt))}
  if (steptyp=="hr") {tdim <- as.character(chron(start+seq(0,length=nt,by=(nstep/24)  ), out.format=date.fmt))}
  if (steptyp=="mn") {tdim <- as.character(chron(start+seq(0,length=nt,by=(nstep/1440)), out.format=date.fmt))}
  
  # reads GrADS binary file
  rm(dum)
  bits=4
  dum <- readBin(filebin,"numeric",n=nv*nx*ny*nt*nz,endian="little",size=bits)
  is.na(dum) <- dum==rmis
  dat <- array (dum, dim=c(nx,ny,nz,nv,nt), 
                dimnames=list(x=xdim,y=ydim,z=zdim,var=vdim,t=tdim))
  return(dat)
}

