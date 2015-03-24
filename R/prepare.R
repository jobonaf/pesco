## functions to prepare the data for a specific day
## (on the grid and on the stations' points)

## CTM
prepare.ctm <- function(ctm.daily, day,
                        x.pnt, y.pnt, x.grd, y.grd) {
  time <- sort(unique(as.POSIXct(as.character(ctm.daily$time),tz="Africa/Algiers")))
  if(min(diff.POSIXt(time)) < as.difftime(1,units="days")) {
    stop("Cannot deal with sub-daily data.")
  }
  idx <- match(format(as.POSIXct(day),format="%Y-%m-%d"),
               format(as.POSIXct(ctm.daily$time),format="%Y-%m-%d"))
  if(is.na(idx)) {
    stop(paste("Cannot find CTM data for ",day,sep=""))
  }
  ctm.day <- ctm.daily$data[,,idx]
  ctm.pnt <- Interp(x=ctm.daily$coords$x,
                    y=ctm.daily$coords$y,
                    z=ctm.day,
                    xp=x.pnt, yp=y.pnt,
                    type="points",method="linear")
  ctm.grd <- Interp(x=ctm.daily$coords$x,
                    y=ctm.daily$coords$y,
                    z=ctm.day,
                    xp=x.grd, yp=y.grd,
                    type="grid",method="linear")
  dum <- expand.grid(ctm.grd$x, ctm.grd$y, KEEP.OUT.ATTRS=F)
  out <- list(points=ctm.pnt,
              grid=list(x=dum[[1]],
                        y=dum[[2]],
                        z=as.vector(ctm.grd$z)))
  return(out)
}

## Emissions (with seasonality)
prepare.emis <- function(emis.winter, emis.summer, day,
                         x.pnt, y.pnt, 
                         x.grd=NULL, y.grd=NULL) {
  day <- as.POSIXct(day, tz="Africa/Algiers")
  mm <- as.numeric(Month(day))
  ##          Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
  coef.w <- c(1.00,1.00,0.75,0.50,0.25,0.00,0.00,0.00,0.25,0.50,0.75,1.00)
  coef.s <- c(0.00,0.00,0.25,0.50,0.75,1.00,1.00,1.00,0.75,0.50,0.25,0.00)
  emis <- (emis.winter$data*coef.w[mm] + emis.summer$data*coef.s[mm]) / (coef.w[mm] + coef.s[mm])
  coords <- emis.winter$coords
  emis.pnt <- Interp(x=coords$x, y=coords$y, z=emis,
                     xp=x.pnt, yp=y.pnt,
                     type="points",method="linear")
  if(!is.null(x.grd) & !is.null(y.grd)) {
    emis.grd <- Interp(x=coords$x, y=coords$y, z=emis,
                       xp=x.grd, yp=y.grd,
                       type="grid",method="linear")    
  } else {
    emis.grd <- list(x=coords$x, y=coords$y, z=emis)
  }
  Emis <- list(points=emis.pnt, grid=emis.grd)
  return(Emis)
}

## Elevation
prepare.elev <- function(elev,
                         x.pnt, y.pnt, 
                         z.pnt=rep(NA,length(x.pnt)), # user defined stations elevation
                         x.grd=NULL, y.grd=NULL) {
  coords <- elev$coords
  elev.pnt <- Interp(x=coords$x, y=coords$y, z=elev$data,
                     xp=x.pnt, yp=y.pnt,
                     type="points",method="linear")
  
  ## if provided, substitute station elevation
  idx <- which(!is.na(z.pnt))
  if(length(idx)>0) elev.pnt$z[idx] <- z.pnt[idx]
  
  if(!is.null(x.grd) & !is.null(y.grd)) {
    elev.grd <- Interp(x=coords$x, y=coords$y, z=elev$data,
                       xp=x.grd, yp=y.grd,
                       type="grid",method="linear")    
  } else {
    elev.grd <- list(x=coords$x, y=coords$y, z=elev$data)
  }
  Elev <- list(points=elev.pnt, grid=elev.grd)
  return(Elev)
}

## Observed data
prepare.obs <- function(obs.daily, day) {
  time <- sort(unique(as.POSIXct(as.character(obs.daily$Time),tz="Africa/Algiers")))
  if(min(diff.POSIXt(time)) < as.difftime(1,units="days")) {
    stop("Cannot deal with sub-daily data.")
  }
  out <- subset(obs.daily, 
                subset=(format(Time,format="%Y-%m-%d")==
                          format(as.POSIXct(day),format="%Y-%m-%d")))
  out <- out[which(!is.na(out[,2])),]
  rownames(out) <- 1:nrow(out)
  return(out)
}
                        