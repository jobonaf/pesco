## functions to prepare the data for a specific day
## (on the grid and on the stations' points)

## CTM
prepare.ctm <- function(ctm.daily, day,
                        x.pnt, y.pnt, x.grd, y.grd,
                        conc.min=10^-6) {
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
  ctm.pnt$z <- pmax(ctm.pnt$z, conc.min)
  ctm.grd$z <- pmax(as.vector(ctm.grd$z), conc.min)
  out <- list(points=ctm.pnt,
              grid=list(x=dum[[1]],
                        y=dum[[2]],
                        z=ctm.grd$z))
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
prepare.obs <- function(obs.daily, day,
                        conc.min=10^-6) {
  time <- sort(unique(as.POSIXct(as.character(obs.daily$Time),tz="Africa/Algiers")))
  if(min(diff.POSIXt(time)) < as.difftime(1,units="days")) {
    stop("Cannot deal with sub-daily data.")
  }
  out <- subset(obs.daily, 
                subset=(format(Time,format="%Y-%m-%d")==
                          format(as.POSIXct(day),format="%Y-%m-%d")))
  out <- out[which(!is.na(out[,2])),]
  out[,2] <- pmax(out[,2], conc.min)
  rownames(out) <- 1:nrow(out)
  return(out)
}
                        
## Prepare what needed for kriging
prepare.day <- function(day,
                        obs.daily,
                        ctm.daily,
                        emis.winter, emis.summer,
                        elev=NULL,
                        verbose=FALSE) {
  ## select the required day from the observations
  obs.day <- prepare.obs(obs.daily=obs.daily, day=day)
  if(verbose) print(paste("Prepared observations for day ",day,sep=""))
  
  ## get the coordinates of the stations with valid data
  coords.pnt <- ll2utm(rlat=obs.day$Lat,
                       rlon=obs.day$Lon,
                       iz=32)
  x.pnt <- coords.pnt$x
  y.pnt <- coords.pnt$y
  
  ## prepare emissions for the required day and interpolate them to the station points
  emis.day <- prepare.emis(emis.winter=emis.winter,
                           emis.summer=emis.summer, 
                           day=day, x.pnt=x.pnt, y.pnt=y.pnt)
  if(verbose) print(paste("Prepared emissions for day ",day,sep=""))
  
  ## get the coordinates of the reference grid
  x.grd <- emis.summer$coords$x
  y.grd <- emis.summer$coords$y
  
  ## prepare CTM concentrations for the required day and interpolate them
  ## to the station points and to the reference grid
  ctm.day <- prepare.ctm(ctm.daily=ctm.daily, day=day, 
                         x.pnt=x.pnt, y.pnt=y.pnt, 
                         x.grd=x.grd, y.grd=y.grd)
  if(verbose) print(paste("Prepared CTM concentrations for day ",day,sep=""))
  
  ## prepare elevation for the required day
  if(is.null(elev)) {
    elev.day <- NULL
  } else {
    elev.day <- prepare.elev(elev=elevation,
                             x.pnt=x.pnt, y.pnt=y.pnt, 
                             z.pnt=obs.day$Elev)  
    if(verbose) print(paste("Prepared elevation for day ",day,sep=""))
  }
  
  Out <- list(obs.day=obs.day, 
              ctm.day=ctm.day, 
              emis.day=emis.day, 
              elev.day=elev.day)
  return(Out)
}