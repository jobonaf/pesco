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
               dimnames(ctm.daily)[[3]])
  ctm.day <- ctm.daily[,,idx]
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
  out <- list(ctm.pnt=ctm.pnt, ctm.grd=ctm.grd)
  return(out)
}

## Emissions (with seasonality)
prepare.emis <- function(emis.winter, emis.summer, day,
                         x.pnt, y.pnt, 
                         x.grd=NULL, y.grd=NULL) {
  mm <- as.numeric(Month(day))
  ##          Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
  coef.w <- c(1.00,1.00,0.75,0.50,0.25,0.00,0.00,0.00,0.25,0.50,0.75,1.00)
  coef.s <- c(0.00,0.00,0.25,0.50,0.75,1.00,1.00,1.00,0.75,0.50,0.25,0.00)
  emis <- (emis.winter*coef.w[mm] + emis.summer*coef.s[mm]) / (coef.w[mm] + coef.s[mm])
  coords <- emis.winter$coords
  emis.pnt <- Interp(x=coords$x, y=coords$y, z=emis,
                    xp=x.pnt, yp=y.pnt,
                    type="points",method="linear")
  if(!is.null(x.grd) & !is.null(y.grd)) {
    emis.grd <- Interp(x=coords$x, y=coords$y, z=emis,
                       xp=x.grd, yp=y.grd,
                       type="grid",method="linear")    
  } else emis.grd <- NULL
  Emis <- list(emis.pnt=emis.pnt, emis.grd=emis.grd)
  return(Emis)
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
  rownames(out) <- 1:nrow(out)
  return(out)
}
                        