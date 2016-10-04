#' ---
#' title: "Demo: How to prepare the data to perform the kriging on a specific day"
#' author: "Giovanni Bonafe'"
#' date: "October 4th, 2016"
#' ---

#' ## How to prepare the data to perform the kriging on a specific day
#' You can reproduce the following R code with 
#' ```demo(prepare.day)``` after loading package ```pesco```. Note that
#' some of the commands used here have been already used in ```demo(daily.synthesis)```.

## load package
require(pesco)

## load daily observations
data(PM10.obs)

## select the required day from the observations
myDay <- "2015-03-02"
PM10.obs.day <- prepare.obs(obs.daily=PM10.obs, day=myDay,
                            pollutant="PM10")

## get the coordinates of the stations with valid data
coords.pnt <- ll2utm(rlat=PM10.obs.day$Lat,
                     rlon=PM10.obs.day$Lon,
                     iz=32)
x.pnt <- coords.pnt$x
y.pnt <- coords.pnt$y
plot(x.pnt, y.pnt, pch=19, cex=0.5, col="grey")
text(x.pnt, y.pnt, labels=PM10.obs.day$PM10)

#' Emissions are supposed to be already defined on the reference grid. 
#' Otherwise you can interpolate them with optional arguments ```x.grd```
#' and ```y.grd``` of ```prepare.emis()```. 

## load emissions
data(emissions)

## prepare emissions for the required day and interpolate them to the station points
emis.day <- prepare.emis(emis.winter=emissions$PM10.winter,
                         emis.summer=emissions$PM10.summer, 
                         day=myDay, x.pnt=x.pnt, y.pnt=y.pnt)

## get the coordinates of the reference grid
x.grd <- emissions$PM10.summer$coords$x
y.grd <- emissions$PM10.summer$coords$y

#' Gridded CTM data must be interpolated to the reference grid and to the points where
#' observed data are available.

## load hourly CTM concentrations
data(PM10.ctm)

## calculate daily averages
PM10.ctm.ave <- dailyCtm(PM10.ctm, statistic="mean")

## prepare CTM concentrations for the required day and interpolate them
## to the station points and to the reference grid
PM10.ctm.day <- prepare.ctm(ctm.daily=PM10.ctm.ave, day=myDay, 
                            x.pnt=x.pnt, y.pnt=y.pnt, 
                            x.grd=x.grd, y.grd=y.grd)
plot(x=PM10.obs.day$PM10, y=PM10.ctm.day$points$z,
     xlab=expression(PM10[obs]~(mu*g/m^3)),
     ylab=expression(PM10[CTM]~(mu*g/m^3)))
abline(a=0,b=1,lty=2)

#' Elevation are supposed to be already defined on the reference grid. 
#' Otherwise you can interpolate them with optional arguments ```x.grd```
#' and ```y.grd``` of ```prepare.elev()```. 
#' If available, station elevation is taken from the metadata.

## load elevation
data(elevation)

## prepare elevation for the required day
elev.day <- prepare.elev(elev=elevation,
                         x.pnt=x.pnt, y.pnt=y.pnt, 
                         z.pnt=PM10.obs.day$Elev)

#' Let's have a look to the data we prepared...

## data at the stations points
signif(data.frame(Obs=PM10.obs.day$PM10, 
                  Ctm=PM10.ctm.day$points$z, 
                  Emis=emis.day$points$z, 
                  Elev=elev.day$points$z), 2)

#' The same result can be achieved with one single function.

dataDay <- prepare.day(day=myDay,
                       obs.daily=PM10.obs,
                       ctm.daily=PM10.ctm.ave,
                       pollutant="PM10",
                       emis.winter=emissions$PM10.winter,
                       emis.summer=emissions$PM10.summer,
                       elev=elevation,
                       verbose=TRUE)

## data at the stations points
signif(data.frame(Obs=dataDay$obs.day$PM10, 
                  Ctm=dataDay$ctm.day$points$z, 
                  Emis=dataDay$emis.day$points$z, 
                  Elev=dataDay$elev.day$points$z), 2)
