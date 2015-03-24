#' ---
#' title: "How to prepare datasets for PESCO starting from ASCII and NetCDF files"
#' author: "Giovanni Bonafe'"
#' date: "March 23rd, 2015"
#' ---

#' ## How to prepare datasets for PESCO starting from ASCII and NetCDF files
#' You can find the following R code in 
#' ```example/``` within the package ```pesco```. It reads the ASCII and NetCDF files
#' in ```example/data/``` and convert them in .rda format, putting the output files
#' in ```data/```.

#' For reading the Arpa-ER NetCDF we use ```read.ncdf.arpaer```, which performs directly 
#' the timezone conversion from UTC to Central Europe Time without
#' Daylight Saving Time, codified as ```'Africa/Algiers'```. Therefore, a NetCDF which 
#' is originally from 00:00 to 23:00 UTC, after reading is converted from 01:00 Africa/Algiers 
#' to 00:00 of the day after.

#+ ctm, eval=FALSE
## Load the package, read the NetCDF, save the data in rda format
require(pesco)                         
setwd("~/R/packages/pesco/examples")
PM10.ctm <- read.ncdf.arpaer("data/ctm.nc")
save(PM10.ctm,file="../data/PM10.ctm.rda")   

#' To read all the files in a directory, each file containing the data of a single
#' monitoring station, we create a vector of characters containing the names of
#' the files. Here we do it first for PM10 observations...

#+ pm10-obs, eval=FALSE
## PM10 observations
ff <- system("ls data/PM*asc",intern=T) 
PM10.obs <- qaria2long(datafiles=ff,      
                    anafile="data/anagr_stzqa.PM10.dat")
save(PM10.obs,file="../data/PM10.obs.rda")    

#' ...and then for NO2.

#+ no2-obs, eval=FALSE
## NO2 observations
ff <- system("ls data/NO2*asc",intern=T) 
NO2.obs <- qaria2long(datafiles=ff,      
                    anafile="data/anagr_stzqa.NO2.dat")
save(NO2.obs,file="../data/NO2.obs.rda")  

#' Finally, we use the function ```read.field``` to read static data.

#+ others, eval=FALSE
## Elevation
elevation <- read.field(file="data/elev_1km.dat",header=F,coords.col=1:2,data.col=3)
save(elevation,file="../data/elevation.rda")      

## Population
population <- read.field(file="data/pop_1km.txt.2010",header=T,sep=",",coords.col=1:2,data.col=3)
save(population,file="../data/population.rda") 

## Emissions 
PM10.summer <- read.field(file="data/PM10_est.txt.2010",header=F,
                          coords.col=1:2,data.col=3,coords.fact=1000)
PM10.winter <- read.field(file="data/PM10_inv.txt.2010",header=F,
                          coords.col=1:2,data.col=3,coords.fact=1000)
PM10.annual <- read.field(file="data/PM10_ann.txt.2010",header=F,
                          coords.col=1:2,data.col=3,coords.fact=1000)
NOx.summer <- read.field(file="data/NOx_est.txt.2010",header=F,
                         coords.col=1:2,data.col=3,coords.fact=1000)
NOx.winter <- read.field(file="data/NOx_inv.txt.2010",header=F,
                         coords.col=1:2,data.col=3,coords.fact=1000)
NOx.annual <- read.field(file="data/NOx_ann.txt.2010",header=F,
                         coords.col=1:2,data.col=3,coords.fact=1000)
emissions <- list(PM10.summer=PM10.summer,
                  PM10.winter=PM10.winter,
                  PM10.annual=PM10.annual,
                  NOx.summer=NOx.summer,
                  NOx.winter=NOx.winter,
                  NOx.annual=NOx.annual)
save(emissions,
     file="../data/emissions.rda") 
