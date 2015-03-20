## This script creates the data/*.rda datasets for package "pesco"
library(pesco)                          ## loads the package
setwd("/home/giovanni/R/packages/pesco/examples")
PM10.ctm <- read.ncdf.arpaer("data/ctm.nc")## reads a NetCDF and...
save(PM10.ctm,file="../data/PM10.ctm.rda")    ## ...saves the data

## PM10 observations
ff <- system("ls data/PM*asc",intern=T) ## list of the ASCII files with obs.PM10 data
PM10.obs <- qaria2long(datafiles=ff,       ## reads obs.PM10 data and...
                    anafile="data/anagr_stzqa.PM10.dat")
save(PM10.obs,file="../data/PM10.obs.rda")    ## ...saves the data

## NO2 observations
ff <- system("ls data/NO2*asc",intern=T) ## list of the ASCII files with obs.PM10 data
NO2.obs <- qaria2long(datafiles=ff,       ## reads obs.PM10 data and...
                    anafile="data/anagr_stzqa.NO2.dat")
save(NO2.obs,file="../data/NO2.obs.rda")    ## ...saves the data

## Elevation
elevation <- read.field(file="data/elev_1km.dat",header=F,coords.col=1:2,data.col=3)
save(elevation,file="../data/elevation.rda")      

## Population
population <- read.field(file="data/pop_1km.txt.2010",header=T,sep=",",coords.col=1:2,data.col=3)
save(population,file="../data/population.rda") 

## Emissions 
PM10.summer <- read.field(file="data/PM10_est.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
PM10.winter <- read.field(file="data/PM10_inv.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
PM10.annual <- read.field(file="data/PM10_ann.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
NOx.summer <- read.field(file="data/NOx_est.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
NOx.winter <- read.field(file="data/NOx_inv.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
NOx.annual <- read.field(file="data/NOx_ann.txt.2010",header=F,coords.col=1:2,data.col=3,coords.fact=1000)
emissions <- list(PM10.summer=PM10.summer,
                  PM10.winter=PM10.winter,
                  PM10.annual=PM10.annual,
                  NOx.summer=NOx.summer,
                  NOx.winter=NOx.winter,
                  NOx.annual=NOx.annual)
save(emissions,
     file="../data/emissions.rda") 
