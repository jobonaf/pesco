## This script creates the data/*.rda datasets for package "pesco"
library(pesco)                          ## loads the package
PMctm <- read.ncdf.arpaer("data/ctm.nc")## reads a NetCDF and...
save(PMctm,file="../data/PMctm.rda")    ## ...saves the data
ff <- system("ls data/PM*asc",intern=T) ## list of the ASCII files with obs.PM10 data
PMobs <- qaria2long(datafiles=ff,       ## reads obs.PM10 data and...
                    anafile="data/anagr_stzqa.PM10.dat")
save(PMobs,file="../data/PMobs.rda")    ## ...saves the data
elevation <- read.geodata(file="data/elev_1km.dat",
                          header=F,     ## reads the elevation and...
                          coords.col=1:2,data.col=3)
save(elevation,
     file="../data/elevation.rda")      ## ...saves it

## reads emissions, converts coordinates in meters and...
PM10.summer <- read.geodata(file="data/PM10_est.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
PM10.summer[[1]] <- PM10.summer[[1]]*1000
PM10.winter <- read.geodata(file="data/PM10_inv.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
PM10.winter[[1]] <- PM10.summer[[1]]*1000
PM10.annual <- read.geodata(file="data/PM10_ann.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
PM10.annual[[1]] <- PM10.annual[[1]]*1000
NOx.summer <- read.geodata(file="data/NOx_est.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
NOx.summer[[1]] <- NOx.summer[[1]]*1000
NOx.winter <- read.geodata(file="data/NOx_inv.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
NOx.winter[[1]] <- NOx.winter[[1]]*1000
NOx.annual <- read.geodata(file="data/NOx_ann.txt.2010",
                            header=F,     
                            coords.col=1:2,data.col=3)
NOx.annual[[1]] <- NOx.annual[[1]]*1000
emissions <- list(PM10.summer=PM10.summer,
                  PM10.winter=PM10.winter,
                  PM10.annual=PM10.annual,
                  NOx.summer=NOx.summer,
                  NOx.winter=NOx.winter,
                  NOx.annual=NOx.annual)
save(emissions,
     file="../data/emissions.rda")      ## ...saves them
