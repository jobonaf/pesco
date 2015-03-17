## load hourly observation
data(NO2.obs)

## calculate daily averages
NO2.obs.ave <- dailyObs(NO2.obs,statistic="mean",pollutant="NO2")
boxplot(data=NO2.obs.max, NO2~Time, range=0, border="orange", col="orange", lty=1)

## calculate daily maxima
NO2.obs.max <- dailyObs(NO2.obs,statistic="max",pollutant="NO2")
rm(NO2.obs)
boxplot(data=NO2.obs.max, NO2~Time, range=0, border="orange", col="orange", lty=1)
boxplot(data=NO2.obs.ave, NO2~Time, range=0, border="olivedrab", col="olivedrab", lty=1, add=T, xaxt="n", yaxt="n")

## load hourly CTM concentrations
data(PM10.ctm)

## calculate daily averages
PM10.ctm.ave <- dailyCtm(PM10.ctm, statistic="mean")
rm(PM10.ctm)
filled.contour(PM10.ctm.ave$data[,,1],color.palette=tim.colors)

