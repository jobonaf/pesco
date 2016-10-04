#' ---
#' title: "Demo: How to perform the kriging on a specific day"
#' author: "Giovanni Bonafe'"
#' date: "October 4th, 2016"
#' ---

#' ## How to perform the kriging on a specific day
#' You can reproduce the following R code with 
#' ```demo(kriging)``` after loading package ```pesco```. Note that
#' some of the commands used here have been already used in ```demo(prepare.day)```.

## load packages
require(pesco)
require(fields)

## load data
data(PM10.obs)
data(emissions)
data(PM10.ctm)
data(elevation)

## prepare data for the day
myDay <- "2015-03-02"
PM10.ctm.ave <- dailyCtm(PM10.ctm, statistic  =  "mean")
dataDay <- prepare.day(day = myDay,
                       obs.daily = PM10.obs,
                       ctm.daily = PM10.ctm.ave,
                       pollutant="PM10",
                       emis.winter = emissions$PM10.winter,
                       emis.summer = emissions$PM10.summer,
                       elev = elevation,
                       verbose = TRUE)

## kriging
K <- kriging (x.pnt = dataDay$ctm.day$points$x, 
              y.pnt = dataDay$ctm.day$points$y, 
              obs = dataDay$obs.day$PM10, 
              model = dataDay$ctm.day,
              proxy.1 = dataDay$emis.day, 
              proxy.2 = dataDay$elev.day, 
              lambda = 0)

#' Let's see the results on a map.

bb <- round(quantile(c(K$K, dataDay$ctm.day$grid$z),
                     probs=seq(0,1,length.out=11)))
cc <- tim.colors(10)
ll <- paste(bb[-length(bb)],bb[-1],sep="-")
xr <- range(dataDay$ctm.day$grid$x)
yr <- range(dataDay$ctm.day$grid$y)

## CTM
plot(dataDay$ctm.day$grid$x, dataDay$ctm.day$grid$y, 
     main="interpolated CTM",
     col=cc[cut(dataDay$ctm.day$grid$z,breaks=bb)], 
     pch=19, cex=0.4, xlab="x UTM", ylab="y UTM")
legend("bottomleft", fill=rev(cc), legend=rev(ll), 
       bg="white", y.intersp=0.8)

## observations
plot(dataDay$ctm.day$points$x, dataDay$ctm.day$points$y, 
     main="observations",
     col=cc[cut(dataDay$obs.day$PM10,breaks=bb)],
     pch=19, cex=1, xlim=xr, ylim=yr, 
     xlab="x UTM", ylab="y UTM")
points(dataDay$ctm.day$points$x, dataDay$ctm.day$points$y, 
       col="black", pch=1, cex=1)
legend("bottomleft", fill=rev(cc), legend=rev(ll), 
       bg="white", y.intersp=0.8)

## kriging: predicted values
plot(K$x, K$y, main="kriging: predicted values",
     col=cc[cut(K$K,breaks=bb)], 
     pch=19, cex=0.4, xlim=xr, ylim=yr, 
     xlab="x UTM", ylab="y UTM")
legend("bottomleft", fill=rev(cc), legend=rev(ll), 
       bty="n", y.intersp=0.8)

## kriging: predicted variances
bb <- round(quantile(K$K.var,
                     probs=seq(0,1,length.out=11)))
cc <- designer.colors(10)
ll <- paste(bb[-length(bb)],bb[-1],sep="-")
plot(K$x, K$y, main="kriging: predicted variances",
     col=cc[cut(K$K.var,breaks=bb)], 
     pch=19, cex=0.4, xlim=xr, ylim=yr, 
     xlab="x UTM", ylab="y UTM")
legend("bottomleft", fill=rev(cc), legend=rev(ll), 
       bty="n", y.intersp=0.8)

