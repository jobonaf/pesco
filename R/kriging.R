kriging <- function (x.pnt, y.pnt, obs,
                     model,
                     proxy.1, 
                     proxy.2=NULL, 
                     lambda, 
                     K.max.dist=200000, # meters 
                     K.min.dist=1000,   # meters 
                     K.pairs.min=1) {
  # remove points from the target grid
  # where model or any proxy is missing
  clean.grids <- function(objs) {
    for (i in 1:length(objs)) {
      idx <- !is.na(objs[[i]]$grid$z)
      if(1==1) {Idx <- idx} else {Idx <- Idx&idx}
    }
    for (i in 1:length(objs)) {
      objs[[i]]$grid$x <- objs[[i]]$grid$x[which(Idx)]
      objs[[i]]$grid$y <- objs[[i]]$grid$y[which(Idx)]
      objs[[i]]$grid$z <- objs[[i]]$grid$z[which(Idx)]
    }
    return(objs)
  }
  if(is.null(proxy.2)) {
    out <- clean.grids(list(model=model, proxy1=proxy.1))
    model <- out$model
    proxy.1 <- out$proxy.1
  } else {
    out <- clean.grids(list(model=model, proxy.1=proxy.1, proxy.2=proxy.2))
    model <- out$model
    proxy.1 <- out$proxy.1
    proxy.2 <- out$proxy.2
  }
  
  ### Trends
  if(is.null(proxy.2)){
    trend.d <- ~ boxcox(model$points$z,lambda) + proxy.1$points$z 
    trend.l <- ~ boxcox(model$grid$z,lambda)   + proxy.1$grid$z   
  } else {
    trend.d <- ~ boxcox(model$points$z,lambda) + proxy.1$points$z + proxy.2$points$z
    trend.l <- ~ boxcox(model$grid$z,lambda)   + proxy.1$grid$z   + proxy.2$grid$z
  }
  
  ### Geodata
  geodata<-as.geodata(cbind(x.pnt,y.pnt,obs))
  
  ### Variogram
  # Empirical variogram calculation
  vario<-variog(messages=FALSE, 
                geodata,
                max.dist= K.max.dist,
                pairs.min= K.pairs.min,
                nugget.tolerance = K.min.dist,
                trend = trend.d,
                lambda=lambda) 
  
  varioest<-likfit(geodata,
                   trend = trend.d,
                   ini.cov.pars=matrix(c(quantile(vario$v,
                                                  p=seq(0,1,0.2)),
                                         quantile(vario$u,
                                                  p=seq(0,1,0.2))),
                                       6,2),
                   cov.model = "exponential",
                   fix.lambda = TRUE,
                   lambda=lambda,
                   messages=FALSE,
                   nospatial = FALSE)
  
  # Kriging
  kkk<-krige.conv(geodata,
                  locations=cbind(model$grid$x, model$grid$y),
                  krige= krige.control(
                    type.krige="OK",
                    dist.epsilon= max(K.min.dist,1e-10),
                    trend.d= trend.d, 
                    trend.l= trend.l,
                    cov.pars=c(max(varioest$sigmasq,1e-10),
                               varioest$phi),
                    nugget=varioest$nugget,
                    lambda=lambda))
  
  # Correction
  out <- list(K     = kkk$predict, 
              K.var = kkk$krige.var,
              x     = model$grid$x,
              y     = model$grid$y)
  return(out)
}


