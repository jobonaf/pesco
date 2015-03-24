kriging <- function (x.pnt, y.pnt, obs, x.grd, y.grd, 
                     model,
                     proxy.1, 
                     proxy.2=NULL, 
                     lambda, 
                     K.max.dist=200000, # meters 
                     K.min.dist=1000,   # meters 
                     K.pairs.min=1) {
  # remove points from the target grid
  # where model or any proxy is missing
  idx <- which(!is.na(model$grid$z) & !is.na(proxy.1$grid$z) & !is.na(proxy.2$grid$z))
  model$grid$z
  
  ### Trends
  if(is.null(proxy2)){
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
                  loc=cbind(x.grd,y.grd),
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
              K.var = kkk$krige.var)
  return(out)
}


