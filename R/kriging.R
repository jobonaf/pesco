kriging <- function (xs, ys, obs, xi, yi, 
                     K.max.dist, K.pairs.min, K.min.dist, 
                     trend.d, trend.l, lambda) {
  ##############################
  ### Fitting and Prediction ###
  ##############################
  ### Geodata
  geodata<-as.geodata(cbind(xs,ys,obs))
  
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
                  loc=cbind(xi,yi),
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
  out <- list(K = kkk$predict, 
              K.var = kkk$krige.var)
  return(out)
}


