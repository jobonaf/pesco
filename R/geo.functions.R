## conversion: lat/long to UTM 32N WGS84
ll2utm <- function(rlat,rlon,iz=32) {
  
  k0=0.9996
  a=6378206.4
  e2=0.00676866
  ep2=0.0068148
  false_e=500000.0
  dtr=3.141592654/180.0
  
  # --- Compute delta longitude in radians
  dl = dtr*(rlon - (6.0*iz-183.0))
  
  # --- Convert phi (latitude) to radians
  p = dtr*rlat
  
  sinp = sin(p)
  N = a/sqrt(1.0-e2*sinp*sinp)
  tanp = tan(p)
  T = tanp*tanp
  cosp = cos(p)
  C = ep2*cosp*cosp
  A1 = dl*cosp
  M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p) 
  - 0.02*sin(6.0*p)
  
  A2 = A1**2
  A3 = A2*A1
  A4 = A2**2
  A5 = A4*A1
  A6 = A4*A2
  T2 = T**2
  
  # --- Compute UTM x and y (km)
  x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0               
                   + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0) 
             + false_e)
  y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0 
                   + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
  # --- in km, unlike false_e
  false_n = 10000. * as.integer(rlat == 0.)
  y = 0.001*k0*y + false_n
  ## in m
  out = list(x=x*1000, y=y*1000)
  return(out)
}


## given lat and lon vectors
## calculates UTM-regular coordinates
ll2utm.grid <- function(lat,lon,round=-2,iz=32) {
  utm <- ll2utm(rlat=as.vector(lat),rlon=as.vector(lon),iz=iz)
  coords <- data.frame(x=round(utm$x,round),
                       y=round(utm$y,round))
  return(coords)
}
