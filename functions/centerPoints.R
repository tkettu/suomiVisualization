countCenterP <- function(coords){
  
  coo <- coordsToList(coords)
  
  kplat <- sum(coo$lat)/length(coo$lat)
  kplon <- sum(coo$lon)/length(coo$lon)
  
  kp <- list(lat=kplat,lon=kplon)
  return(kp)
}

addCenterPoints <- function(k){
  
  len <- length(k$name)
  
  for (i in 1:len){
    center <- countCenterP(k$coord[i])
    k$centerLat[i] <- center$lat
    k$centerLon[i] <- center$lon
  }
  
  return(k)
}