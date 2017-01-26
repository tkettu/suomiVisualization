#### Data Handlers ####

# kml-file

xmlToData <- function(data){
  k <- 1
  len <- length(data)
  coords <- list()
  for(i in 1:len){
    
    nam <- data[i]$Placemark$name
    koords <- data[i]$Placemark$Polygon$outerBoundaryIs$LinearRing$coordinates
    if (length(koords)>0){
      coords$name[k] <- nam  
      coords$coord[k] <- koords
      k <- k+1
    }else{
      len2 <- length(data[i]$Placemark$MultiGeometry)
      #Handle Multigeometries
      for(j in 1:len2){
        coords$name[k] <- nam
        coords$coord[k] <- data[i]$Placemark$MultiGeometry[j]$Polygon$outerBoundaryIs$LinearRing$coordinates
        k <- k + 1
      }
    }
    
  }
  
  return(coords)
}