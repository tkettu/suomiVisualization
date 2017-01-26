#### Coords to numeric ####

#Returns list of lat lon pairs from single lat lon string
coordsToList <- function(coo){
  l <- as.list(strsplit(toString(coo)," ")[[1]])
  len <- length(l)
  
  coords <- list()
  
  for(i in 1:len){
    
    ko <- as.list(strsplit(unlist(l[i]),",")[[1]])
    
    
    coords$lat[i] <- as.numeric(ko[1])
    coords$lon[i] <- as.numeric(ko[2])
  }
  
  coords <- data.frame(coords)
  return(coords)
  #return(l)
  
}


#Todo this any faster than n^2?
countiesToFrame <- function(co){
  
  len <- length(co$name)
  
  lt <- list()
  k <- 0
  
  for (i in 1:len){
    n <- toString(co$name[i])
    c <- co$coord[i]
    po <- co$popul[i]
    
    cl <- coordsToList(c)
    
    len2 <- length(cl$lat)
    
    for(j in 1:len2){ 
      k <- k+1
      lt$long[k] <- cl$la[j]
      lt$lat[k] <- cl$lo[j]
      lt$group[k] <- i
      lt$order[k] <- k
      lt$name[k] <- n
      lt$popul[k] <- po
      
    }
    
  }
  co <- data.frame(lt)
  return(co)
}