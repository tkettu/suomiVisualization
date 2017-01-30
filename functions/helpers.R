
countCenterP <- function(coords){
  
  coo <- coordsToList(coords)
  
  #kplat <- sum(coo$lat)/length(coo$lat)
  #kplon <- sum(coo$lon)/length(coo$lon)
  
  kplat <- sum(coo$lon)/length(coo$lon)
  kplon <- sum(coo$lat)/length(coo$lat)
  
  kp <- list(lat=kplat,lon=kplon)
  return(kp)
}

addCenterPoints <- function(k){
  
  
  
  len <- length(k$name)
  
  for (i in 1:len){
    #if(i>1 ){
     # if( k$name[i]!=k$name[i-1]){
        center <- countCenterP(k$coord[i])
        k$centerLat[i] <- center$lat
        k$centerLon[i] <- center$lon
      #}
    #}else{
      #Do nothing
    #}
  }
  
  return(k)
}

#convert degrees to radians
degRad <- function(deg){
  return (deg*pi/180)
}

#distance <- function(x1,y1,x2,y2){
distance <- function(lat1,lon1,lat2,lon2){

  # USE:
  # Haversine
  # formula:	a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
  # c = 2 ⋅ atan2( √a, √(1−a) )
  # d = R ⋅ c
  # where	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
  # note that angles need to be in radians to pass to trig functions!
  
  lat1 <- degRad(lat1)
  lat2 <- degRad(lat2)
  lon1 <- degRad(lon1)
  lon2 <- degRad(lon2)
  
  delta.lat <- (lat2 - lat1)
  delta.lon <- (lon2 - lon1)
  
  R <- 6371
  
  a <- sin(delta.lat/2)^2 + cos(lat1)*cos(lat2)*sin(delta.lon/2)^2
  c <- 2*atan2(sqrt(a),sqrt(1-a))

  return(R*c)
  #OR
  # d = r*do
  # where do = arccos(sin (lat1))*sin(lat2) + cos(lat2)*cos(|lon2-lon1|))
  #lat lon in radius
}

countDistMatrix <- function(kWithC){
  n <- length(kWithC$name)
  
  m <- matrix(0L, nrow=n,ncol=n)
  rownames(m) <- kWithC$name
  colnames(m) <- kWithC$name
  
  for (i in 1:n){
    x1 <- kWithC$centerLat[i]
    y1 <- kWithC$centerLon[i]
    for (k in 1:i){
      x2 <- kWithC$centerLat[k]
      y2 <- kWithC$centerLon[k]
      m[i,k] <- distance(x1,y1,x2,y2)
    }
    for (j in i:n){
      x2 <- kWithC$centerLat[j]
      y2 <- kWithC$centerLon[j]
      m[i,j] <- distance(x1,y1,x2,y2)
    }
  }
  
  return(m)
}

findNearest <- function(d,c){
  distances <- d[c,]
  
  distances <- order(distances)
  
  return(distances)
}

#### divide country by distances####

takeToPop2 <- function(kunta,subPop,g){
  
  nearest <- findNearest(distM,kunta)
  
  
  subFin <- list()
  j <- 1
  
  for (i in 1:(g-1)){
    
    newNearest <- nearest
    pop <-0
    popn <- 0
    k<-1
    ta <- j
    while (pop < subPop){
      popn <- pop + countiesWithCenter$popul[nearest[k]]
  
      if(popn > subPop){
        
        if(popn > ( 2*subPop - pop)){
          
        }else{
         
          subFin$name[j] <- toString(countiesWithCenter$name[nearest[k]])
          subFin$group[j] <- i
          newNearest <- newNearest[-1]
          pop <- popn 
          k <- k + 1
          j <- j + 1
        }
        break
      }else{
        
        subFin$name[j] <- toString(countiesWithCenter$name[nearest[k]])
        subFin$group[j] <- i
        newNearest <- newNearest[-1]
        pop <- popn
        k <- k + 1
        j <- j + 1
      }
    
    }
    tl <- j
    thispop <- pop
    print(thispop)
    for(a in ta:tl){
      subFin$groupPopulation[a] <- thispop 
    }
    nearest <- newNearest
    pop <-0
    k<-1
    
    
  }
  
  #newNearest <- subset(nearest, subFin$name !%in% nearest)
  
  return(subFin)
}


takeToRekPop <- function(kunnat0, kunta0=NA, divid, popu){
  #popu <- sum(kunnatWithCenter$popul)
  subPop <- round(popu/divid)
  kunnatDivided <- kunnat0
  kunnatDivided$divider <- 0
  if(is.na(kunta0)){
    kunta0 <- countiesWithCenter$name[order(countiesWithCenter$popul,decreasing = T)[1]]  
  }
  
  #g <- 1
  subList <- takeToPop2(kunta0, subPop,divid)
  
  return(subList)
  
}

addDivider2 <- function(dat, divid){
  
  newDat <- dat
  nd <- cbind(divid$name[order(divid$name)],divid$group[order(divid$name)])
  
  len <- length(dat$name)
  k <- 1
  for (i in 1:len){
    if(k <= length(divid$name))
      if(dat$name[i] == nd[k,1]){       #TODO ei toimi kun 2 jaettu, k menee yli 75
        newDat$divider[i] <- nd[k,2]
        #if(i+1 <= len){
        if(i+1 <= len){
          if(dat$name[i] != dat$name[i+1]){
            k <- k+1
          }
        }
      }
  }
  
  return(newDat)
}

