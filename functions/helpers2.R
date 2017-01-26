

makeNaapuriMat <- function(naapurit){
  
  len <- length(naapurit[,1])
  
  matr <- matrix(nrow = len,ncol = len)
  matr[,] <- 0
  colnames(matr) <- naapurit[,1]
  rownames(matr) <- naapurit[,1]
  
  
  for(i in 1:len){
    name <- toString(naapurit[i,1])
    naapA <- naapurit[i,]
    naapA <- naapA[naapA != ""]
    len2 <- length(naapA)
    for(j in 2:len2){
      #if(naapurit[i,j]!=""){
      naap <- toString(naapurit[i,j])
      k <- match(naap,naapurit[,1])
      matr[i,k] <- 1
      matr[k,i] <- 1
      #}
    }
    
  }
  
  return(matr)
}

findNeighbours <- function(county,neigM){
  
  return(subset(colnames(neigM),neigM[,county]==1))
}

singleNeighbours <- function(neigM){
  
  len <- length(neigM[1,])
  
  singles <- list()
  k <- 1
  for (i in 1:len){
    n <- length(subset(colnames(neigM),neigM[,i]==1))
    if(n==1){
      singles[k] <- colnames(neigM)[i]
      k <- k+1
    }
  }
  
  return(singles)
  #length(subset(colnames(neighboursMatrix),neighboursMatrix[,"Kontiolahti"]==1))
}

#if naapuri %in% singles
#espoonnaapurit <- findNeighbours("Espoo",neighboursMatrix)
#popul$popul[popul$name %in% espoonnaapurit]

findPop <- function(county){
  
  #df popul contains population data
  return(popul[popul$name==county,]$popul)
  
}

#### Divide counties by "network" distances####
#Takes counties population df (names and population), county name to start from and how many parts want to divide 
takeToPop <- function(counties0, county0=NA, divid=3, useDistances=FALSE){
  popu <- sum(counties0[2])
  subPop <- round(popu/divid)
  countiesDivided <- counties0
  #Todo divider initially 0 or divid + 1?
  countiesDivided$divider <- as.character(0)
  
  if(is.na(county0)){
    county0 <- toString(counties0[1][order(counties0[2],decreasing = T)[1],])
  }
  
  subList <- divideCountry(counties0=counties0,county0=county0,subPop=subPop, divid=divid,useDistances=useDistances)
  
  return(subList)
}

divideCountry <- function(counties0, county0, subPop, divid,useDistances=FALSE){
  
  
  require(igraph)
  g <- graph_from_adjacency_matrix(neighboursMatrix,mode="undirected")
  nearest <- as.data.frame( shortest.paths(g,v=county0))
  #nearest <- shortest.paths(g,v=county0)
  nearest0 <- nearest[order(nearest)]
  
  
  subFin <- list()
  j <- 0
  
  for(i in 1:(divid-1)){
    
    newNearest <- nearest0
    pop <- 0
    k <- 0

    while(pop < subPop){
     
      k <- k + 1
      j <- j + 1
      #Todo tämä on vaarallinen kohta, menee helposti rikki jos ei täsmälleen samoja nimiä eri frameissa
      pop <- pop + counties0[2][counties0[1]==toString(colnames(nearest0[k]))]
      #print(pop)
      subFin$name[j] <- toString(counties0[1][counties0[1]==colnames(nearest0[k])])
      #print(subFin$name[j])
      subFin$group[j] <- i
      newNearest <- newNearest[-1]
    }
    nearest0 <- newNearest
    
    
  }
  
  return(subFin)
  
  
}
####Add divider ####
addDivider <- function(dat, divid){
  
  newDat <- dat
  #nd <- cbind(divid$name[order(divid$name)],divid$group[order(divid$name)])
  nd <- divid[order(divid[1]),]
  
  len <- length(dat$name)
  k <- 1
  for (i in 1:len){
    if(k <= length(divid$name)){
      if(dat$name[i] == toString(nd$name[k])){       #TODO ei toimi kun 2 jaettu, k menee yli 75
        newDat$divider[i] <- nd$group[k]
        #if(i+1 <= len){
        if(i+1 <= len){
          if(dat$name[i] != dat$name[i+1]){
            k <- k+1
          }
        }
      }
    }
  }
  
  return(newDat)
}


# takeSubPop <- function(county,subPop,neigM){
#   
#   countyNs <- findNeighbours(county=county,neigM=neigM)
#   
#   #if(exists("groupNs")){
#   #  groupNs <- c(groupNs, countyNs)
#   #  groupNs <- unique(groupNs)
#   #}else{
#   #  groupNs <-  countyNs
#   #}
# 
#   singles <- singleNeighbours(neigM = neigM)
#   origpop <- findPop(county)
#   #subFin <- takeToRekPop( countyNs)
#   neigChosen <- FALSE
#   len <- length(countyNs)
#   for (i in 1:len){
#     #if(countyNs[i] %in% singles){
#     if(is.element(countyNs[i],singles)){
#       groupNs <- c(county,countyNs[i] )
#       origpop <- origpop + findPop(countyNs[i])
#       neigChosen <- TRUE
#       break
#     } 
#   }
#   
#   if(!neigChosen){
#     maxPop <- 0
#     newC <- ""
#     for (i in 1:len){
#        cPop <-findPop(countyNs[i])
#        if(cPop > maxPop){
#          newC <- countyNs[i]
#          maxPop <- cPop
#        }
#       
#     }
#     groupNs <- c(county, newC)
#     origpop <- origpop + maxPop
#   }
#   
#   dividedCountry <- takeToRekPop(groupNs,subPop,singles=singles,origpop=origpop)
#   
#   return(groupNs)
#   #return(dividedCountry)
#   #TODO tarkista ensin onko naapuri singleissä ja lisää sen populaatio, sitten etsi suurin naapuri ja 
#   #lisää sen populaatio kok. populaatioon, tarkista onko yli subPop
#   # Tarkista myös mitkä kunnat on jo käytetty
#   
# }
# 
# takeToRekPop <- function(counties,subPop,singles=NA,origpop=0){
#   #Otetaan single tai väkirikkain naapuri countiesin naapureista jos subpop ei ylitetty
#   #Pitäisi myös tarkistaa onko joukko saarrettu, Ehkä ensin jaetaan osiin
#   
# }
# 
# # Finds (node) distances from county,
# # county: county to find distances
# # neig: neigbourmatrix
# findDistances <- function(county, neig){
#   countyNs <- findNeighbours(county,neig)
#   
#   distancesToC <- list(name=countyNs,dist=rep.int(1,length(countyNs)))
#   
#   
#   
#   
#   return(distancesToC)
#   
# }

#library(igraph)
# g <- graph_from_adjacency_matrix(neighboursMatrix,mode="undirected")
#nearestHelsinki <- as.data.frame( shortest.paths(g,v="Helsinki"))
#nearestHels <- nearestHelsinki[order(nearestHelsinki)]
#colnames(nearestHels)
#if(exists("gN")){
#  gN <-c(gN,espoonnaapurit)
#}else{
#  gN <- espoonnaapurit
#}