#SFRDNV (problem 1 no vector)
#@author Marc Hilderbrand

SFRDNV <- function(a,b,w,epsilon=0.01) {
  
  #validity check
  if(!is.vector(a, mode="numeric") | !is.vector(b, mode="numeric") | !is.vector(w, mode="numeric")){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a)){
    return(-1)
  }
  for(i in 1:length(a))
    if(a[i] < 0 | b[i] < 0 | w[i] < 0 | epsilon < 0){
      return( -1)
    }
  
  sumWeight<- sum(w)
  
  #order a in numerical order and order w in the same order
  newOrder<-order(a)
  newW<-w[newOrder]
  newA<-a[newOrder]
  sumA<-0
  sumB<-0
  
  for(i in 1:length(newA)){
    sumA<- newW[i] + sumA
    if(sumA >= sumWeight/2){
      aCoord<-newA[i]
      break
    }
  }
  
  #repeat above for b
  newOrder<-order(b)
  newW<-w[newOrder]
  newB<-b[newOrder]
  
  for(i in 1:length(newB)){
    sumB<- newW[i] + sumB
    if(sumB >= sumWeight/2){
      bCoord<-newB[i]
      break
    }
  }
  
  TotalCost<-0
  
  for(i in 1:length(w)){
    TotalCost<- TotalCost + w[i] *(abs(aCoord-a[i]) + abs(bCoord-b[i]))
  }

  return(list(aCoord, bCoord, TotalCost))

}
