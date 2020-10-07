B1Policy <- function(D,A,v,r,B1,xL,sigmaL,epsilon=0.001,kmin=100,iterlimit=0) {
  
  #validity check
  if(!is.vector(D, mode="numeric") | !is.vector(A, mode="numeric") | !is.vector(v, mode="numeric") | !is.vector(r, mode="numeric")){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a)){
    return(-1)
  }
  for(i in 1:length(a)){
    if(a[i] < 0 | b[i] < 0 | w[i] < 0 | epsilon < 0){
      return(-1)
    }
  }
 # calculate the intital values  
  Q[1] <- sqrt(2(D*A)/(V*R))
  k[1] <- 9999
  Rprime <- (1/(2*sqrt(2*pi)))*(B1/A)*sigmaL*(Q[1]/sigmaL)^2
  s <- list()
  TRC[1] <- NULL
  Qdiff[1] <- NULL
  kdiff[1] <- NULL
  
  # begin first for loop calculating R
  for(i in 2:(iterlimit+1)){
    R <- 2*ln(Rprime/(Q[i-1]))
    if(R >= 0){
      k[i] = sqrt(R)
    }
    else{
      k[i] <- kmin
    }
    Q[i] <- Q[1]*sqrt(1 + (B1/A)*(1-pnorm(k[i])))
    s[i] <- xL + k[i]*sigmaL
    TRC[i] = ((Q[i]/2)*v*r)+((D*A)/Q[i])
    Qdiff[i] <- abs(Q[i] - Q[i-1])
    kdif[i] <- abs(k[i] - k[i-1])
    
    #break for loop if converged
    if(Qdiff[i] <= sigmaL & kdiff[i] <= sigmaL){
      break
    print(iterlimit)
      }
  }
  
  # trim vectors to remove unused iterations
  tempQ[i] <- Q[]
  temps[i] <- 
  tempTRC[i] <- 
  tempQdiff[i] <- 
  tempkdif[i] <- 
  for()
  
  #return(data.frame("s"=s[i], "k"=k[i], "Q"=Q[i], "TRC"=TRC[i], "kdiff"=kdiff[i], "Qdiff"=Qdiff[i])) 
             
}