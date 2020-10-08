B1Policy <- function(D,A,v,r,B1,xl,sl,epsilon=0.001,kmin=100,iterlimit=0) {
  
  #validity check
  if(!is.vector(D, mode="numeric") | !is.vector(A, mode="numeric") | !is.vector(v, mode="numeric") | !is.vector(r, mode="numeric")| 
     !is.vector(B1, mode="numeric") | !is.vector(xl, mode="numeric") | !is.vector(sl, mode="numeric")){
    return(-1)
  }
  if(length(D) != length(A) | length(A) != length(v) | length(v) != length(r)){
    return(-1)
  }
  for(i in 1:length(A)){
    if(D[i] < 0 | A[i] < 0 | v[i] < 0 | r < 0){
      return(-1)
    }
  }
 # calculate the initial values  

  Q <- c(sqrt(2*(D*A)/(v*r)))
  k <- c(9999)
  Rprime <- (1/(2*sqrt(2*pi)))*(B1/A)*sl*(Q[1]/sl)^2
  s <- list()
  TRC <- c(NA)
  Qdiff <- c(NA)
  kdiff <- c(NA)
  
  # begin first loop calculating R
  for(i in 2:(iterlimit+1)){
    R <- 2*log(Rprime/(Q[i-1]))
    if(R >= 0){
      k[i] <- sqrt(R)
    }
    else{
      k[i] <- kmin
    }
    Q[i] <- Q[1]*sqrt(1 + (B1/A)*(1-pnorm(k[i])))
    s[i] <- xl + k[i]*sl
    TRC[i] <- ((Q[i]/2)*v*r)+((D*A)/Q[i])
    Qdiff[i] <- abs(Q[i] - Q[i-1])
    kdiff[i] <- abs(k[i] - k[i-1])
    
    #break for loop if converged
    if(Qdiff[i] <= epsilon & kdiff[i] <= epsilon){
      break
    print(iterlimit)
      }
  }
  
  # trim vectors to remove unused iterations
 
  return(data.frame("s"=s, "k"=k, "Q"=Q, "TRC"=TRC, "kdiff"=kdiff, "Qdiff"=Qdiff)) 
             
}