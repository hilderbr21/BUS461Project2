B1Policy <- function(D,A,v,r,B1,xl,sl,epsilon=0.001,kmin=100,iterlimit=0) {
  
  #validity check
  if(!is.vector(D, mode="numeric") | !is.vector(A, mode="numeric") | !is.vector(v, mode="numeric") | !is.vector(r, mode="numeric")| 
     !is.vector(B1, mode="numeric") | !is.vector(xl, mode="numeric") | !is.vector(sl, mode="numeric") |
     !is.vector(epsilon, mode="numeric") | !is.vector(kmin, mode="numeric") | !is.vector(iterlimit, mode="numeric")){
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
 # calculate the intital values  

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
  
  # # trim vectors to remove unused iterations
  # Qtrimmed[i] <- Q[1:NA]
  # temps[i] <- s[]
  # tempTRC[i] <- TRC[]
  # tempQdiff[i] <- Qdiff[]
  # tempkdiff[i] <- kdiff[]
  # 
  # for(i in length(s)){
  #   if(!is.null(s[i]) | !is.na(s[i])){
  #     strimmed[i] <- s[i]
  #   }
  # }
  # for(i in length(k)){
  #   if(is.null(k[i]) | is.na(k[i])){
  #     #skip
  #   } else{
  #     ktrimmed[i] <- k[i]
  #   }
  # }
  # for(i in length(Q)){
  #   if(is.null(Q[i]) | is.na(Q[i])){
  #     #skip
  #   } else{
  #       Qtrimmed[i] <- Q[i]
  #   }
  # }
  # for(i in length(TRC)){
  #   if(is.null(TRC[i]) | is.na(TRC[i])){
  #     #skip
  #   } else{
  #     TRCtrimmed[i] <- TRC[i]
  #   }
  # }
  # for(i in length(kdiff)){
  #   if(is.null(kdiff[i]) | is.na(kdiff[i])){
  #     #skip
  #   } else{
  #     kdifftrimmed[i] <- kdiff[i]
  #   }
  # }
  # for(i in length(Qdiff)){
  #   if(is.null(Qdiff[i]) | is.na(Qdiff[i])){
  #     #skip
  #   } else{
  #     Qdifftrimmed[i] <- Qdiff[i]
  #   }
  # }
 
  return(data.frame("s"=strimmed[i], "k"=ktrimmed[i], "Q"=Qtrimmed[i], "TRC"=TRCtrimmed[i], "kdiff"=kdifftrimmed[i], "Qdiff"=Qdifftrimmed[i])) 
             
}