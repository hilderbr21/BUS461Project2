#SFEDNV (problem 2 no vector)
#@author Joe Crowley


SFEDNV <- function(a,b,w,epsilon=0.01,iterlimit=100) {
  
  #validity check
  if(!is.vector(a, mode="numeric") | !is.vector(b, mode="numeric") | !is.vector(w, mode="numeric")
     | !is.numeric(epsilon) | !is.numeric(iterlimit)){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a) | length(epsilon)
     != 1 | length(iterlimit) != 1){
    return(-1)
  }
  for(i in 1:length(a))
    if(a[i] <= 0 | b[i] <= 0 | w[i] <= 0 | epsilon <= 0 | iterlimit <= 0){
      return(-1)
    }
  
  #fixes error in attempting to converge past available values
  if(iterlimit > length(a)){
    iterlimit <- length(a)
  }
  
  # calculate x0 and y0 using calcCentroid
  x <- c(sum(w*a)/sum(w))
  y <- c(sum(w*b)/sum(w))

  
  # calculate revised coordinates
  for(j in 2:(iterlimit+1)){
    #reset vars
    gi <- 0
    xNumerTemp<-0
    yNumerTemp<-0
    DenomTemp<-0
    print(length(w))
    for(i in 1:length(w)){
      gi <- (w[i]/sqrt(((x[j-1]-a[i])**2) + ((y[j-1]-b[i])**2)))
      
      xNumerTemp <- xNumerTemp + (a[i]*gi)
      yNumerTemp <- yNumerTemp + (b[i]*gi)
      DenomTemp <- DenomTemp + gi
    }
    print(gi)
    print(xNumerTemp)
    
    x[j] <-xNumerTemp/DenomTemp
    y[j] <- yNumerTemp/DenomTemp
    
    print(x[j])
    # test for convergence
    if(abs(x[j] - x[j-1]) <= epsilon | abs(y[j] - y[j-1]) <= epsilon){
      print("true!!!!!!!!!!!!")
      convergance <- TRUE
      
      #convergence has occurred, return x, y, and Total Cost.
      for(l in length(w)){#vectorize this
        TC <- w[l]*sqrt((x[j]-a[l])**2 + (y[j]-b[l])**2)
      }
      
      dfx <- x[j]
      dfy <- y[j]
      converge <- list("x"=dfx, "y"=dfy, "Total Cost"=TC)
      return(converge) #the returned list is edited to comply with provided unit test
    }
  } 

  #reached end of iterations, return total iterations
  
  #calculate total cost
  for(l in length(w)){#vectorize this
    TC <- w[l]*sqrt((x[iterlimit+1]-a[l])**2 + (y[iterlimit+1]-b[l])**2)
  }
  
  Argument_List<-list("x"=x[iterlimit+1], "y"=y[iterlimit+1], "total_cost"=TC)
  return(Argument_List)
}


