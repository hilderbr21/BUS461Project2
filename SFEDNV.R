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
  x <- c(sum(w[1]*a[1])/sum(w[1]))
  y <- c(sum(w[1]*b[1])/sum(w[1]))
  xNumerTemp<-0
  xDenomTemp<-0
  yNumerTemp<-0
  yDenomTemp<-0
  
  # calculate revised coordinates
  for(j in 2:iterlimit){
    xNumerTemp<- xNumerTemp + (w[j]*a[j])
    xDenomTemp <- xDenomTemp + w[j]
    yNumerTemp <- yNumerTemp + (w[j]*b[j])
    yDenomTemp <- yDenomTemp + w[j]
    x[j] <-xNumerTemp/xDenomTemp
    y[j] <- yNumerTemp/yDenomTemp
    iterations <- j-1
    # test for convergence
    print("iterlimit")
    
    if(abs(x[j] - x[j-1]) <= epsilon | abs(y[j] - y[j-1]) <= epsilon){
      print("true!!!!!!!!!!!!")
      convergance <- TRUE
      TC<-w*sqrt((abs(x[j]-a[j]) + abs(y[j]-b[j])))
      #convergence has occurred, return x, y, and Total Cost.
      
      dfx <- x[j]
      dfy <- y[j]
      df <- data.frame("x"=dfx, "y"=dfy, "Total Cost"=TC)
      print(df)
      return(df)
    }
  } 

  #reached end of iterations, return total iterations
  newa <- a[iterlimit]
  newb <- b[iterlimit]
  newx <- x[iterlimit]
  newy <- y[iterlimit]
  TC<-w*sqrt((abs(newx-newa) + abs(newy-newb)))
  print(newx)
  print(newa)
  Argument_List<-data.frame("x"=newx, "y"=newy, "total_cost"=TC)
  return(Argument_List)
  #return(c(x,y,TC))
}


