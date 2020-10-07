#SFEDNV (problem 2 no vector)
#@author Joe Crowley

#PLEASE REMOVE THE @Joe COMMENTS AFTER YOU HAVE MADE THE PROPOSED CHANGES

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
 
   
  # calculate x0 and y0 using calcCentroid 
  x <- c(sum(w[1]*a[1])/sum(w[1]))
  y <- c(sum(w[1]*b[1])/sum(w[1]))
  xNumerTemp<-0
  xDenomTemp<-0
  yNumerTemp<-0
  yDenomTemp<-0

  print(x)
  # calculate revised coordinates
  for(j in 2:(length(iterlimit) + 1)){
    xNumerTemp<- xNumerTemp + (w[j]*a[j])
    xDenomTemp <- xDenomTemp + w[j]
    yNumerTemp <- yNumerTemp + (w[j]*b[j])
    yDenomTemp <- yDenomTemp + w[j]
    x[j] <-xNumerTemp/xDenomTemp
    y[j] <- yNumerTemp/yDenomTemp
    iterations <- j-1
    print("iteration ")
    print(x)
    # test for convergence
    if(abs(x[j] - x[j-1]) <= epsilon){
      #if both if statements are true, then convergence has occurred
      if(abs(y[j] - y[j-1]) <= epsilon){
        convergance <- TRUE
        TC<-w*sqrt((abs(x[j]-a) + abs(y[j]-b)))
        #convergence has occurred, return x, y, and Total Cost.
        return(data.frame("x"=x[j], "y"=y[j], "Total Cost"=TC))
      }
    }
  } 

  #reached end of iterations, return total iterations
  TC<-w*sqrt((abs(x[iterlimit]-a) + abs(y[iterlimit]-b)))
  Argument_List<-data.frame("x"=x[iterlimit], "y"=y[iterlimit], "Total Cost"=TC)
  return(Argument_List)
  #return(c(x,y,TC))
}


