#SFRDNV (problem 2 no vector)
#@author Joe Crowley

SFEDNV <- function(a,b,w,epsilon=0.01,iterlim) {
  
  #validity check
  if(!is.vector(a, mode="numeric") | !is.vector(b, mode="numeric") | !is.vector(w, mode="numeric")
     | !is.numeric(interlimit)){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a)){
    return(-1)
  }
  for(i in 1:length(a))
  if(a[i] < 0 | b[i] < 0 | w[i] < 0 | epsilon < 0 | interlimit < 0){
    return( -1)
  }

  # calculate x0 and y0 using calcCentroid 
  x<-0
  y<-0
  xNumerTemp<-0
  xDenomTemp<-0
  yNumerTemp<-0
  yDenomTemp<-0
  i<-2

  # calculate revised coordinates
  for(j in 2:(iterlim + 1)){
    xNumerTemp<- xNumerTemp + (w[j]*a[j])
    xDenomTemp <- xDenomTemp + w[j]
    yNumerTemp <- yNumerTemp + (w[j]*b[j])
    yDenomTemp <- yDenomTemp + w[j]
   x[j] <-xNumerTemp/xDenomTemp
   y[j] <-yNumerTemp/yDenomTemp

    # test for convergence  
    if (abs(x[i] - x[i-1]) <= epsilon && abs(y[i] - y[i-1]) <= epsilon){
    
      #if test passes, calculate Total Cost
      total_cost<-w*sqrt((abs(x-a) + abs(y-b)))
      Argument_List<-list(x, y, total_cost )
      return(Argument_List)
    }
  }
  return(-1)
}

