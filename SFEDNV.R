SFEDNV <- function(a,b,w,epsilon=0.01) {
  if(!numeric(a) | !numeric(b) | !numeric(w)){ #Run a test to make sure values >=0
    return -1;
  }
  if(a < 0  | b < 0 | w < 0 | epsilon < 0){
    return(-1);
  }
  
}

# calculate x0 and y0 using calcCentroid 
x<-0
y<-0
xNumerTemp<-0
xDenomTemp<-0
yNumerTemp<-0
yDenomTemp<-0
i<-1

# calculate revised coordinates
calcCentroid<-for(j in 2: i + 1){
  xNumerTemp<- xNumerTemp + (w[j]*a[j])
  xDenomTemp <- xDenomTemp + w[j]
  yNumerTemp <- yNumerTemp + (w[j]*b[j])
  yDenomTemp <- yDenomTemp + w[j]
  
  x[j] <-xNumerTemp/xDenomTemp
  y[j] <-yNumerTemp/yDenomTemp
}

# test for convergence  
  if (abs(x[i] - x[i-1]) <= epsilion && abs(y[i] - y[i-1]) <= epsilion){
    
#if test passes, calculate Total Cost
    total_cost<-w*sqrt((abs(x-a) + abs(y-b)))
    Argument_List<-list(x, y, total_cost )
    return(Argument_List)
 } 

