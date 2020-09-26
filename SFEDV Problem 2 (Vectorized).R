SFEDNV <- function(a,b,w,epsilon=0.01) {
  if(!numeric(a) | !numeric(b) | !numeric(w)){
    return -1;
  }
  if(a < 0  | b < 0 | w < 0 | epsilon < 0){
    return("Error. Enter a weight greater than 0");
  }
  
}

x<-0
y<-0
xNumerTemp<-0
xDenomTemp<-0
yNumerTemp<-0
yDenomTemp<-0

