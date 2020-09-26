SFRDNV <- function(a,b,w,epsilon=0.01) {
  if(!is.numeric(a) | !is.numeric(b) | !is.numeric(w)){
    return -1;
  }
  if(a < 0  | b < 0 | w < 0 | epsilon < 0){
    return -1;
  }
  
  
  
  sortingMatrix <- matrix(a,b,w, dimnames=c('a','b','w'))
  print(sortingMatrix)
}
  