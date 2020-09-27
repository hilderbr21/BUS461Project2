SFRDNV <- function(a,b,w,epsilon=0.01) {
  
  if(!is.vector(a, mode="numeric") | !is.vector(b, mode="numeric") | !is.vector(w, mode="numeric")){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a)){
    return(-1)
  }
  for(i in 1:length(a))
    if(a[i] < 0 | b[i] < 0 | w[i] < 0 | epsilon < 0){
      return( -1)
  }
  
  
  
  
  #sortingMatrix <- matrix(a,b,w, dimnames=c('a','b','w'))
  #print(sortingMatrix)
}
  