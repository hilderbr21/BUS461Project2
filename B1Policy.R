B1Policy <- function(D,A,v,r) {
  
  #validity check
  if(!is.vector(D, mode="numeric") | !is.vector(A, mode="numeric") | !is.vector(v, mode="numeric") | !is.vector(r, mode="numeric")){
    return(-1)
  }
  if(length(a) != length(b) | length(b) != length(w) | length(w) != length(a)){
    return(-1)
  }
  for(i in 1:length(a)){
    if(a[i] < 0 | b[i] < 0 | w[i] < 0 | epsilon < 0){
      return(-1)
    }
  }
  
  
  
}