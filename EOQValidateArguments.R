# EOQValidateArguments
# Validates the EOQ standard arguments, D, A, v, and r
# returns:
# 0 if all arguments valid
# -1 if denominator (v * r) is <= 0
# -2 if numerator (2DA) is <= 0

EOQValidateArguments<-function(D, A, v, r)
{
  # construct empty return argument
  rslt<-list(error=FALSE)
  
  # construct argument vector for testing
  args<-c(D, A, v, r)
  names(args)<-c("D", "A", "v", "r")
  
  if (any(args <= 0))
  {
    rslt['errmsg']<-paste("Argument(s) must be positive: ", paste(names(args)[args < 0], collapse=", "))
    rslt['error']<-TRUE
  }
  
  return(rslt)
}


