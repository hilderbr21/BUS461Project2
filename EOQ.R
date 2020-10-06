source("EOQValidateArguments.R")
# EOQ(D, A, v, r)
# Calcualtes the economic order quantity
# D - annual demand
# A - aggregate order cost
# v - unit acquisition cost
# r - unit holding cost percentage rate

# returns:
# Q* if all arguments valid
# -1 if v or r non-positive
# -2 if D or A non-positive

EOQ<-function(D, A, v, r)
{
  rslt<-EOQValidateArguments(D, A, v, r)
  
  if (rslt$error)
  {
    # an argument was invalid and rslt contains an error code
    return(rslt)
  }
  
  Qstar<-sqrt(2 * D * A / (v * r))
  TRC<-Qstar/2 * v * r + D * A/Qstar
  
  # construct the return result
  rslt<-list(error=FALSE, Q=Qstar, TRC=TRC, ATC=TRC + D*v)
  
  return(rslt)
  
}