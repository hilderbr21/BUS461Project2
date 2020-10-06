# Customer class unit tests
# 
require(RUnit)
source("EOQ.R")

D<-10000
A<-500
v<-12.95
r<-0.35

test.AllValidArgs<-function()
{
  # Verifies that function indicates all good when all arguments are valid
  # i.e. there is no error
  rslt<-EOQValidateArguments(D, A, v, r)
  checkEquals(FALSE, rslt$error)
}

test.validD<-function()
{
  # Valid D with
  # invalid A, valid v and r
  # valid A, invalid v, valid r
  # valid A, valid v, invalid r
  # invalid A, valid v, invalid r
  # invalid A, invalid v, valid r
  # invalid A, invalid v, invalid r
  
  rslt<-EOQValidateArguments(D, -A, v, r)
  checkEquals(TRUE, rslt$error)
  checkEquals("A", substr(rslt$errmsg, nchar(rslt$errmsg), nchar(rslt$errmsg)))
  
  checkEquals(TRUE, EOQValidateArguments(D, A, -v, r)$error)
  checkEquals(TRUE, EOQValidateArguments(D, A, v, -r)$error)
  checkEquals(TRUE, EOQValidateArguments(D, -A, -v, r)$error)
  
  rslt<-EOQValidateArguments(D, -A, v, -r)
  checkEquals("A, r", substr(rslt$errmsg, 32, 35))
  
  checkEquals(TRUE, EOQValidateArguments(D, -A, v, -r)$error)
  checkEquals(TRUE, EOQValidateArguments(D, -A, -v, -r)$error)
  
  
}




