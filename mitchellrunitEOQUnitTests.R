# Customer class unit tests
# 
require(RUnit)
source("EOQ.R")

D<-10000
A<-500
v<-12.95
r<-0.35

test.validEOQ<-function()
{
   expected<-sqrt(2*D*A/(v*r))
   checkEquals(expected, EOQ(D, A, v, r)$Q)
   
}



