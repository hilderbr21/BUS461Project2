#Unit Test for B1Policy
#
#@author Marc Hilderbrand
#
library(comprehenr)

if (!suppressMessages(require(RUnit)))
{
  stop("Terminating... required package RUnit is not installed.")
}

source("B1Policy.R")

#starting values -=DUMMY VALUES=-
D<-700#annual unit demand
A<-3.20#aggregate order placement cost
v<-12#unit aquisition cost
r<-0.24#annual holding cost percentage rate
B1<-32#shortage cost per stockout occasion
xl<-100#expected demand
sl<-30#demand standard deviation (review time +) lead time
epsilon<-0.001#desired level of confidence(i.e. maxiumum difference between iterations)
          #default 0.01
iterlimit<-0#maximum number of iterations, default 0.01
kmin<-0#minumum acceptable safety factor, default 0




#return list -=DUMMY VALUES=-
#answer
tempk = c(9999,1.389,1.152,1.058,1.016,0.996,0.987,0.983,0.981,0.980,rep(0.979,11))
tempQ = c(39.441,53.278,59.121,61.748,62.967,63.542,63.815,63.945,64.008,64.037,64.052,
          64.058,64.062,64.063,64.064,64.064,64.064,rep(64.065,4))
tempQdif = c(NA,to_vec(for(i in 2:length(tempQ)) signif(sum(tempQ[i] - tempQ[i-1]),3)))
temps = c(NA,to_vec(for(i in 2:length(tempk)) signif((xl+tempk[i]*sl),3)))
tempkdif = c(NA,to_vec(for(i in 2:length(tempk)) signif(sum(tempk[i] - tempk[i-1]),3)))
Q = 64.064
k = 0.979
s = 129.3639
#TRC incomplete
tempTRC = to_vec(for(i in length(tempQ)) (tempQ[i]/2) + (tempk[i]*sl)*(v*r) + ((D*A)/Q))
answer<-list(policy = list("s - order point" = s,
                           "k - safety factor" = k,
                           "Q - order quantity" = Q),
             "TRC - Total Relevent cost" = 0,
             "converged" = TRUE,
             iterations = data.frame(iteration = (1:21),
                                     s = (temps),
                                     Q = (tempQ),
                                     k = (tempk),
                                     TRC = (tempTRC),
                                     Qdiff = (tempQdif),
                                     kdiff = (tempkdif)))
#print(answer)
test.B1Policyvalid<- function(){
  #check with correct answer
  #default epsilon
  checkEquals(answer,B1Policy(D,A,v,r,B1,xl,sl))
}

test.B1Policyinvalid<- function(){
  
}
