if (!suppressMessages(require(RUnit)))
{
  stop("Terminating... required package RUnit is not installed.")
}

source("B1Policy.R")

#starting values -=DUMMY VALUES=-
D<-0#annual unit demand
A<-0#aggregate order placement cost
v<-0#unit aquisition cost
r<-0#annual holding cost percentage rate
B1<-0#shortage cost per stockout occasion
xl<-0#expected demand
sl<-0#demand standard deviation (review time +) lead time
epsilon<-0#desired level of confidence(i.e. maxiumum difference between iterations)
          #defaul 0.01
iterlimit<-0#maximum number of iterations, default 0.01
kmin<-0#minumum acceptable safety factor, default 0

#return list -=DUMMY VALUES=-
answer<-list(policy = list(s = "order point",
                           k = "safety factor",
                           Q = "order quantity"),
             TRC = "Total Relevent cost",
             converged = "TRUE/FALSE",
             iterations = data.frame(s = (1:2),
                                     Q = (1:2),
                                     k = (1:2),
                                     TRC = (1:2),
                                     Qdiff = (1:2),
                                     kdiff = (1:2)))

test.B1Policyvalid<- function(){
  #check with correct answer
  #default epsilon
  checkEquals(answer,B1Policy(D,A,v,r,B1,xl,sl))
}

test.B1Policyinvalid<- function(){
  
}
