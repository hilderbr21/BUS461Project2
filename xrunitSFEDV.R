#Unit Test for SFEDV (Problem 2 no vector)
#@Author Joe Crowley

if (!suppressMessages(require(RUnit)))
{
  stop("Terminating... required package RUnit is not installed.")
}

source("SFEDV.R")

#starting values
a<-c(2,6,9,12,3,6,1,6,10,10,2,1,4,4)
b<-c(3,1,9,5,9,2,6,4,3,8,8,4,5,2)
w<-c(20,10,45,15,25,5,15,10,5,40,5,5,5,5)
iterlimit<-14

x<-7.7116
y<-7.2385
TC<-2011.2
convergance<-TRUE
iterations<-14 


test.SFEDVvalid<- function(){
  #check with correct answer
  #default epsilon
  checkEquals(list(x,y,TC,convergance,iterations),SFEDV(a,b,w,iterlimit=iterlimit))
  #modified epsilon
  checkEquals(list(x,y,TC,FALSE,14),SFEDV(a,b,w,epsilon=0.001,iterlimit=iterlimit))
}

test.SFEDVinvalid<- function(){
  #check for invalid arguments
  #1. invalid datatype
  checkEquals(-1, SFEDV("This is a test.",b,w,epsilon=0.01,iterlimit))
  #2. inconsistent vector sizes
  checkEquals(-1, SFEDV(c(1,2,3,4,5,6),c(1,2,4,5),c(1,2,3,4,5),iterlimit=iterlimit))
  #3. negative number in one of the vectors
  checkEquals(-1, SFEDV(c(1,2,3,-4),c(1,2,3,4),c(1,2,3,4),iterlimit=iterlimit))
  #4. negative scalar precision
  checkEquals(-1, SFEDV(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),-0.5,iterlimit=iterlimit))
  #5. negative iterlimit
  checkEquals(-1, SFEDV(c(iterlimit=-4)))
}
