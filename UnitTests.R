#Obsolete unit test file

if (!suppressMessages(require(RUnit)))
{
  stop("Terminating... required package RUnit is not installed.")
}

source("SFRDNV.R")

#starting values
a<-c(2,6,9,12,3,6,1,6,10,10,2,1,4,4)
b<-c(3,1,9,5,9,2,6,4,3,8,8,4,5,2)
w<-c(20,10,45,15,25,5,15,10,5,40,5,5,5,5)
TC<-1175

test.SFRDNVvalid<- function(){
  
  #check with correct answer
  #default epsilon
  checkEquals(SFRDNV(a,b,w),c(6,8,1175))
  #modified epsilon
  checkEquals(c(6,8,1175),SFRDNV(a,b,w,epsilon=0.001))
}

test.SFRDNVinvalid<- function(){
  
  #check for invalid arguments
  #1. invalid datatype
  checkEquals(1, SFRDNV("Gary Mitchell is the best professor!",b,w))
  print("test 1 complete")
  #2. inconsistent vector sizes
  checkEquals(-1, SFRDNV(c(1,2,3,4,5,6),c(1,2,4,5),c(1,2,3,4,5)))
  print("test 2 complete")
  #3. negative number in one of the vectors
  checkEquals(-1, SFRDNV(c(1,2,3,-4),c(1,2,3,4),c(1,2,3,4)))
  #4. negative scalar precision
  checkEquals(-1, SFRDNV(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),-0.5))
  
}

TestSFEDNV<- function(){
  #manually name new coordinates x(new_coord), y(new_coord)
  #calculate total_cost
  #manually test converged 
  #create a data.frame called data.frame.SFEDNV containing: 
  #"x(new_coord)", "y(new_coord)", and "total_cost"
  a<-c(2,6,9,12,3,6,1,6,10,10,2,1,4,4)
  b<-c(3,1,9,5,9,2,6,4,3,8,8,4,5,2)
  w<-c(20,10,45,15,25,5,15,10,5,40,5,5,5,5)
  TC<-1175
  
  #check with correct answer
  #default epsilon
  checkEquals(c(6,8,1175),SFRDNV(a,b,w))
  #modified epsilon
  checkEquals(c(6,8,1175),SFRDNV(a,b,w,epsilon=0.001))
  
  #check for invalid arguments
  #1. invalid datatype
  checkEquals(-1, SFEDNV(x(new_coord), y(new_coord), total_cost))
  #2. inconsistent vector sizes
  checkEquals(-1, SFRDNV(c(1,2,3,4,5,6),c(1,2,4,5),c(1,2,3,4,5)))
  #3. negative number in one of the vectors
  checkEquals(-1, SFRDNV(c(1,2,3,-4),c(1,2,3,4),c(1,2,3,4)))
  #4. negative scalar precision
  checkEquals(-1, SFRDNV(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),-0.5)) 
}

TestSFRDV<- function(){
  
}

TestSFEDV<- function(){
  
}

TestB1Policy<- function(){
  
}