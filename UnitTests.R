#Unit Test

require(RUnit)

TestSFRDNV<- function(){
  
  #manually calculate correct answer for x, y, total cost for SFRDNV
  
  
  a<-c(2,6,9,12,3,6,1,6,10,10,2,1,4,4)
  b<-c(3,1,9,5,9,2,6,4,3,8,8,4,5,2)
  w<-c(20,10,45,15,25,5,15,10,5,40,5,5,5,5)
  
  #checkEquals()
  
  #check for invalid arguments
  #1. invalid datatype
  checkEquals(-1, SFRDNV(a,b,w))
  #2. inconsistent vector sizes
  checkEquals(-1, SFRDNV(c(1,2,3,4,5,6),c(1,2,4,5),c(1,2,3,4,5)))
  #3. negative number in one of the vectors
  checkEquals(-1, SFRDNV(c(1,2,3,-4),c(1,2,3,4),c(1,2,3,4)))
  #4. negative scalar precision
  checkEquals(-1, SFRDNV(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),-0.5))
  
}

TestSFEDNV<- function(){
  
}

TestSFRDV<- function(){
  
}

TestSFEDV<- function(){
  
}

TestB1Policy<- function(){
  
}