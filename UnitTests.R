#Unit Test

require(RUnit)

TestSFRDNV<- function(){
  
}

TestSFEDNV<- function(){
  #manually name new coordinates x(new_coord), y(new_coord)
  #calculate total_cost
  #manually test converged 
  #create a data.frame called data.frame.SFEDNV containing: 
  #"x(new_coord)", "y(new_coord)", and "total_cost"
  x(new_coord)<-0
  y(new_coord)<-0
  total_cost<-0
  
  #check for invalid arguments
  #1. invalid datatype
  checkEquals(-1, SFEDNV(x(new_coord), y(new_coord), total_cost))
  #2. 
}

TestSFRDV<- function(){
  
}

TestSFEDV<- function(){
  
}

TestB1Policy<- function(){
  
}