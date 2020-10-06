# mmcqueue function unit tests
# 
require(RUnit)

# source functions to test
source("SFRDNV.R")


test.SFRDNVValidLengths<-function()
{
  dat<-read.csv("example1_esf.csv")
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFRDNV(0,b,w))
  checkEquals(-1, SFRDNV(a,0,w))
  rslt<-SFRDNV(a,b,w)
  checkEquals(-1, SFRDNV(a,b,0))
  
}


test.SFRDNVValidNumerics<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFRDNV(nonnum, b, w))
  checkEquals(-1, SFRDNV(a, nonnum, w))
  checkEquals(-1, SFRDNV(a, b, nonnum))
  checkEquals(-1, SFRDNV(a, b, w, "0.001"))
  
  
}


test.SFRDNVExample1<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYRectilinearMinisum(a, b, w)
  actual<-SFRDNV(a, b, w)
  
  checkEquals(expected$x, actual$x)
  checkEquals(expected$y, actual$y)
  
  checkEquals(expected$total_cost, actual$total_cost)
  
  expvec<-unlist(expected)
  names(expvec)<-NULL
  actvec<-unlist(actual)
  names(actvec)<-NULL
  
  compare<-data.frame(expvec, actvec)
  colnames(compare)<-c("Expected", "Actual")
  rownames(compare)<-c("x", "y", "Total Cost")
  
  cat("\n\n")
  print(compare)
  cat("\n\n")
  
}

test.SFRDNVExample2<-function()
{
  dat<-read.csv("example2_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYRectilinearMinisum(a, b, w)
  actual<-SFRDNV(a, b, w)
  
  checkEquals(expected$x, actual$x)
  checkEquals(expected$y, actual$y)
  
  checkEquals(expected$total_cost, actual$total_cost)
  
  expvec<-unlist(expected)
  names(expvec)<-NULL
  actvec<-unlist(actual)
  names(actvec)<-NULL
  
  compare<-data.frame(expvec, actvec)
  colnames(compare)<-c("Expected", "Actual")
  rownames(compare)<-c("x", "y", "Total Cost")
  
  cat("\n\n")
  print(compare)
  cat("\n\n")
  
  # now test with other epsilons
  for (dec in 1:4)
  {
    expected<-VERIFYRectilinearMinisum(a, b, w, 1/10^dec)
    actual<-SFRDNV(a, b, w, 1/10^dec)
    
    checkEquals(expected$x, actual$x)
    checkEquals(expected$y, actual$y)
    
    checkEquals(expected$total_cost, actual$total_cost)
    
    expvec<-unlist(expected)
    names(expvec)<-NULL
    actvec<-unlist(actual)
    names(actvec)<-NULL
    
    compare<-data.frame(expvec, actvec)
    colnames(compare)<-c("Expected", "Actual")
    rownames(compare)<-c("x", "y", "Total Cost")
    
    cat("\n\n")
    print(compare)
    cat("\n\n")
    
  }
}




