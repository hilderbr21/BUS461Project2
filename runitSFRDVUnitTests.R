# mmcqueue function unit tests
# 
require(RUnit)

# source functions to test
source("SFRDV.R")


test.SFRDVValidLengths<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFRDV(0,b,w))
  checkEquals(-1, SFRDV(a,0,w))
  rslt<-SFRDV(a,b,w)
  checkEquals(-1, SFRDV(a,b,0))
  
}


test.SFRDVValidNumerics<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFRDV(nonnum, b, w))
  checkEquals(-1, SFRDV(a, nonnum, w))
  checkEquals(-1, SFRDV(a, b, nonnum))
  checkEquals(-1, SFRDV(a, b, w, "0.001"))
  
  
}


test.SFRDVExample1<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYRectilinearMinisum(a, b, w)
  actual<-SFRDV(a, b, w)
  
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

test.SFRDVExample2<-function()
{
  dat<-read.csv("example2_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYRectilinearMinisum(a, b, w)
  actual<-SFRDV(a, b, w)
  
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
    actual<-SFRDV(a, b, w, 1/10^dec)
    
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




