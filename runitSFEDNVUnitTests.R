# mmcqueue function unit tests
# 
require(RUnit)

# source functions to test
source("SFEDNV.R")


test.SFEDNVValidLengths<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFEDNV(0,b,w))
  checkEquals(-1, SFEDNV(a,0,w))
  checkEquals(-1, SFEDNV(a,b,0))
  
  checkEquals(-1, SFEDNV(a,b,w,0.001,seq(0.001,0.010,0.001)))
  
}


test.SFEDNVValidNumerics<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  n<-length(a)
  nonnum<-rep("a",n)
  
  checkEquals(-1, SFEDNV(nonnum, b, w))
  checkEquals(-1, SFEDNV(a, nonnum, w))
  checkEquals(-1, SFEDNV(a, b, nonnum))
  checkEquals(-1, SFEDNV(a, b, w, "0.001"))
  
  
}


test.SFEDNVExample1<-function()
{
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYEuclideanMinisum(a, b, w)
  actual<-SFEDNV(a, b, w)
  
  checkEquals(expected$x, actual$x)
  checkEquals(expected$y, actual$y)
  
  checkEquals(expected$total_cost, actual$total_cost)
  
  checkTrue(actual$converged)
  
  expvec<-c(expected$x, expected$y, expected$total_cost)
  actvec<-c(expected$x, expected$y, expected$total_cost)
  
  compare<-data.frame(expvec, actvec)
  colnames(compare)<-c("Expected", "Actual")
  rownames(compare)<-c("x", "y", "Total Cost")
  
  cat("\n\n")
  print(compare)
  cat("\n\n")
  
}

test.SFEDNVExample2<-function()
{
  dat<-read.csv("example2_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  # test with default epsilon
  
  expected<-VERIFYEuclideanMinisum(a, b, w)
  actual<-SFEDNV(a, b, w)
  
  checkEquals(expected$x, actual$x)
  checkEquals(expected$y, actual$y)
  
  checkEquals(expected$total_cost, actual$total_cost)
  
  expvec<-c(expected$x, expected$y, expected$total_cost)
  actvec<-c(expected$x, expected$y, expected$total_cost)
  
  compare<-data.frame(expvec, actvec)
  colnames(compare)<-c("Expected", "Actual")
  rownames(compare)<-c("x", "y", "Total Cost")
  
  cat("\n\n")
  print(compare)
  cat("\n\n")
  
  # now test with other epsilons
  for (dec in 1:4)
  {
    expected<-VERIFYEuclideanMinisum(a, b, w, 1/10^dec)
    actual<-SFEDNV(a, b, w, 1/10^dec)
    
    checkEquals(expected$x, actual$x)
    checkEquals(expected$y, actual$y)
    
    checkEquals(expected$total_cost, actual$total_cost)
    
    checkTrue(actual$converged)
    
    expvec<-c(expected$x, expected$y, expected$total_cost)
    actvec<-c(expected$x, expected$y, expected$total_cost)
    
    compare<-data.frame(expvec, actvec)
    colnames(compare)<-c("Expected", "Actual")
    rownames(compare)<-c("x", "y", "Total Cost")
    
    cat("\n\n")
    print(compare)
    cat("\n\n")
    
    # finally, test the Iterations data frame
    for (metric in c("x","y","total_cost"))
    {
      checkEquals(expected$Iterations[,metric], actual$Iterations[,metric])
    }
    
    
  }
}



test.SFEDNVExample3<-function()
{
  # termination before convergence
  
  dat<-read.csv("example1_esf.csv")
  
  a<-dat$a
  b<-dat$b
  w<-dat$w
  
  dec<-3
  
  
  expected<-VERIFYEuclideanMinisum(a, b, w, iterlimit=8)
  actual<-SFEDNV(a, b, w, iterlimit=8)
  
  checkEquals(expected$x, actual$x)
  checkEquals(expected$y, actual$y)
  
  checkEquals(expected$total_cost, actual$total_cost)
  
  checkTrue(!actual$converged)
  
  expvec<-c(expected$x, expected$y, expected$total_cost)
  actvec<-c(expected$x, expected$y, expected$total_cost)
  
  # finally, test the Iterations data frame
  for (metric in c("x","y","total_cost"))
  {
    checkEquals(expected$Iterations[,metric], actual$Iterations[,metric])
  }
  
  
  compare<-data.frame(expvec, actvec)
  colnames(compare)<-c("Expected", "Actual")
  rownames(compare)<-c("x", "y", "Total Cost")
  
  cat("\n\n")
  print(compare)
  cat("\n\n")
  
}

