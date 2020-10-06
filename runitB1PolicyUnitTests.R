# B1Policy unit tests
# 

require(RUnit)
source("B1Policy.R")

D<-700
A<-3.20
v<-12.00
r<-0.24
B1<-32
xl<-100
sl<-30
epsilon<-0.001
iterlimit<-100
kmin<-0

multi<-1:5
nonnum<-"a"


test.B1PolicyValidLengths<-function()
{
   checkEquals(-1, B1Policy(multi, A, v, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, multi, v, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, multi, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, multi, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, multi, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, multi, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, multi, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, multi, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, epsilon, multi, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, epsilon, iterlimit, multi))
   
   checkTrue(is.list(B1Policy(D, A, v, r, B1, xl, sl, epsilon, iterlimit, kmin)))
   
}



test.B1PolicyValidNumerics<-function()
{
   checkEquals(-1, B1Policy(nonnum, A, v, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, nonnum, v, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, nonnum, r, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, nonnum, B1, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, nonnum, xl, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, nonnum, sl, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, nonnum, epsilon, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, nonnum, iterlimit, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, epsilon, nonnum, kmin))
   checkEquals(-1, B1Policy(D, A, v, r, B1, xl, sl, epsilon, iterlimit, nonnum))
   
   checkTrue(is.list(B1Policy(D, A, v, r, B1, xl, sl, epsilon, iterlimit, kmin)))
   
   
}



test.B1PolicyExample1<-function()
{
   rslt<-B1Policy(D,A,v,r,B1,xl, sl)
   print(rslt)

   # verify (s,Q)
   checkEquals(round(rslt$policy$s,4), 129.3639, tolerance=0.0001)
   checkEquals(round(rslt$policy$Q,4), 64.0639, tolerance=0.0001)


}


test.B1PolicyRandomExamples<-function()
{
   load("B1PolicyExpectedResults")
   
   for (i in 1:length(expected))
   {
      rslt<-B1Policy(expected[[i]]$D,
                     expected[[i]]$A,
                     expected[[i]]$v,
                     expected[[i]]$r,
                     expected[[i]]$B1,
                     expected[[i]]$xl, 
                     expected[[i]]$sl,
                     expected[[i]]$epsilon,
                     expected[[i]]$iterlimit,
                     expected[[i]]$kmin)
      
      # verify (s,Q)
      checkEquals(expected[[i]]$rslt$policy$s, rslt$policy$s, tolerance = 0.0001)
      checkEquals(expected[[i]]$rslt$policy$Q, rslt$policy$Q, tolerance = 0.0001)
      checkEquals(expected[[i]]$rslt$TRC, rslt$TRC, tolerance = 0.0001)
      checkEquals(expected[[i]]$rslt$converged, rslt$converged)
      
      # now verify the iterations
   
      for (metric in c("s", "k", "Q", "TRC", "kdiff", "Qdiff"))
      {
         checkEquals(expected[[i]]$rslt$Iterations[,metric], rslt$Iterations[[,metric]])
      }
   }
}

#test.B1PolicyRandomExamples()

