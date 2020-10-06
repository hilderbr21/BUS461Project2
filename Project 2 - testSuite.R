# Project 2 test suite
# 
# Mitchell's unit test suite

rm(list=ls())

# set current directory as source directory
script.dir<-getSrcDirectory(function(){})
setwd(script.dir)

# capture test suite name from filename
testsuitename<-getSrcFilename(function(){})
testsuitename<-substr(testsuitename, 1, nchar(testsuitename)-2)

seedVal<-100

load("VERIFYEuclideanMinisum.RData")
load("VERIFYRectilinearMinisum.RData")


unitTests<-dir(".", "^runit*")
for (f in unitTests)
{
  source(f)
}

# function to consolidate test results and simplify feedback
# 
streamlineResults<-function(tests, printResults=TRUE)
{
   if (printResults)
   {
      printTextProtocol(tests)
   }
   
   temp<-names(tests[[1]][[8]])
   classname<-substr(temp, nchar(" ./runit"), nchar(temp)-nchar("UnitTests.R"))
   
   results<-tests[[1]][[8]][[1]]
   # print(results)
   
   testnames<-names(results)
   # print(testnames)
   
   out<-sapply(testnames, function(x){
      testr<-results[[x]]
      if(testr$kind=="success")
      {
        return(c(testr$checkNum, testr$kind, testr$time))
      }else
      {
        return(c(0, testr$kind, Inf))
      }
      
      # return(ifelse(testr$kind=="success",
      #               c(testr$checkNum, testr$kind, testr$time),
      #               c(NA, testr$kind, NA)))
      
   })
   
   formout<-as.data.frame(t(out))
   
   formout<-cbind(rep(classname, dim(formout)[1]), 
                  rownames(formout),
                  formout)
   colnames(formout)<-c("Class/Function", "Test.Function", "Checks","Test.Result","Elapsed.Time")
   rownames(formout)<-1:dim(formout)[1]
   
   return(formout)
   
}

capturename<-paste(testsuitename, " - Test Results.txt")

# capture console output to text file
sink(file=capturename)


testResults<-list()
testResults[[length(testResults)+1]]<-runTestFile("runitSFRDVUnitTests.R", rngKind = "default", rngNormalKind = "default")
testResults[[length(testResults)+1]]<-runTestFile("runitSFRDNVUnitTests.R", rngKind = "default", rngNormalKind = "default")
testResults[[length(testResults)+1]]<-runTestFile("runitSFEDVUnitTests.R", rngKind = "default", rngNormalKind = "default")
testResults[[length(testResults)+1]]<-runTestFile("runitSFEDNVUnitTests.R", rngKind = "default", rngNormalKind = "default")
testResults[[length(testResults)+1]]<-runTestFile("runitB1PolicyUnitTests.R", rngKind = "default", rngNormalKind = "default")


# print test results
# 
test<-lapply(testResults, function(x){streamlineResults(x)})

for (i in 1:length(test))
{
   if (i == 1)
   {
      byclass<-test[[1]]
   } else
   {
      byclass<-rbind(byclass, test[[i]])
   }
}

cat("\n\nTest Results by class:\n\n")

print(byclass)

# turn off console capture
sink()


filename<-paste(testsuitename, " - test results by class.csv", sep="")
write.csv(byclass, file=filename)

rm(list=ls(pattern="VERIFY*"))

# model for standard approach
# printTextProtocol(testDistResults)
# printTextProtocol(testCustResults)
# printTextProtocol(testSourcePopResults)


# the following is an alternative way to define the tests to be executed
# instead of creating the test script file. I prefer the above approach
# because I can segregate tests into multiple files and control
# which sets of tests I execute by executing only for a particular file
#
# testsuite.MMcQueue<-defineTestSuite("MMcQueue",
#                                dirs = getwd(),
#                                testFileRegexp = "^runit*.+\\.R",
#                                testFuncRegexp = "test.+"
# )
# 
# run the test suite and save the results
#testResults<-runTestSuite(testsuite.MMcQueue)
#

