# testsuiteMMcQueue.R
# 
require(RUnit)
source("EOQ.R")

# repeat the following two lines for each set of unit test specifications
testEOQValidateArgumentsResults<-runTestFile("mitchellrunitEOQValidateArgumentUnitTests.R")
testEOQResults<-runTestFile("mitchellrunitEOQUnitTests.R")


# print test results
printTextProtocol(testEOQValidateArgumentsResults)
printTextProtocol(testEOQResults)


