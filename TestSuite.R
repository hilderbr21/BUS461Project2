require(RUnit)

testSFRDNV<-function()
# define the test suite
testsuite.SFRDNV<-defineTestSuite("SFRDNV",
                               dirs = getwd(),
                               testFileRegexp = "^runit*.+\\.R",
                               testFuncRegexp = "test.+"
)

# run the test suite and save the results
testResults<-runTestSuite(testsuite.SFRDNV)

# now, obtain a formatted printout of the test results
printTextProtocol(testResults)

