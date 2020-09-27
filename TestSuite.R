#Unit test suit
#@author Marc Hilderbrand

require(RUnit)

#TEST SUITE FOR SFRDNV
# define the test suite
testsuite.SFRDNV<-defineTestSuite("SFRDNV",
                               dirs = getwd(),
                               testFileRegexp = "^runit*.+\\.R",
                               testFuncRegexp = "test.+"
)

# run the test suite and save the results
SFRDNVtestResults<-runTestSuite(testsuite.SFRDNV)

# now, obtain a formatted printout of the test results
printTextProtocol(SFRDNVtestResults)



#TEST SUITE FOR SFRDV
# define the test suite
testsuite.SFRDV<-defineTestSuite("SFRDV",
                                  dirs = getwd(),
                                  testFileRegexp = "^runit*.+\\.R",
                                  testFuncRegexp = "test.+"
)

# run the test suite and save the results
SFRDVtestResults<-runTestSuite(testsuite.SFRDV)

# now, obtain a formatted printout of the test results
printTextProtocol(SFRDVtestResults)



#TEST SUITE FOR SFEDNV
# define the test suite
testsuite.SFEDNV<-defineTestSuite("SFEDNV",
                                 dirs = getwd(),
                                 testFileRegexp = "^runit*.+\\.R",
                                 testFuncRegexp = "test.+"
)

# run the test suite and save the results
SFRDVtestResults<-runTestSuite(testsuite.SFEDNV)

# now, obtain a formatted printout of the test results
printTextProtocol(SFRDVtestResults)

