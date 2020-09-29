#Unit test suite
#
#Source to run all unit test in console. Include all files in same directory.
#
#This test suite runs all files starting with "runit" as unit tests.
#
#@author Marc Hilderbrand


require(RUnit)

#TEST SUITE FOR SFRDNV
# define the test suite
testsuite.AllTests<-defineTestSuite("AllTests",
                               dirs = getwd(),
                               testFileRegexp = "^runit*.+\\.R",
                               testFuncRegexp = "test.+"
)

# run the test suite and save the results
AlltestResults<-runTestSuite(testsuite.AllTests)

# now, obtain a formatted printout of the test results
printTextProtocol(AlltestResults)
