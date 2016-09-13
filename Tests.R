library('RUnit')

source('test/FeaturesTest.R')

test.suite <- defineTestSuite("features",
                              dirs = file.path("test/"),
                              testFileRegexp = '^\\w+Test.R$')
test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)