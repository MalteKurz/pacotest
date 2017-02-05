context("Regression tests for pacotest")

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR1", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'pairwiseMax', 'splitTrainEvaluate', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})
  
context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR2", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'all', 'splitTrainEvaluate', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})
  

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR3", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'pairwiseMax', 'splitTrainEvaluate', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR4", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'all', 'splitTrainEvaluate', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = penalty")
test_that("ECORR5", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'pairwiseMax', 'penalty', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = penalty")
test_that("ECORR6", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'all', 'penalty', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})


context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = penalty")
test_that("ECORR7", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'pairwiseMax', 'penalty', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = penalty")
test_that("ECORR8", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'all', 'penalty', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = SumMedian")
test_that("ECORR9", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumMedian', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = ProdThirdsII")
test_that("ECORR9", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'ProdThirdsII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = SumThirdsIII")
test_that("ECORR9", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumThirdsIII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

