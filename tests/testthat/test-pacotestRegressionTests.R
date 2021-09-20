context("Regression tests for pacotest")

context("unit tests for pacotest with options:
testType=CCC, grouping = TreeCCC, finalComparison = pairwiseMax")
test_that("CCC5", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'withEstUncert')
  values = c('CCC', 'TreeCCC', 'pairwiseMax', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=CCC, grouping = TreeCCC, finalComparison = all")
test_that("CCC6", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'withEstUncert')
  values = c('CCC', 'TreeCCC', 'all', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})


context("unit tests for pacotest with options:
testType=CCC, grouping = TreeEC, finalComparison = pairwiseMax")
test_that("CCC7", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'withEstUncert')
  values = c('CCC', 'TreeEC', 'pairwiseMax', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=CCC, grouping = TreeEC, finalComparison = all")
test_that("CCC8", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'withEstUncert')
  values = c('CCC', 'TreeEC', 'all', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=CCC, grouping = SumMedian")
test_that("CCC9", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('CCC', 'SumMedian', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=CCC, grouping = ProdThirdsII")
test_that("CCC10", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('CCC', 'ProdThirdsII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=CCC, grouping = SumThirdsIII")
test_that("CCC11", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('CCC', 'SumThirdsIII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

