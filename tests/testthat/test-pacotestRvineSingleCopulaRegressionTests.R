context("Regression tests for pacotestRvineSingleCopula")

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula1", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'pairwiseMax', 'splitTrainEvaluate', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula2", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeECORR', 'all', 'splitTrainEvaluate', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})


context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula3", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'pairwiseMax', 'splitTrainEvaluate', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula4", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert')
  values = c('ECORR', 'TreeEC', 'all', 'splitTrainEvaluate', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = penalty, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula5", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeECORR', 'pairwiseMax', 'penalty', 'TRUE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = penalty, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula6", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeECORR', 'all', 'penalty', 'TRUE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})


context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = penalty, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula7", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeEC', 'pairwiseMax', 'penalty', 'TRUE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = penalty, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula8", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeEC', 'all', 'penalty', 'TRUE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = SumMedian, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula9", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumMedian', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = ProdThirdsII, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula10", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'ProdThirdsII', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = SumThirdsIII, withEstUncert = TRUE")
test_that("pacotestRvineSingleCopula11", {
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumThirdsIII', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = penalty, withEstUncert = TRUE, estUncertWithRanks = TRUE")
test_that("pacotestRvineSingleCopula5", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeECORR', 'pairwiseMax', 'penalty', 'TRUE', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = penalty, withEstUncert = TRUE, estUncertWithRanks = TRUE")
test_that("pacotestRvineSingleCopula6", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeECORR', 'all', 'penalty', 'TRUE', 'TRUE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
testType=VI")
test_that("pacotestRvineSingleCopula12", {
  
  properties = c('testType')
  values = c('VI')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=EC")
test_that("pacotestRvineSingleCopula13", {
  
  properties = c('testType')
  values = c('EC')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5[1:341,], rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
testType=ECOV")
test_that("pacotestRvineSingleCopula14", {
  
  properties = c('testType')
  values = c('ECOV')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 1)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
    
    expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
    
  }
  
})



