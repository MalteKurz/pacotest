context("Regression tests for pacotestRvineSeq")

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = penalty, withEstUncert = TRUE")
test_that("pacotestRvineSeq1", {
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeECORR', 'all', 'penalty', 'TRUE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    xx = pacotestRvineSeq(data5, rvmHatData5, pacotestOptions[[iTest]], 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})


context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = penalty, withEstUncert = FALSE")
test_that("pacotestRvineSeq2", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeEC', 'pairwiseMax', 'penalty', 'FALSE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    
    xxPacotestOptions = pacotestset(pacotestOptions[[iTest]], groupedScatterplots = TRUE, decisionTreePlot = TRUE)
    
    xx = pacotestRvineSeq(data5, rvmHatData5, xxPacotestOptions, 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = penalty, withEstUncert = FALSE")
test_that("pacotestRvineSeq3", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'finalComparison', 'sizeKeepingMethod', 'withEstUncert', 'estUncertWithRanks')
  values = c('ECORR', 'TreeEC', 'all', 'penalty', 'FALSE', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    
    xxPacotestOptions = pacotestset(pacotestOptions[[iTest]], groupedScatterplots = TRUE, decisionTreePlot = TRUE)
    
    xx = pacotestRvineSeq(data5, rvmHatData5, xxPacotestOptions, 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = SumMedian, withEstUncert = FALSE")
test_that("pacotestRvineSeq4", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumMedian', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    
    xxPacotestOptions = pacotestset(pacotestOptions[[iTest]], groupedScatterplots = TRUE, decisionTreePlot = TRUE)
    
    xx = pacotestRvineSeq(data5, rvmHatData5, xxPacotestOptions, 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = ProdThirdsII, withEstUncert = FALSE")
test_that("pacotestRvineSeq5", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'ProdThirdsII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    
    xxPacotestOptions = pacotestset(pacotestOptions[[iTest]], groupedScatterplots = TRUE, decisionTreePlot = TRUE)
    
    xx = pacotestRvineSeq(data5, rvmHatData5, xxPacotestOptions, 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=ECORR, grouping = SumThirdsIII, withEstUncert = FALSE")
test_that("pacotestRvineSeq6", {
  testthat::skip_on_cran()
  
  properties = c('testType', 'grouping', 'withEstUncert')
  values = c('ECORR', 'SumThirdsIII', 'FALSE')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    
    xxPacotestOptions = pacotestset(pacotestOptions[[iTest]], groupedScatterplots = TRUE, decisionTreePlot = TRUE)
    
    xx = pacotestRvineSeq(data5, rvmHatData5, xxPacotestOptions, 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

context("unit tests for pacotest with options:
        testType=VI")
test_that("pacotestRvineSeq7", {
  
  properties = c('testType')
  values = c('VI')
  
  thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
  
  
  maxNTests = length(pacotestOptions)
  
  resPacotestComputed = matrix(NA, maxNTests, 3)
  
  for (iTest in thisTestsInd)
  {
    set.seed(seedsPerTest[iTest])
    xx = pacotestRvineSeq(data5, rvmHatData5, pacotestOptions[[iTest]], 0.05)$pValues
    resPacotestComputed[iTest,1:3] = c(xx[3,1:2], xx[2,1])
    
    expect_equal(resPacotestComputed[iTest,1:3], hardCodedResPacotestRvineSeq[iTest,1:3], tolerance = 1e-2)
    
  }
  
})

