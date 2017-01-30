context("Regression tests for pacotest")

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR1", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeECORR'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'pairwiseMax'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'splitTrainEvaluate'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})
  
context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR2", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeECORR'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'all'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'splitTrainEvaluate'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})
  

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR3", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeEC'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'pairwiseMax'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'splitTrainEvaluate'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = splitTrainEvaluate")
test_that("ECORR4", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeEC'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'all'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'splitTrainEvaluate'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = pairwiseMax, sizeKeepingMethod = penalty")
test_that("ECORR5", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeECORR'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'pairwiseMax'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'penalty'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeECORR, finalComparison = all, sizeKeepingMethod = penalty")
test_that("ECORR6", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeECORR'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'all'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'penalty'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})


context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = pairwiseMax, sizeKeepingMethod = penalty")
test_that("ECORR7", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeEC'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'pairwiseMax'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'penalty'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = TreeEC, finalComparison = all, sizeKeepingMethod = penalty")
test_that("ECORR8", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 4)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'TreeEC'))
  ind[,3] = sapply(pacotestOptions, function(x) identical(x$finalComparison, 'all'))
  ind[,4] = sapply(pacotestOptions, function(x) identical(x$sizeKeepingMethod, 'penalty'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})

context("unit tests for pacotest with options:
testType=ECORR, grouping = SumMedian, ProdThirdsII, SumThirdsIII")
test_that("ECORR9", {
  
  maxNTests = length(pacotestOptions)
  
  ind = matrix(NA, maxNTests, 2)
  
  ind[,1] = sapply(pacotestOptions, function(x) identical(x$testType, 'ECORR'))
  ind[,2] = sapply(pacotestOptions, function(x) identical(x$grouping, 'SumMedian') | 
                     identical(x$grouping, 'ProdThirdsII') | identical(x$grouping, 'SumThirdsIII'))
  
  xx = apply(ind, 1, all)
  yy = 1:maxNTests
  
  thisTestsInd = yy[xx]
  
  ## load hard-coded results
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  unitTestKernel(data, pacotestOptions, hardCodedResults, seedsPerTest, thisTestsInd)
  
})


#   
#   set.seed(51312)
#   # Data set 6
#   N= 768
#   # Four-dimensional D-vine
#   structure = matrix(c(4,0,0,0,
#                        1,3,2,0,
#                        2,1,2,0,
#                        3,2,1,1),4,4,TRUE)
#   
#   families = array(3,dim=dim(structure))
#   families  = matrix(c(0,0,0,0,
#                        5,0,0,0,
#                        1,1,0,0,
#                        2,2,2,0),4,4,TRUE)
#   par2  = matrix(c(0,0,0,0,
#                    0,0,0,0,
#                    0,0,0,0,
#                    2.5,8,6,0),4,4,TRUE)
#   par  = matrix(c(0,0,0,0,
#                   5,0,0,0,
#                   0.4,0.3,0,0,
#                   0.5,0.8,0.6,0),4,4,TRUE)
#   names = c("V1", "V2", "V3","V4")
#   
#   rvm = RVineMatrix(structure,families,par,par2,names)
#   rvm = RVineMatrixNormalize(rvm)
#   
#   U = RVineSim(N,rvm)
#   
#   rvmHat = RVineSeqEst(U,rvm)
#   
#   pacotestOptions1 = pacotestset(pacotestOptions1, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions2 = pacotestset(pacotestOptions2, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions3 = pacotestset(pacotestOptions3, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions4 = pacotestset(pacotestOptions4, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions5 = pacotestset(pacotestOptions5, withEstUncert = TRUE)
#   pacotestOptions6 = pacotestset(pacotestOptions6, withEstUncert = TRUE)
#   pacotestOptions7 = pacotestset(pacotestOptions7, withEstUncert = TRUE)
#   pacotestOptions8 = pacotestset(pacotestOptions8, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions9 = pacotestset(pacotestOptions9, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions10 = pacotestset(pacotestOptions10, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions11 = pacotestset(pacotestOptions11, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions12 = pacotestset(pacotestOptions12, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions13 = pacotestset(pacotestOptions13, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions14 = pacotestset(pacotestOptions14, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions15 = pacotestset(pacotestOptions15, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions16 = pacotestset(pacotestOptions16, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions17 = pacotestset(pacotestOptions17, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions18 = pacotestset(pacotestOptions18, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   pacotestOptions19 = pacotestset(pacotestOptions19, withEstUncert = TRUE, aggPvalsNumbRep = 1)
#   
#   pacotestOptions20 = pacotestset(pacotestOptions20, withEstUncert = TRUE)
#   pacotestOptions21 = pacotestset(pacotestOptions21, withEstUncert = TRUE)
#   pacotestOptions22 = pacotestset(pacotestOptions22, withEstUncert = TRUE)
#   pacotestOptions23 = pacotestset(pacotestOptions23, withEstUncert = TRUE)
#   pacotestOptions24 = pacotestset(pacotestOptions24, withEstUncert = TRUE)
#   pacotestOptions25 = pacotestset(pacotestOptions25, withEstUncert = TRUE)
#   pacotestOptions26 = pacotestset(pacotestOptions26, withEstUncert = TRUE)
#   pacotestOptions27 = pacotestset(pacotestOptions27, withEstUncert = TRUE)
#   pacotestOptions28 = pacotestset(pacotestOptions28, withEstUncert = TRUE)
#   pacotestOptions29 = pacotestset(pacotestOptions29, withEstUncert = TRUE)
#   pacotestOptions30 = pacotestset(pacotestOptions30, withEstUncert = TRUE)
#   pacotestOptions31 = pacotestset(pacotestOptions31, withEstUncert = TRUE)
#   pacotestOptions32 = pacotestset(pacotestOptions32, withEstUncert = TRUE)
#   pacotestOptions33 = pacotestset(pacotestOptions33, withEstUncert = TRUE)
#   pacotestOptions34 = pacotestset(pacotestOptions34, withEstUncert = TRUE)
#   pacotestOptions35 = pacotestset(pacotestOptions35, withEstUncert = TRUE)
#   
#   
#   
#   resultData6Test1 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions1, 3, 1)$pValue
#   resultData6Test2 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions2, 3, 1)$pValue
#   resultData6Test3 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions3, 3, 1)$pValue
#   resultData6Test4 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions4, 3, 1)$pValue
#   resultData6Test5 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions5, 3, 1)$pValue
#   resultData6Test6 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions6, 3, 1)$pValue
#   resultData6Test7 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions7, 3, 1)$pValue
#   resultData6Test8 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions8, 3, 1)$pValue
#   resultData6Test9 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions9, 3, 1)$pValue
#   resultData6Test10 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions10, 3, 1)$pValue
#   resultData6Test11 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions11, 3, 1)$pValue
#   resultData6Test12 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions12, 3, 1)$pValue
#   resultData6Test13 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions13, 3, 1)$pValue
#   resultData6Test14 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions14, 3, 1)$pValue
#   resultData6Test15 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions15, 3, 1)$pValue
#   resultData6Test16 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions16, 3, 1)$pValue
#   resultData6Test17 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions17, 3, 1)$pValue
#   resultData6Test18 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions18, 3, 1)$pValue
#   resultData6Test19 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions19, 3, 1)$pValue
#   
#   resultData6Test20 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions20, 3, 1)$pValue
#   resultData6Test21 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions21, 3, 1)$pValue
#   resultData6Test22 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions22, 3, 1)$pValue
#   resultData6Test23 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions23, 3, 1)$pValue
#   resultData6Test24 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions24, 3, 1)$pValue
#   resultData6Test25 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions25, 3, 1)$pValue
#   resultData6Test26 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions26, 3, 1)$pValue
#   resultData6Test27 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions27, 3, 1)$pValue
#   resultData6Test28 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions28, 3, 1)$pValue
#   resultData6Test29 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions29, 3, 1)$pValue
#   resultData6Test30 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions30, 3, 1)$pValue
#   resultData6Test31 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions31, 3, 1)$pValue
#   resultData6Test32 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions32, 3, 1)$pValue
#   resultData6Test33 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions33, 3, 1)$pValue
#   resultData6Test34 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions34, 3, 1)$pValue
#   resultData6Test35 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions35, 3, 1)$pValue
#   
#   expect_equal(resultData6Test1,0.7616097837018107608, tolerance = 1e-2)
#   expect_equal(resultData6Test2,0.25689682268935032372, tolerance = 1e-2)
#   expect_equal(resultData6Test3,0.46754118033759028972, tolerance = 1e-2)
#   expect_equal(resultData6Test4,0.22474099157151450346, tolerance = 1e-2)
#   expect_equal(resultData6Test5,0.97358975668846381435, tolerance = 1e-2)
#   expect_equal(resultData6Test6,0.66050587531959226162, tolerance = 1e-2)
#   expect_equal(resultData6Test7,0.48210543321482057788, tolerance = 1e-2)
#   expect_equal(resultData6Test8,0.46659658497814548994, tolerance = 1e-2)
#   expect_equal(resultData6Test9,0.6613863291933046451, tolerance = 1e-2)
#   expect_equal(resultData6Test10,0.42032188856387486808, tolerance = 1e-2)
#   expect_equal(resultData6Test11,0.6340536034609467464, tolerance = 1e-2)
#   expect_equal(resultData6Test12,0.59719240064985901206, tolerance = 1e-2)
#   expect_equal(resultData6Test13,0.47837673185617834992, tolerance = 1e-2)
#   expect_equal(resultData6Test14,0.65017048880111238951, tolerance = 1e-2)
#   expect_equal(resultData6Test15,0.73967587328354000853, tolerance = 1e-2)
#   expect_equal(resultData6Test16,0.49423684545298229853, tolerance = 1e-2)
#   expect_equal(resultData6Test17,0.1164354048339920622, tolerance = 1e-2)
#   expect_equal(resultData6Test18,0.8407410989303726323, tolerance = 1e-2)
#   expect_equal(resultData6Test19,0.13122934519427853495, tolerance = 1e-2)
#   
#   expect_equal(resultData6Test20,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test21,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test22,0.97681339893325080936, tolerance = 1e-2)
#   expect_equal(resultData6Test23,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test24,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test25,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test26,0.97681339893325080936, tolerance = 1e-2)
#   expect_equal(resultData6Test27,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test28,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test29,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test30,0.97681339893325080936, tolerance = 1e-2)
#   expect_equal(resultData6Test31,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test32,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test33,0.97358975668844760509, tolerance = 1e-2)
#   expect_equal(resultData6Test34,0.97681339893325080936, tolerance = 1e-2)
#   expect_equal(resultData6Test35,0.97358975668844760509, tolerance = 1e-2)
  
  


