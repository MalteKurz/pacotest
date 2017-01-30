context("Regression tests for pacotest with testType = ECORR")


test_that("unit tests for ECORR", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  library("methods")
  
  
  pacotestOptions = list()
  # Define the test types
  pacotestOptions[[1]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[2]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[3]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',trainingDataFraction=0.34, finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[4]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[5]] = pacotestset(testType='ECORR',grouping = "SumMedian", withEstUncert = FALSE)
  
  pacotestOptions[[6]] = pacotestset(testType='ECORR',grouping = "ProdThirdsII", withEstUncert = FALSE)
  
  pacotestOptions[[7]] = pacotestset(testType='ECORR',grouping = "SumThirdsIII", withEstUncert = FALSE)
  
  pacotestOptions[[8]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[9]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[10]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',trainingDataFraction=0.34, expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[11]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  
  pacotestOptions[[12]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[13]] = pacotestset(testType='ECORR', grouping = 'TreeEC',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[14]] = pacotestset(testType='ECORR', grouping = 'TreeEC',trainingDataFraction=0.34, finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[15]] = pacotestset(testType='ECORR', grouping = 'TreeEC',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[16]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[17]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[18]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',trainingDataFraction=0.34, expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions[[19]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  
  pacotestOptions[[20]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[21]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "penalty")
  
  pacotestOptions[[22]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',gamma0Partition='SumQuartiles', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[23]] = pacotestset(testType='ECORR', grouping = 'TreeECORR',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[24]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[25]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[26]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',gamma0Partition='SumQuartiles', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[27]] = pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  
  pacotestOptions[[28]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[29]] = pacotestset(testType='ECORR', grouping = 'TreeEC',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "penalty")
  
  pacotestOptions[[30]] = pacotestset(testType='ECORR', grouping = 'TreeEC',gamma0Partition='SumQuartiles', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[31]] = pacotestset(testType='ECORR', grouping = 'TreeEC',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[32]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[33]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[34]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',gamma0Partition='SumQuartiles', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions[[35]] = pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  
  resPacotestComputed = matrix(NA, 35, 4)
  
  
  filePath <- system.file("unitTestData", "hardCodedResEcorr.csv", package="pacotest")
  hardCodedResults = read.table(filePath, header = FALSE, sep = ",")
  
  set.seed(1921)
  
  for (iTest in 1:35)
  {
    optionValues = paste(pacotestOptions[[1]],';', sep='')
    optionNames = paste(names(pacotestOptions[[1]]),':', sep='')
    optionString = paste(c(rbind(optionNames,optionValues)), collapse=" ")

    
    resPacotestComputed[iTest,] = pacotestForData1_4(data1, data2, data3, data4, pacotestOptions[[iTest]])
    for (iDataset in 1:4)
    {
      expect_equal(resPacotestComputed[iTest, iDataset], hardCodedResults[iTest, iDataset], tolerance = 1e-2,
                   info = paste("Data set number: ", iDataset, "; pacotestOptions: ", optionString, collapse=''))
      
    }
  }
  
  
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
  
  
})

