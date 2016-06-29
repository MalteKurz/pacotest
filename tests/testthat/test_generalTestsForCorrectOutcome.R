context("general tests for correct outcome")

test_that("unit tests for ECORR", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  
  
  
  # Define the test types
  pacotestOptions1=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions2=pacotestset(testType='ECORR', grouping = 'TreeECORR',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions3=pacotestset(testType='ECORR', grouping = 'TreeECORR',trainingDataFraction=0.34, finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions4=pacotestset(testType='ECORR', grouping = 'TreeECORR',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions5=pacotestset(testType='ECORR',grouping = "SumMedian", withEstUncert = FALSE)
  
  pacotestOptions6=pacotestset(testType='ECORR',grouping = "ProdThirdsII", withEstUncert = FALSE)
  
  pacotestOptions7=pacotestset(testType='ECORR',grouping = "SumThirdsIII", withEstUncert = FALSE)
  
  pacotestOptions8=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions9=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions10=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',trainingDataFraction=0.34, expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions11=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  
  pacotestOptions12=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions13=pacotestset(testType='ECORR', grouping = 'TreeEC',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions14=pacotestset(testType='ECORR', grouping = 'TreeEC',trainingDataFraction=0.34, finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions15=pacotestset(testType='ECORR', grouping = 'TreeEC',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions16=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions17=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions18=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',trainingDataFraction=0.34, expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  pacotestOptions19=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "splitTrainEvaluate")
  
  
  pacotestOptions20=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions21=pacotestset(testType='ECORR', grouping = 'TreeECORR',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "penalty")
  
  pacotestOptions22=pacotestset(testType='ECORR', grouping = 'TreeECORR',gamma0Partition='SumQuartiles', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions23=pacotestset(testType='ECORR', grouping = 'TreeECORR',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions24=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions25=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "penalty")
  
  pacotestOptions26=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',gamma0Partition='SumQuartiles', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions27=pacotestset(testType='ECORR', grouping = 'TreeECORR', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  
  pacotestOptions28=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions29=pacotestset(testType='ECORR', grouping = 'TreeEC',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "penalty")
  
  pacotestOptions30=pacotestset(testType='ECORR', grouping = 'TreeEC',gamma0Partition='SumQuartiles', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions31=pacotestset(testType='ECORR', grouping = 'TreeEC',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions32=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions33=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "penalty")
  
  pacotestOptions34=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',gamma0Partition='SumQuartiles', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions35=pacotestset(testType='ECORR', grouping = 'TreeEC', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  
  set.seed(3131)
  # Simulate first data set
  N = 1000
  data1 = matrix(runif(3*N),N,3)
  theta = (4*data1[,1]-2)^3
  
  etheta = expm1(-theta);
  data1[,3] = -1/theta*log(1+etheta/(exp(-theta*data1[,2])*(1/data1[,3]-1)+1));
  
  # Simulate second data set
  N = 1002
  data2 = matrix(runif(5*N),N,5)
  
  
  # Simulate third data set
  N = 1050
  data3 = matrix(runif(3*N),N,3)
  theta = 12 + 8*sin(0.4*(3*data3[,1]+2)^2)
  
  etheta = expm1(-theta);
  data3[,3] = -1/theta*log(1+etheta/(exp(-theta*data3[,2])*(1/data3[,3]-1)+1));
  
  # Simulate fourth data set
  N = 1070
  W = matrix(runif(3*N),N,3)
  data4 = matrix(NA,N,3)
  theta = 2
  
  data4[,1] = W[,1]
  data4[,2] = (W[,1]^(-theta)*(W[,2]^((-theta)/(1+theta))-1)+1)^(-1/theta);
  theta_23_1 = theta /(1+theta)
  data4[,3] = (W[,2]^(-theta_23_1)*(W[,3]^((-theta_23_1)/(1+theta_23_1))-1)+1)^(-1/theta_23_1);
  data4[,3] = (W[,1]^(-theta)*(data4[,3]^((-theta)/(1+theta))-1)+1)^(-1/theta);
  # Get Pseudo-Obs from the conditional copula C_23|1
  U = matrix(NA,N,2)
  U[,1] = (data4[,1]^theta*(data4[,2]^(-theta)-1)+1)^(-(1+theta)/theta);
  U[,2] = (data4[,1]^theta*(data4[,3]^(-theta)-1)+1)^(-(1+theta)/theta);
  
  data4[,c(2,3)] = U
  
  
  set.seed(31191)
  # Data set 5
  N= 756
  # Four-dimensional D-vine
  structure = matrix(c(4,0,0,0,
                       1,3,2,0,
                       2,1,2,0,
                       3,2,1,1),4,4,TRUE)
  
  families = array(3,dim=dim(structure))
  par2 = array(0,dim=dim(structure))
  names = c("V1", "V2", "V3","V4")
  par_firstTree = BiCopTau2Par(3, 0.5)
  par_secondTree = par_firstTree/(1+par_firstTree)
  par_thirdTree = par_firstTree/(1+2*par_firstTree)
  par  = matrix(c(0,0,0,0,
                  par_thirdTree,0,0,0,
                  par_secondTree,par_secondTree,0,0,
                  par_firstTree,par_firstTree,par_firstTree,0),4,4,TRUE)
  
  rvm = RVineMatrix(structure,families,par,par2,names)
  rvm = RVineMatrixNormalize(rvm)
  svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)
  
  U = RVineSim(N,rvm)
  
  # Compute CPITs for the whole vine
  cPitData = pacotest:::getCpitsFromVine(U, svcmDataFrame)
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  
  cPit1 = pacotest:::getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = pacotest:::getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  
  Udata = cbind(cPit1,cPit2)
  W = U[,svcmDataFrame$condset[[copulaInd]]]
  
  set.seed(31312)
  # Data set 6
  N= 1756
  # Four-dimensional D-vine
  structure = matrix(c(4,0,0,0,
                       1,3,2,0,
                       2,1,2,0,
                       3,2,1,1),4,4,TRUE)
  
  families = array(3,dim=dim(structure))
  families  = matrix(c(0,0,0,0,
                       5,0,0,0,
                       1,1,0,0,
                       2,2,2,0),4,4,TRUE)
  par2  = matrix(c(0,0,0,0,
                   0,0,0,0,
                   0,0,0,0,
                   2.5,8,6,0),4,4,TRUE)
  par  = matrix(c(0,0,0,0,
                  5,0,0,0,
                  0.4,0.3,0,0,
                  0.5,0.8,0.6,0),4,4,TRUE)
  names = c("V1", "V2", "V3","V4")
  
  rvm = RVineMatrix(structure,families,par,par2,names)
  rvm = RVineMatrixNormalize(rvm)
  svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)
  
  U = RVineSim(N,rvm)
  
  # Compute CPITs for the whole vine
  cPitData = pacotest:::getCpitsFromVine(U, svcmDataFrame)
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  
  cPit1 = pacotest:::getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = pacotest:::getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  
  Udata = cbind(cPit1,cPit2)
  W = U[,svcmDataFrame$condset[[copulaInd]]]
  
  
  set.seed(1921)
  
  ##
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,2.1246893133763933292e-12, tolerance = 1e-4)
  expect_equal(resultData2Test1,0.63520407773957221487, tolerance = 1e-4)
  expect_equal(resultData3Test1,5.752268106284930127e-10, tolerance = 1e-4)
  expect_equal(resultData4Test1,0.15587677566236446403, tolerance = 1e-4)
  
  ##
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,1.9217960556261459715e-13, tolerance = 1e-4)
  expect_equal(resultData2Test2,0.44452957033286516486, tolerance = 1e-4)
  expect_equal(resultData3Test2,9.6432739571383763177e-10, tolerance = 1e-4)
  expect_equal(resultData4Test2,0.15619267110582790714, tolerance = 1e-4)
  
  ##
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,1.1102230246251565404e-15, tolerance = 1e-4)
  expect_equal(resultData2Test3,0.5580923850607153458, tolerance = 1e-4)
  expect_equal(resultData3Test3,2.3669399773496024864e-12, tolerance = 1e-4)
  expect_equal(resultData4Test3,0.1541884126560324475, tolerance = 1e-4)
  
  ##
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,3.3317792969000947778e-13, tolerance = 1e-4)
  expect_equal(resultData2Test4,0.47397446101156270881, tolerance = 1e-4)
  expect_equal(resultData3Test4,4.0479203322618673155e-10, tolerance = 1e-4)
  expect_equal(resultData4Test4,0.18825661750246180937, tolerance = 1e-4)
  
  ##
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,1.8540724511240114225e-14, tolerance = 1e-4)
  expect_equal(resultData2Test5,0.38342942676277014247, tolerance = 1e-4)
  expect_equal(resultData3Test5,0.029434655647674223822, tolerance = 1e-4)
  expect_equal(resultData4Test5,0.98048071162177008464, tolerance = 1e-4)
  
  ##
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,5.3219144424687669925e-07, tolerance = 1e-4)
  expect_equal(resultData2Test6,0.35321452437203049168, tolerance = 1e-4)
  expect_equal(resultData3Test6,4.8030468491333522252e-12, tolerance = 1e-4)
  expect_equal(resultData4Test6,0.11816799466211991287, tolerance = 1e-4)
  
  ##
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0, tolerance = 1e-4)
  expect_equal(resultData2Test7,0.37068998206199255119, tolerance = 1e-4)
  expect_equal(resultData3Test7,0.85332915470892367615, tolerance = 1e-4)
  expect_equal(resultData4Test7,0.20442665798885406581, tolerance = 1e-4)
  
  
  ##
  resultData1Test8 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions8)$pValue
  resultData2Test8 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions8)$pValue
  resultData3Test8 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions8)$pValue
  resultData4Test8 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions8)$pValue
  
  expect_equal(resultData1Test8,1.1429746038515986584e-13, tolerance = 1e-4)
  expect_equal(resultData2Test8,0.59295520957221348191, tolerance = 1e-4)
  expect_equal(resultData3Test8,3.9912728677649056408e-10, tolerance = 1e-4)
  expect_equal(resultData4Test8,0.21495265190788415133, tolerance = 1e-4)
  
  ##
  resultData1Test9 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions9)$pValue
  resultData2Test9 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions9)$pValue
  resultData3Test9 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions9)$pValue
  resultData4Test9 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions9)$pValue
  
  expect_equal(resultData1Test9,5.4373172631017041567e-13, tolerance = 1e-4)
  expect_equal(resultData2Test9,0.53315281445507345381, tolerance = 1e-4)
  expect_equal(resultData3Test9,8.0054185502831387566e-10, tolerance = 1e-4)
  expect_equal(resultData4Test9,0.12813098312621101771, tolerance = 1e-4)
  
  ##
  resultData1Test10 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions10)$pValue
  resultData2Test10 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions10)$pValue
  resultData3Test10 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions10)$pValue
  resultData4Test10 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions10)$pValue
  
  expect_equal(resultData1Test10,1.6098233857064769836e-15, tolerance = 1e-4)
  expect_equal(resultData2Test10,0.619130004810437784, tolerance = 1e-4)
  expect_equal(resultData3Test10,1.8795520695391587651e-12, tolerance = 1e-4)
  expect_equal(resultData4Test10,0.17673130429000261099, tolerance = 1e-4)
  
  ##
  resultData1Test11 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions11)$pValue
  resultData2Test11 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions11)$pValue
  resultData3Test11 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions11)$pValue
  resultData4Test11 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions11)$pValue
  
  expect_equal(resultData1Test11,2.6623148130511253839e-13, tolerance = 1e-4)
  expect_equal(resultData2Test11,0.50311142788708784757, tolerance = 1e-4)
  expect_equal(resultData3Test11,6.250877593316772618e-10, tolerance = 1e-4)
  expect_equal(resultData4Test11,0.16726361497538227985, tolerance = 1e-4)
  
  ##
  resultData1Test12 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions12)$pValue
  resultData2Test12 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions12)$pValue
  resultData3Test12 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions12)$pValue
  resultData4Test12 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions12)$pValue
  
  expect_equal(resultData1Test12,2.5190405317232489324e-12, tolerance = 1e-4)
  expect_equal(resultData2Test12,0.55176889867673017953, tolerance = 1e-4)
  expect_equal(resultData3Test12,1.0080859480510184767e-09, tolerance = 1e-4)
  expect_equal(resultData4Test12,0.1690533342375308834, tolerance = 1e-4)
  
  ##
  resultData1Test13 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions13)$pValue
  resultData2Test13 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions13)$pValue
  resultData3Test13 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions13)$pValue
  resultData4Test13 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions13)$pValue
  
  expect_equal(resultData1Test13,1.85873538782743708e-12, tolerance = 1e-4)
  expect_equal(resultData2Test13,0.40894465103703903086, tolerance = 1e-4)
  expect_equal(resultData3Test13,5.4038018504343199311e-10, tolerance = 1e-4)
  expect_equal(resultData4Test13,0.15699172106865283416, tolerance = 1e-4)
  
  ##
  resultData1Test14 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions14)$pValue
  resultData2Test14 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions14)$pValue
  resultData3Test14 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions14)$pValue
  resultData4Test14 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions14)$pValue
  
  expect_equal(resultData1Test14,8.5487172896137053613e-15, tolerance = 1e-4)
  expect_equal(resultData2Test14,0.55045671042871491263, tolerance = 1e-4)
  expect_equal(resultData3Test14,3.2835401064801317261e-12, tolerance = 1e-4)
  expect_equal(resultData4Test14,0.16076681715336549328, tolerance = 1e-4)
  
  ##
  resultData1Test15 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions15)$pValue
  resultData2Test15 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions15)$pValue
  resultData3Test15 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions15)$pValue
  resultData4Test15 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions15)$pValue
  
  expect_equal(resultData1Test15,4.0872860651575138036e-13, tolerance = 1e-4)
  expect_equal(resultData2Test15,0.41687690526596948848, tolerance = 1e-4)
  expect_equal(resultData3Test15,3.4853187003136554267e-10, tolerance = 1e-4)
  expect_equal(resultData4Test15,0.20337320388824658979, tolerance = 1e-4)
  
  ##
  resultData1Test16 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions16)$pValue
  resultData2Test16 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions16)$pValue
  resultData3Test16 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions16)$pValue
  resultData4Test16 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions16)$pValue
  
  expect_equal(resultData1Test16,3.4767189127649089642e-12, tolerance = 1e-4)
  expect_equal(resultData2Test16,0.50423600959622016937, tolerance = 1e-4)
  expect_equal(resultData3Test16,2.1512736037010427026e-10, tolerance = 1e-4)
  expect_equal(resultData4Test16,0.14569436465787743984, tolerance = 1e-4)
  
  ##
  resultData1Test17 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions17)$pValue
  resultData2Test17 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions17)$pValue
  resultData3Test17 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions17)$pValue
  resultData4Test17 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions17)$pValue
  
  expect_equal(resultData1Test17,1.0745848655346890155e-12, tolerance = 1e-4)
  expect_equal(resultData2Test17,0.5523203441913488998, tolerance = 1e-4)
  expect_equal(resultData3Test17,9.9395125463530575871e-10, tolerance = 1e-4)
  expect_equal(resultData4Test17,0.17874838533804909835, tolerance = 1e-4)
  
  ##
  resultData1Test18 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions18)$pValue
  resultData2Test18 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions18)$pValue
  resultData3Test18 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions18)$pValue
  resultData4Test18 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions18)$pValue
  
  expect_equal(resultData1Test18,3.8302694349567900645e-15, tolerance = 1e-4)
  expect_equal(resultData2Test18,0.58424540728909668541, tolerance = 1e-4)
  expect_equal(resultData3Test18,1.4301781980918804038e-11, tolerance = 1e-4)
  expect_equal(resultData4Test18,0.14077158420207858125, tolerance = 1e-4)
  
  ##
  resultData1Test19 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions19)$pValue
  resultData2Test19 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions19)$pValue
  resultData3Test19 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions19)$pValue
  resultData4Test19 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions19)$pValue
  
  expect_equal(resultData1Test19,1.1696199564426024153e-12, tolerance = 1e-4)
  expect_equal(resultData2Test19,0.47242161671558513536, tolerance = 1e-4)
  expect_equal(resultData3Test19,8.8058427216708423657e-10, tolerance = 1e-4)
  expect_equal(resultData4Test19,0.14158713933062910817, tolerance = 1e-4)
  
  ##
  resultData1Test20 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions20)$pValue
  resultData2Test20 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions20)$pValue
  resultData3Test20 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions20)$pValue
  resultData4Test20 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions20)$pValue
  
  expect_equal(resultData1Test20,0, tolerance = 1e-4)
  expect_equal(resultData2Test20,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test20,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test20,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test21 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions21)$pValue
  resultData2Test21 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions21)$pValue
  resultData3Test21 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions21)$pValue
  resultData4Test21 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions21)$pValue
  
  expect_equal(resultData1Test21,0, tolerance = 1e-4)
  expect_equal(resultData2Test21,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test21,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test21,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test22 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions22)$pValue
  resultData2Test22 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions22)$pValue
  resultData3Test22 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions22)$pValue
  resultData4Test22 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions22)$pValue
  
  expect_equal(resultData1Test22,0, tolerance = 1e-4)
  expect_equal(resultData2Test22,0.55145510054908453146, tolerance = 1e-4)
  expect_equal(resultData3Test22,2.2204460492503130808e-16, tolerance = 1e-4)
  expect_equal(resultData4Test22,0.053177235107229048339, tolerance = 1e-4)
  
  ##
  resultData1Test23 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions23)$pValue
  resultData2Test23 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions23)$pValue
  resultData3Test23 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions23)$pValue
  resultData4Test23 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions23)$pValue
  
  expect_equal(resultData1Test23,0, tolerance = 1e-4)
  expect_equal(resultData2Test23,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test23,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test23,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test24 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions24)$pValue
  resultData2Test24 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions24)$pValue
  resultData3Test24 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions24)$pValue
  resultData4Test24 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions24)$pValue
  
  expect_equal(resultData1Test24,0, tolerance = 1e-4)
  expect_equal(resultData2Test24,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test24,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test24,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test25 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions25)$pValue
  resultData2Test25 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions25)$pValue
  resultData3Test25 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions25)$pValue
  resultData4Test25 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions25)$pValue
  
  expect_equal(resultData1Test25,0, tolerance = 1e-4)
  expect_equal(resultData2Test25,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test25,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test25,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test26 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions26)$pValue
  resultData2Test26 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions26)$pValue
  resultData3Test26 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions26)$pValue
  resultData4Test26 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions26)$pValue
  
  expect_equal(resultData1Test26,0, tolerance = 1e-4)
  expect_equal(resultData2Test26,0.55145510054908453146, tolerance = 1e-4)
  expect_equal(resultData3Test26,2.2204460492503130808e-16, tolerance = 1e-4)
  expect_equal(resultData4Test26,0.053177235107229048339, tolerance = 1e-4)
  
  ##
  resultData1Test27 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions27)$pValue
  resultData2Test27 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions27)$pValue
  resultData3Test27 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions27)$pValue
  resultData4Test27 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions27)$pValue
  
  expect_equal(resultData1Test27,0, tolerance = 1e-4)
  expect_equal(resultData2Test27,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test27,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test27,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test28 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions28)$pValue
  resultData2Test28 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions28)$pValue
  resultData3Test28 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions28)$pValue
  resultData4Test28 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions28)$pValue
  
  expect_equal(resultData1Test28,0, tolerance = 1e-4)
  expect_equal(resultData2Test28,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test28,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test28,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test29 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions29)$pValue
  resultData2Test29 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions29)$pValue
  resultData3Test29 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions29)$pValue
  resultData4Test29 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions29)$pValue
  
  expect_equal(resultData1Test29,0, tolerance = 1e-4)
  expect_equal(resultData2Test29,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test29,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test29,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test30 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions30)$pValue
  resultData2Test30 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions30)$pValue
  resultData3Test30 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions30)$pValue
  resultData4Test30 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions30)$pValue
  
  expect_equal(resultData1Test30,0, tolerance = 1e-4)
  expect_equal(resultData2Test30,0.55145510054908453146, tolerance = 1e-4)
  expect_equal(resultData3Test30,2.2204460492503130808e-16, tolerance = 1e-4)
  expect_equal(resultData4Test30,0.053177235107229048339, tolerance = 1e-4)
  
  ##
  resultData1Test31 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions31)$pValue
  resultData2Test31 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions31)$pValue
  resultData3Test31 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions31)$pValue
  resultData4Test31 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions31)$pValue
  
  expect_equal(resultData1Test31,0, tolerance = 1e-4)
  expect_equal(resultData2Test31,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test31,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test31,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test32 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions32)$pValue
  resultData2Test32 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions32)$pValue
  resultData3Test32 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions32)$pValue
  resultData4Test32 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions32)$pValue
  
  expect_equal(resultData1Test32,0, tolerance = 1e-4)
  expect_equal(resultData2Test32,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test32,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test32,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test33 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions33)$pValue
  resultData2Test33 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions33)$pValue
  resultData3Test33 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions33)$pValue
  resultData4Test33 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions33)$pValue
  
  expect_equal(resultData1Test33,0, tolerance = 1e-4)
  expect_equal(resultData2Test33,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test33,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test33,0.98048071162180483462, tolerance = 1e-4)
  
  ##
  resultData1Test34 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions34)$pValue
  resultData2Test34 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions34)$pValue
  resultData3Test34 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions34)$pValue
  resultData4Test34 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions34)$pValue
  
  expect_equal(resultData1Test34,0, tolerance = 1e-4)
  expect_equal(resultData2Test34,0.55145510054908453146, tolerance = 1e-4)
  expect_equal(resultData3Test34,2.2204460492503130808e-16, tolerance = 1e-4)
  expect_equal(resultData4Test34,0.053177235107229048339, tolerance = 1e-4)
  
  ##
  resultData1Test35 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions35)$pValue
  resultData2Test35 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions35)$pValue
  resultData3Test35 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions35)$pValue
  resultData4Test35 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions35)$pValue
  
  expect_equal(resultData1Test35,0, tolerance = 1e-4)
  expect_equal(resultData2Test35,0.38342942676277114167, tolerance = 1e-4)
  expect_equal(resultData3Test35,1.4357270927689569362e-10, tolerance = 1e-4)
  expect_equal(resultData4Test35,0.98048071162180483462, tolerance = 1e-4)
  
  
  
  
  set.seed(51312)
  # Data set 6
  N= 768
  # Four-dimensional D-vine
  structure = matrix(c(4,0,0,0,
                       1,3,2,0,
                       2,1,2,0,
                       3,2,1,1),4,4,TRUE)
  
  families = array(3,dim=dim(structure))
  families  = matrix(c(0,0,0,0,
                       5,0,0,0,
                       1,1,0,0,
                       2,2,2,0),4,4,TRUE)
  par2  = matrix(c(0,0,0,0,
                   0,0,0,0,
                   0,0,0,0,
                   2.5,8,6,0),4,4,TRUE)
  par  = matrix(c(0,0,0,0,
                  5,0,0,0,
                  0.4,0.3,0,0,
                  0.5,0.8,0.6,0),4,4,TRUE)
  names = c("V1", "V2", "V3","V4")
  
  rvm = RVineMatrix(structure,families,par,par2,names)
  rvm = RVineMatrixNormalize(rvm)
  
  U = RVineSim(N,rvm)
  
  rvmHat = RVineSeqEst(U,rvm)$RVM
  
  pacotestOptions1 = pacotestset(pacotestOptions1, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions2 = pacotestset(pacotestOptions2, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions3 = pacotestset(pacotestOptions3, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions4 = pacotestset(pacotestOptions4, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions5 = pacotestset(pacotestOptions5, withEstUncert = TRUE)
  pacotestOptions6 = pacotestset(pacotestOptions6, withEstUncert = TRUE)
  pacotestOptions7 = pacotestset(pacotestOptions7, withEstUncert = TRUE)
  pacotestOptions8 = pacotestset(pacotestOptions8, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions9 = pacotestset(pacotestOptions9, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions10 = pacotestset(pacotestOptions10, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions11 = pacotestset(pacotestOptions11, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions12 = pacotestset(pacotestOptions12, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions13 = pacotestset(pacotestOptions13, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions14 = pacotestset(pacotestOptions14, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions15 = pacotestset(pacotestOptions15, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions16 = pacotestset(pacotestOptions16, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions17 = pacotestset(pacotestOptions17, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions18 = pacotestset(pacotestOptions18, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  pacotestOptions19 = pacotestset(pacotestOptions19, withEstUncert = TRUE, aggPvalsNumbRep = 1)
  
  pacotestOptions20 = pacotestset(pacotestOptions20, withEstUncert = TRUE)
  pacotestOptions21 = pacotestset(pacotestOptions21, withEstUncert = TRUE)
  pacotestOptions22 = pacotestset(pacotestOptions22, withEstUncert = TRUE)
  pacotestOptions23 = pacotestset(pacotestOptions23, withEstUncert = TRUE)
  pacotestOptions24 = pacotestset(pacotestOptions24, withEstUncert = TRUE)
  pacotestOptions25 = pacotestset(pacotestOptions25, withEstUncert = TRUE)
  pacotestOptions26 = pacotestset(pacotestOptions26, withEstUncert = TRUE)
  pacotestOptions27 = pacotestset(pacotestOptions27, withEstUncert = TRUE)
  pacotestOptions28 = pacotestset(pacotestOptions28, withEstUncert = TRUE)
  pacotestOptions29 = pacotestset(pacotestOptions29, withEstUncert = TRUE)
  pacotestOptions30 = pacotestset(pacotestOptions30, withEstUncert = TRUE)
  pacotestOptions31 = pacotestset(pacotestOptions31, withEstUncert = TRUE)
  pacotestOptions32 = pacotestset(pacotestOptions32, withEstUncert = TRUE)
  pacotestOptions33 = pacotestset(pacotestOptions33, withEstUncert = TRUE)
  pacotestOptions34 = pacotestset(pacotestOptions34, withEstUncert = TRUE)
  pacotestOptions35 = pacotestset(pacotestOptions35, withEstUncert = TRUE)
  
  
  
  resultData6Test1 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions1, 3, 1)$pValue
  resultData6Test2 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions2, 3, 1)$pValue
  resultData6Test3 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions3, 3, 1)$pValue
  resultData6Test4 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions4, 3, 1)$pValue
  resultData6Test5 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions5, 3, 1)$pValue
  resultData6Test6 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions6, 3, 1)$pValue
  resultData6Test7 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions7, 3, 1)$pValue
  resultData6Test8 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions8, 3, 1)$pValue
  resultData6Test9 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions9, 3, 1)$pValue
  resultData6Test10 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions10, 3, 1)$pValue
  resultData6Test11 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions11, 3, 1)$pValue
  resultData6Test12 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions12, 3, 1)$pValue
  resultData6Test13 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions13, 3, 1)$pValue
  resultData6Test14 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions14, 3, 1)$pValue
  resultData6Test15 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions15, 3, 1)$pValue
  resultData6Test16 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions16, 3, 1)$pValue
  resultData6Test17 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions17, 3, 1)$pValue
  resultData6Test18 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions18, 3, 1)$pValue
  resultData6Test19 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions19, 3, 1)$pValue
  
  resultData6Test20 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions20, 3, 1)$pValue
  resultData6Test21 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions21, 3, 1)$pValue
  resultData6Test22 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions22, 3, 1)$pValue
  resultData6Test23 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions23, 3, 1)$pValue
  resultData6Test24 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions24, 3, 1)$pValue
  resultData6Test25 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions25, 3, 1)$pValue
  resultData6Test26 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions26, 3, 1)$pValue
  resultData6Test27 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions27, 3, 1)$pValue
  resultData6Test28 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions28, 3, 1)$pValue
  resultData6Test29 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions29, 3, 1)$pValue
  resultData6Test30 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions30, 3, 1)$pValue
  resultData6Test31 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions31, 3, 1)$pValue
  resultData6Test32 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions32, 3, 1)$pValue
  resultData6Test33 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions33, 3, 1)$pValue
  resultData6Test34 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions34, 3, 1)$pValue
  resultData6Test35 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions35, 3, 1)$pValue
  
  expect_equal(resultData6Test1,0.7616097837018107608, tolerance = 1e-4)
  expect_equal(resultData6Test2,0.25689682268935032372, tolerance = 1e-4)
  expect_equal(resultData6Test3,0.46754118033759028972, tolerance = 1e-4)
  expect_equal(resultData6Test4,0.22474099157151450346, tolerance = 1e-4)
  expect_equal(resultData6Test5,0.97358975668846381435, tolerance = 1e-4)
  expect_equal(resultData6Test6,0.66050587531959226162, tolerance = 1e-4)
  expect_equal(resultData6Test7,0.48210543321482057788, tolerance = 1e-4)
  expect_equal(resultData6Test8,0.46659658497814548994, tolerance = 1e-4)
  expect_equal(resultData6Test9,0.6613863291933046451, tolerance = 1e-4)
  expect_equal(resultData6Test10,0.42032188856387486808, tolerance = 1e-4)
  expect_equal(resultData6Test11,0.6340536034609467464, tolerance = 1e-4)
  expect_equal(resultData6Test12,0.59719240064985901206, tolerance = 1e-4)
  expect_equal(resultData6Test13,0.47837673185617834992, tolerance = 1e-4)
  expect_equal(resultData6Test14,0.65017048880111238951, tolerance = 1e-4)
  expect_equal(resultData6Test15,0.73967587328354000853, tolerance = 1e-4)
  expect_equal(resultData6Test16,0.49423684545298229853, tolerance = 1e-4)
  expect_equal(resultData6Test17,0.1164354048339920622, tolerance = 1e-4)
  expect_equal(resultData6Test18,0.8407410989303726323, tolerance = 1e-4)
  expect_equal(resultData6Test19,0.13122934519427853495, tolerance = 1e-4)
  
  expect_equal(resultData6Test20,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test21,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test22,0.97681339893325080936, tolerance = 1e-4)
  expect_equal(resultData6Test23,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test24,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test25,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test26,0.97681339893325080936, tolerance = 1e-4)
  expect_equal(resultData6Test27,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test28,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test29,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test30,0.97681339893325080936, tolerance = 1e-4)
  expect_equal(resultData6Test31,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test32,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test33,0.97358975668844760509, tolerance = 1e-4)
  expect_equal(resultData6Test34,0.97681339893325080936, tolerance = 1e-4)
  expect_equal(resultData6Test35,0.97358975668844760509, tolerance = 1e-4)
  
  
})


test_that("unit tests for ECOV, EC and VI", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  
  
  
  # Define the test types
  pacotestOptions1=pacotestset(testType='VI')
  pacotestOptions2=pacotestset(testType='EC',grouping = "SumMedian")
  
  pacotestOptions3=pacotestset(testType='EC',grouping = "ProdThirdsII")
  
  pacotestOptions4=pacotestset(testType='EC',grouping = "SumThirdsIII")
  
  pacotestOptions5=pacotestset(testType='ECOV',grouping = "SumMedian", withEstUncert = FALSE)
  
  pacotestOptions6=pacotestset(testType='ECOV',grouping = "ProdThirdsII", withEstUncert = FALSE)
  
  pacotestOptions7=pacotestset(testType='ECOV',grouping = "SumThirdsIII", withEstUncert = FALSE)
  
  pacotestOptions8=pacotestset(testType='ECOV', grouping = 'TreeECOV', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions9=pacotestset(testType='ECOV', grouping = 'TreeECOV',expMinSampleSize=56, finalComparison = 'pairwiseMax', sizeKeepingMethod = "penalty")
  
  pacotestOptions10=pacotestset(testType='ECOV', grouping = 'TreeECOV',gamma0Partition='SumQuartiles', finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions11=pacotestset(testType='ECOV', grouping = 'TreeECOV',aggInfo="meanPairwise", finalComparison = 'pairwiseMax', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions12=pacotestset(testType='ECOV', grouping = 'TreeECOV', finalComparison = 'all', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions13=pacotestset(testType='ECOV', grouping = 'TreeECOV', finalComparison = 'all',expMinSampleSize=56, sizeKeepingMethod = "penalty")
  
  pacotestOptions14=pacotestset(testType='ECOV', grouping = 'TreeECOV', finalComparison = 'all',gamma0Partition='SumQuartiles', expMinSampleSize = 100, sizeKeepingMethod = "penalty")
  
  pacotestOptions15=pacotestset(testType='ECOV', grouping = 'TreeECOV', finalComparison = 'all',aggInfo="meanPairwise", expMinSampleSize = 100, sizeKeepingMethod = "penalty")

  pacotestOptions16 = pacotestset(pacotestOptions5, withEstUncert = TRUE)
  pacotestOptions17 = pacotestset(pacotestOptions6, withEstUncert = TRUE)
  pacotestOptions18 = pacotestset(pacotestOptions7, withEstUncert = TRUE)
  pacotestOptions19 = pacotestset(pacotestOptions8, withEstUncert = TRUE)
  pacotestOptions20 = pacotestset(pacotestOptions10, withEstUncert = TRUE)
  pacotestOptions21 = pacotestset(pacotestOptions11, withEstUncert = TRUE)
  pacotestOptions22 = pacotestset(pacotestOptions12, withEstUncert = TRUE)
  pacotestOptions23 = pacotestset(pacotestOptions13, withEstUncert = TRUE)
  pacotestOptions24 = pacotestset(pacotestOptions14, withEstUncert = TRUE)
  pacotestOptions25 = pacotestset(pacotestOptions15, withEstUncert = TRUE)
  
  set.seed(51312)
  # Data set 6
  N= 768
  # Four-dimensional D-vine
  structure = matrix(c(4,0,0,0,
                       1,3,2,0,
                       2,1,2,0,
                       3,2,1,1),4,4,TRUE)
  
  families = array(3,dim=dim(structure))
  families  = matrix(c(0,0,0,0,
                       5,0,0,0,
                       1,1,0,0,
                       2,2,2,0),4,4,TRUE)
  par2  = matrix(c(0,0,0,0,
                   0,0,0,0,
                   0,0,0,0,
                   2.5,8,6,0),4,4,TRUE)
  par  = matrix(c(0,0,0,0,
                  5,0,0,0,
                  0.4,0.3,0,0,
                  0.5,0.8,0.6,0),4,4,TRUE)
  names = c("V1", "V2", "V3","V4")
  
  rvm = RVineMatrix(structure,families,par,par2,names)
  rvm = RVineMatrixNormalize(rvm)
  
  U = RVineSim(N,rvm)
  
  rvmHat = RVineSeqEst(U,rvm)$RVM

  
  resultData6Test1 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions1, 3, 1)$pValue
  
  resultData6Test2 = pacotest:::pacotestRvineSingleCopula(U[1:341,], rvmHat, pacotestOptions2, 3, 1)$pValue
  resultData6Test3 = pacotest:::pacotestRvineSingleCopula(U[1:341,], rvmHat, pacotestOptions3, 3, 1)$pValue
  resultData6Test4 = pacotest:::pacotestRvineSingleCopula(U[1:341,], rvmHat, pacotestOptions4, 3, 1)$pValue
  
  resultData6Test5 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions5, 3, 1)$pValue
  resultData6Test6 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions6, 3, 1)$pValue
  resultData6Test7 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions7, 3, 1)$pValue
  resultData6Test8 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions8, 3, 1)$pValue
  resultData6Test9 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions9, 3, 1)$pValue
  resultData6Test10 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions10, 3, 1)$pValue
  resultData6Test11 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions11, 3, 1)$pValue
  resultData6Test12 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions12, 3, 1)$pValue
  resultData6Test13 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions13, 3, 1)$pValue
  resultData6Test14 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions14, 3, 1)$pValue
  resultData6Test15 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions15, 3, 1)$pValue
  
  resultData6Test16 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions16, 3, 1)$pValue
  resultData6Test17 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions17, 3, 1)$pValue
  resultData6Test18 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions18, 3, 1)$pValue
  resultData6Test19 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions19, 3, 1)$pValue
  resultData6Test20 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions20, 3, 1)$pValue
  resultData6Test21 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions21, 3, 1)$pValue
  resultData6Test22 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions22, 3, 1)$pValue
  resultData6Test23 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions23, 3, 1)$pValue
  resultData6Test24 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions24, 3, 1)$pValue
  resultData6Test25 = pacotest:::pacotestRvineSingleCopula(U, rvmHat, pacotestOptions25, 3, 1)$pValue
  
  expect_equal(resultData6Test1,0.70699999999999996181, tolerance = 1e-4)
  expect_equal(resultData6Test2,0.57099999999999995204, tolerance = 1e-4)
  expect_equal(resultData6Test3,0.8329999999999999627, tolerance = 1e-4)
  expect_equal(resultData6Test4,0.35899999999999998579, tolerance = 1e-4)
  expect_equal(resultData6Test5,0.4697714112048757551, tolerance = 1e-4)
  expect_equal(resultData6Test6,0.32018336201425989795, tolerance = 1e-4)
  expect_equal(resultData6Test7,0.85306274124527348146, tolerance = 1e-4)
  expect_equal(resultData6Test8,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test9,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test10,0.84799768664968644405, tolerance = 1e-4)
  expect_equal(resultData6Test11,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test12,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test13,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test14,0.84799768664968644405, tolerance = 1e-4)
  expect_equal(resultData6Test15,0.46977141120487586612, tolerance = 1e-4)
  expect_equal(resultData6Test16,0.50070969489726790957, tolerance = 1e-4)
  expect_equal(resultData6Test17,0.31251998961408167244, tolerance = 1e-4)
  expect_equal(resultData6Test18,0.8636715705286200917, tolerance = 1e-4)
  expect_equal(resultData6Test19,0.50070969489726846469, tolerance = 1e-4)
  expect_equal(resultData6Test20,0.86678889065076192288, tolerance = 1e-4)
  expect_equal(resultData6Test21,0.50070969489726846469, tolerance = 1e-4)
  expect_equal(resultData6Test22,0.50070969489726846469, tolerance = 1e-4)
  expect_equal(resultData6Test23,0.50070969489726846469, tolerance = 1e-4)
  expect_equal(resultData6Test24,0.86678889065076192288, tolerance = 1e-4)
  expect_equal(resultData6Test25,0.50070969489726846469, tolerance = 1e-4)
  
  
})

