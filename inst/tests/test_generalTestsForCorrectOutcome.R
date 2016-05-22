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
  
  pacotestOptions5=pacotestset(testType='ECORR',grouping = "SumMedian", withEstUncert = FALSE,
                               expMinSampleSize = NULL, trainingDataFraction = NULL,
                               aggPvalsNumbRep = NULL, aggInfo = NULL, finalComparison = NULL)
  
  pacotestOptions6=pacotestset(testType='ECORR',grouping = "ProdThirdsII", withEstUncert = FALSE,
                               expMinSampleSize = NULL, trainingDataFraction = NULL,
                               aggPvalsNumbRep = NULL, aggInfo = NULL, finalComparison = NULL)
  
  pacotestOptions7=pacotestset(testType='ECORR',grouping = "SumThirdsIII", withEstUncert = FALSE,
                               expMinSampleSize = NULL, trainingDataFraction = NULL,
                               aggPvalsNumbRep = NULL, aggInfo = NULL, finalComparison = NULL)
  
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
  
  expect_equal(resultData1Test1,2.1246893133763933292e-12)
  expect_equal(resultData2Test1,0.63520407773957221487)
  expect_equal(resultData3Test1,5.752268106284930127e-10)
  expect_equal(resultData4Test1,0.15587677566236446403)
  
  ##
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,1.9217960556261459715e-13)
  expect_equal(resultData2Test2,0.44452957033286516486)
  expect_equal(resultData3Test2,9.6432739571383763177e-10)
  expect_equal(resultData4Test2,0.15619267110582790714)
  
  ##
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,1.1102230246251565404e-15)
  expect_equal(resultData2Test3,0.5580923850607153458)
  expect_equal(resultData3Test3,2.3669399773496024864e-12)
  expect_equal(resultData4Test3,0.1541884126560324475)
  
  ##
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,3.3317792969000947778e-13)
  expect_equal(resultData2Test4,0.47397446101156270881)
  expect_equal(resultData3Test4,4.0479203322618673155e-10)
  expect_equal(resultData4Test4,0.18825661750246180937)
  
  ##
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,1.8540724511240114225e-14)
  expect_equal(resultData2Test5,0.38342942676277014247)
  expect_equal(resultData3Test5,0.029434655647674223822)
  expect_equal(resultData4Test5,0.98048071162177008464)
  
  ##
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,5.3219144424687669925e-07)
  expect_equal(resultData2Test6,0.35321452437203049168)
  expect_equal(resultData3Test6,4.8030468491333522252e-12)
  expect_equal(resultData4Test6,0.11816799466211991287)
  
  ##
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0)
  expect_equal(resultData2Test7,0.37068998206199255119)
  expect_equal(resultData3Test7,0.85332915470892367615)
  expect_equal(resultData4Test7,0.20442665798885406581)
  
  
  ##
  resultData1Test8 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions8)$pValue
  resultData2Test8 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions8)$pValue
  resultData3Test8 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions8)$pValue
  resultData4Test8 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions8)$pValue
  
  expect_equal(resultData1Test8,1.1429746038515986584e-13)
  expect_equal(resultData2Test8,0.59295520957221348191)
  expect_equal(resultData3Test8,3.9912728677649056408e-10)
  expect_equal(resultData4Test8,0.21495265190788415133)
  
  ##
  resultData1Test9 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions9)$pValue
  resultData2Test9 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions9)$pValue
  resultData3Test9 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions9)$pValue
  resultData4Test9 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions9)$pValue
  
  expect_equal(resultData1Test9,5.4373172631017041567e-13)
  expect_equal(resultData2Test9,0.53315281445507345381)
  expect_equal(resultData3Test9,8.0054185502831387566e-10)
  expect_equal(resultData4Test9,0.12813098312621101771)
  
  ##
  resultData1Test10 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions10)$pValue
  resultData2Test10 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions10)$pValue
  resultData3Test10 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions10)$pValue
  resultData4Test10 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions10)$pValue
  
  expect_equal(resultData1Test10,1.6098233857064769836e-15)
  expect_equal(resultData2Test10,0.619130004810437784)
  expect_equal(resultData3Test10,1.8795520695391587651e-12)
  expect_equal(resultData4Test10,0.17673130429000261099)
  
  ##
  resultData1Test11 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions11)$pValue
  resultData2Test11 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions11)$pValue
  resultData3Test11 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions11)$pValue
  resultData4Test11 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions11)$pValue
  
  expect_equal(resultData1Test11,2.6623148130511253839e-13)
  expect_equal(resultData2Test11,0.50311142788708784757)
  expect_equal(resultData3Test11,6.250877593316772618e-10)
  expect_equal(resultData4Test11,0.16726361497538227985)
  
  ##
  resultData1Test12 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions12)$pValue
  resultData2Test12 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions12)$pValue
  resultData3Test12 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions12)$pValue
  resultData4Test12 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions12)$pValue
  
  expect_equal(resultData1Test12,3.2707170305457111681e-13)
  expect_equal(resultData2Test12,0.65396980026711015732)
  expect_equal(resultData3Test12,2.87306844981571885e-11)
  expect_equal(resultData4Test12,0.05375193971867942011)
  
  ##
  resultData1Test13 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions13)$pValue
  resultData2Test13 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions13)$pValue
  resultData3Test13 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions13)$pValue
  resultData4Test13 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions13)$pValue
  
  expect_equal(resultData1Test13,1.1768364061026659328e-14)
  expect_equal(resultData2Test13,0.20370895359530449831)
  expect_equal(resultData3Test13,9.6390895265585641027e-11)
  expect_equal(resultData4Test13,0.17664072750158610337)
  
  ##
  resultData1Test14 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions14)$pValue
  resultData2Test14 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions14)$pValue
  resultData3Test14 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions14)$pValue
  resultData4Test14 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions14)$pValue
  
  expect_equal(resultData1Test14,3.7291281174134383036e-12)
  expect_equal(resultData2Test14,0.8550627055221047268)
  expect_equal(resultData3Test14,9.4380059323384557501e-13)
  expect_equal(resultData4Test14,0.05182346283757088834)
  
  ##
  resultData1Test15 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions15)$pValue
  resultData2Test15 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions15)$pValue
  resultData3Test15 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions15)$pValue
  resultData4Test15 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions15)$pValue
  
  expect_equal(resultData1Test15,3.804734305390411464e-13)
  expect_equal(resultData2Test15,0.46527422923961048173)
  expect_equal(resultData3Test15,1.9743429113816546305e-11)
  expect_equal(resultData4Test15,0.39610379555683894459)
  
  ##
  resultData1Test16 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions16)$pValue
  resultData2Test16 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions16)$pValue
  resultData3Test16 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions16)$pValue
  resultData4Test16 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions16)$pValue
  
  expect_equal(resultData1Test16,1.7909007610228400154e-12)
  expect_equal(resultData2Test16,0.46749104846293476623)
  expect_equal(resultData3Test16,2.478175553655148633e-09)
  expect_equal(resultData4Test16,0.028987135311187350872)
  
  ##
  resultData1Test17 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions17)$pValue
  resultData2Test17 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions17)$pValue
  resultData3Test17 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions17)$pValue
  resultData4Test17 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions17)$pValue
  
  expect_equal(resultData1Test17,5.3512749786932545248e-14)
  expect_equal(resultData2Test17,0.98874320867005216495)
  expect_equal(resultData3Test17,4.4420978007053690817e-10)
  expect_equal(resultData4Test17,0.094016345439719040122)
  
  ##
  resultData1Test18 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions18)$pValue
  resultData2Test18 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions18)$pValue
  resultData3Test18 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions18)$pValue
  resultData4Test18 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions18)$pValue
  
  expect_equal(resultData1Test18,5.4674368366036674161e-09)
  expect_equal(resultData2Test18,0.63918407741523308729)
  expect_equal(resultData3Test18,0.24127058393461153774)
  expect_equal(resultData4Test18,0.066533758580545221406)
  
  ##
  resultData1Test19 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions19)$pValue
  resultData2Test19 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions19)$pValue
  resultData3Test19 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions19)$pValue
  resultData4Test19 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions19)$pValue
  
  expect_equal(resultData1Test19,1.3847459845450771354e-09)
  expect_equal(resultData2Test19,0.10526948130555380256)
  expect_equal(resultData3Test19,2.4579371649124936994e-08)
  expect_equal(resultData4Test19,0.23685230605856111286)
  
  
  
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
  
  expect_equal(resultData6Test1,0.7616097837018107608)
  expect_equal(resultData6Test2,0.25689682268935032372)
  expect_equal(resultData6Test3,0.46754118033759028972)
  expect_equal(resultData6Test4,0.22474099157151450346)
  expect_equal(resultData6Test5,0.97358975668846381435)
  expect_equal(resultData6Test6,0.66050587531959226162)
  expect_equal(resultData6Test7,0.48210543321482057788)
  expect_equal(resultData6Test8,0.46659658497814548994)
  expect_equal(resultData6Test9,0.6613863291933046451)
  expect_equal(resultData6Test10,0.42032188856387486808)
  expect_equal(resultData6Test11,0.6340536034609467464)
  expect_equal(resultData6Test12,0.59719240064985901206)
  expect_equal(resultData6Test13,0.47837673185617834992)
  expect_equal(resultData6Test14,0.65017048880111238951)
  expect_equal(resultData6Test15,0.73967587328354000853)
  expect_equal(resultData6Test16,0.49423684545298229853)
  expect_equal(resultData6Test17,0.1164354048339920622)
  expect_equal(resultData6Test18,0.8407410989303726323)
  expect_equal(resultData6Test19,0.13122934519427853495)
  
  
})

