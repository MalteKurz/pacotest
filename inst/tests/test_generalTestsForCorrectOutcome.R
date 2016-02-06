context("general tests for correct outcome")

test_that("unit tests for ERC", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  
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
  svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)$variables
  
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
  svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)$variables
  
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
  
  # Define first test type
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$ERCtype = 'chi2'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  pacotestOptions1$finalComparison = 'pairwiseMax'
  
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,6.747824521369238937e-12)
  expect_equal(resultData2Test1,0.54837431230034150431)
  expect_equal(resultData3Test1,6.3621330426144595549e-13)
  expect_equal(resultData4Test1,0.19718603557591019015)
  
  # Define second test type
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$ERCtype = 'chi2'
  pacotestOptions2$Grouping = 'TreeERCchi2'
  pacotestOptions2$finalComparison = 'pairwiseMax'
  
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,2.0561330416057899129e-13)
  expect_equal(resultData2Test2,0.4724491699757810137)
  expect_equal(resultData3Test2,8.8887952554017601869e-11)
  expect_equal(resultData4Test2,0.16216672971204970644)
  
  # Define third test type
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$ERCtype = 'chi2'
  pacotestOptions3$Grouping = 'TreeERCchi2'
  pacotestOptions3$finalComparison = 'pairwiseMax'
  
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,3.2751579226442117942e-15)
  expect_equal(resultData2Test3,0.56515797435065617815)
  expect_equal(resultData3Test3,5.1547655033346018172e-13)
  expect_equal(resultData4Test3,0.16719854809316819777)
  
  # Define fourth test type
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$ERCtype = 'chi2'
  pacotestOptions4$Grouping = 'TreeERCchi2'
  pacotestOptions4$finalComparison = 'pairwiseMax'
  
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,6.7212901910806976957e-13)
  expect_equal(resultData2Test4,0.39351569625630455906)
  expect_equal(resultData3Test4,8.0557782666801358573e-13)
  expect_equal(resultData4Test4,0.2108259163343279563)
  
  # Define fifth test type
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$ERCtype = 'chi2'
  
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,2.1094237467877974268e-14)
  expect_equal(resultData2Test5,0.38705697988022236267)
  expect_equal(resultData3Test5,0.31096627103168605899)
  expect_equal(resultData4Test5,0.53365617218709471281)
  
  # Define sixth test type
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$ERCtype = 'chi2'
  
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,0.88737165766259773481)
  expect_equal(resultData2Test6,0.077376748544651263728)
  expect_equal(resultData3Test6,0.81333770215943657078)
  expect_equal(resultData4Test6,0.0043935785665404347711)
  
  # Define seventh test type
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$ERCtype = 'chi2'
  
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0.89545899920300386921)
  expect_equal(resultData2Test7,0.23908782274707562898)
  expect_equal(resultData3Test7,0.45311029913162337301)
  expect_equal(resultData4Test7,0.037277790614566375105)
  
  
  # Define first test type
  pacotestOptions8=pacotestset(TestType='ERC')
  pacotestOptions8$ERCtype = 'chi2'
  pacotestOptions8$Grouping = 'TreeERCchi2'
  pacotestOptions8$finalComparison = 'all'
  
  resultData1Test8 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions8)$pValue
  resultData2Test8 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions8)$pValue
  resultData3Test8 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions8)$pValue
  resultData4Test8 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions8)$pValue
  
  expect_equal(resultData1Test8,1.5570877920367820479e-13)
  expect_equal(resultData2Test8,0.45283578468966628749)
  expect_equal(resultData3Test8,2.3037127760971998214e-14)
  expect_equal(resultData4Test8,0.21988081013202520619)
  
  # Define second test type
  pacotestOptions9=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions9$ERCtype = 'chi2'
  pacotestOptions9$Grouping = 'TreeERCchi2'
  pacotestOptions9$finalComparison = 'all'
  
  resultData1Test9 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions9)$pValue
  resultData2Test9 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions9)$pValue
  resultData3Test9 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions9)$pValue
  resultData4Test9 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions9)$pValue
  
  expect_equal(resultData1Test9,1.1734502258775592054e-12)
  expect_equal(resultData2Test9,0.50019733787994291596)
  expect_equal(resultData3Test9,1.3259948694610557141e-12)
  expect_equal(resultData4Test9,0.17302613830172214326)
  
  # Define third test type
  pacotestOptions10=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions10$ERCtype = 'chi2'
  pacotestOptions10$Grouping = 'TreeERCchi2'
  pacotestOptions10$finalComparison = 'all'
  
  resultData1Test10 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions10)$pValue
  resultData2Test10 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions10)$pValue
  resultData3Test10 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions10)$pValue
  resultData4Test10 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions10)$pValue
  
  expect_equal(resultData1Test10,4.3298697960381105077e-15)
  expect_equal(resultData2Test10,0.6262659148713292101)
  expect_equal(resultData3Test10,6.1062266354383609723e-16)
  expect_equal(resultData4Test10,0.20561538542137952623)
  
  # Define fourth test type
  pacotestOptions11=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions11$ERCtype = 'chi2'
  pacotestOptions11$Grouping = 'TreeERCchi2'
  pacotestOptions11$finalComparison = 'all'
  
  resultData1Test11 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions11)$pValue
  resultData2Test11 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions11)$pValue
  resultData3Test11 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions11)$pValue
  resultData4Test11 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions11)$pValue
  
  expect_equal(resultData1Test11,1.1501355423604309181e-12)
  expect_equal(resultData2Test11,0.50161697436848429188)
  expect_equal(resultData3Test11,6.1561866715464930166e-14)
  expect_equal(resultData4Test11,0.19060016502803306393)
  
  
  
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
  
  
  # Define the test types
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$ERCtype = 'chi2WithEstimation'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  pacotestOptions1$finalComparison = 'pairwiseMax'
  pacotestOptions1$AggPvalsNumbRep = 1
  
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$ERCtype = 'chi2WithEstimation'
  pacotestOptions2$Grouping = 'TreeERCchi2'
  pacotestOptions2$finalComparison = 'pairwiseMax'
  pacotestOptions2$AggPvalsNumbRep = 1
  
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$ERCtype = 'chi2WithEstimation'
  pacotestOptions3$Grouping = 'TreeERCchi2'
  pacotestOptions3$finalComparison = 'pairwiseMax'
  pacotestOptions3$AggPvalsNumbRep = 1
  
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$ERCtype = 'chi2WithEstimation'
  pacotestOptions4$Grouping = 'TreeERCchi2'
  pacotestOptions4$finalComparison = 'pairwiseMax'
  pacotestOptions4$AggPvalsNumbRep = 1
  
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$ERCtype = 'chi2WithEstimation'
  
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$ERCtype = 'chi2WithEstimation'
  
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$ERCtype = 'chi2WithEstimation'
  
  pacotestOptions8=pacotestset(TestType='ERC')
  pacotestOptions8$ERCtype = 'chi2WithEstimation'
  pacotestOptions8$Grouping = 'TreeERCchi2'
  pacotestOptions8$finalComparison = 'all'
  pacotestOptions8$AggPvalsNumbRep = 1
  
  pacotestOptions9=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions9$ERCtype = 'chi2WithEstimation'
  pacotestOptions9$Grouping = 'TreeERCchi2'
  pacotestOptions9$finalComparison = 'all'
  pacotestOptions9$AggPvalsNumbRep = 1
  
  pacotestOptions10=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions10$ERCtype = 'chi2WithEstimation'
  pacotestOptions10$Grouping = 'TreeERCchi2'
  pacotestOptions10$finalComparison = 'all'
  pacotestOptions10$AggPvalsNumbRep = 1
  
  pacotestOptions11=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions11$ERCtype = 'chi2WithEstimation'
  pacotestOptions11$Grouping = 'TreeERCchi2'
  pacotestOptions11$finalComparison = 'all'
  pacotestOptions11$AggPvalsNumbRep = 1
  
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
  
  expect_equal(resultData6Test1,0.22185963874400715934)
  expect_equal(resultData6Test2,0.81853415514460237112)
  expect_equal(resultData6Test3,0.17520223260212630656)
  expect_equal(resultData6Test4,0.073496161308055585337)
  expect_equal(resultData6Test5,0.49343446518015587898)
  expect_equal(resultData6Test6,0.89780976876241724849)
  expect_equal(resultData6Test7,0.9710702182702302121)
  expect_equal(resultData6Test8,0.47870072891438175677)
  expect_equal(resultData6Test9,0.47710526905101735551)
  expect_equal(resultData6Test10,0.030759519295320059129)
  expect_equal(resultData6Test11,0.4428576682008489307)
  
  
})

