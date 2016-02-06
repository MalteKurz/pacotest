context("general tests for correct outcome")

test_that("unit tests for ERC", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  
  
  
  # Define the test types
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$withEstUncert = FALSE
  pacotestOptions1$Grouping = 'TreeERCchi2'
  pacotestOptions1$finalComparison = 'pairwiseMax'
  
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$withEstUncert = FALSE
  pacotestOptions2$Grouping = 'TreeERCchi2'
  pacotestOptions2$finalComparison = 'pairwiseMax'
  
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$withEstUncert = FALSE
  pacotestOptions3$Grouping = 'TreeERCchi2'
  pacotestOptions3$finalComparison = 'pairwiseMax'
  
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$withEstUncert = FALSE
  pacotestOptions4$Grouping = 'TreeERCchi2'
  pacotestOptions4$finalComparison = 'pairwiseMax'
  
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$withEstUncert = FALSE
  
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$withEstUncert = FALSE
  
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$withEstUncert = FALSE
  
  pacotestOptions8=pacotestset(TestType='ERC')
  pacotestOptions8$withEstUncert = FALSE
  pacotestOptions8$Grouping = 'TreeERCchi2'
  pacotestOptions8$finalComparison = 'all'
  
  pacotestOptions9=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions9$withEstUncert = FALSE
  pacotestOptions9$Grouping = 'TreeERCchi2'
  pacotestOptions9$finalComparison = 'all'
  
  pacotestOptions10=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions10$withEstUncert = FALSE
  pacotestOptions10$Grouping = 'TreeERCchi2'
  pacotestOptions10$finalComparison = 'all'
  
  pacotestOptions11=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions11$withEstUncert = FALSE
  pacotestOptions11$Grouping = 'TreeERCchi2'
  pacotestOptions11$finalComparison = 'all'
  
  
  pacotestOptions12=pacotestset(TestType='ERC')
  pacotestOptions12$withEstUncert = FALSE
  pacotestOptions12$Grouping = 'TreeEC'
  pacotestOptions12$finalComparison = 'pairwiseMax'
  
  pacotestOptions13=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions13$withEstUncert = FALSE
  pacotestOptions13$Grouping = 'TreeEC'
  pacotestOptions13$finalComparison = 'pairwiseMax'
  
  pacotestOptions14=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions14$withEstUncert = FALSE
  pacotestOptions14$Grouping = 'TreeEC'
  pacotestOptions14$finalComparison = 'pairwiseMax'
  
  pacotestOptions15=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions15$withEstUncert = FALSE
  pacotestOptions15$Grouping = 'TreeEC'
  pacotestOptions15$finalComparison = 'pairwiseMax'
  
  pacotestOptions16=pacotestset(TestType='ERC')
  pacotestOptions16$withEstUncert = FALSE
  pacotestOptions16$Grouping = 'TreeEC'
  pacotestOptions16$finalComparison = 'all'
  
  pacotestOptions17=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions17$withEstUncert = FALSE
  pacotestOptions17$Grouping = 'TreeEC'
  pacotestOptions17$finalComparison = 'all'
  
  pacotestOptions18=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions18$withEstUncert = FALSE
  pacotestOptions18$Grouping = 'TreeEC'
  pacotestOptions18$finalComparison = 'all'
  
  pacotestOptions19=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions19$withEstUncert = FALSE
  pacotestOptions19$Grouping = 'TreeEC'
  pacotestOptions19$finalComparison = 'all'
  
  
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
  
  ##
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,6.747824521369238937e-12)
  expect_equal(resultData2Test1,0.54837431230034150431)
  expect_equal(resultData3Test1,6.3621330426144595549e-13)
  expect_equal(resultData4Test1,0.19718603557591019015)
  
  ##
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,2.0561330416057899129e-13)
  expect_equal(resultData2Test2,0.4724491699757810137)
  expect_equal(resultData3Test2,8.8887952554017601869e-11)
  expect_equal(resultData4Test2,0.16216672971204970644)
  
  ##
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,3.2751579226442117942e-15)
  expect_equal(resultData2Test3,0.56515797435065617815)
  expect_equal(resultData3Test3,5.1547655033346018172e-13)
  expect_equal(resultData4Test3,0.16719854809316819777)
  
  ##
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,6.7212901910806976957e-13)
  expect_equal(resultData2Test4,0.39351569625630455906)
  expect_equal(resultData3Test4,8.0557782666801358573e-13)
  expect_equal(resultData4Test4,0.2108259163343279563)
  
  ##
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,2.1094237467877974268e-14)
  expect_equal(resultData2Test5,0.38705697988022236267)
  expect_equal(resultData3Test5,0.31096627103168605899)
  expect_equal(resultData4Test5,0.53365617218709471281)
  
  ##
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,0.88737165766259773481)
  expect_equal(resultData2Test6,0.077376748544651263728)
  expect_equal(resultData3Test6,0.81333770215943657078)
  expect_equal(resultData4Test6,0.0043935785665404347711)
  
  ##
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0.89545899920300386921)
  expect_equal(resultData2Test7,0.23908782274707562898)
  expect_equal(resultData3Test7,0.45311029913162337301)
  expect_equal(resultData4Test7,0.037277790614566375105)
  
  
  ##
  resultData1Test8 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions8)$pValue
  resultData2Test8 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions8)$pValue
  resultData3Test8 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions8)$pValue
  resultData4Test8 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions8)$pValue
  
  expect_equal(resultData1Test8,1.5570877920367820479e-13)
  expect_equal(resultData2Test8,0.45283578468966628749)
  expect_equal(resultData3Test8,2.3037127760971998214e-14)
  expect_equal(resultData4Test8,0.21988081013202520619)
  
  ##
  resultData1Test9 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions9)$pValue
  resultData2Test9 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions9)$pValue
  resultData3Test9 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions9)$pValue
  resultData4Test9 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions9)$pValue
  
  expect_equal(resultData1Test9,1.1734502258775592054e-12)
  expect_equal(resultData2Test9,0.50019733787994291596)
  expect_equal(resultData3Test9,1.3259948694610557141e-12)
  expect_equal(resultData4Test9,0.17302613830172214326)
  
  ##
  resultData1Test10 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions10)$pValue
  resultData2Test10 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions10)$pValue
  resultData3Test10 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions10)$pValue
  resultData4Test10 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions10)$pValue
  
  expect_equal(resultData1Test10,4.3298697960381105077e-15)
  expect_equal(resultData2Test10,0.6262659148713292101)
  expect_equal(resultData3Test10,6.1062266354383609723e-16)
  expect_equal(resultData4Test10,0.20561538542137952623)
  
  ##
  resultData1Test11 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions11)$pValue
  resultData2Test11 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions11)$pValue
  resultData3Test11 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions11)$pValue
  resultData4Test11 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions11)$pValue
  
  expect_equal(resultData1Test11,1.1501355423604309181e-12)
  expect_equal(resultData2Test11,0.50161697436848429188)
  expect_equal(resultData3Test11,6.1561866715464930166e-14)
  expect_equal(resultData4Test11,0.19060016502803306393)
  
  ##
  resultData1Test12 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions12)$pValue
  resultData2Test12 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions12)$pValue
  resultData3Test12 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions12)$pValue
  resultData4Test12 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions12)$pValue
  
  expect_equal(resultData1Test12,5.3579363168410054641e-13)
  expect_equal(resultData2Test12,0.21607210966704470945)
  expect_equal(resultData3Test12,0)
  expect_equal(resultData4Test12,0.058652488280924619524)
  
  ##
  resultData1Test13 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions13)$pValue
  resultData2Test13 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions13)$pValue
  resultData3Test13 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions13)$pValue
  resultData4Test13 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions13)$pValue
  
  expect_equal(resultData1Test13,2.3536728122053318657e-14)
  expect_equal(resultData2Test13,0.20835036557912756106)
  expect_equal(resultData3Test13,0)
  expect_equal(resultData4Test13,0.078094646254984190215)
  
  ##
  resultData1Test14 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions14)$pValue
  resultData2Test14 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions14)$pValue
  resultData3Test14 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions14)$pValue
  resultData4Test14 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions14)$pValue
  
  expect_equal(resultData1Test14,2.2285506773300767236e-12)
  expect_equal(resultData2Test14,0.63563310162250197255)
  expect_equal(resultData3Test14,9.3888154356625008745e-06)
  expect_equal(resultData4Test14,0.073643213589614786585)
  
  ##
  resultData1Test15 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions15)$pValue
  resultData2Test15 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions15)$pValue
  resultData3Test15 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions15)$pValue
  resultData4Test15 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions15)$pValue
  
  expect_equal(resultData1Test15,1.2017054018542694394e-12)
  expect_equal(resultData2Test15,0.48908837759476631035)
  expect_equal(resultData3Test15,2.3758772726978349965e-14)
  expect_equal(resultData4Test15,0.40437081150486697823)
  
  ##
  resultData1Test16 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions16)$pValue
  resultData2Test16 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions16)$pValue
  resultData3Test16 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions16)$pValue
  resultData4Test16 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions16)$pValue
  
  expect_equal(resultData1Test16,2.2665203047722570773e-12)
  expect_equal(resultData2Test16,0.49813093314971479408)
  expect_equal(resultData3Test16,0)
  expect_equal(resultData4Test16,0.057388060064764823132)
  
  ##
  resultData1Test17 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions17)$pValue
  resultData2Test17 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions17)$pValue
  resultData3Test17 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions17)$pValue
  resultData4Test17 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions17)$pValue
  
  expect_equal(resultData1Test17,1.1310952174881094834e-12)
  expect_equal(resultData2Test17,0.96223367368371715003)
  expect_equal(resultData3Test17,0)
  expect_equal(resultData4Test17,0.09024584529879253747)
  
  ##
  resultData1Test18 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions18)$pValue
  resultData2Test18 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions18)$pValue
  resultData3Test18 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions18)$pValue
  resultData4Test18 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions18)$pValue
  
  expect_equal(resultData1Test18,7.2897365921420487211e-09)
  expect_equal(resultData2Test18,0.78577717543843506043)
  expect_equal(resultData3Test18,8.5703066520226300895e-06)
  expect_equal(resultData4Test18,0.034649092929630698201)
  
  ##
  resultData1Test19 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions19)$pValue
  resultData2Test19 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions19)$pValue
  resultData3Test19 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions19)$pValue
  resultData4Test19 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions19)$pValue
  
  expect_equal(resultData1Test19,2.0012340673503103972e-09)
  expect_equal(resultData2Test19,0.21054596063108055315)
  expect_equal(resultData3Test19,0)
  expect_equal(resultData4Test19,0.25564293974175689161)
  
  
  
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
  
  pacotestOptions1$withEstUncert = TRUE
  pacotestOptions2$withEstUncert = TRUE
  pacotestOptions3$withEstUncert = TRUE
  pacotestOptions4$withEstUncert = TRUE
  pacotestOptions5$withEstUncert = TRUE
  pacotestOptions6$withEstUncert = TRUE
  pacotestOptions7$withEstUncert = TRUE
  pacotestOptions8$withEstUncert = TRUE
  pacotestOptions9$withEstUncert = TRUE
  pacotestOptions10$withEstUncert = TRUE
  pacotestOptions11$withEstUncert = TRUE
  pacotestOptions12$withEstUncert = TRUE
  pacotestOptions13$withEstUncert = TRUE
  pacotestOptions14$withEstUncert = TRUE
  pacotestOptions15$withEstUncert = TRUE
  pacotestOptions16$withEstUncert = TRUE
  pacotestOptions17$withEstUncert = TRUE
  pacotestOptions18$withEstUncert = TRUE
  pacotestOptions19$withEstUncert = TRUE
  
  pacotestOptions1$AggPvalsNumbRep = 1
  pacotestOptions2$AggPvalsNumbRep = 1
  pacotestOptions3$AggPvalsNumbRep = 1
  pacotestOptions4$AggPvalsNumbRep = 1
  pacotestOptions8$AggPvalsNumbRep = 1
  pacotestOptions9$AggPvalsNumbRep = 1
  pacotestOptions10$AggPvalsNumbRep = 1
  pacotestOptions11$AggPvalsNumbRep = 1
  pacotestOptions12$AggPvalsNumbRep = 1
  pacotestOptions13$AggPvalsNumbRep = 1
  pacotestOptions14$AggPvalsNumbRep = 1
  pacotestOptions15$AggPvalsNumbRep = 1
  pacotestOptions16$AggPvalsNumbRep = 1
  pacotestOptions17$AggPvalsNumbRep = 1
  pacotestOptions18$AggPvalsNumbRep = 1
  pacotestOptions19$AggPvalsNumbRep = 1
  
  
  
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
  expect_equal(resultData6Test12,0.76782331184175245387)
  expect_equal(resultData6Test13,0.81664648835198594412)
  expect_equal(resultData6Test14,0.63829843150255993756)
  expect_equal(resultData6Test15,0.077304754980899170747)
  expect_equal(resultData6Test16,0.89249136445633692194)
  expect_equal(resultData6Test17,0.37535315122586121461)
  expect_equal(resultData6Test18,0.21765036986132035857)
  expect_equal(resultData6Test19,0.021845774625410707692)
  
  
})

