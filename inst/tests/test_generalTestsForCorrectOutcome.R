context("general tests for correct outcome")

test_that("unit tests for ERC", {
  
  library("pacotest")
  library("testthat")
  
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
  
  
  # Define first test type
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$ERCtype = 'standard'
  
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,4.561482302989361e-10)
  expect_equal(resultData2Test1,0.4919463098622998)
  expect_equal(resultData3Test1,0.0002067850512569303)
  expect_equal(resultData4Test1,0.1126485299039938)
  
  # Define second test type
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$ERCtype = 'standard'
  
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,1.928416648588893e-09)
  expect_equal(resultData2Test2,0.4752841276010049)
  expect_equal(resultData3Test2,0.000186806965688846)
  expect_equal(resultData4Test2,0.09829836082764432)
  
  # Define third test type
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$ERCtype = 'standard'
  
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,6.172173883101095e-12)
  expect_equal(resultData2Test3,0.505947292841025)
  expect_equal(resultData3Test3,1.849904748330022e-05)
  expect_equal(resultData4Test3,0.07255508028022073)
  
  # Define fourth test type
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$ERCtype = 'standard'
  
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,7.028754245297364e-10)
  expect_equal(resultData2Test4,0.4050238674964468)
  expect_equal(resultData3Test4,0.0001961734309284813)
  expect_equal(resultData4Test4,0.06624575270482191)
  
  # Define fifth test type
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$ERCtype = 'standard'
  
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,5.88418203051333e-14)
  expect_equal(resultData2Test5,0.362074701139534)
  expect_equal(resultData3Test5,0.03034480602226375)
  expect_equal(resultData4Test5,0.8809929353872938)
  
  # Define sixth test type
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$ERCtype = 'standard'
  
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,0.9162881137898811)
  expect_equal(resultData2Test6,0.07789799924404761)
  expect_equal(resultData3Test6,0.5290482309555449)
  expect_equal(resultData4Test6,0.014736328867194)
  
  # Define seventh test type
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$ERCtype = 'standard'
  
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0.951273639467054)
  expect_equal(resultData2Test7,0.2393838650132822)
  expect_equal(resultData3Test7,0.6486563896539215)
  expect_equal(resultData4Test7,0.1209385404840901)
  
  
  
  
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
  par_firstTree = BiCopTau2Par(3, tau)
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
  
  
  
  resultData5Test1 = pacotest(Udata,W,pacotestOptions1)$pValue
  resultData5Test2 = pacotest(Udata,W,pacotestOptions2)$pValue
  resultData5Test3 = pacotest(Udata,W,pacotestOptions3)$pValue
  resultData5Test4 = pacotest(Udata,W,pacotestOptions4)$pValue
  resultData5Test5 = pacotest(Udata,W,pacotestOptions5)$pValue
  resultData5Test6 = pacotest(Udata,W,pacotestOptions6)$pValue
  resultData5Test7 = pacotest(Udata,W,pacotestOptions7)$pValue
  
  expect_equal(resultData5Test1,0.59698433554410501589)
  expect_equal(resultData5Test2,0.5108486612543823302)
  expect_equal(resultData5Test3,0.53326556795685631229)
  expect_equal(resultData5Test4,0.5569561099109747726)
  expect_equal(resultData5Test5,0.68172357330616750737)
  expect_equal(resultData5Test6,0.023852352740848070667)
  expect_equal(resultData5Test7,0.18236630813887733105)
  
})

