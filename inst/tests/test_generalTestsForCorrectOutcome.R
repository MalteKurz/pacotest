context("general tests for correct outcome")

test_that("unit tests for ERC", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  library('MCMCpack')
  library('fBasics')
  
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
  
  
  
  resultData5Test1 = pacotest(Udata,W,pacotestOptions1)$pValue
  resultData5Test2 = pacotest(Udata,W,pacotestOptions2)$pValue
  resultData5Test3 = pacotest(Udata,W,pacotestOptions3)$pValue
  resultData5Test4 = pacotest(Udata,W,pacotestOptions4)$pValue
  resultData5Test5 = pacotest(Udata,W,pacotestOptions5)$pValue
  resultData5Test6 = pacotest(Udata,W,pacotestOptions6)$pValue
  resultData5Test7 = pacotest(Udata,W,pacotestOptions7)$pValue
  
  expect_equal(resultData5Test1,0.57379803160849718324)
  expect_equal(resultData5Test2,0.55423774339339271222)
  expect_equal(resultData5Test3,0.52065711751478249703)
  expect_equal(resultData5Test4,0.51525339058554542326)
  expect_equal(resultData5Test5,0.19100677218125783341)
  expect_equal(resultData5Test6,0.37219710113708792676)
  expect_equal(resultData5Test7,0.39406928943229990736)
  
  
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
  
  
  
  resultData6Test1 = pacotest(Udata,W,pacotestOptions1)$pValue
  resultData6Test2 = pacotest(Udata,W,pacotestOptions2)$pValue
  resultData6Test3 = pacotest(Udata,W,pacotestOptions3)$pValue
  resultData6Test4 = pacotest(Udata,W,pacotestOptions4)$pValue
  resultData6Test5 = pacotest(Udata,W,pacotestOptions5)$pValue
  resultData6Test6 = pacotest(Udata,W,pacotestOptions6)$pValue
  resultData6Test7 = pacotest(Udata,W,pacotestOptions7)$pValue
  
  expect_equal(resultData6Test1,0.45002852538496596058)
  expect_equal(resultData6Test2,0.52134078346324663755)
  expect_equal(resultData6Test3,0.48119761795612780997)
  expect_equal(resultData6Test4,0.55648280932661886578)
  expect_equal(resultData6Test5,0.52871876986336929782)
  expect_equal(resultData6Test6,0.93462247149359378717)
  expect_equal(resultData6Test7,0.27515467935462911697)
  
  
  # Define eight test type
  pacotestOptions8=pacotestset(TestType='ERC',Grouping = "TreeEC")
  pacotestOptions8$ERCtype = 'standard'
  
  resultData1Test8 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions8)$pValue
  resultData2Test8 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions8)$pValue
  resultData3Test8 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions8)$pValue
  resultData4Test8 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions8)$pValue
  
  expect_equal(resultData1Test8,3.9570671184208094928e-09)
  expect_equal(resultData2Test8,0.73256838088250231245)
  expect_equal(resultData3Test8,0.00084414989453640565387)
  expect_equal(resultData4Test8,0.04445133985730387316)
  
  set.seed(111)
  
  # Define first test type
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$ERCtype = 'oracle'
  
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,2.6206814496276820137e-12)
  expect_equal(resultData2Test1,0.48585234983484870686)
  expect_equal(resultData3Test1,5.6399396264339429763e-10)
  expect_equal(resultData4Test1,0.15378496665734620041)
  
  # Define second test type
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$ERCtype = 'oracle'
  
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,4.8794301932275629952e-13)
  expect_equal(resultData2Test2,0.50644225374625595482)
  expect_equal(resultData3Test2,9.3526453248671259644e-10)
  expect_equal(resultData4Test2,0.12894395393968893782)
  
  # Define third test type
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$ERCtype = 'oracle'
  
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,1.6653345369377348106e-15)
  expect_equal(resultData2Test3,0.60178730623559784085)
  expect_equal(resultData3Test3,6.8174355050132362521e-12)
  expect_equal(resultData4Test3,0.12084577556887388106)
  
  # Define fourth test type
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$ERCtype = 'oracle'
  
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,1.0563772079308364482e-12)
  expect_equal(resultData2Test4,0.45688453651817906298)
  expect_equal(resultData3Test4,5.9894911252911242627e-10)
  expect_equal(resultData4Test4,0.14183362174306801684)
  
  # Define fifth test type
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$ERCtype = 'oracle'
  
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,1.7541523789077473339e-14)
  expect_equal(resultData2Test5,0.38295418902432443176)
  expect_equal(resultData3Test5,0.029280252382309468473)
  expect_equal(resultData4Test5,0.98046244733278564709)
  
  # Define sixth test type
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$ERCtype = 'oracle'
  
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,0.84162174591100802346)
  expect_equal(resultData2Test6,0.071552479354619835661)
  expect_equal(resultData3Test6,0.55778889977942291978)
  expect_equal(resultData4Test6,0.021653271668211626633)
  
  # Define seventh test type
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$ERCtype = 'oracle'
  
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0.9209692274109746446)
  expect_equal(resultData2Test7,0.22344162181455740068)
  expect_equal(resultData3Test7,0.68096584974330975903)
  expect_equal(resultData4Test7,0.10258495090255403959)
  
  
  
  set.seed(1991)
  
  # Define first test type
  pacotestOptions1=pacotestset(TestType='ERC')
  pacotestOptions1$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test1 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions1)$pValue
  resultData2Test1 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions1)$pValue
  resultData3Test1 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions1)$pValue
  resultData4Test1 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions1)$pValue
  
  expect_equal(resultData1Test1,2.9642954757491679629e-13)
  expect_equal(resultData2Test1,0.47899678217504670918)
  expect_equal(resultData3Test1,6.9124361790073862721e-10)
  expect_equal(resultData4Test1,0.11604648921065396916)
  
  # Define second test type
  pacotestOptions2=pacotestset(TestType='ERC',ExpMinSampleSize=56)
  pacotestOptions2$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test2 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions2)$pValue
  resultData2Test2 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions2)$pValue
  resultData3Test2 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions2)$pValue
  resultData4Test2 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions2)$pValue
  
  expect_equal(resultData1Test2,3.9312997301976793096e-13)
  expect_equal(resultData2Test2,0.51757187632552470458)
  expect_equal(resultData3Test2,6.9972971861176347375e-10)
  expect_equal(resultData4Test2,0.14607469526678751315)
  
  # Define third test type
  pacotestOptions3=pacotestset(TestType='ERC',TrainingDataFraction=0.34)
  pacotestOptions3$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test3 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions3)$pValue
  resultData2Test3 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions3)$pValue
  resultData3Test3 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions3)$pValue
  resultData4Test3 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions3)$pValue
  
  expect_equal(resultData1Test3,8.4376949871511897072e-15)
  expect_equal(resultData2Test3,0.62315954562301456576)
  expect_equal(resultData3Test3,2.2178925362936752208e-12)
  expect_equal(resultData4Test3,0.13535577276547461345)
  
  # Define fourth test type
  pacotestOptions4=pacotestset(TestType='ERC',aggInfo="meanPairwise")
  pacotestOptions4$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test4 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions4)$pValue
  resultData2Test4 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions4)$pValue
  resultData3Test4 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions4)$pValue
  resultData4Test4 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions4)$pValue
  
  expect_equal(resultData1Test4,5.4045656838752620388e-13)
  expect_equal(resultData2Test4,0.55064513931344583053)
  expect_equal(resultData3Test4,7.9331485824951641916e-10)
  expect_equal(resultData4Test4,0.12067722140480685589)
  
  # Define fifth test type
  pacotestOptions5=pacotestset(TestType='ERC',Grouping = "SumMedian",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions5$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test5 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions5)$pValue
  resultData2Test5 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions5)$pValue
  resultData3Test5 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions5)$pValue
  resultData4Test5 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions5)$pValue
  
  expect_equal(resultData1Test5,1.7541523789077473339e-14)
  expect_equal(resultData2Test5,0.38295418902432443176)
  expect_equal(resultData3Test5,0.029280252382309468473)
  expect_equal(resultData4Test5,0.98046244733278564709)
  
  # Define sixth test type
  pacotestOptions6=pacotestset(TestType='ERC',Grouping = "ProdThirdsI",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions6$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test6 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions6)$pValue
  resultData2Test6 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions6)$pValue
  resultData3Test6 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions6)$pValue
  resultData4Test6 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions6)$pValue
  
  expect_equal(resultData1Test6,0.84162174591100802346)
  expect_equal(resultData2Test6,0.071552479354619835661)
  expect_equal(resultData3Test6,0.55778889977942291978)
  expect_equal(resultData4Test6,0.021653271668211626633)
  
  # Define seventh test type
  pacotestOptions7=pacotestset(TestType='ERC',Grouping = "SumThirdsII",
                               ExpMinSampleSize = NULL, TrainingDataFraction = NULL,
                               AggPvalsNumbRep = NULL, aggInfo = NULL)
  pacotestOptions7$ERCtype = 'oracle'
  pacotestOptions1$Grouping = 'TreeERCchi2'
  
  resultData1Test7 = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions7)$pValue
  resultData2Test7 = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions7)$pValue
  resultData3Test7 = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions7)$pValue
  resultData4Test7 = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions7)$pValue
  
  expect_equal(resultData1Test7,0.9209692274109746446)
  expect_equal(resultData2Test7,0.22344162181455740068)
  expect_equal(resultData3Test7,0.68096584974330975903)
  expect_equal(resultData4Test7,0.10258495090255403959)
  
})

