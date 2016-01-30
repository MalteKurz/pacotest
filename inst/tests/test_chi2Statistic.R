context("chi2 test statistic")

test_that("unit tests for ERC", {
  
  library("pacotest")
  library("testthat")
  library("VineCopula")
  library("numDeriv")
  library('MCMCpack')
  library('fBasics')
  
  
  set.seed(3141)
  
  
  N = 1034 # Sample size
  
  tau = 0.5
  
  # Four-dimensional Clayton copula (D-vine)
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
  
  U = RVineSim(N,rvm)
  rvmHat = RVineSeqEst(U,rvm)$RVM
  
  svcmDataFrameHat = pacotest:::rVineDataFrameRep(rvmHat)$variables
  
  ind = matrix(NA,N,2)
  ind[,1] = c(rep(TRUE,N/2),rep(FALSE,N/2))
  ind[,2] = !ind[,1]
  
  res = pacotest:::testStatEqualCorrWithoutEstimation(U, svcmDataFrameHat, ind)
  resWithEstimation = pacotest:::testStatEqualCorrWithEstimation(U, svcmDataFrameHat, ind)
  
  
  expect_equal(res$testStat,0.019913692476495194195)
  expect_equal(res$pValue,0.88777839627590227067)
  
  expect_equal(resWithEstimation$testStat,0.017573566369596227421)
  expect_equal(resWithEstimation$pValue,0.89453715405386580173)
  
  
  ind = matrix(NA,N,3)
  ind[,1] = c(rep(TRUE,500),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,500),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,500),rep(FALSE,331),rep(TRUE,N-831))
  
  
  res = pacotest:::testStatEqualCorrWithoutEstimation(U, svcmDataFrameHat, ind)
  resWithEstimation = pacotest:::testStatEqualCorrWithEstimation(U, svcmDataFrameHat, ind)
  
  expect_equal(res$testStat,0.048459334734335994532)
  expect_equal(res$pValue,0.97606151453549416797)
  
  expect_equal(resWithEstimation$testStat,0.045530358645845779575)
  expect_equal(resWithEstimation$pValue,0.97749199215700122156)
  
  
  ind = matrix(NA,N,4)
  ind[,1] = c(rep(TRUE,300),rep(FALSE,200),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,300),rep(FALSE,200),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,300),rep(FALSE,200),rep(FALSE,331),rep(TRUE,N-831))
  ind[,4] = c(rep(FALSE,300),rep(TRUE,200),rep(FALSE,331),rep(FALSE,N-831))
  
  
  res = pacotest:::testStatEqualCorrWithoutEstimation(U, svcmDataFrameHat, ind)
  resWithEstimation = pacotest:::testStatEqualCorrWithEstimation(U, svcmDataFrameHat, ind)
  
  expect_equal(res$testStat,0.055479327317821756915)
  expect_equal(res$pValue,0.99658178997021218404)
  
  expect_equal(resWithEstimation$testStat,0.05125015765542074303)
  expect_equal(resWithEstimation$pValue,0.99696125608624075465)
  
  
  
  set.seed(31412131)
  # Data set 2
  N= 1756
  # Four-dimensional D-vine
  structure = matrix(c(4,0,0,0,
                       1,3,2,0,
                       2,1,2,0,
                       3,2,1,1),4,4,TRUE)
  families  = matrix(c(0,0,0,0,
                       5,0,0,0,
                       1,1,0,0,
                       2,2,2,0),4,4,TRUE)
  par2  = matrix(c(0,0,0,0,
                   0,0,0,0,
                   0,0,0,0,
                   3,8,6,0),4,4,TRUE)
  par  = matrix(c(0,0,0,0,
                  5,0,0,0,
                  0.4,0.3,0,0,
                  0.5,0.8,0.6,0),4,4,TRUE)
  names = c("V1", "V2", "V3","V4")
  
  rvm2 = RVineMatrix(structure,families,par,par2,names)
  rvm2 = RVineMatrixNormalize(rvm2)
  
  U2 = RVineSim(N,rvm2)
  rvmHat2 = RVineSeqEst(U2,rvm2)$RVM
  
  svcmDataFrameHat2 = pacotest:::rVineDataFrameRep(rvmHat2)$variables
  
  ind = matrix(NA,N,2)
  ind[,1] = c(rep(TRUE,N/2),rep(FALSE,N/2))
  ind[,2] = !ind[,1]
  
  res2 = pacotest:::testStatEqualCorrWithoutEstimation(U2, svcmDataFrameHat2, ind)
  resWithEstimation2 = pacotest:::testStatEqualCorrWithEstimation(U2, svcmDataFrameHat2, ind)
  
  
  expect_equal(res2$testStat,0.039978469586072304387)
  expect_equal(res2$pValue,0.84152268356999970766)
  
  expect_equal(resWithEstimation2$testStat,0.016783546044932356328)
  expect_equal(resWithEstimation2$pValue,0.89692141488942633831)
  
  
  ind = matrix(NA,N,3)
  ind[,1] = c(rep(TRUE,500),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,500),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,500),rep(FALSE,331),rep(TRUE,N-831))
  
  res2 = pacotest:::testStatEqualCorrWithoutEstimation(U2, svcmDataFrameHat2, ind)
  resWithEstimation2 = pacotest:::testStatEqualCorrWithEstimation(U2, svcmDataFrameHat2, ind)
  
  
  expect_equal(res2$testStat,0.069919838268819423099)
  expect_equal(res2$pValue,0.96564411933409000088)
  
  expect_equal(resWithEstimation2$testStat,0.031277987879511671976)
  expect_equal(resWithEstimation2$pValue,0.98448266011797580521)
  
  
  ind = matrix(NA,N,4)
  ind[,1] = c(rep(TRUE,300),rep(FALSE,200),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,300),rep(FALSE,200),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,300),rep(FALSE,200),rep(FALSE,331),rep(TRUE,N-831))
  ind[,4] = c(rep(FALSE,300),rep(TRUE,200),rep(FALSE,331),rep(FALSE,N-831))
  
  res2 = pacotest:::testStatEqualCorrWithoutEstimation(U2, svcmDataFrameHat2, ind)
  resWithEstimation2 = pacotest:::testStatEqualCorrWithEstimation(U2, svcmDataFrameHat2, ind)
  
  
  expect_equal(res2$testStat,0.070925651334994108121)
  expect_equal(res2$pValue,0.99508185005149674129)
  
  expect_equal(resWithEstimation2$testStat,0.031782253825141076886)
  expect_equal(resWithEstimation2$pValue,0.99850734737477131464)
  
})

