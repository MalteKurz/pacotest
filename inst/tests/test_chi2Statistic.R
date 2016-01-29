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
  
})

