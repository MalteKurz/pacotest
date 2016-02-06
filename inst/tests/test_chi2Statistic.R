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
  
  
  expect_equal(res$testStat,0.64034211409376129431)
  expect_equal(res$pValue,0.4235869398574387823)
  
  expect_equal(resWithEstimation$testStat,0.56509311895736236142)
  expect_equal(resWithEstimation$pValue,0.45221539535817567579)
  
  
  ind = matrix(NA,N,3)
  ind[,1] = c(rep(TRUE,500),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,500),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,500),rep(FALSE,331),rep(TRUE,N-831))
  
  
  res = pacotest:::testStatEqualCorrWithoutEstimation(U, svcmDataFrameHat, ind)
  resWithEstimation = pacotest:::testStatEqualCorrWithEstimation(U, svcmDataFrameHat, ind)
  
  expect_equal(res$testStat,1.5582520864971858288)
  expect_equal(res$pValue,0.45880681345004847849)
  
  expect_equal(resWithEstimation$testStat,1.4640687567395958002)
  expect_equal(resWithEstimation$pValue,0.4809296014258914731)
  
  
  ind = matrix(NA,N,4)
  ind[,1] = c(rep(TRUE,300),rep(FALSE,200),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,300),rep(FALSE,200),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,300),rep(FALSE,200),rep(FALSE,331),rep(TRUE,N-831))
  ind[,4] = c(rep(FALSE,300),rep(TRUE,200),rep(FALSE,331),rep(FALSE,N-831))
  
  
  res = pacotest:::testStatEqualCorrWithoutEstimation(U, svcmDataFrameHat, ind)
  resWithEstimation = pacotest:::testStatEqualCorrWithEstimation(U, svcmDataFrameHat, ind)
  
  expect_equal(res$testStat,1.7839860580917361599)
  expect_equal(res$pValue,0.61842593405983881105)
  
  expect_equal(resWithEstimation$testStat,1.6479933841949565565)
  expect_equal(resWithEstimation$pValue,0.64855725616395010213)
  
  
  
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
  
  
  expect_equal(res2$testStat,1.6752839226413402951)
  expect_equal(res2$pValue,0.19555228871052821038)
  
  expect_equal(resWithEstimation2$testStat,0.70330445282512621397)
  expect_equal(resWithEstimation2$pValue,0.40167557291709143907)
  
  
  ind = matrix(NA,N,3)
  ind[,1] = c(rep(TRUE,500),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,500),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,500),rep(FALSE,331),rep(TRUE,N-831))
  
  res2 = pacotest:::testStatEqualCorrWithoutEstimation(U2, svcmDataFrameHat2, ind)
  resWithEstimation2 = pacotest:::testStatEqualCorrWithEstimation(U2, svcmDataFrameHat2, ind)
  
  
  expect_equal(res2$testStat,2.9299666079825024134)
  expect_equal(res2$pValue,0.23108184958970501288)
  
  expect_equal(resWithEstimation2$testStat,1.3106905960240724518)
  expect_equal(resWithEstimation2$pValue,0.51926273123862976)
  
  
  ind = matrix(NA,N,4)
  ind[,1] = c(rep(TRUE,300),rep(FALSE,200),rep(FALSE,N-500))
  ind[,2] = c(rep(FALSE,300),rep(FALSE,200),rep(TRUE,331),rep(FALSE,N-831))
  ind[,3] = c(rep(FALSE,300),rep(FALSE,200),rep(FALSE,331),rep(TRUE,N-831))
  ind[,4] = c(rep(FALSE,300),rep(TRUE,200),rep(FALSE,331),rep(FALSE,N-831))
  
  res2 = pacotest:::testStatEqualCorrWithoutEstimation(U2, svcmDataFrameHat2, ind)
  resWithEstimation2 = pacotest:::testStatEqualCorrWithEstimation(U2, svcmDataFrameHat2, ind)
  
  
  expect_equal(res2$testStat,2.9721148561868826476)
  expect_equal(res2$pValue,0.3959445289661274181)
  
  expect_equal(resWithEstimation2$testStat,1.331820494912132169)
  expect_equal(resWithEstimation2$pValue,0.72159121006110882313)
  
})

