context("Regression tests for pacotestRvineSingleCopula")

# test_that("pacotestRvineSingleCopula1", {
#   
#   properties = c('withEstUncert')
#   values = c('TRUE')
#   thisTestsInd = filterPacotestOptionLists(pacotestOptions, properties, values)
#   
#   
#   maxNTests = length(pacotestOptions)
#   
#   resPacotestComputed = matrix(NA, maxNTests, 1)
#   
#   for (iTest in thisTestsInd)
#   {
#     set.seed(seedsPerTest[iTest])
#     resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
#     
#     expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
#     
#   }
#   
# })


test_that("pacotestRvineSingleCopula2", {
  
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

rvmHat = RVineSeqEst(U,rvm)

maxNTests = length(pacotestOptions)

resPacotestComputed = matrix(NA, maxNTests, 1)

for (iTest in c(71, 75:95))
{
  set.seed(seedsPerTest[iTest])
  resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5, rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
  
  expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
  
}

for (iTest in 72:74)
{
  set.seed(seedsPerTest[iTest])
  resPacotestComputed[iTest,1] = pacotestRvineSingleCopula(data5[1:341,], rvmHatData5, pacotestOptions[[iTest]], 3, 1)$pValue
  
  expect_equal(resPacotestComputed[iTest,1],hardCodedResPacotestRvineSingleCopula[iTest,1], tolerance = 1e-2)
  
}



})
