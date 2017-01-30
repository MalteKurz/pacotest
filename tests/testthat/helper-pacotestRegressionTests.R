library("pacotest")
library("testthat")
library("VineCopula")
library("numDeriv")
library("methods")

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
par_firstTree = VineCopula::BiCopTau2Par(3, 0.5)
par_secondTree = par_firstTree/(1+par_firstTree)
par_thirdTree = par_firstTree/(1+2*par_firstTree)
par  = matrix(c(0,0,0,0,
                par_thirdTree,0,0,0,
                par_secondTree,par_secondTree,0,0,
                par_firstTree,par_firstTree,par_firstTree,0),4,4,TRUE)

rvm = VineCopula::RVineMatrix(structure,families,par,par2,names)
rvm = VineCopula::RVineMatrixNormalize(rvm)
svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)

U = VineCopula::RVineSim(N,rvm)

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

rvm = VineCopula::RVineMatrix(structure,families,par,par2,names)
rvm = VineCopula::RVineMatrixNormalize(rvm)
svcmDataFrame = pacotest:::rVineDataFrameRep(rvm)

U = VineCopula::RVineSim(N,rvm)

# Compute CPITs for the whole vine
cPitData = pacotest:::getCpitsFromVine(U, svcmDataFrame)

# Obtain the cPits to be tested
copulaInd = nrow(svcmDataFrame)

cPit1 = pacotest:::getCpit1(cPitData, svcmDataFrame, copulaInd)
cPit2 = pacotest:::getCpit2(cPitData, svcmDataFrame, copulaInd)


Udata = cbind(cPit1,cPit2)
W = U[,svcmDataFrame$condset[[copulaInd]]]



pacotestForData1_4 <- function(data1, data2, data3, data4, pacotestOptions)
{
  res = vector('numeric', length=4)
  
  res[1] = pacotest(data1[,c(2,3)],data1[,1],pacotestOptions)$pValue
  res[2] = pacotest(data2[,c(1,5)],data2[,c(2,3,4)],pacotestOptions)$pValue
  res[3] = pacotest(data3[,c(2,3)],data3[,1],pacotestOptions)$pValue
  res[4] = pacotest(data4[,c(2,3)],data4[,1],pacotestOptions)$pValue
  
  return(res)
  
}


