

covOfCorrelationsWithEstimation = function(data, svcmDataFrame, ind, cPitData, theta)
{
  
  #indexCombinations = getIndexCombinations(RVM)
  
  gInv = gInvRvine(data, svcmDataFrame, ind, cPitData, theta)
  omega = omegaRvine(data, svcmDataFrame, ind, cPitData, theta)
  
  varMat = gInv %*% omega %*% t(gInv)
  
  return(varMat)
}


getMatrixForPairwiseComparison = function(nGroups)
{
  # Obtain linear transformation matrix used to compute pairwise differences
  if (nGroups == 2)
  {
    A = matrix(c(1,-1),1,2,TRUE)
  }
  else if (nGroups == 3)
  {
    A = matrix(c(1,-1,0,
                 1,0,-1),2,3,TRUE)
  }
  else if (nGroups == 4)
  {
    A = matrix(c(1,-1,0,0,
                 0,1,-1,0,
                 0,0,1,-1),3,4,TRUE)
  }
  
  return(A)
  
}

testStatEqualCorrWithEstimation = function(data, svcmDataFrame, ind)
{
  d <- ncol(data)
  # Check whether the transferred indicator matrix is a valid grouping indicator matrix
  ind = as.matrix(ind)
  nGroups = ncol(ind)
  if (nGroups < 2 ) 
    stop("At least two groups have to be specified.")
  if (nGroups > 4 ) 
    stop("The maximum number of groups is 4.")
  if (dim(data)[1] != dim(ind)[1]) 
    stop("Dimensions of 'data' and 'ind' do not match.")
  if (any(rowSums(ind)>1))
    stop("Non disjunct groups are not implemented.")
  
  ind = (ind == 1) # transfer the possibly numeric matrix into a matrix of logicals
    
  
  nGroups = ncol(ind)
  nObs = nrow(data)
  
  # Compute CPITs for the whole vine
  cPitData = getCpitsFromVine(data, svcmDataFrame)
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Compute the parameters solving the estimation equations
  theta = vector(length=(4+nGroups))
  nObsPerGroup = vector(length=nGroups)
  
  theta[1] = mean(cPit1) # Mu for the first CPIT
  theta[2] = mean((cPit1-theta[1])^2) # Variance for the first CPIT
  
  theta[3] = mean(cPit2) # Mu for the second CPIT
  theta[4] = mean((cPit2-theta[3])^2) # Variance for the second CPIT
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[ind[,iGroup]]
    cPit2InGroup = cPit2[ind[,iGroup]]
    
    nObsPerGroup[iGroup] = length(cPit1InGroup)
    
    rhoInGroup = mean((cPit1InGroup-theta[1])*(cPit2InGroup-theta[3])/sqrt(theta[2]*theta[4])) # Rho in the group
    
    # Place the correlation parameters in the parameter vector
    theta[(4+iGroup)] = rhoInGroup
    
  }
  
  lambdas = nObsPerGroup/nObs
  
  # Obtain the variance-covariance matrix with estimation uncertainty
  sigma = covOfCorrelationsWithEstimation(data, svcmDataFrame, ind, cPitData, theta)
  
  nCol = ncol(sigma)
  
  sigmaRhos = sigma[(nCol-nGroups+1):nCol,(nCol-nGroups+1):nCol]
  rhos = as.matrix(theta[5:(4+nGroups)],1,nGroups)
  
  A = getMatrixForPairwiseComparison(nGroups)
  
  testStat = sqrt(nObs)*(t(A%*%rhos) %*% solve(A %*% sigmaRhos %*% t(A)) %*% A%*%rhos)
  
  pValue = 1 - pchisq(testStat,nrow(A))
  
  return(list(testStat = testStat, pValue = pValue, theta = theta, sigma = sigma))
  
}


testStatEqualCorrWithoutEstimation = function(data, svcmDataFrame, ind)
{
  d <- ncol(data)
  # Check whether the transferred indicator matrix is a valid grouping indicator matrix
  ind = as.matrix(ind)
  nGroups = ncol(ind)
  if (nGroups < 2 ) 
    stop("At least two groups have to be specified.")
  if (nGroups > 4 ) 
    stop("The maximum number of groups is 4.")
  if (dim(data)[1] != dim(ind)[1]) 
    stop("Dimensions of 'data' and 'ind' do not match.")
  if (any(rowSums(ind)>1))
    stop("Non disjunct groups are not implemented.")
  
  
  nGroups = ncol(ind)
  nObs = nrow(data)
  
  # Compute CPITs for the whole vine
  cPitData = getCpitsFromVine(data, svcmDataFrame)
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
# 
#   
#   # Compute the parameters solving the estimation equations
#   theta = vector(length=(4+nGroups))
#   nObsPerGroup = vector(length=nGroups)
#   
#   theta[1] = mean(cPit1) # Mu for the first CPIT
#   theta[2] = mean((cPit1-theta[1])^2) # Variance for the first CPIT
#   
#   theta[3] = mean(cPit2) # Mu for the second CPIT
#   theta[4] = mean((cPit2-theta[3])^2) # Variance for the second CPIT
#   
#   # Obtain the variance-covariance matrix without estimation uncertainty
#   sigma = matrix(0,4+nGroups,4+nGroups)
#   
#   # Variance of the mean estimators
#   sigma[1,1] = theta[2]
#   sigma[3,3] = theta[4]
#   
#   # Variance of the variance estimators
#   sigma[2,2] = mean((cPit1-theta[1])^4) - theta[2]^2
#   sigma[4,4] = mean((cPit2-theta[3])^4) - theta[4]^2
#   
#   for (iGroup in 1:nGroups)
#   {
#     # Obtain the subsample
#     cPit1InGroup = cPit1[ind[,iGroup]]
#     cPit2InGroup = cPit2[ind[,iGroup]]
#     
#     # Obtain standardized CPITs
#     cPit1InGroupStandardized = (cPit1InGroup-theta[1])/sqrt(theta[2])
#     cPit2InGroupStandardized = (cPit2InGroup-theta[3])/sqrt(theta[4])
#     
#     nObsInGroup = length(cPit1InGroup)
#     
#     rhoInGroup = mean((cPit1InGroup-theta[1])*(cPit2InGroup-theta[3])/sqrt(theta[2]*theta[4])) # Rho in the group
#     
#     # Place the correlation parameters in the parameter vector
#     theta[(4+iGroup)] = rhoInGroup
#     
#     lambdaInGroup = nObsInGroup/nObs
#     
#     sigma[(4+iGroup),(4+iGroup)] = 1/lambdaInGroup*var(theta[(4+iGroup)]/2*(cPit2InGroupStandardized^2+cPit1InGroupStandardized^2)
#                                                        -cPit2InGroupStandardized*cPit1InGroupStandardized)
#     
#     
#   }
#   
#   nCol = ncol(sigma)
#   
#   sigmaRhos = sigma[(nCol-nGroups+1):nCol,(nCol-nGroups+1):nCol]
#   rhos = as.matrix(theta[5:(4+nGroups)],1,nGroups)
#   
#   A = getMatrixForPairwiseComparison(nGroups)
#   
#   out=list()
#   out$testStat = as.numeric(sqrt(nObs)*(t(A%*%rhos) %*% solve(A %*% sigmaRhos %*% t(A)) %*% A%*%rhos))
#   
  
  out = testStatEqualCorrWithoutEstimationCpp(ind, cbind(cPit1,cPit2))
  
  out$pValue = 1 - pchisq(out$testStat,nGroups-1)
  
  #return(list(testStat = testStat, pValue = pValue, theta = theta, sigma = sigma))
  return(out)
}

