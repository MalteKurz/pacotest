

covOfCorrelationsWithEstimation = function(data, svcmDataFrame, ind, cPitData, theta)
{
  ind = (ind == 1) # transfer the possibly numeric matrix into a matrix of logicals
  
  #indexCombinations = getIndexCombinations(RVM)
  
  gInv = gInvRvine(data, svcmDataFrame, ind, cPitData, theta)
  omega = omegaRvine(data, svcmDataFrame, ind, cPitData, theta)
  
  varMat = gInv %*% omega %*% t(gInv)
  
  return(varMat)
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
  
  out = testStatEqualCorrWithEstimationCpp(ind, cbind(cPit1,cPit2), data, svcmDataFrame, cPitData)
  
  out$pValue = 1 - pchisq(out$testStat,nGroups-1)
  
  return(out)
  
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
  
  out = testStatEqualCorrWithoutEstimationCpp(ind, cbind(cPit1,cPit2))
  
  out$pValue = 1 - pchisq(out$testStat,nGroups-1)
  
  return(out)
}

