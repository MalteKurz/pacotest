

covOfParameters = function(data, svcmDataFrame, cPitData, withRanks = FALSE)
{
  
  gInv = getGinvD(data, svcmDataFrame, TRUE)
  omega = getOmegaWithLikesD(data, svcmDataFrame, cPitData, TRUE)
  if (withRanks)
  {
    bSsp = bSspForCovWithRanks(data, svcmDataFrame, cPitData, TRUE)
    omega = omega + bSsp
  }
  xx = t(omega)
  omega[upper.tri(omega)] = xx[upper.tri(xx)]
  
  varMat = gInv %*% omega %*% t(gInv)
  
  return(list(varMat=varMat, gInv=gInv))
}

stepwiseSeRVine <- function(data, RVM, withRanks = FALSE)
{
  
  data <- as.matrix(data)
  d <- dim(RVM$Matrix)[1]
  o <- diag(RVM$Matrix)
  
  if(any(o != length(o):1)){  
    RVM =  getFromNamespace('normalizeRVineMatrix','VineCopula')(RVM)
    data = data[,o[length(o):1]]
  }
  
  if (!is(RVM, "RVineMatrix")) 
    stop("'RVM' has to be an RVineMatrix object.")
  
  if (any(data > 1) || any(data < 0)) 
    stop("Data has be in the interval [0,1].")
  
  if (dim(data)[2] != d) 
    stop("Dimensions of 'data' and 'RVM' do not match.")
  
  if (dim(data)[1] < 2) 
    stop("Number of observations has to be at least 2.")
  
  #if (any(o != length(o):1))
  #  stop("The RVM Matrix needs to be provided in normalized form")
  
  
  svcmDataFrame = rVineDataFrameRep(RVM)
  
  # Compute CPITs for the whole vine
  cPitData = getCpitsFromVine(data, svcmDataFrame, withRanks)
  
  xx = covOfParameters(data, svcmDataFrame, cPitData, withRanks)
  covHat = xx$varMat
  gInv = xx$gInv
  g = solve(gInv)
  se = sqrt(diag(covHat)/dim(data)[1])
  
  se1Matrix = matrix(0,d,d)
  se2Matrix = matrix(0,d,d)
  
  I = 1
  copulaInd = 1
  
  for (tree in 1:(d-1))
  {
    for (jj in 1:(d-tree))
    {
      if (svcmDataFrame$nPar[copulaInd]>0){
        se1Matrix[(d-tree+1),(d-tree-jj+1)] = se[I]
        I = I+1
        if (svcmDataFrame$nPar[copulaInd]>1){
          se2Matrix[(d-tree+1),(d-tree-jj+1)] = se[I]
          I = I+1
        }
      }
      copulaInd = copulaInd +1
    }
  }
  
  return(list(se1Matrix=se1Matrix,se2Matrix=se2Matrix, covHat = covHat, g = g))
  
}

