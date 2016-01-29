pacotestRvineSeq <- function(data, RVM, pacotestOptions, level=0.05, illustration=2)
{
  data <- as.matrix(data)
  if (any(data > 1) || any(data < 0)) 
    stop("Data has be in the interval [0,1].")
  if (dim(data)[2] != dim(RVM$Matrix)[1]) 
    stop("Dimensions of 'data' and 'RVM' do not match.")
  if (dim(data)[1] < 2) 
    stop("Number of observations has to be at least 2.")
  if (!is(RVM, "RVineMatrix")) 
    stop("'RVM' has to be an RVineMatrix object.")
  
  d <- dim(RVM$Matrix)[1]
  
  o <- diag(RVM$Matrix)
  
  oldRVM <- RVM
  
  if (any(o != length(o):1)) {
    RVM <- getFromNamespace('normalizeRVineMatrix','VineCopula')(RVM)
    data <- data[, o[length(o):1]]
  }
  
  numbRejections = 0
  out = rep(list(), (d-2)*(d-1)/2)
  
  for (k in (d-1):2) {
    numbTests = sum((d-2):(k-1))
    thisTreeLevel = level/numbTests
    
    for (i in (k - 1):1) {
      
      subRVM = extractSubTree(RVM, tree = (d-k+1), copulaNumber = (k-i), data)
      
      # The ind variable needs to be obtained from the grouping methods
      svcmDataFrame = rVineDataFrameRep(subRVM$RVM)$variables
      
      # Compute CPITs for the whole vine
      cPitData = getCpitsFromVine(subRVM$data, svcmDataFrame)
      
      # Obtain the cPits to be tested
      copulaInd = nrow(svcmDataFrame)
      
      cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
      cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
      
      Udata = cbind(cPit1,cPit2)
      W = subRVM$data[,svcmDataFrame$condset[[copulaInd]]]
      
      out[[(k-1)*(k-2)/2+i]] = pacotest(Udata,W,pacotestOptions, data = subRVM$data, svcmDataFrame = svcmDataFrame)
      
      if (illustration == 1) {
        message(oldRVM$Matrix[i, i],",",oldRVM$Matrix[k, i],
                "|", 
                paste(oldRVM$Matrix[(k + 1):d, i],collapse = ","),
                " pValue:",
                out[[(k-1)*(k-2)/2+i]]$pValue)
      }
      
      if (out[[(k-1)*(k-2)/2+i]]$pValue < thisTreeLevel)
      {
        numbRejections = numbRejections +1
      }
    }
    
    if (illustration == 2)
    {
      message((d-k+1),". Tree: ",
              numbRejections," rejections at a individual test level of ",
              thisTreeLevel)
    }
    
    if (numbRejections>0)
    {
      message("Stopped the sequential test due to a rejection")
      return(out)
    }
  }
  return(out)
}
