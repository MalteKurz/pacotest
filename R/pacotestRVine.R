pacotestRvineSeq <- function(data, RVM, pacotestOptions, level=0.05, illustration=2, stopIfRejected = TRUE)
{
  pacotestOptions = pacotestset(pacotestOptions)
  
  data <- as.matrix(data)
  d <- dim(RVM$Matrix)[1]
  o <- diag(RVM$Matrix)
  
  if (!is(RVM, "RVineMatrix")) 
    stop("'RVM' has to be an RVineMatrix object.")
  
  if (any(data > 1) || any(data < 0)) 
    stop("Data has be in the interval [0,1].")
  
  if (dim(data)[2] != d) 
    stop("Dimensions of 'data' and 'RVM' do not match.")
  
  if (dim(data)[1] < 2) 
    stop("Number of observations has to be at least 2.")
  
  oldRVM <- RVM
  
  if (any(o != length(o):1)) {
    RVM <- getFromNamespace('normalizeRVineMatrix','VineCopula')(RVM)
    data <- data[, o[length(o):1]]
  }
  
  testResultSummary = data.frame(matrix(ncol = 4, nrow = d-2))
  names(testResultSummary) = c("Tree", "NumbOfRejections", "IndividualTestLevel", "Interpretation")
  
  rejectH0 = FALSE
  
  out = matrix(list(),nrow = d, ncol =d)
  pValues = matrix(NA, nrow=d, ncol=d)
  
  for (k in (d-1):2) {
    numbTests = (d-2)*(d-1)/2
    thisTreeLevel = level/numbTests
    
    # reset numbRejections in each tree
    numbRejections = 0
    
    for (i in (k - 1):1) {
      
      subRVM = extractSubTree(RVM, tree = (d-k+1), copulaNumber = (k-i), data)
      
      # Obtain the svcm data frame representation
      svcmDataFrame = rVineDataFrameRep(subRVM$RVM)
      
      # Compute CPITs for the whole vine
      if (exists('estUncertWithRanks', pacotestOptions))
      {
        withDerivs = pacotestOptions$estUncertWithRanks
      }
      else
      {
        withDerivs = FALSE
      }
      cPitData = getCpitsFromVine(subRVM$data, svcmDataFrame, withDerivs)
      
      # Obtain the cPits to be tested
      copulaInd = nrow(svcmDataFrame)
      
      cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
      cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
      
      if (!is.data.frame(subRVM$data))
      {
        subRVM$data = as.data.frame(subRVM$data)
      }
      
      Udata = cbind(cPit1,cPit2)
      cPit1Name = paste(dimnames(subRVM$data)[[2]][svcmDataFrame$var1[copulaInd]],
                        paste(dimnames(subRVM$data)[[2]][svcmDataFrame$condset[[copulaInd]]], collapse=","), sep="|")
      cPit2Name = paste(dimnames(subRVM$data)[[2]][svcmDataFrame$var2[copulaInd]],
                        paste(dimnames(subRVM$data)[[2]][svcmDataFrame$condset[[copulaInd]]], collapse=","), sep="|")
      dimnames(Udata)[[2]] = c(cPit1Name, cPit2Name)
      
      W = subRVM$data[,svcmDataFrame$condset[[copulaInd]], drop=FALSE]
      
      out[k,i][[1]] = pacotest(Udata,W,pacotestOptions, data = subRVM$data, svcmDataFrame = svcmDataFrame, cPitData = cPitData)
      
      pValues[k,i] = out[k,i][[1]]$pValue
      
      if (illustration == 1) {
        message(oldRVM$Matrix[i, i],",",oldRVM$Matrix[k, i],
                "|", 
                paste(oldRVM$Matrix[(k + 1):d, i],collapse = ","),
                " pValue:",
                out[k,i][[1]]$pValue)
      }
      
      if (out[k,i][[1]]$pValue < thisTreeLevel)
      {
        numbRejections = numbRejections +1
      }
    }
    
    
    testResultSummary[(d-k), 1:3] = c((d-k+1), numbRejections, thisTreeLevel)
    
    
    if (numbRejections > 0)
    {
      testResultSummary[(d-k), 4] = paste(numbRejections, "rejections at a individual test level of ",
                                          thisTreeLevel, "--> Sequential test procedure can be stopped due to a rejection")
    } else if (numbRejections == 0)
    {
      if (k == 2)
      {
        testResultSummary[(d-k), 4] = paste("Simplifying assumption can not be rejected at a ", level*100, " % level")
      }
      else
      {
        testResultSummary[(d-k), 4] = "No rejection --> Continue the sequential test in the next tree"
      }
    }
    
    rejectH0 = any(rejectH0, numbRejections>0)
    
    if (illustration == 2)
    {
      message((d-k+1),". Tree: ",
              testResultSummary$Interpretation[(d-k)])
    }
    
    if (rejectH0 & stopIfRejected)
    {
      message("Stopped the sequential test due to a rejection")
      return(list(pacotestResultLists=out, pValues = pValues, testResultSummary = testResultSummary))
    }
    
  }
  return(list(pacotestResultLists = out, pValues = pValues, testResultSummary = testResultSummary))
}


pacotestRvineSingleCopula <- function(data, RVM, pacotestOptions, tree, copulaNumber)
{
  pacotestOptions = pacotestset(pacotestOptions)
  
  data <- as.matrix(data)
  d <- dim(RVM$Matrix)[1]
  o <- diag(RVM$Matrix)
  
  if (!is(RVM, "RVineMatrix")) 
    stop("'RVM' has to be an RVineMatrix object.")
  
  if (any(data > 1) || any(data < 0)) 
    stop("Data has be in the interval [0,1].")
  
  if (dim(data)[2] != d) 
    stop("Dimensions of 'data' and 'RVM' do not match.")
  
  if (dim(data)[1] < 2) 
    stop("Number of observations has to be at least 2.")
  
  if (!(tree > 1 && tree <= (d-1)))
    stop("'tree' has do be larger than 2 and at most dim (of the vine) - 1")
  
  if (!(copulaNumber > 0 && copulaNumber <= (d-tree)))
    stop("Invalid copula number")
  
  if (any(o != length(o):1)) {
    RVM <- getFromNamespace('normalizeRVineMatrix','VineCopula')(RVM)
    data <- data[, o[length(o):1]]
  }
  
  subRVM = extractSubTree(RVM, tree = tree, copulaNumber = copulaNumber, data)
  
  # Obtain the svcm data frame representation
  svcmDataFrame = rVineDataFrameRep(subRVM$RVM)
  
  # Compute CPITs for the whole vine
  if (exists('estUncertWithRanks', pacotestOptions))
  {
    withDerivs = pacotestOptions$estUncertWithRanks
  }
  else
  {
    withDerivs = FALSE
  }
  cPitData = getCpitsFromVine(subRVM$data, svcmDataFrame, withDerivs) # derivatives are only needed for asmpt with ranks
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  
  if (!is.data.frame(subRVM$data))
  {
    subRVM$data = as.data.frame(subRVM$data)
  }
  
  Udata = cbind(cPit1,cPit2)
  cPit1Name = paste(dimnames(subRVM$data)[[2]][svcmDataFrame$var1[copulaInd]],
                    paste(dimnames(subRVM$data)[[2]][svcmDataFrame$condset[[copulaInd]]], collapse=","), sep="|")
  cPit2Name = paste(dimnames(subRVM$data)[[2]][svcmDataFrame$var2[copulaInd]],
                    paste(dimnames(subRVM$data)[[2]][svcmDataFrame$condset[[copulaInd]]], collapse=","), sep="|")
  dimnames(Udata)[[2]] = c(cPit1Name, cPit2Name)
  
  W = subRVM$data[,svcmDataFrame$condset[[copulaInd]], drop=FALSE]
  
  pacotestResultList = pacotest(Udata,W,pacotestOptions, data = subRVM$data, svcmDataFrame = svcmDataFrame, cPitData = cPitData)
  
  return(pacotestResultList)
  
}

