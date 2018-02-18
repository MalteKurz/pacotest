

getIndInParVector = function(svcmDataFrame, copulaInd, withCopula=TRUE)
{
  cPit1ParInd = sort(svcmDataFrame$cPit1ParInd[[copulaInd]])
  cPit2ParInd = sort(svcmDataFrame$cPit2ParInd[[copulaInd]])
  
  if (withCopula)
  {
    copulaParInd = svcmDataFrame$parInd[[copulaInd]]
    cPitsParInd = sort(unique(c(copulaParInd,
                                cPit1ParInd,
                                cPit2ParInd)))
    
    copulaParVectorInd = match(copulaParInd, cPitsParInd)
    cPit1ParVectorInd = match(cPit1ParInd, cPitsParInd)
    cPit2ParVectorInd = match(cPit2ParInd, cPitsParInd)
    
    return(list(copulaParVectorInd=copulaParVectorInd,
                cPit1ParVectorInd=cPit1ParVectorInd,
                cPit2ParVectorInd=cPit2ParVectorInd))
  }
  else
  {
    cPitsParInd = sort(unique(c(cPit1ParInd,
                                cPit2ParInd)))
    cPit1ParVectorInd = match(cPit1ParInd, cPitsParInd)
    cPit2ParVectorInd = match(cPit2ParInd, cPitsParInd)
    
    return(list(cPit1ParVectorInd=cPit1ParVectorInd,
                cPit2ParVectorInd=cPit2ParVectorInd))
  }
  
  
}


scoreWithCpits =  function(params, data, svcmDataFrame, copulaInd)
{
  parVectorInd = getIndInParVector(svcmDataFrame, copulaInd, withCopula=TRUE)
  
  cPit1CopulaInd = sort(svcmDataFrame$cPit1CopulaInd[[copulaInd]])
  cPit2CopulaInd = sort(svcmDataFrame$cPit2CopulaInd[[copulaInd]])
  
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           params[parVectorInd$cPit1ParVectorInd],
                                           cPit1CopulaInd)
  cPit1 = computeCpits(data, xx, copulaInd, 'cPit1')
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           params[parVectorInd$cPit2ParVectorInd],
                                           cPit2CopulaInd)
  cPit2 = computeCpits(data, xx, copulaInd, 'cPit2')
  
  parCopula = params[parVectorInd$copulaParVectorInd]
  familyCopula = svcmDataFrame$family[copulaInd]
  
  result = score(parCopula,cPit1,cPit2,familyCopula)
  #result = mean(log(BiCopPDF(cPit1,cPit2,familyCopula,par[1],par[2])))
  return(result)
}



like =  function(params,u1,u2,family)
{
  nPar = getNumbOfParameters(family)
  par = getParAsScalars(nPar,params)
  result = mean(log(BiCopPDF(u1,u2,family,par[1],par[2])))
  return(result)
}

score =  function(params,u1,u2,family)
{
  side = getSideIfParameterAtBound(params,family)
  result = grad(like,params, method='simple',u1=u1,u2=u2,family=family, side=side)
  
  return(result)
}


likeMultFactor =  function(params,u1,u2,family,multFactor)
{
  nPar = getNumbOfParameters(family)
  par = getParAsScalars(nPar,params)
  result = mean(log(BiCopPDF(u1,u2,family,par[1],par[2]))*multFactor)
  return(result)
}


hessianLike = function(theta,u1,u2,family)
{
  side = getSideIfParameterAtBound(theta,family)
  result = jacobian(score,theta, method='simple',u1=u1,u2=u2,family=family, side=side)
  
  return(result)
}


hessianLikeWithCpits = function(theta, thetaIsCloseToUpperBound, data, svcmDataFrame, copulaInd)
{
  side = rep(NA, times = length(thetaIsCloseToUpperBound))
  side[thetaIsCloseToUpperBound] = -1
  result = jacobian(scoreWithCpits,theta, method='simple',data=data,svcmDataFrame=svcmDataFrame,copulaInd=copulaInd, side = side)
  
  return(result)
}


cPit2Mult =  function(par, data, svcmDataFrame, copulaInd, multFactor)
{
  
  cPit2CopulaInd = sort(svcmDataFrame$cPit2CopulaInd[[copulaInd]])
  
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           par,
                                           cPit2CopulaInd)
  cPit2 = computeCpits(data, xx, copulaInd, 'cPit2')
  
  result = mean(cPit2*multFactor)
  
  return(result)
}


cPit1Mult =  function(par, data, svcmDataFrame, copulaInd, multFactor)
{
  
  cPit1CopulaInd = sort(svcmDataFrame$cPit1CopulaInd[[copulaInd]])
  
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           par,
                                           cPit1CopulaInd)
  cPit1 = computeCpits(data, xx, copulaInd, 'cPit1')
  
  result = mean(cPit1*multFactor)
  
  return(result)
}


cPit1_mult_cPit2 =  function(par, data, svcmDataFrame, copulaInd, mucPit1, mucPit2, multFactor)
{
  parVectorInd = getIndInParVector(svcmDataFrame, copulaInd, withCopula=FALSE)
  
  cPit1CopulaInd = sort(svcmDataFrame$cPit1CopulaInd[[copulaInd]])
  cPit2CopulaInd = sort(svcmDataFrame$cPit2CopulaInd[[copulaInd]])
  
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           par[parVectorInd$cPit1ParVectorInd],
                                           cPit1CopulaInd)
  cPit1 = computeCpits(data, xx, copulaInd, 'cPit1')
  xx = insertParameterVectorsIntoDataFrame(svcmDataFrame, 
                                           par[parVectorInd$cPit2ParVectorInd],
                                           cPit2CopulaInd)
  cPit2 = computeCpits(data, xx, copulaInd, 'cPit2')
  
  result = mean((cPit1-mucPit1)*
                  (cPit2-mucPit2)*
                  multFactor)
  
  return(result)
}


deriv1cPit2Mult = function(params, paramsIsCloseToUpperBound, data, svcmDataFrame, copulaInd, multFactor)
{
  side = rep(NA, times = length(paramsIsCloseToUpperBound))
  side[paramsIsCloseToUpperBound] = -1
  result = grad(cPit2Mult, params, method='simple', data=data, svcmDataFrame=svcmDataFrame, copulaInd=copulaInd, multFactor=multFactor, side = side)
  
  return(result)
}


deriv1cPit1Mult = function(params, paramsIsCloseToUpperBound, data, svcmDataFrame, copulaInd, multFactor)
{
  side = rep(NA, times = length(paramsIsCloseToUpperBound))
  side[paramsIsCloseToUpperBound] = -1
  result = grad(cPit1Mult, params, method='simple', data=data, svcmDataFrame=svcmDataFrame, copulaInd=copulaInd, multFactor=multFactor, side = side)
  
  return(result)
}


deriv1cPit1_mult_cPit2 = function(params, paramsIsCloseToUpperBound, data, svcmDataFrame, copulaInd, mucPit1, mucPit2, multFactor)
{
  side = rep(NA, times = length(paramsIsCloseToUpperBound))
  side[paramsIsCloseToUpperBound] = -1
  result = grad(cPit1_mult_cPit2, params, method='simple', data=data, svcmDataFrame=svcmDataFrame, copulaInd=copulaInd, mucPit1=mucPit1, mucPit2=mucPit2, multFactor=multFactor, side = side)
  
  return(result)
}


getGinvD = function(data, svcmDataFrame, includeLastCopula = FALSE)
{
  
  d <- ncol(data)
  if (includeLastCopula)
  {
    nCopulas = d*(d-1)/2
  }
  else
  {
    nCopulas = d*(d-1)/2-1
  }
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nParametersFirstTree = sum(svcmDataFrame$nPar[1:(d-1)])
  
  dInvUpperLeft = matrix(0,nrow=nParametersFirstTree,ncol=nParametersFirstTree)
  dLower = matrix(0,nrow=nParameters-nParametersFirstTree,ncol=nParameters)
  
  # First tree copulas (CPITs are not depending on parameters)
  for (jCopula in 1:(d-1))
  {
    if (svcmDataFrame$nPar[jCopula])
    {
    parameters = extractParametersToVectors(svcmDataFrame, jCopula)
    cPit1 = data[,svcmDataFrame$var1[jCopula]]
    cPit2 = data[,svcmDataFrame$var2[jCopula]]
    dInvUpperLeft[svcmDataFrame$parInd[[jCopula]],
                  svcmDataFrame$parInd[[jCopula]]] = 
      1/hessianLike(parameters$parCopula,cPit1,cPit2,svcmDataFrame$family[jCopula])
    }
    
  }
  
  
  
  # Higher trees
  if (nCopulas>=d)
  {
    for (jCopula in d:nCopulas)
    {
      if (svcmDataFrame$nPar[jCopula])
      {
        parameters = extractParametersToVectors(svcmDataFrame, jCopula)
        xx = hessianLikeWithCpits(parameters$parCpits, parameters$parIsCloseToUpperBoundCpits,
                                  data, svcmDataFrame, jCopula)
        dLower[svcmDataFrame$parInd[[jCopula]]-nParametersFirstTree,
               parameters$cPitsParInd] = xx #[((nrow(xx)-svcmDataFrame$nPar[jCopula]+1):nrow(xx)),]
      }
    }
  }
  
  if (nParameters>nParametersFirstTree)
  {
    dLowerRight = dLower[,(nParametersFirstTree+1):nParameters]
    dInvLowerRight = forwardsolve(dLowerRight,diag(nParameters-nParametersFirstTree))
  }
  else
  {
    dInvLowerRight = matrix(0, nParameters-nParametersFirstTree, nParameters-nParametersFirstTree)
  }
  
  if (nParametersFirstTree)
  {
    dLowerLeft = dLower[,1:nParametersFirstTree]
    xxForDInv = -dInvLowerRight %*% dLowerLeft %*% dInvUpperLeft
  }
  else
  {
    xxForDInv = matrix(0, nParameters-nParametersFirstTree, nParametersFirstTree)
  }
  
  
  dInv = rbind(cbind(dInvUpperLeft,
                     matrix(0,nrow=nParametersFirstTree,ncol=nParameters-nParametersFirstTree)),
               cbind(xxForDInv,
                     dInvLowerRight))
  
  return(dInv)
  
}


gInvRvine = function(data, svcmDataFrame, indList, cPitData, theta)
{
  d <- ncol(data)
  
  nCopulas = d*(d-1)/2-1
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  # Obtain the cPits
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the subsamples
  nGroups = length(indList)
  C = matrix(0,nrow=nGroups,ncol=nParameters + 4*nGroups)
  J = matrix(0,nrow=4*nGroups,ncol=nParameters)
  
  dInv = getGinvD(data, svcmDataFrame)
  
  xx = extractParametersToVectors(svcmDataFrame, copulaInd)
  parCpit1 = xx$parCpit1
  parCpit2 = xx$parCpit2
  parCpitsPair = xx$parCpitsWithoutCopula
  
  parIsCloseToUpperBoundCpit1 = xx$parIsCloseToUpperBoundCpit1
  parIsCloseToUpperBoundCpit2 = xx$parIsCloseToUpperBoundCpit2
  parIsCloseToUpperBoundCpitsPair = xx$parIsCloseToUpperBoundCpitsWithoutCopula
  
  cPit1ParInd = xx$cPit1ParInd
  cPit2ParInd = xx$cPit2ParInd
  cPitsPairParInd = xx$cPitsWithoutCopulaParInd
  
  
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[indList[[iGroup]]]
    cPit2InGroup = cPit2[indList[[iGroup]]]
    dataInGroup = data[indList[[iGroup]],]
    
    # Obtain the estimated parameters
    mu1 = theta[1 + 4*(iGroup-1)]
    var1 = theta[2 + 4*(iGroup-1)]
    mu2 = theta[3 + 4*(iGroup-1)]
    var2 = theta[4 + 4*(iGroup-1)]
    
    if (length(cPit1ParInd))
    {
      # First moment of the first CPIT (row) meets likelihood (copula parameter) columns
      J[1 + 4*(iGroup-1),cPit1ParInd] =
        deriv1cPit1Mult(parCpit1, parIsCloseToUpperBoundCpit1, dataInGroup,svcmDataFrame , copulaInd ,-1)
      # Variance of the first CPIT (row) meets likelihood (copula parameter) columns
      J[2 + 4*(iGroup-1),cPit1ParInd] =
        deriv1cPit1Mult(parCpit1, parIsCloseToUpperBoundCpit1, dataInGroup,svcmDataFrame , copulaInd ,-2*(cPit1InGroup-mu1))
    }
    
    if (length(cPit2ParInd))
    {
      # First moment of the first CPIT (row) meets likelihood (copula parameter) columns
      J[3 + 4*(iGroup-1),cPit2ParInd] =
        deriv1cPit2Mult(parCpit2, parIsCloseToUpperBoundCpit2, dataInGroup ,svcmDataFrame , copulaInd ,-1)
      # Variance of the first CPIT (row) meets likelihood (copula parameter) columns
      J[4 + 4*(iGroup-1),cPit2ParInd] =
        deriv1cPit2Mult(parCpit2, parIsCloseToUpperBoundCpit2, dataInGroup ,svcmDataFrame , copulaInd ,-2*(cPit2InGroup-mu2))
    }
    
    
    if (length(cPitsPairParInd))
    {
      C[iGroup,cPitsPairParInd] = deriv1cPit1_mult_cPit2(parCpitsPair, parIsCloseToUpperBoundCpitsPair, dataInGroup, svcmDataFrame , copulaInd , mu1, mu2, -1/sqrt(var1*var2))
    }
    
    # Next two lines are zero by construction
    #C[iGroup,nParameters+ 1 + 4*(iGroup-1)] = mean((cPit2InGroup-mu2)/sqrt(var1*var2))
    #C[iGroup,nParameters+ 3 + 4*(iGroup-1)] = mean((cPit1InGroup-mu1)/sqrt(var1*var2))
    
    C[iGroup,nParameters+ 2 + 4*(iGroup-1)] = mean(0.5*(cPit1InGroup-mu1)*(cPit2InGroup-mu2)/sqrt(var1^3*var2))
    C[iGroup,nParameters+ 4 + 4*(iGroup-1)] = mean(0.5*(cPit1InGroup-mu1)*(cPit2InGroup-mu2)/sqrt(var1*var2^3))
    
  }
  
  
  hInv = rbind(cbind(dInv,matrix(0,nrow=nParameters,ncol=4*nGroups)),
               cbind(-J%*%dInv,diag(4*nGroups)))
  
  gInv = rbind(cbind(hInv,matrix(0,nrow=nParameters + 4*nGroups,ncol=nGroups)),
               cbind(-C%*%hInv,diag(nGroups)))
  
  return(gInv)
}



gInvRvineCov = function(data, svcmDataFrame, indList, cPitData, theta)
{
  d <- ncol(data)
  
  nCopulas = d*(d-1)/2-1
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  # Obtain the cPits
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the subsamples
  nGroups = length(indList)
  C = matrix(0,nrow=nGroups,ncol=nParameters + 2*nGroups)
  J = matrix(0,nrow=2*nGroups,ncol=nParameters)
  
  dInv = getGinvD(data, svcmDataFrame)
  
  xx = extractParametersToVectors(svcmDataFrame, copulaInd)
  parCpit1 = xx$parCpit1
  parCpit2 = xx$parCpit2
  parCpitsPair = xx$parCpitsWithoutCopula
  
  parIsCloseToUpperBoundCpit1 = xx$parIsCloseToUpperBoundCpit1
  parIsCloseToUpperBoundCpit2 = xx$parIsCloseToUpperBoundCpit2
  parIsCloseToUpperBoundCpitsPair = xx$parIsCloseToUpperBoundCpitsWithoutCopula
  
  cPit1ParInd = xx$cPit1ParInd
  cPit2ParInd = xx$cPit2ParInd
  cPitsPairParInd = xx$cPitsWithoutCopulaParInd
  
  
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[indList[[iGroup]]]
    cPit2InGroup = cPit2[indList[[iGroup]]]
    dataInGroup = data[indList[[iGroup]],]
    
    # Obtain the estimated parameters
    mu1 = theta[1 + 2*(iGroup-1)]
    mu2 = theta[2 + 2*(iGroup-1)]
    
    if (length(cPit1ParInd))
    {
      # First moment of the first CPIT (row) meets likelihood (copula parameter) columns
      J[1 + 2*(iGroup-1),cPit1ParInd] =
        deriv1cPit1Mult(parCpit1, parIsCloseToUpperBoundCpit1, dataInGroup,svcmDataFrame , copulaInd ,-1)
    }
    
    if (length(cPit2ParInd))
    {
      # First moment of the second CPIT (row) meets likelihood (copula parameter) columns
      J[2 + 2*(iGroup-1),cPit2ParInd] =
        deriv1cPit2Mult(parCpit2, parIsCloseToUpperBoundCpit2, dataInGroup ,svcmDataFrame , copulaInd ,-1)
    }
    
    
    if (length(cPitsPairParInd))
    {
      C[iGroup,cPitsPairParInd] = deriv1cPit1_mult_cPit2(parCpitsPair, parIsCloseToUpperBoundCpitsPair, dataInGroup, svcmDataFrame , copulaInd , mu1, mu2, -1)
    }
    
    # Next two lines are zero by construction
    #C[iGroup,nParameters+ 1 + 4*(iGroup-1)] = mean((cPit2InGroup-mu2))
    #C[iGroup,nParameters+ 3 + 4*(iGroup-1)] = mean((cPit1InGroup-mu1))
    
  }
  
  
  hInv = rbind(cbind(dInv,matrix(0,nrow=nParameters,ncol=2*nGroups)),
               cbind(-J%*%dInv,diag(2*nGroups)))
  
  gInv = rbind(cbind(hInv,matrix(0,nrow=nParameters + 2*nGroups,ncol=nGroups)),
               cbind(-C%*%hInv,diag(nGroups)))
  
  return(gInv)
}

