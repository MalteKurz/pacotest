

deriv1LikeMult = function(theta,u1,u2,family,multFactor)
{
  side = getSideIfParameterAtBound(theta,family)
  result = grad(likeMultFactor,theta, method='simple',u1=u1,u2=u2,family=family,multFactor=multFactor, side=side)
  
  return(result)
}


likeMult =  function(params2,v1,v2,family2,params1,u1,u2,family1)
{
  nPar1 = getNumbOfParameters(family1)
  par1 = getParAsScalars(nPar1,params1)
  
  nPar2 = getNumbOfParameters(family2)
  par2 = getParAsScalars(nPar2,params2)
  
  result = mean(log(BiCopPDF(u1,u2,family1,par1[1],par1[2]))*log(BiCopPDF(v1,v2,family2,par2[1],par2[2])))
  
  return(result)
}


likeMultDeriv =  function(params1,u1,u2,family1,params2,v1,v2,family2)
{
  side = getSideIfParameterAtBound(params2,family2)
  result = grad(likeMult,params2, method='simple',v1=v1,v2=v2,family2=family2,params1=params1,u1=u1,u2=u2,family1=family1, side=side)
  return(result)
}


deriv2LikeMult = function(params1,u1,u2,family1,params2,v1,v2,family2)
{
  side = getSideIfParameterAtBound(params1,family1)
  result = jacobian(likeMultDeriv,params1, method='simple',u1=u1,u2=u2,family1=family1,params2=params2,v1=v1,v2=v2,family2=family2, side=side)
  
  return(result)
}


## Asympt with ranks section
likeVar1 =  function(u1,u2,family,params)
{
  nPar = getNumbOfParameters(family)
  par = getParAsScalars(nPar,params)
  
  result = log(BiCopPDF(u1,u2,family,par[1],par[2]))
  
  return(result)
}

likeVar2 =  function(u2,u1,family,params)
{
  nPar = getNumbOfParameters(family)
  par = getParAsScalars(nPar,params)
  
  result = log(BiCopPDF(u1,u2,family,par[1],par[2]))
  
  return(result)
}


computeLikeWithCpitsDerivs = function(parVector, data, svcmDataFrame, cPitData, copulaInd)
{
  d <- ncol(data)
  nObs = nrow(data)
  
  likeWithCpitsDerivs = array(0, dim = c(nObs, d))
  
  family = svcmDataFrame$family[copulaInd]
  
  if (copulaInd<d)
  {
    cPit1 = data[,svcmDataFrame$var1[copulaInd]]
    cPit2 = data[,svcmDataFrame$var2[copulaInd]]
  }
  else
  {
    cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
    cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  }
  
  ## with ranks
  xxSide = vector(length=length(cPit1))
  xxSide[] = NA
  xxSide[cPit1>0.99] = -1
  xxSide[cPit1<0.01] = 1
  likeVar1Deriv = grad(likeVar1,cPit1, side = xxSide ,u2=cPit2,family=family,params=parVector, method='simple')
  
  xxSide = vector(length=length(cPit2))
  xxSide[] = NA
  xxSide[cPit2>0.99] = -1
  xxSide[cPit2<0.01] = 1
  likeVar2Deriv = grad(likeVar2,cPit2, side = xxSide ,u1=cPit1,family=family,params=parVector, method='simple')
  
  if (copulaInd < d) # first tree
  {
    likeWithCpitsDerivs[, svcmDataFrame$var1[copulaInd]] = likeVar1Deriv
    likeWithCpitsDerivs[, svcmDataFrame$var2[copulaInd]] = likeVar2Deriv
    
  }
  else
  {
    condset = svcmDataFrame$condset[[copulaInd]]
    
    cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, copulaInd, svcmDataFrame$var1[copulaInd])
    likeWithCpitsDerivs[, svcmDataFrame$var1[copulaInd]] = likeVar1Deriv * cPit1Deriv
    
    cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, copulaInd, svcmDataFrame$var2[copulaInd])
    likeWithCpitsDerivs[, svcmDataFrame$var2[copulaInd]] = likeVar2Deriv * cPit2Deriv
    
    
    for (condsetVariable in condset)
    {
      cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, copulaInd, condsetVariable)
      cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, copulaInd, condsetVariable)
      
      likeWithCpitsDerivs[, condsetVariable] = likeVar1Deriv * cPit1Deriv + likeVar2Deriv * cPit2Deriv
      
    }
    
  }
  
  return(likeWithCpitsDerivs)
}


helpingfunctionBSspForCovWithRanks = function(parVector, data, svcmDataFrame, cPitData, copulaInd)
{
  d <- ncol(data)
  nObs = nrow(data)
  
  family = svcmDataFrame$family[copulaInd]
  
  w = array(0, dim = c(nObs, d))
  
  likeWithCpitsDerivs = computeLikeWithCpitsDerivs(parVector, data, svcmDataFrame, cPitData, copulaInd)
  
  orderingInds = apply(data,2,order, decreasing=TRUE)
  
  for (iVar in 1:d)
  {
    xx = likeWithCpitsDerivs[orderingInds[,iVar], iVar]
    w[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
  }
  
  
  sumOfW = apply(w,1,sum)
  
  return(sumOfW)
  
  
}


scoresForBSspWithRanks = function(params, data, svcmDataFrame, cPitData, copulaInd)
{
  
  family = svcmDataFrame$family[copulaInd]
  d <- ncol(data)
  
  if (copulaInd<d)
  {
    cPit1 = data[,svcmDataFrame$var1[copulaInd]]
    cPit2 = data[,svcmDataFrame$var2[copulaInd]]
  }
  else
  {
    cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
    cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  }
  
  result = likeVar1(cPit1, cPit2, family, params)
  
  return(result)
}


likeMultWithRanks =  function(params2,copulaInd2, params1,copulaInd1, data, svcmDataFrame, cPitData)
{
  xx1 = helpingfunctionBSspForCovWithRanks(params1, data, svcmDataFrame, cPitData, copulaInd1)
  xx2 = scoresForBSspWithRanks(params1, data, svcmDataFrame, cPitData, copulaInd1)
    
  yy1 = helpingfunctionBSspForCovWithRanks(params2, data, svcmDataFrame, cPitData, copulaInd2)
  yy2 = scoresForBSspWithRanks(params2, data, svcmDataFrame, cPitData, copulaInd2)
  
  cov1 = mean((xx1-mean(xx1))*(yy1-mean(yy1)))
  cov2 = mean((xx2-mean(xx2))*(yy1-mean(yy1)))
  cov3 = mean((xx1-mean(xx1))*(yy2-mean(yy2)))
  
  result = cov1 + cov2 + cov3
  
  
  return(result)
}


likeMultDerivWithRanks =  function(params1,copulaInd1, params2,copulaInd2, data, svcmDataFrame, cPitData)
{
  xxFamily2 = svcmDataFrame$family[copulaInd2]
  side = getSideIfParameterAtBound(params2,xxFamily2)
  result = grad(likeMultWithRanks,params2, method='simple',
                copulaInd2=copulaInd2,
                params1=params1,copulaInd1=copulaInd1,
                data=data, svcmDataFrame=svcmDataFrame, cPitData=cPitData, side=side)
  return(result)
}


deriv2LikeMultWithRanks = function(params1,copulaInd1, params2,copulaInd2, data, svcmDataFrame, cPitData)
{
  xxFamily1 = svcmDataFrame$family[copulaInd1]
  side = getSideIfParameterAtBound(params1,xxFamily1)
  result = jacobian(likeMultDerivWithRanks,params1, method='simple',
                    copulaInd1=copulaInd1,
                    params2=params2,copulaInd2=copulaInd2,
                    data=data, svcmDataFrame=svcmDataFrame, cPitData=cPitData, side=side)
  return(result)
}


bSspForCovWithRanks = function(data, svcmDataFrame, cPitData, includeLastCopula = FALSE)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  if (includeLastCopula)
  {
    nCopulas = d*(d-1)/2
  }
  else
  {
    nCopulas = d*(d-1)/2-1
  }
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  bSsp = matrix(0,nrow=nParameters,ncol=nParameters)
  
  
  for (jCopula in 1:nCopulas)
  {
    
    if (svcmDataFrame$nPar[jCopula])
    {
    parVector1 = svcmDataFrame$par[[jCopula]]
    
    for (lCopula in 1:jCopula)
    {
      if (svcmDataFrame$nPar[lCopula]) #&&
            #((lCopula == jCopula) || !any(svcmDataFrame$copulaInd[lCopula]==c(svcmDataFrame$cPit1CopulaInd[[jCopula]],svcmDataFrame$cPit2CopulaInd[[jCopula]]))))
      {
        parVector2 = svcmDataFrame$par[[lCopula]]
        
        bSsp[svcmDataFrame$parInd[[jCopula]],svcmDataFrame$parInd[[lCopula]]] =
          deriv2LikeMultWithRanks(parVector1,jCopula,
                                  parVector2,lCopula,
                                  data, svcmDataFrame, cPitData)
        
        
      }
    }
    
    }
    
  }
  
  return(bSsp)
  
}


getOmegaWithLikesD = function(data, svcmDataFrame, cPitData, includeLastCopula = FALSE)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  if (includeLastCopula)
  {
    nCopulas = d*(d-1)/2
  }
  else
  {
    nCopulas = d*(d-1)/2-1
  }
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  D = matrix(0,nrow=nParameters,ncol=nParameters)
  
  for (jCopula in 1:nCopulas)
  {
    if (svcmDataFrame$nPar[jCopula])
    {
      family_1 = svcmDataFrame$family[jCopula]
      par_1 = svcmDataFrame$par[[jCopula]]
      if (jCopula<d)
      {
        cPit1_1 = data[,svcmDataFrame$var1[jCopula]]
        cPit2_1 = data[,svcmDataFrame$var2[jCopula]]
      }
      else
      {
        cPit1_1 = getCpit1(cPitData, svcmDataFrame, jCopula)
        cPit2_1 = getCpit2(cPitData, svcmDataFrame, jCopula)
      }
      
      for (lCopula in 1:jCopula)
      {
        if (svcmDataFrame$nPar[lCopula])
              #((lCopula == jCopula) || !any(svcmDataFrame$copulaInd[lCopula]==c(svcmDataFrame$cPit1CopulaInd[[jCopula]],svcmDataFrame$cPit2CopulaInd[[jCopula]]))))
        {
          family_2 = svcmDataFrame$family[lCopula]
          par_2 = svcmDataFrame$par[[lCopula]]
          if (lCopula<d)
          {
            cPit1_2 = data[,svcmDataFrame$var1[lCopula]]
            cPit2_2 = data[,svcmDataFrame$var2[lCopula]]
          }
          else
          {
            cPit1_2 = getCpit1(cPitData, svcmDataFrame, lCopula)
            cPit2_2 = getCpit2(cPitData, svcmDataFrame, lCopula)
          }
          
          D[svcmDataFrame$parInd[[jCopula]],svcmDataFrame$parInd[[lCopula]]] = deriv2LikeMult(par_1,cPit1_1,cPit2_1,family_1,
                                                                                              par_2,cPit1_2,cPit2_2,family_2)
        }
      }
    }
  }
  
  return(D)  
  
}



computeEstEqWithCpitsDerivs = function(data, cPitData, svcmDataFrame, copulaInd, indGroup, theta)
{
  d <- ncol(data)
  nObs = nrow(data)
  
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the subsample
  cPit1InGroup = cPit1[indGroup]
  cPit2InGroup = cPit2[indGroup]
  nObsPerGroup = length(cPit1InGroup)
  
  mu1WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  mu2WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  sigma1WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  sigma2WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  rhoWithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  
  
  
  # Obtain the estimated parameters
  mu1 = theta[1]
  sigma2_1 = theta[2]
  mu2 = theta[3]
  sigma2_2 = theta[4]
  rho = theta[5]
  
  
  condset = svcmDataFrame$condset[[copulaInd]]
  
  cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, copulaInd, svcmDataFrame$var1[copulaInd])
  cPit1DerivInGroup = cPit1Deriv[indGroup]
  
  mu1WithCpitsDerivs[, svcmDataFrame$var1[copulaInd]] = -cPit1DerivInGroup
  sigma1WithCpitsDerivs[, svcmDataFrame$var1[copulaInd]] = -2*(cPit1InGroup-mu1)*cPit1DerivInGroup
  rhoWithCpitsDerivs[, svcmDataFrame$var1[copulaInd]] = -cPit1DerivInGroup * (cPit2InGroup - mu2) / sqrt(sigma2_1*sigma2_2)
  
  cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, copulaInd, svcmDataFrame$var2[copulaInd])
  cPit2DerivInGroup = cPit2Deriv[indGroup]
  
  mu2WithCpitsDerivs[, svcmDataFrame$var2[copulaInd]] = -cPit2DerivInGroup
  sigma2WithCpitsDerivs[, svcmDataFrame$var2[copulaInd]] = -2*(cPit2InGroup-mu2)*cPit2DerivInGroup
  rhoWithCpitsDerivs[, svcmDataFrame$var2[copulaInd]] = -cPit2DerivInGroup * (cPit1InGroup - mu1) / sqrt(sigma2_1*sigma2_2)
  
  
  for (condsetVariable in condset)
  {
    cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, copulaInd, condsetVariable)
    cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, copulaInd, condsetVariable)
    
    cPit1DerivInGroup = cPit1Deriv[indGroup]
    cPit2DerivInGroup = cPit2Deriv[indGroup]
    
    mu1WithCpitsDerivs[, condsetVariable] = -cPit1DerivInGroup
    sigma1WithCpitsDerivs[, condsetVariable] = -2*(cPit1InGroup-mu1)*cPit1DerivInGroup
    
    mu2WithCpitsDerivs[, condsetVariable] = -cPit2DerivInGroup
    sigma2WithCpitsDerivs[, condsetVariable] = -2*(cPit2InGroup-mu2)*cPit2DerivInGroup
    
    rhoWithCpitsDerivs[, condsetVariable] = (-cPit1DerivInGroup * (cPit2InGroup - mu2) - cPit2DerivInGroup * (cPit1InGroup - mu1)) / sqrt(sigma2_1*sigma2_2)
    
    
  }
  
  
  return(list(mu1WithCpitsDerivs = mu1WithCpitsDerivs, sigma1WithCpitsDerivs = sigma1WithCpitsDerivs,
              mu2WithCpitsDerivs = mu2WithCpitsDerivs, sigma2WithCpitsDerivs = sigma2WithCpitsDerivs,
              rhoWithCpitsDerivs = rhoWithCpitsDerivs))
  
}

helpingfunctionBSspForEqualCorrWithRanks = function(data, cPitData, svcmDataFrame, copulaInd, indGroup, theta)
{
  d <- ncol(data)
  nObs = nrow(data)
  dataInGroup = data[indGroup,]
  nObsPerGroup = length(indGroup)
  
  wMu1WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  wMu2WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  wSigma1WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  wSigma2WithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  wRhoWithCpitsDerivs = array(0, dim = c(nObsPerGroup, d))
  
  result = array(0, dim = c(nObsPerGroup, 5))
  
  estEqWithCpitsDerivs = computeEstEqWithCpitsDerivs(data, cPitData, svcmDataFrame, copulaInd, indGroup, theta)
  
  orderingInds = apply(dataInGroup,2,order, decreasing=TRUE)
  
  for (iVar in 1:d)
  {
    xx = estEqWithCpitsDerivs$mu1WithCpitsDerivs[orderingInds[,iVar], iVar]
    wMu1WithCpitsDerivs[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
    xx = estEqWithCpitsDerivs$mu2WithCpitsDerivs[orderingInds[,iVar], iVar]
    wMu2WithCpitsDerivs[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
    xx = estEqWithCpitsDerivs$sigma1WithCpitsDerivs[orderingInds[,iVar], iVar]
    wSigma1WithCpitsDerivs[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
    xx = estEqWithCpitsDerivs$sigma2WithCpitsDerivs[orderingInds[,iVar], iVar]
    wSigma2WithCpitsDerivs[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
    xx = estEqWithCpitsDerivs$rhoWithCpitsDerivs[orderingInds[,iVar], iVar]
    wRhoWithCpitsDerivs[orderingInds[,iVar], iVar] = cumsum(xx)/nObs
    
  }
  
  
  result[,1] = apply(wMu1WithCpitsDerivs,1,sum)
  result[,3] = apply(wMu2WithCpitsDerivs,1,sum)
  result[,2] = apply(wSigma1WithCpitsDerivs,1,sum)
  result[,4] = apply(wSigma2WithCpitsDerivs,1,sum)
  result[,5] = apply(wRhoWithCpitsDerivs,1,sum)
  
  return(result)
  
  
}


bsspEstEqEqualCorrSingleGroup =  function(data, svcmDataFrame, cPitData, copulaInd, indGroup, theta, aInGroup, bInGroup)
{
  nObs = nrow(data)
  nObsPerGroup = length(indGroup)
  lambdaInGroup = nObsPerGroup/nObs
  
  xx = helpingfunctionBSspForEqualCorrWithRanks(data, cPitData, svcmDataFrame, copulaInd, indGroup, theta)
  yy = cbind(aInGroup, bInGroup)
  
  result = matrix(0, 5, 5)
  
  for (iEstEq in 1:5)
  {
    for (jEstEq in iEstEq:5)
    {
      cov1 = mean((xx[,iEstEq]-mean(xx[,iEstEq]))*(xx[,jEstEq]-mean(xx[,jEstEq])))
      cov2 = mean((yy[,iEstEq]-mean(yy[,iEstEq]))*(xx[,jEstEq]-mean(xx[,jEstEq])))
      cov3 = mean((xx[,iEstEq]-mean(xx[,iEstEq]))*(yy[,jEstEq]-mean(yy[,jEstEq])))
      
      result[jEstEq, iEstEq] = (cov1 + cov2 + cov3)/lambdaInGroup
      
    }
    
  }
  
  
  xx = t(result)
  result[upper.tri(result)] = xx[upper.tri(xx)]
  
  
  return(result)
}

bsspEstEqEqualCorr =  function(data, svcmDataFrame, indList, cPitData, copulaInd, theta, listOfMultipliers)
{
  nObs = nrow(data)
  # Obtain the subsamples
  nGroups = length(indList)
  
  bSsp = matrix(0,nrow = 4*nGroups+nGroups,ncol = 4*nGroups+nGroups)
  
  for (iGroup in 1:nGroups)
  {
    aInGroup = listOfMultipliers$aInGroups[[iGroup]]
    bInGroup = listOfMultipliers$bInGroups[[iGroup]]
    
    # Obtain the estimated parameters
    thetaInGroup = c(theta[(1 + 4*(iGroup-1)) : (4 + 4*(iGroup-1))], theta[(4*nGroups+iGroup)])
    indGroup = indList[[iGroup]]
    
    xx = bsspEstEqEqualCorrSingleGroup(data, svcmDataFrame, cPitData, copulaInd, indGroup, thetaInGroup, aInGroup, bInGroup)
    
    thisGroupStartIndMuSigma = 4*(iGroup-1)
    thisGroupMuSigmaIndexSet = (1 + thisGroupStartIndMuSigma):(4 + thisGroupStartIndMuSigma)
    thisGroupRhoIndex = 4*nGroups + iGroup
    thisGroupIndexSet = c(thisGroupMuSigmaIndexSet, thisGroupRhoIndex)
    
    bSsp[thisGroupIndexSet, thisGroupIndexSet] = xx
    
  }
  
  xx = t(bSsp)
  bSsp[upper.tri(bSsp)] = xx[upper.tri(xx)]
  
  
  return(bSsp)
  
}

likeMultCrossTermWithRanks =  function(params1,copulaInd1, data, svcmDataFrame, cPitData, yyEstEqWithCpitsDeriv, yyEstEq, indGroup)
{
  nObs = nrow(data)
  nObsPerGroup = length(indGroup)
  lambdaInGroup = nObsPerGroup/nObs
  
  xx1 = helpingfunctionBSspForCovWithRanks(params1, data, svcmDataFrame, cPitData, copulaInd1)
  xx2 = scoresForBSspWithRanks(params1, data, svcmDataFrame, cPitData, copulaInd1)
  
  xx1 = xx1[indGroup]
  xx2 = xx2[indGroup]
  
  cov1 = mean((xx1-mean(xx1))*(yyEstEqWithCpitsDeriv-mean(yyEstEqWithCpitsDeriv)))
  cov2 = mean((xx2-mean(xx2))*(yyEstEqWithCpitsDeriv-mean(yyEstEqWithCpitsDeriv)))
  cov3 = mean((xx1-mean(xx1))*(yyEstEq-mean(yyEstEq)))
  
  result = (cov1 + cov2 + cov3)/lambdaInGroup
  
  
  return(result)
}


deriv1LikeMultWithRanks =  function(params1,copulaInd1, data, svcmDataFrame, cPitData, yyEstEqWithCpitsDeriv, yyEstEq, indGroup)
{
  xxFamily1 = svcmDataFrame$family[copulaInd1]
  side = getSideIfParameterAtBound(params1,xxFamily1)
  result = grad(likeMultCrossTermWithRanks,params1, method='simple',
                copulaInd1=copulaInd1,
                data=data, svcmDataFrame=svcmDataFrame, cPitData=cPitData,
                yyEstEqWithCpitsDeriv = yyEstEqWithCpitsDeriv, yyEstEq = yyEstEq, indGroup = indGroup, side=side)
  return(result)
}


bsspEstEqEqualCorrCrossTermsSinglegroup = function(data, svcmDataFrame, cPitData, copulaInd, indGroup, theta, aInGroup, bInGroup)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  bSsp = matrix(0,nrow=nParameters,ncol=5)
  
  yyEstEqWithCpitsDeriv = helpingfunctionBSspForEqualCorrWithRanks(data, cPitData, svcmDataFrame, copulaInd, indGroup, theta)
  yyEstEq = cbind(aInGroup, bInGroup)
  
  for (jCopula in 1:nCopulas)
  {
    
    if (svcmDataFrame$nPar[jCopula])
    {
      parVector1 = svcmDataFrame$par[[jCopula]]
      
      for (iTheta in 1:5)
      {
        bSsp[svcmDataFrame$parInd[[jCopula]],iTheta] =
          deriv1LikeMultWithRanks(parVector1,jCopula, data, svcmDataFrame, cPitData,
                                  yyEstEqWithCpitsDeriv[,iTheta], yyEstEq[,iTheta], indGroup)
        
      }
      
    }
    
  }
  
  return(bSsp)
  
}

bsspEstEqEqualCorrCrossTerms =  function(data, svcmDataFrame, indList, cPitData, copulaInd, theta, listOfMultipliers)
{
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  # Obtain the subsamples
  nGroups = length(indList)
  
  bSsp = matrix(0,nrow=4*nGroups+nGroups,ncol=nParameters)
  
  for (iGroup in 1:nGroups)
  {
    aInGroup = listOfMultipliers$aInGroups[[iGroup]]
    bInGroup = listOfMultipliers$bInGroups[[iGroup]]
    
    # Obtain the estimated parameters
    thetaInGroup = c(theta[(1 + 4*(iGroup-1)) : (4 + 4*(iGroup-1))], theta[(4*nGroups+iGroup)])
    indGroup = indList[[iGroup]]
    
    xx = bsspEstEqEqualCorrCrossTermsSinglegroup(data, svcmDataFrame, cPitData, copulaInd, indGroup, thetaInGroup, aInGroup, bInGroup)
    
    thisGroupStartIndMuSigma = 4*(iGroup-1)
    thisGroupMuSigmaIndexSet = (1 + thisGroupStartIndMuSigma):(4 + thisGroupStartIndMuSigma)
    thisGroupRhoIndex = 4*nGroups + iGroup
    thisGroupIndexSet = c(thisGroupMuSigmaIndexSet, thisGroupRhoIndex)
    
    bSsp[thisGroupIndexSet, ] = t(xx)
    
  }
  
  
  return(bSsp)
  
}


getOmegaWithLikesE = function(data, svcmDataFrame, indList, cPitData, listOfMultipliers)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nGroups = length(indList)
  
  E = matrix(0,nrow=4*nGroups+nGroups,ncol=nParameters)
  
  for (jCopula in 1:nCopulas)
  {
    if (svcmDataFrame$nPar[jCopula])
    {
      family_1 = svcmDataFrame$family[jCopula]
      par_1 = svcmDataFrame$par[[jCopula]]
      if (jCopula<d)
      {
        cPit1_1 = data[,svcmDataFrame$var1[jCopula]]
        cPit2_1 = data[,svcmDataFrame$var2[jCopula]]
      }
      else
      {
        cPit1_1 = getCpit1(cPitData, svcmDataFrame, jCopula)
        cPit2_1 = getCpit2(cPitData, svcmDataFrame, jCopula)
      }
      
      for (iGroup in 1:nGroups)
      {
        cPit1InGroup = cPit1_1[indList[[iGroup]]]
        cPit2InGroup = cPit2_1[indList[[iGroup]]]
        
        nObsPerGroup = length(cPit1InGroup)
        lambdaInGroup = nObsPerGroup/nObs
        
        E[1 + 4*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,1])/lambdaInGroup 
        E[2 + 4*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,2])/lambdaInGroup
        E[3 + 4*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,3])/lambdaInGroup
        E[4 + 4*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,4])/lambdaInGroup
        
        E[(4*nGroups + iGroup),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$bInGroups[[iGroup]])/lambdaInGroup
      }
    }
  }
  
  
  return(E)
  
  
}

omegaMuSigmaRhoInSingleGroup = function(cPit1InGroup, cPit2InGroup, theta, nObs)
{
  
  omega = matrix(0,nrow = 5,ncol = 5)
  
  # Obtain the estimated parameters
  mu1 = theta[1]
  var1 = theta[2]
  mu2 = theta[3]
  var2 = theta[4]
  rho = theta[5]
  
  nObsPerGroup = length(cPit1InGroup)
  lambdaInGroup = nObsPerGroup/nObs
  
  bInGroups = rho - (cPit1InGroup - mu1) * (cPit2InGroup - mu2) / sqrt(var1*var2)
  
  aInGroups = cbind(mu1-cPit1InGroup,
                    var1-(cPit1InGroup-mu1)^2,
                    mu2-cPit2InGroup,
                    var2-(cPit2InGroup-mu2)^2)
  
  omega[1:4, 1:4] = 1/nObsPerGroup *(t(aInGroups) %*% aInGroups)/lambdaInGroup
  
  omega[5, 1:4] = 1/nObsPerGroup *(t(aInGroups) %*% bInGroups)/lambdaInGroup
  
  omega[5, 5] = mean(bInGroups^2)/lambdaInGroup
  
  return(list(aInGroups = aInGroups, bInGroups = bInGroups, omega = omega))
  
}


omegaMuSigmaRho = function(data, indList, cPit1, cPit2, theta)
{
  nObs = nrow(data)
  # Obtain the subsamples
  nGroups = length(indList)
  
  omega = matrix(0,nrow = 4*nGroups+nGroups,ncol = 4*nGroups+nGroups)
  
  aInGroups = vector("list",nGroups)
  bInGroups = vector("list",nGroups)
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[indList[[iGroup]]]
    cPit2InGroup = cPit2[indList[[iGroup]]]
    
    # Obtain the estimated parameters
    thetaInGroup = c(theta[(1 + 4*(iGroup-1)) : (4 + 4*(iGroup-1))], theta[(4*nGroups+iGroup)])
    
    xx = omegaMuSigmaRhoInSingleGroup(cPit1InGroup, cPit2InGroup, thetaInGroup, nObs)
    
    bInGroups[[iGroup]] = xx$bInGroups
    
    aInGroups[[iGroup]] = xx$aInGroups
    
    thisGroupStartIndMuSigma = 4*(iGroup-1)
    thisGroupMuSigmaIndexSet = (1 + thisGroupStartIndMuSigma):(4 + thisGroupStartIndMuSigma)
    thisGroupRhoIndex = 4*nGroups + iGroup
    thisGroupIndexSet = c(thisGroupMuSigmaIndexSet, thisGroupRhoIndex)
    
    omega[thisGroupIndexSet, thisGroupIndexSet] = xx$omega
    
  }
  
  listOfMultipliers = list(aInGroups=aInGroups,bInGroups=bInGroups)
  
  xx = t(omega)
  omega[upper.tri(omega)] = xx[upper.tri(xx)]
  
  return(list(omega =omega, listOfMultipliers = listOfMultipliers))
  
}



omegaRvine = function(data, svcmDataFrame, indList, cPitData, theta, withRanks)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nParametersFirstTree = sum(svcmDataFrame$nPar[1:(d-1)])
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the subsamples
  nGroups = length(indList)
  
  omega = matrix(0,nrow=nParameters+4*nGroups+nGroups,ncol=nParameters+4*nGroups+nGroups)
  
  xx = omegaMuSigmaRho(data, indList, cPit1, cPit2, theta)
  
  omegaCorrelations = xx$omega
  listOfMultipliers = xx$listOfMultipliers
  
  if(withRanks)
  {
    bSsp = bsspEstEqEqualCorr(data, svcmDataFrame, indList, cPitData, copulaInd, theta, listOfMultipliers)
    omegaCorrelations = omegaCorrelations + bSsp
  }
  
  omega[(nParameters+1):(nParameters+5*nGroups), (nParameters+1):(nParameters+5*nGroups)] = omegaCorrelations
  

  if (nParameters)
  {
    omegaWithLikesD = getOmegaWithLikesD(data, svcmDataFrame, cPitData)
    omegasWithLikesE = getOmegaWithLikesE(data, svcmDataFrame, indList, cPitData, listOfMultipliers)
    
    if(withRanks)
    {
      bSsp = bSspForCovWithRanks(data, svcmDataFrame, cPitData)
      omegaWithLikesD = omegaWithLikesD + bSsp
      
      bSsp = bsspEstEqEqualCorrCrossTerms(data, svcmDataFrame, indList, cPitData, copulaInd, theta, listOfMultipliers)
      omegasWithLikesE = omegasWithLikesE + bSsp
    }
    
    omega[1:nParameters,1:nParameters] = omegaWithLikesD
    omega[(nParameters+1):(nParameters+4*nGroups+nGroups),1:nParameters] = omegasWithLikesE
    
  }
  
  
  xx = t(omega)
  omega[upper.tri(omega)] = xx[upper.tri(xx)]
  
  
  return(omega)
}


getOmegaWithLikesCovE = function(data, svcmDataFrame, indList, cPitData, listOfMultipliers)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nGroups = length(indList)
  
  E = matrix(0,nrow=2*nGroups+nGroups,ncol=nParameters)
  
  for (jCopula in 1:nCopulas)
  {
    if (svcmDataFrame$nPar[jCopula])
    {
      family_1 = svcmDataFrame$family[jCopula]
      par_1 = svcmDataFrame$par[[jCopula]]
      if (jCopula<d)
      {
        cPit1_1 = data[,svcmDataFrame$var1[jCopula]]
        cPit2_1 = data[,svcmDataFrame$var2[jCopula]]
      }
      else
      {
        cPit1_1 = getCpit1(cPitData, svcmDataFrame, jCopula)
        cPit2_1 = getCpit2(cPitData, svcmDataFrame, jCopula)
      }
      
      for (iGroup in 1:nGroups)
      {
        cPit1InGroup = cPit1_1[indList[[iGroup]]]
        cPit2InGroup = cPit2_1[indList[[iGroup]]]
        
        nObsPerGroup = length(cPit1InGroup)
        lambdaInGroup = nObsPerGroup/nObs
        
        E[1 + 2*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,1])/lambdaInGroup 
        E[2 + 2*(iGroup-1),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$aInGroups[[iGroup]][,2])/lambdaInGroup
        
        E[(2*nGroups + iGroup),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1InGroup,cPit2InGroup,family_1,listOfMultipliers$bInGroups[[iGroup]])/lambdaInGroup
      }
    }
  }
  
  
  return(E)
  
  
}


omegaRvineCov = function(data, svcmDataFrame, indList, cPitData, theta, withRanks)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nParametersFirstTree = sum(svcmDataFrame$nPar[1:(d-1)])
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the subsamples
  nGroups = length(indList)
  
  omega = matrix(0,nrow=nParameters+2*nGroups+nGroups,ncol=nParameters+2*nGroups+nGroups)
  bb = matrix(0,nGroups,2*nGroups)
  cc = matrix(0,nGroups,nGroups)
  
  aInGroups = vector("list",nGroups)
  bInGroups = vector("list",nGroups)
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[indList[[iGroup]]]
    cPit2InGroup = cPit2[indList[[iGroup]]]
    dataInGroup = data[indList[[iGroup]],]
    
    # Obtain the estimated parameters
    mu1 = theta[(1 + 2*(iGroup-1))]
    mu2 = theta[(2 + 2*(iGroup-1))]
    
    nObsPerGroup = length(cPit1InGroup)
    lambdaInGroup = nObsPerGroup/nObs
    
    bInGroups[[iGroup]] = theta[(2*nGroups+iGroup)] - (cPit1InGroup - mu1) * (cPit2InGroup - mu2)
    
    aInGroups[[iGroup]] = cbind(mu1-cPit1InGroup,
                                mu2-cPit2InGroup)
    
    omega[(nParameters+1 + 2*(iGroup-1)):(nParameters+2 + 2*(iGroup-1)),
          (nParameters+1 + 2*(iGroup-1)):(nParameters+2 + 2*(iGroup-1))] = 1/nObsPerGroup *(t(aInGroups[[iGroup]]) %*% aInGroups[[iGroup]])/lambdaInGroup
    
    bb[iGroup,(1 + 2*(iGroup-1)):(2 + 2*(iGroup-1))] = 1/nObsPerGroup *(t(aInGroups[[iGroup]]) %*% bInGroups[[iGroup]])/lambdaInGroup
    
    cc[iGroup,iGroup] = mean(bInGroups[[iGroup]]^2)/lambdaInGroup
    
  }
  
  
  omega[(nParameters+2*nGroups+1):(nParameters+2*nGroups+nGroups),(nParameters+1):(nParameters+2*nGroups)] = bb
  
  omega[(nParameters+2*nGroups+1):(nParameters+2*nGroups+nGroups),(nParameters+2*nGroups+1):(nParameters+2*nGroups+nGroups)] = cc
  
  
  listOfMultipliers = list(aInGroups=aInGroups,bInGroups=bInGroups)
  
  if (nParameters)
  {
    omegaWithLikesD = getOmegaWithLikesD(data, svcmDataFrame, cPitData)
    if(withRanks)
    {
      bSsp = bSspForCovWithRanks(data, svcmDataFrame, cPitData)
      omegaWithLikesD = omegaWithLikesD + bSsp
    }
    omega[1:nParameters,1:nParameters] = omegaWithLikesD
    
    omega[(nParameters+1):(nParameters+2*nGroups+nGroups),1:nParameters] = getOmegaWithLikesCovE(data, svcmDataFrame, indList, cPitData, listOfMultipliers)
  }
  
  
  xx = t(omega)
  omega[upper.tri(omega)] = xx[upper.tri(xx)]
  
  
  return(omega)
}

