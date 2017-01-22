

deriv1LikeMult = function(theta,u1,u2,family,multFactor)
{
  
  result = grad(likeMultFactor,theta, method='simple',u1=u1,u2=u2,family=family,multFactor=multFactor)
  
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
  result = grad(likeMult,params2, method='simple',v1=v1,v2=v2,family2=family2,params1=params1,u1=u1,u2=u2,family1=family1)
  return(result)
}


deriv2LikeMult = function(params1,u1,u2,family1,params2,v1,v2,family2)
{
  
  result = jacobian(likeMultDeriv,params1, method='simple',u1=u1,u2=u2,family1=family1,params2=params2,v1=v1,v2=v2,family2=family2)
  
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
  result = grad(likeMultWithRanks,params2, method='simple',
                copulaInd2=copulaInd2,
                params1=params1,copulaInd1=copulaInd1,
                data=data, svcmDataFrame=svcmDataFrame, cPitData=cPitData)
  return(result)
}


deriv2LikeMultWithRanks = function(params1,copulaInd1, params2,copulaInd2, data, svcmDataFrame, cPitData)
{
  
  result = jacobian(likeMultDerivWithRanks,params1, method='simple',
                    copulaInd1=copulaInd1,
                    params2=params2,copulaInd2=copulaInd2,
                    data=data, svcmDataFrame=svcmDataFrame, cPitData=cPitData)
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
  bb = matrix(0,nGroups,4*nGroups)
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
    mu1 = theta[(1 + 4*(iGroup-1))]
    var1 = theta[(2 + 4*(iGroup-1))]
    mu2 = theta[(3 + 4*(iGroup-1))]
    var2 = theta[(4 + 4*(iGroup-1))]
    
    nObsPerGroup = length(cPit1InGroup)
    lambdaInGroup = nObsPerGroup/nObs
    
    bInGroups[[iGroup]] = theta[(4*nGroups+iGroup)] - (cPit1InGroup - mu1) * (cPit2InGroup - mu2) / sqrt(var1*var2)
    
    aInGroups[[iGroup]] = cbind(mu1-cPit1InGroup,
                        var1-(cPit1InGroup-mu1)^2,
                        mu2-cPit2InGroup,
                        var2-(cPit2InGroup-mu2)^2)
    
    omega[(nParameters+1 + 4*(iGroup-1)):(nParameters+4 + 4*(iGroup-1)),
          (nParameters+1 + 4*(iGroup-1)):(nParameters+4 + 4*(iGroup-1))] = 1/nObsPerGroup *(t(aInGroups[[iGroup]]) %*% aInGroups[[iGroup]])/lambdaInGroup
    
    bb[iGroup,(1 + 4*(iGroup-1)):(4 + 4*(iGroup-1))] = 1/nObsPerGroup *(t(aInGroups[[iGroup]]) %*% bInGroups[[iGroup]])/lambdaInGroup
    
    cc[iGroup,iGroup] = mean(bInGroups[[iGroup]]^2)/lambdaInGroup
    
  }
  
  
  omega[(nParameters+4*nGroups+1):(nParameters+4*nGroups+nGroups),(nParameters+1):(nParameters+4*nGroups)] = bb
  
  omega[(nParameters+4*nGroups+1):(nParameters+4*nGroups+nGroups),(nParameters+4*nGroups+1):(nParameters+4*nGroups+nGroups)] = cc
  

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
    
    omega[(nParameters+1):(nParameters+4*nGroups+nGroups),1:nParameters] = getOmegaWithLikesE(data, svcmDataFrame, indList, cPitData, listOfMultipliers)
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

