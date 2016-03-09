

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


getOmegaWithLikesD = function(data, svcmDataFrame, cPitData)
{
  
  d <- ncol(data)
  nObs = nrow(data)
  nCopulas = d*(d-1)/2-1
  
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



omegaRvine = function(data, svcmDataFrame, indList, cPitData, theta)
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
    omega[1:nParameters,1:nParameters] = getOmegaWithLikesD(data, svcmDataFrame, cPitData)
    omega[(nParameters+1):(nParameters+4*nGroups+nGroups),1:nParameters] = getOmegaWithLikesE(data, svcmDataFrame, indList, cPitData)
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
  
  
  return(list(D=D,E=E))
  
  
}


omegaRvineCov = function(data, svcmDataFrame, indList, cPitData, theta)
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
    omega[1:nParameters,1:nParameters] = getOmegaWithLikesD(data, svcmDataFrame, cPitData)
    omega[(nParameters+1):(nParameters+2*nGroups+nGroups),1:nParameters] = getOmegaWithLikesCovE(data, svcmDataFrame, indList, cPitData)
  }
  
  
  xx = t(omega)
  omega[upper.tri(omega)] = xx[upper.tri(xx)]
  
  
  return(omega)
}

