

deriv2Like = function(theta,u1,u2,family)
{
  
  result = hessian(like,theta,u1=u1,u2=u2,family=family)
  
  return(result)
}


deriv1LikeMult = function(theta,u1,u2,family,multFactor)
{
  
  result = grad(likeMultFactor,theta,u1=u1,u2=u2,family=family,multFactor=multFactor)
  
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
  result = grad(likeMult,params2,v1=v1,v2=v2,family2=family2,params1=params1,u1=u1,u2=u2,family1=family1)
  return(result)
}


deriv2LikeMult = function(params1,u1,u2,family1,params2,v1,v2,family2)
{
  
  result = jacobian(likeMultDeriv,params1,u1=u1,u2=u2,family1=family1,params2=params2,v1=v1,v2=v2,family2=family2)
  
  return(result)
}


getOmegaWithLikes = function(data, svcmDataFrame, indList, cPitData, theta, listOfMultipliers)
{
  
  d <- ncol(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  
  D = matrix(0,nrow=nParameters,ncol=nParameters)
  
  nGroups = length(indList)
  
  E = matrix(0,nrow=4+nGroups,ncol=nParameters)
  
  for (jCopula in 1:nCopulas)
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
    
    E[1,svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1_1,cPit2_1,family_1,listOfMultipliers$a[,1])
    E[2,svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1_1,cPit2_1,family_1,listOfMultipliers$a[,2])
    E[3,svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1_1,cPit2_1,family_1,listOfMultipliers$a[,3])
    E[4,svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1_1,cPit2_1,family_1,listOfMultipliers$a[,4])
    
    for (iGroup in 1:nGroups)
    {
      E[(4+iGroup),svcmDataFrame$parInd[[jCopula]]] = deriv1LikeMult(par_1,cPit1_1[indList[[iGroup]]],cPit2_1[indList[[iGroup]]],family_1,listOfMultipliers$bInGroups[[iGroup]])
    }
    
  }
  
  
  return(list(D=D,E=E))
  
  
}


omegaRvine = function(data, svcmDataFrame, indList, cPitData, theta)
{
  
  d <- ncol(data)
  nCopulas = d*(d-1)/2-1
  
  nParameters = sum(svcmDataFrame$nPar[1:nCopulas])
  nParametersFirstTree = sum(svcmDataFrame$nPar[1:(d-1)])
  
  # Obtain the cPits to be tested
  copulaInd = nrow(svcmDataFrame)
  cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
  cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
  
  # Obtain the estimated parameters
  mu1 = theta[1]
  var1 = theta[2]
  
  mu2 = theta[3]
  var2 = theta[4]
  
  a = cbind(mu1-cPit1,
            var1-(cPit1-mu1)^2,
            mu2-cPit2,
            var2-(cPit2-mu2)^2)
  
  aa = matrix(0,4,4)
  
  aa[1,1] = mean(a[,1]^2)
  aa[2,2] = mean(a[,2]^2)
  aa[3,3] = mean(a[,3]^2)
  aa[4,4] = mean(a[,4]^2)
  
  aa[2,1] = mean(a[,2]*a[,1])
  aa[3,1] = mean(a[,3]*a[,1])
  aa[4,1] = mean(a[,4]*a[,1])
  
  aa[3,2] = mean(a[,3]*a[,2])
  aa[4,2] = mean(a[,4]*a[,2])
  
  aa[4,3] = mean(a[,4]*a[,3])
  
  # Obtain the subsamples
  nGroups = length(indList)
  nObs = nrow(data)
  
  omega = matrix(0,nrow=nParameters+4+nGroups,ncol=nParameters+4+nGroups)
  bb = matrix(0,nGroups,4)
  cc = matrix(0,nGroups,nGroups)
  
  aInGroups = vector("list",nGroups)
  bInGroups = vector("list",nGroups)
  
  for (iGroup in 1:nGroups)
  {
    # Obtain the subsample
    cPit1InGroup = cPit1[indList[[iGroup]]]
    cPit2InGroup = cPit2[indList[[iGroup]]]
    dataInGroup = data[indList[[iGroup]],]
    
    nObsPerGroup = length(cPit1InGroup)
    lambdaInGroup = nObsPerGroup/nObs
    
    bInGroups[[iGroup]] = theta[(4+iGroup)] - (cPit1InGroup - mu1) * (cPit2InGroup - mu2) / sqrt(var1*var2)
    
    aInGroups[[iGroup]] = cbind(mu1-cPit1InGroup,
                        var1-(cPit1InGroup-mu1)^2,
                        mu2-cPit2InGroup,
                        var2-(cPit2InGroup-mu2)^2)
    
    bb[iGroup,1] = mean(aInGroups[[iGroup]][,1]*bInGroups[[iGroup]])
    bb[iGroup,2] = mean(aInGroups[[iGroup]][,2]*bInGroups[[iGroup]])
    bb[iGroup,3] = mean(aInGroups[[iGroup]][,3]*bInGroups[[iGroup]])
    bb[iGroup,4] = mean(aInGroups[[iGroup]][,4]*bInGroups[[iGroup]])
    
    cc[iGroup,iGroup] = mean(bInGroups[[iGroup]]^2)/lambdaInGroup
    
  }
  
  
  omega[(nParameters+1):(nParameters+4),(nParameters+1):(nParameters+4)] = aa
  
  omega[(nParameters+5):(nParameters+4+nGroups),(nParameters+1):(nParameters+4)] = bb
  
  omega[(nParameters+5):(nParameters+4+nGroups),(nParameters+5):(nParameters+4+nGroups)] = cc
  

  listOfMultipliers = list(a=a,aInGroups=aInGroups,bInGroups=bInGroups)

  res = getOmegaWithLikes(data, svcmDataFrame, indList, cPitData, theta, listOfMultipliers)
  
  omega[1:nParameters,1:nParameters] = res$D
  omega[(nParameters+1):(nParameters+4+nGroups),1:nParameters] = res$E
  
  omega = t(triang(omega))+omega-Triang(omega)
  
  
  return(omega)
}

