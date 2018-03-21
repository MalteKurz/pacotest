

getParAsScalars = function(nPar,par)
{
  if (nPar == 0)
  {
    params = c(0,0)
  }
  else if (nPar == 1)
  {
    params = c(par,0)
  }
  else
  {
    params = par
  }
  
  return(params)
}


getSideIfParameterAtBound = function(params,family)
{
  parIsCloseToUpperBound = checkIfParIsCloseToUpperBound(params,family)
  
  nPar = getNumbOfParameters(family)
  side = rep(NA,nPar)
  
  side[parIsCloseToUpperBound] = -1
  
  return(side)
}

checkIfParIsCloseToUpperBound = function(params,family)
{
  nPar = getNumbOfParameters(family)
  par = getParAsScalars(nPar,params)
  
  delta = 1e-4
  
  isClose = rep(FALSE, nPar)
  
  if (family == 1 || family == 2)
  { # Gaussian & Student t
    if (par[1] + delta >= 1) { isClose[1] = TRUE }
  } else if (family == 23 || family == 33)
  { # Rotated Clayton (90 and 270 degrees)
    if (par[1] + delta >= 0) { isClose[1] = TRUE }
  } else if (family == 4 || family == 14)
  { # (Survival) Gumbel
    if (par[1] + delta >= 100) { isClose[1] = TRUE }
  } else if (family == 5)
  { # Frank
    if (par[1] + delta == 0){ isClose[1] = TRUE }
  } else if (family == 24 || family == 34 || family == 26 || family == 36)
  { # Rotated Gumbel (90 and 270 degrees) & Rotated Joe (90 and 270 degrees)
    if (par[1] + delta >= -1){ isClose[1] = TRUE }
  }  else if (family == 7 || family == 17 || family == 9 || family == 19)
  { # (Survival) BB1 & (Survival) BB7
    if (par[1] + delta >= 5){ isClose[1] = TRUE }
    if (par[2] + delta >= 6){ isClose[2] = TRUE }
  }  else if (family == 8 || family == 18)
  { # (Survival) BB6
    if (par[1] + delta >= 6){ isClose[1] = TRUE }
    if (par[2] + delta >= 6){ isClose[2] = TRUE }
  }  else if (family == 10 || family == 20)
  { # (Survival) BB8
    if (par[1] + delta >= 6){ isClose[1] = TRUE }
    if (par[2] + delta >= 1){ isClose[2] = TRUE }
  }  else if (family == 27 || family == 37)
  { # Rotated BB1 (90 and 270 degrees)
    if (par[1] + delta >= 0){ isClose[1] = TRUE }
    if (par[2] + delta >= -1){ isClose[2] = TRUE }
  }   else if (family == 28 || family == 38)
  { # Rotated BB6 (90 and 270 degrees)
    if (par[1] + delta >= -1){ isClose[1] = TRUE }
    if (par[2] + delta >= -1){ isClose[2] = TRUE }
  }    else if (family == 29 || family == 39 || family == 30 || family == 40)
  { # Rotated BB7 (90 and 270 degrees) & Rotated BB8 (90 and 270 degrees)
    if (par[1] + delta >= -1){ isClose[1] = TRUE }
    if (par[2] + delta >= 0){ isClose[2] = TRUE }
  }    else if (family == 104 || family == 114 || family == 204 || family == 214)
  { # (Survival) Tawn type 1 & (Survival) Tawn type 2
    if (par[2] + delta >= 1){ isClose[2] = TRUE }
  } else if (family == 124 || family == 134 || family == 224 || family == 234)
  { # Tawn type 1 (90 and 270 degrees) & Tawn type 2 (90 and 270 degrees)
    if (par[1] + delta >= -1){ isClose[1] = TRUE }
    if (par[2] + delta >= 1){ isClose[2] = TRUE }
  } 
  
  return(isClose)
  
}



hfun = function(family,cPit1,cPit2,params)
{
  out = BiCopHfunc1(cPit2, cPit1, family, params[1], params[2])
  
  #out = .C("Hfunc1",
  #         as.integer(family),
  #         as.integer(length(cPit1)), 
  #         as.double(cPit1), 
  #         as.double(cPit2), 
  #         as.double(params[1]),
  #         as.double(params[2]), 
  #         as.double(rep(0, length(cPit1))), 
  #         PACKAGE = "VineCopula")[[7]]
  
  return(out)
}


vfun = function(family,cPit1,cPit2,params)
{
  out = BiCopHfunc2(cPit2, cPit1, family, params[1], params[2])
  
  #out = .C("Hfunc2",
  #         as.integer(family),
  #         as.integer(length(cPit1)), 
  #         as.double(cPit2), 
  #         as.double(cPit1), 
  #         as.double(params[1]),
  #         as.double(params[2]), 
  #         as.double(rep(0, length(cPit1))), 
  #         PACKAGE = "VineCopula")[[7]]
  
  return(out)
}


getCpit1 = function(cPitData, svcmDataFrame, copulaInd)
{
  
  #if (cPit1hfun[jCopula]) # That's always the case
  cPit1 = cPitData$hfun[,svcmDataFrame$cPit1Ind[copulaInd]]
  
  return(cPit1)
  
}


getCpit2 = function(cPitData, svcmDataFrame, copulaInd)
{
  
  if (svcmDataFrame$cPit2hfun[copulaInd])
  {
    cPit2 = cPitData$hfun[,svcmDataFrame$cPit2Ind[copulaInd]]
  }
  else
  {
    cPit2 = cPitData$vfun[,svcmDataFrame$cPit2Ind[copulaInd]]
  }
  
  return(cPit2)
  
}

getCpit1Deriv = function(cPitData, svcmDataFrame, copulaInd, variable)
{
  
  #if (cPit1hfun[jCopula]) # That's always the case
  cPit1Deriv = cPitData$hfunDerivs[,svcmDataFrame$cPit1Ind[copulaInd], variable]
  
  return(cPit1Deriv)
  
}


getCpit2Deriv = function(cPitData, svcmDataFrame, copulaInd, variable)
{
  
  if (svcmDataFrame$cPit2hfun[copulaInd])
  {
    cPit2Deriv = cPitData$hfunDerivs[,svcmDataFrame$cPit2Ind[copulaInd], variable]
  }
  else
  {
    cPit2Deriv = cPitData$vfunDerivs[,svcmDataFrame$cPit2Ind[copulaInd], variable]
  }
  
  return(cPit2Deriv)
  
}


hfunCpit2 =  function(cPit2, family, cPit1, params)
{
  result = hfun(family,cPit1,cPit2,params)
  
  return(result)
}

hfunDerivCpit1 = function(family,cPit1,cPit2,params)
{
  result = BiCopPDF(cPit1, cPit2, family, params[1], params[2])
  
  return(result)
}

hfunDerivCpit2 = function(family,cPit1,cPit2,params)
{
  xxSide = vector(length=length(cPit2))
  xxSide[] = NA
  xxSide[cPit2>0.99] = -1
  xxSide[cPit2<0.01] = 1
  result = grad(hfunCpit2,cPit2, side = xxSide, method='simple', family=family, cPit1=cPit1, params=params)
  
  return(result)
}

vfunCpit1 =  function(cPit1, family, cPit2, params)
{
  result = vfun(family,cPit1,cPit2,params)
  
  return(result)
}

vfunDerivCpit1 = function(family,cPit1,cPit2,params)
{
  xxSide = vector(length=length(cPit1))
  xxSide[] = NA
  xxSide[cPit1>0.99] = -1
  xxSide[cPit1<0.01] = 1
  result = grad(vfunCpit1,cPit1, side = xxSide, method='simple', family=family, cPit2=cPit2, params=params)
  
  return(result)
}

vfunDerivCpit2 = function(family,cPit1,cPit2,params)
{
  result = BiCopPDF(cPit1, cPit2, family, params[1], params[2])
  
  return(result)
}


getCpitsFromVine <- function(data, svcmDataFrame, withDerivs = FALSE)
{
  
  d <- ncol(data)
  nObs <- nrow(data)
  nCopulas = d*(d-1)/2
  
  cPitData <- list()
  cPitData$hfun <- array(NA, dim = c(nObs, nCopulas))
  cPitData$vfun <- array(NA, dim = c(nObs, nCopulas))
  
  if (withDerivs == TRUE)
  {
    cPitData$hfunDerivs <- array(0, dim = c(nObs, nCopulas,d))
    cPitData$vfunDerivs <- array(0, dim = c(nObs, nCopulas,d))
    
  }
  
  # First tree
  for (jCopula in 1:(d-1)) {
    cPit1 = data[,svcmDataFrame$var1[jCopula]]
    cPit2 = data[,svcmDataFrame$var2[jCopula]]
    params = getParAsScalars(svcmDataFrame$nPar[jCopula],svcmDataFrame$par[[jCopula]])
    if (svcmDataFrame$hfun[jCopula])
    {
      cPitData$hfun[, jCopula] = hfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    if (svcmDataFrame$vfun[jCopula])
    {
      cPitData$vfun[, jCopula] = vfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    
    if (withDerivs == TRUE)
    {
      #influencingVariables = c(svcmDataFrame$var1[jCopula],svcmDataFrame$var2[jCopula],svcmDataFrame$condset[jCopula])
      
      if (svcmDataFrame$hfun[jCopula])
      {
        cPitData$hfunDerivs[, jCopula, svcmDataFrame$var1[jCopula]] = hfunDerivCpit1(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        cPitData$hfunDerivs[, jCopula, svcmDataFrame$var2[jCopula]] = hfunDerivCpit2(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        
      }
      if (svcmDataFrame$vfun[jCopula])
      {
        cPitData$vfunDerivs[, jCopula, svcmDataFrame$var1[jCopula]] = vfunDerivCpit1(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        cPitData$vfunDerivs[, jCopula, svcmDataFrame$var2[jCopula]] = vfunDerivCpit2(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        
      }
    }
    
  }
  
  for (jCopula in d:nCopulas) {
    
    cPit1 = getCpit1(cPitData, svcmDataFrame, jCopula)
    cPit2 = getCpit2(cPitData, svcmDataFrame, jCopula)
    
    params = getParAsScalars(svcmDataFrame$nPar[jCopula],svcmDataFrame$par[[jCopula]])
    
    if (svcmDataFrame$hfun[jCopula])
    {
      cPitData$hfun[, jCopula] = hfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    
    if (svcmDataFrame$vfun[jCopula])
    {
      cPitData$vfun[, jCopula] = vfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    
    if (withDerivs == TRUE)
    {
      condset = svcmDataFrame$condset[jCopula]
      
      if (svcmDataFrame$hfun[jCopula])
      {
        xxHfunDerivCpit1 = hfunDerivCpit1(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        xxHfunDerivCpit2 = hfunDerivCpit2(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        
        cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, jCopula, svcmDataFrame$var1[jCopula])
        cPitData$hfunDerivs[, jCopula, svcmDataFrame$var1[jCopula]] = xxHfunDerivCpit1 * cPit1Deriv
        
        cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, jCopula, svcmDataFrame$var2[jCopula])
        cPitData$hfunDerivs[, jCopula, svcmDataFrame$var2[jCopula]] = xxHfunDerivCpit2 * cPit2Deriv
        
        for (condsetVariable in condset)
        {
          cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, jCopula, condsetVariable)
          cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, jCopula, condsetVariable)
          cPitData$hfunDerivs[, jCopula, condsetVariable] = xxHfunDerivCpit1 * cPit1Deriv + xxHfunDerivCpit2 * cPit2Deriv
          
        }
        
      }
      
      if (svcmDataFrame$vfun[jCopula])
      {
        xxVfunDerivCpit1 = vfunDerivCpit1(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        xxVfunDerivCpit2 = vfunDerivCpit2(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
        
        cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, jCopula, svcmDataFrame$var1[jCopula])
        cPitData$vfunDerivs[, jCopula, svcmDataFrame$var1[jCopula]] = xxVfunDerivCpit1 * cPit1Deriv
        
        cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, jCopula, svcmDataFrame$var2[jCopula])
        cPitData$vfunDerivs[, jCopula, svcmDataFrame$var2[jCopula]] = xxVfunDerivCpit2 * cPit2Deriv
        
        for (condsetVariable in condset)
        {
          cPit1Deriv = getCpit1Deriv(cPitData, svcmDataFrame, jCopula, condsetVariable)
          cPit2Deriv = getCpit2Deriv(cPitData, svcmDataFrame, jCopula, condsetVariable)
          cPitData$vfunDerivs[, jCopula, condsetVariable] = xxVfunDerivCpit1 * cPit1Deriv + xxVfunDerivCpit2 * cPit2Deriv
          
        }
        
      }
    }
    
  }

return(cPitData)
}


extractParametersToVectors = function(svcmDataFrame, copulaInd)
{
  
  cPit1ParInd = sort(svcmDataFrame$cPit1ParInd[[copulaInd]])
  cPit2ParInd = sort(svcmDataFrame$cPit2ParInd[[copulaInd]])
  
  cPit1CopulaInd = sort(svcmDataFrame$cPit1CopulaInd[[copulaInd]])
  cPit2CopulaInd = sort(svcmDataFrame$cPit2CopulaInd[[copulaInd]])
  
  parCpit1 = unlist(svcmDataFrame$par[cPit1CopulaInd])
  parCpit2 = unlist(svcmDataFrame$par[cPit2CopulaInd])
  
  parIsCloseToUpperBoundCpit1 = unlist(svcmDataFrame$parIsCloseToUpperBound[cPit1CopulaInd])
  parIsCloseToUpperBoundCpit2 = unlist(svcmDataFrame$parIsCloseToUpperBound[cPit2CopulaInd])
  
  copulaParInd = svcmDataFrame$parInd[[copulaInd]]
  parCopula = svcmDataFrame$par[[copulaInd]]
  parIsCloseToUpperBoundCopula = svcmDataFrame$parIsCloseToUpperBound[[copulaInd]]
  
  # Collect everything in one variable
  cPitsParInd = sort(unique(c(copulaParInd,
                              cPit1ParInd,
                              cPit2ParInd)))
  
  cPitsCopulaInd = sort(unique(c(copulaInd,
                                 cPit1CopulaInd,
                                 cPit2CopulaInd)))
  
  parCpits = unlist(svcmDataFrame$par[cPitsCopulaInd])
  
  parIsCloseToUpperBoundCpits = unlist(svcmDataFrame$parIsCloseToUpperBound[cPitsCopulaInd])
  
  # The same but without the parameters of the last copula
  cPitsWithoutCopulaParInd = sort(unique(c(cPit1ParInd,
                              cPit2ParInd)))
  
  cPitsWithoutCopulaCopulaInd = sort(unique(c(cPit1CopulaInd,
                                 cPit2CopulaInd)))
  
  parCpitsWithoutCopula = unlist(svcmDataFrame$par[cPitsWithoutCopulaCopulaInd])
  
  parIsCloseToUpperBoundCpitsWithoutCopula = unlist(svcmDataFrame$parIsCloseToUpperBound[cPitsWithoutCopulaCopulaInd])
  
  return(list(parCopula=parCopula, copulaParInd=copulaParInd, copulaInd=copulaInd, parIsCloseToUpperBoundCopula=parIsCloseToUpperBoundCopula,
              parCpit1=parCpit1, cPit1ParInd=cPit1ParInd, cPit1CopulaInd=cPit1CopulaInd, parIsCloseToUpperBoundCpit1=parIsCloseToUpperBoundCpit1,
              parCpit2=parCpit2, cPit2ParInd=cPit2ParInd, cPit2CopulaInd=cPit2CopulaInd, parIsCloseToUpperBoundCpit2=parIsCloseToUpperBoundCpit2,
              parCpits=parCpits, cPitsParInd=cPitsParInd, cPitsCopulaInd=cPitsCopulaInd, parIsCloseToUpperBoundCpits=parIsCloseToUpperBoundCpits,
              parCpitsWithoutCopula=parCpitsWithoutCopula, cPitsWithoutCopulaParInd=cPitsWithoutCopulaParInd, cPitsWithoutCopulaCopulaInd=cPitsWithoutCopulaCopulaInd, parIsCloseToUpperBoundCpitsWithoutCopula=parIsCloseToUpperBoundCpitsWithoutCopula))
  
}


insertParameterVectorsIntoDataFrame = function(svcmDataFrame,parVector,copulaIndVector)
{
  
  copulasNotNeeded = setdiff(svcmDataFrame$copulaInd, copulaIndVector)
  
  svcmDataFrame$par[copulasNotNeeded] = rep(list(NULL), length(copulasNotNeeded))
  
  I = 1
  for (jCopula in copulaIndVector) {
    
    if (svcmDataFrame$nPar[jCopula] == 0)
    {
      
    }
    else if (svcmDataFrame$nPar[jCopula] == 1)
    {
      svcmDataFrame$par[[jCopula]] = parVector[I]
      I=I+1
    }
    else
    {
      svcmDataFrame$par[[jCopula]] = parVector[I:(I+1)]
      I=I+2
    }
    
  }
  
  
  return(svcmDataFrame)
  
}


computeCpits <- function(data, svcmDataFrame, copulaInd, whichCpit)
{
  d <- ncol(data)
  nObs <- nrow(data)
  nCopulas = d*(d-1)/2
  
  cPitData <- list()
  cPitData$hfun <- array(NA, dim = c(nObs, nCopulas))
  cPitData$vfun <- array(NA, dim = c(nObs, nCopulas))
  
  if (whichCpit == 'cPit1')
  {
  xx = svcmDataFrame$cPit1CopulaInd[[copulaInd]]
  }
  else if (whichCpit == 'cPit2')
  {
    xx = svcmDataFrame$cPit2CopulaInd[[copulaInd]]
  }
  else if (whichCpit == 'cPitPair')
  {
    xx = c(svcmDataFrame$cPit1CopulaInd[[copulaInd]],
           svcmDataFrame$cPit2CopulaInd[[copulaInd]])
  }
  # Copulas needed
  copulasIndFirstTree = xx[xx<d]
  copulasIndHigherTree = xx[xx>=d]
  
  # First tree
  for (jCopula in copulasIndFirstTree)
    {
    cPit1 = data[,svcmDataFrame$var1[jCopula]]
    cPit2 = data[,svcmDataFrame$var2[jCopula]]
    params = getParAsScalars(svcmDataFrame$nPar[jCopula],svcmDataFrame$par[[jCopula]])
    if (svcmDataFrame$hfun[jCopula])
    {
      cPitData$hfun[, jCopula] = hfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    if (svcmDataFrame$vfun[jCopula])
    {
      cPitData$vfun[, jCopula] = vfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
  }
  
  
  for (jCopula in copulasIndHigherTree)
    {
    
    cPit1 = getCpit1(cPitData, svcmDataFrame, jCopula)
    cPit2 = getCpit2(cPitData, svcmDataFrame, jCopula)
    
    params = getParAsScalars(svcmDataFrame$nPar[jCopula],svcmDataFrame$par[[jCopula]])
    
    if (svcmDataFrame$hfun[jCopula])
    {
      cPitData$hfun[, jCopula] = hfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
    if (svcmDataFrame$vfun[jCopula])
    {
      cPitData$vfun[, jCopula] = vfun(svcmDataFrame$family[jCopula],cPit1,cPit2,params)
    }
  }
  
  cPitOut = list()
  
  if (whichCpit == 'cPit1' || whichCpit == 'cPitPair')
  { 
    cPit1 = getCpit1(cPitData, svcmDataFrame, copulaInd)
    
    if (whichCpit == 'cPitPair')
    {
      cPitOut$cPit1 = cPit1
    }
    else
    {
      cPitOut = cPit1
    }
  }
  if (whichCpit == 'cPit2' || whichCpit == 'cPitPair')
  {
    cPit2 = getCpit2(cPitData, svcmDataFrame, copulaInd)
    
    if (whichCpit == 'cPitPair')
    {
      cPitOut$cPit2 = cPit2
    }
    else
    {
      cPitOut = cPit2
    }
  }
  
  return(cPitOut)
  
}


