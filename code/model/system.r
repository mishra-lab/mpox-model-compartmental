# functions for running the model

sys.init.X = function(X0,t){
  # expand "X0" into a new dimension "t"
  X = array( # initialize with NA
    dim = c(length(t),dim(X0)),
    dimnames = c(list(t=t),dimnames(X0))
  )
  # copy X0 into the first index in time
  if (ndim(X0)==1){ X[1,] = X0 }
  if (ndim(X0)==2){ X[1,,] = X0 }
  if (ndim(X0)==3){ X[1,,,] = X0 }
  if (ndim(X0)==4){ X[1,,,,] = X0 }
  return(X)
}

sys.run.n = function(P.n,t,para=T){
  if (para){
    R.n = par.lapply(P.n,sys.run,t=t,export=c('ndim','sys.init.X','sys.dX','sys.inc'))
  } else {
    R.n = lapply(P.n,sys.run,t=t)
  }
}

sys.run = function(P,t){
  # runs the model
  X   = sys.init.X(P$X0,t)           # initialize main variable (population)
  inc = sys.init.X(P$X0[,,1:2]*NA,t) # initialize incidence
  # run the model
  for (i in seq(1,length(t))){
    Ri = sys.dX(P,X[i,,,],t[i])
    inc[i,,,] = Ri$inc # store incidence
    if (i < length(t)){ # last iteration (i) only computes incidence
      X[i+1,,,]  = X[i,,,] + (t[i+1] - t[i]) * Ri$dX # Euler step
    }
  }
  return(list(
    P = P,
    X = X,
    t = t,
    inc = inc
  ))
}

sys.dX = function(P,X,t){
  # compute dX/dt for this X
  dX  = 0*X # initialize
  # incidence (to exposed)
  inc = sys.inc(P,X,t)
  inf = sweep(X[,,1:2],c(1,2),inc,'*')
  dX[,,1:2] = dX[,,1:2] - inf
  dX[,,3]   = dX[,,3] + rowSums(inf,dim=2)
  # incubation
  dXi = X[,,3] / P$dur.exp
  dX[,,3] = dX[,,3] - dXi
  dX[,,4] = dX[,,4] + dXi
  # recovery
  dXi = X[,,4] / P$dur.inf
  dX[,,4] = dX[,,4] - dXi
  dX[,,5] = dX[,,5] + dXi
  return(list(
    dX  = dX,
    inc = inc
  ))
}

sys.inc = function(P,X,t){
  # compute incidence
  prev.city.risk = X[,,4] / P$X.city.risk
  inc = (
    # multiply beta & 'C' (with mixing) by appropriate prevalence & sum across 'others'
    P$beta.sex * rowSums(sweep(P$C.sex,c(3,4),prev.city.risk,'*'),dim=2) +
    P$beta.com * rowSums(sweep(P$C.com,c(3,4),prev.city.risk,'*'),dim=2)
  )
  # TODO: vax
  return(inc)
}