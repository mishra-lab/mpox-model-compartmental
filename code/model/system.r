# functions for running the model
library('parallel')

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

sys.run.n = function(P.n,t,cpus=7){
  # run the model, usually in parallel
  if (cpus > 1){
    R.n = mclapply(P.n,sys.run,t=t,mc.cores=cpus)
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
  # print(sys.inc(list.update(P,sar.sex=0),X,t)/inc) # DEBUG
  inf = X[,,1:2] * inc
  dX[,,1:2] = dX[,,1:2] - inf
  dX[,,3]   = dX[,,3] + rowSums(inf,dim=2)
  # incubation
  dXi = X[,,3] / P$dur.exp
  dX[,,3] = dX[,,3] - dXi
  dX[,,4] = dX[,,4] + dXi
  # isolation
  dXi = X[,,4] / P$dur.inf / (1-P$iso.prop)
  dX[,,4] = dX[,,4] - dXi
  dX[,,5] = dX[,,5] + dXi
  # recovery
  dXi = X[,,5] / P$dur.inf / P$iso.prop
  dX[,,5] = dX[,,5] - dXi
  dX[,,6] = dX[,,6] + dXi
  # vaccination
  dXi = sys.vax(P,X,t)
  dX[,,1] = dX[,,1] - dXi
  dX[,,2] = dX[,,2] + dXi
  return(list(
    dX  = dX,
    inc = inc
  ))
}

sys.vax = function(P,X,t){
  if (t >= P$vax.t0 && t < (P$vax.t0 + P$vax.dt)){
    return(P$X.vax.city.risk / P$vax.dt)
  } else {
    return(0)
  }
}

sys.inc = function(P,X,t){
  # compute incidence
  prev.city.risk = X[,,4] / P$X.city.risk
  inc = (
    # multiply sar & 'C' (with mixing) by appropriate prevalence & sum across 'others'
    P$sar.sex * rowSums(sweep(P$C.sex,c(3,4),prev.city.risk,'*'),dim=2) +
    P$sar.com * rowSums(sweep(P$C.com,c(3,4),prev.city.risk,'*'),dim=2)
  )
  return(outer(inc,P$health.sus))
}

sys.R0 = function(P){
  # compute the R0 for each city (assuming no inter-city mixing)
  M.sex = def.mix(P$X.city.risk,P$C.sex.city.risk,P$asso.sex,1)
  M.com = def.mix(P$X.city.risk,P$C.com.city.risk,P$asso.com,1)
  R0 = lapply(list(A=1,B=2),function(city){
    Kij = function(i,j){
      (
        P$sar.sex * P$C.sex.city.risk[city,i] * M.sex[city,i,city,j] +
        P$sar.com * P$C.com.city.risk[city,i] * M.com[city,i,city,j]
      ) * P$dur.inf * (1-P$iso.prop) * P$X.city.risk[city,i] / P$X.city.risk[city,j]
    }
    K = array(c(Kij(1,1),Kij(2,1),Kij(1,2),Kij(2,2)),c(2,2))
    return(max(eigen(K)$values))
  })
}

sys.fit.R0 = function(R0,var,range,P=NULL,city='A'){
  # adjust P[[var]] within range so that sys.R0[[city]] == R0
  if (is.null(P)){ P = def.params() }
  obj.fun = function(x){
    P[[var]] = x
    R0.x = sys.R0(def.params.cond(P))[[city]]
    return((R0-R0.x)^2)
  }
  opt = optimize(obj.fun,range)
  if (opt$objective > 1e-3){
    warning(
      '\n  did not converge',
      '\n  objective: ',opt$objective,
      '\n  target R0: ',R0,
      '\n  var: "',var,'", current value = ',opt$minimum)
  }
  return(opt$minimum)
}
