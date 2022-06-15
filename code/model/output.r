library('reshape2')

.get.long = function(R,t=R$t,city=.dn$city,risk=.dn$risk,health=.dn$health,name='X'){
  # extract X (or inc, etc.) from R, melt it, and subset it
  D = melt(R[[name]],value.name=name)
  D = D[
    (D$t %in% t) &
    (D$city %in% city) &
    (D$risk %in% risk) &
    (D$health %in% health),
  ]
  return(D)
}

.clean.out = function(out,name,...){
  # add output name, remove source values (X) and add back some combined group names
  # e.g. colnames(out) = c(t, value, name, city, risk, health, seed)
  out$name = name
  out$X = NULL
  args = list(...)
  for (name in names(args)){
    arg = args[[name]]
    if (!(name %in% colnames(out))){
      out[[name]] = ifelse(length(arg)==1,arg,paste(arg,collapse=' '))
    }
  }
  return(out)
}

out.fun.n = function(R.n,name,...){
  # compute an output for all elements in R.n
  return(do.call(rbind,lapply(R.n,get(paste0('out.',name)),...)))
}

# All main output functions do:
# - melt the data from R into long format (usually from R$X)
# - subset the data based on some stratifications
# - compute & possibly aggregate the output across remaining stratifications

out.prevalence = function(R,t=R$t,mode='prop',strat=NULL,scale=NULL,
                          city=.dn$city,risk=.dn$risk,health='inf'){
  # infection prevalence, but can also be used for other health states
  # prop aggregation uses sum(num) / sum(den)
  # absolute aggregate uses sum(num)
  D = .get.long(R,t,city=city,risk=risk)
  f.0      = formula(paste('X ~',paste(c('t',strat),collapse=' + ')))
  f.health = formula(paste('X ~',paste(c('t','health',strat),collapse=' + ')))
  if (mode %in% c('prop')){ # proportion
    out = aggregate(f.0,D,sum)
    out$value = aggregate(f.health,D[D$health %in% health,],sum)$X / out$X
    name = 'prev.prop'
  }
  if (mode %in% c('abs')){ # absolute counts
    out = rename.cols(aggregate(f.health,D[D$health %in% health,],sum),X='value')
    name = 'prev.abs'
  }
  scale = ifelse(!missing(scale),scale,switch(mode,prop=100,abs=1))
  out$value = out$value * scale
  return(.clean.out(out,name,city=city,risk=risk,health=health,seed=R$P$seed))
}

out.incidence = function(R,t=R$t,mode='rate',strat='health',scale=NULL,
                         city=.dn$city,risk=.dn$risk,health=c('sus','vax')){
  # infection incidence, as a rate (per-susceptible) or absolute (counts)
  # rate aggregation uses sum(sus * inc) / sum(sus) - i.e. a weighted average of inc by sus
  # absolute aggregation uses sum(inf)
  D     = .get.long(R,t,city=city,risk=risk,health=health)
  D$inc = .get.long(R,t,city=city,risk=risk,health=health,name='inc')$inc
  D$inf = D$X * D$inc
  f.X   = formula(paste('X ~',paste(c('t',strat),collapse=' + ')))
  f.inf = formula(paste('inf ~',paste(c('t',strat),collapse=' + ')))
  if (mode %in% c('rate','cum.rate')){ # per-susceptible
    out = aggregate(f.X,D,sum)
    out$value = aggregate(f.inf,D,sum)$inf / out$X
    name = 'inc.rate'
  }
  if (mode %in% c('abs','cum.abs')){ # infection counts
    out = rename.cols(aggregate(f.inf,D,sum),inf='value')
    name = 'inc.abs'
  }
  if (mode %in% c('cum.rate','cum.abs')){
    out$value = cumfun.group(out,strat)
    name = paste0(name,'.cum')
  }
  scale = ifelse(!missing(scale),scale,switch(mode,rate=1000,cum.rate=100,abs=1,cum.abs=1))
  out$value = out$value * scale
  return(.clean.out(out,name,city=city,risk=risk,health=health,seed=R$P$seed))
}

