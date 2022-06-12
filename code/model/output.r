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

.clean.out = function(out,name,aggr,city=NULL,risk=NULL,health=NULL,...){
  # add output name, remove source values (X) and add back some combined group names if aggr
  # e.g. colnames(out) = c(t, value, name, city, risk, health, seed)
  out$name = name
  out$X = NULL
  if (aggr){
    if (!is.null(city))   { out$city   = paste(city,  collapse=' ') }
    if (!is.null(risk))   { out$risk   = paste(risk,  collapse=' ') }
    if (!is.null(health)) { out$health = paste(health,collapse=' ') }
  }
  args = list(...)
  for (name in names(args)){ out[[name]] = args[[name]] }
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
# aggregation is across all stratifications or none; so, to aggregate across risk but not city,
# use: rbind(out.{fun}(...,city='A'),out.{fun}(...,city='B')), etc.

out.prevalence = function(R,t=R$t,city=.dn$city,risk=.dn$risk,health='inf',aggr=T){
  # infection prevalence, but can also be used for other health states
  # aggregation uses sum(num) / sum(den)
  D = .get.long(R,t,city,risk)
  if (aggr){
    out = aggregate(X~t,D,sum) # denominator
    out$value = aggregate(X~t+health,D[D$health %in% health,],sum)$X / out$X # num / den
  } else {
    out = aggregate(X~t+city+risk,D,sum) # denominator
    out$value = aggregate(X~t+city+risk+health,D[D$health %in% health,],sum)$X / out$X  # num / den
  }
  return(.clean.out(out,'prevalence',aggr,city=city,risk=risk,seed=R$P$seed))
}

out.incidence = function(R,t=R$t,city=.dn$city,risk=.dn$risk,health=c('sus','vax'),aggr=T,rate=T){
  # infection incidence, as a rate (per-susceptible) or absolute (counts)
  # rate aggregation uses sum(sus * inc) / sum(sus) - i.e. a weighted average of inc by sus
  # absolute aggregation uses sum(inf)
  D     = .get.long(R,t,city,risk,health)
  D$inc = .get.long(R,t,city,risk,health,name='inc')$inc
  D$inf = D$X * D$inc
  if (aggr){
    if (rate){ # per-susceptible
      out = aggregate(X~t,D,sum)
      out$value = aggregate(inf~t,D,sum)$inf / out$X
    } else { # infection counts
      out = rename.cols(aggregate(inf~t,D,sum),inf='value')
    }
  } else {
    if (rate){ # per-susceptible
      out = aggregate(X~t+city+risk+health,D,sum)
      out$value = aggregate(inf~t+city+risk+health,D,sum)$inf / out$X
    } else { # infection counts
      out = rename.cols(aggregate(inf~t+city+risk+health,D,sum),inf='value')
    }
  }
  return(.clean.out(out,'incidence',aggr,city=city,risk=risk,health=health,seed=R$P$seed))
}

