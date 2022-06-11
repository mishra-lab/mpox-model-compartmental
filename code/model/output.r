library('reshape2')

.get.long = function(R,t=R$t,city=.dimnames$city,risk=.dimnames$risk,health=.dimnames$health,name='X'){
  # extract "X" (or "inc") from R, melt it, and subset it
  D = melt(R[[name]],value.name=name)
  D = D[
    (D$t %in% t) &
    (D$city %in% city) &
    (D$risk %in% risk) &
    (D$health %in% health),
  ]
  return(D)
}

.clean.out = function(out,name,aggr,city=NULL,risk=NULL,health=NULL){
  # add output name, remove source values ("X") and add back some combined group names if aggr
  out$name = name
  out$X = NULL
  if (aggr){
    if (!is.null(city))   { out$city   = paste(city,  collapse=' ') }
    if (!is.null(risk))   { out$risk   = paste(risk,  collapse=' ') }
    if (!is.null(health)) { out$health = paste(health,collapse=' ') }
  }
  return(out)
}

out.prevalence = function(R,t=R$t,city=.dimnames$city,risk=.dimnames$risk,health='inf',aggr=T){
  D = .get.long(R,t,city,risk)
  if (aggr){
    out = aggregate(X~t,D,sum) # denominator
    out$value = aggregate(X~t+health,D[D$health %in% health,],sum)$X / out$X # num / den
  } else {
    out = aggregate(X~t+city+risk,D,sum) # denominator
    out$value = aggregate(X~t+city+risk+health,D[D$health %in% health,],sum)$X / out$X  # num / den
  }
  return(.clean.out(out,'prevalence',aggr,city=city,risk=risk))
}

out.incidence = function(R,t=R$t,city=.dimnames$city,risk=.dimnames$risk,health=c('sus','vax'),aggr=T,rate=T){
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
  return(.clean.out(out,'incidence',aggr,city=city,risk=risk,health=health))
}

