library('parallel')

ndim = function(x){ length(dim(x)) }

cumfun.group = function(x,strat,fun=cumsum,name='value'){
  # apply a cumulative function to x[[value]], stratified by x[strat]
  return(do.call(ave,c(list(x=x[[name]],FUN=fun),x[strat])))
}

rename.cols = function(x,...){
  # e.g. rename.cols(X,a='apples') would rename column 'a' to 'apples'
  args = list(...)
  for (name in names(args)){
    x[[args[[name]]]] = x[[name]]
    x[[name]] = NULL
  }
  return(x)
}

list.update = function(x,...){
  # e.g. list.update(list(a=1,b=2),a=3) -> list(a=3,b=2)
  args = list(...)
  for (name in names(args)){
    x[[name]] = args[[name]]
  }
  return(x)
}

ci.to.q = function(ci){
  # e.g. ci=.95 -> c(.025,.975)
  return(c((1-ci)/2,1-(1-ci)/2))
}

par.lapply = function(x,fun,export=NULL,n.cores=7,...){
  # lapply fun on x in parallel, using n.cores, after exporting some variables
  cl = makeCluster(n.cores)
  clusterExport(cl,export)
  R = parLapply(cl,x,fun,...)
  stopCluster(cl)
  return(R)
}
