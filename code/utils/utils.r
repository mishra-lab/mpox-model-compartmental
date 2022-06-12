library('parallel')

ndim = function(x){ length(dim(x)) }

rename.cols = function(x,...){
  # e.g. rename.cols(X,a='apples') would rename column 'a' to 'apples'
  map = list(...)
  for (name in names(map)){
    x[[map[[name]]]] = x[[name]]
    x[[name]] = NULL
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
