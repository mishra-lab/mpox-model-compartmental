ndim = function(x){ length(dim(x)) }

root.path = function(...,create=FALSE){
  root = strsplit(file.path(getwd(),''),file.path('','code',''))[[1]][1]
  path = file.path(root,...)
  if (create & !dir.exists(dirname(path))){ dir.create(dirname(path),recursive=TRUE) }
  return(path)
}

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

list.merge = function(...){
  args = list(...)
  x = args[[1]]
  for (arg in args[2:length(args)]){
    for (name in names(arg)){
      x[[name]] = arg[[name]]
    }
  }
  return(x)
}

list.prefix = function(x,pre){
  return(setNames(x,paste0(pre,names(x))))
}

ci.to.q = function(ci){
  # e.g. ci=.95 -> c(.025,.975)
  return(c((1-ci)/2,1-(1-ci)/2))
}

colour.adj = function(col,f=0){
  col.rgb = col2rgb(col)
  if (f == 0){ col.adj = col.rgb }
  if (f <  0){ col.adj = col.rgb * (1 + f) }
  if (f >  0){ col.adj = col.rgb + (255 - col.rgb) * f }
  return(do.call(rgb,c(as.list(col.adj),maxColorValue=255)))
}
