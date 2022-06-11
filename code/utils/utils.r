
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