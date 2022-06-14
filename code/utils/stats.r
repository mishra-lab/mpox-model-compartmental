
# f.{distr} functions allow mean (m) & std dev (sd) parameterization of some common distributions
# where 'x' can be: d - density; p - cum density; q - quantile; r - random sample

f.gamma = function(x,m,sd,...){
  shape = m^2/sd^2
  rate = shape/m
  return(get(paste0(x,'gamma'))(...,shape=shape,rate=rate))
}

f.beta = function(x,m,sd,...){
  alpha = ((1-m)/sd^2 - 1/m)*m^2
  beta = alpha*(1/m - 1)
  return(get(paste0(x,'beta'))(...,shape1=alpha,shape2=beta))
}

confusion.solve = function(X,AP,PP,sens=NULL,spec=NULL,ppv=NULL,npv=NULL,dn=NULL){
  # given the total (X), actual positive (AP), predicted positive (PP),
  # and 1 other metric of the confusion matrix (sens,spec,ppv,npv),
  # resolve the confusion matrix & return all 4 metrics
  AN = X - AP # actual negative
  PN = X - PP # predicted negative
  confusion.alloc = function(TP){ # starting from true positive ...
    # |  X | PP : PN |
    # |--------------|  > AP = TP + FN
    # | AP | TP : FN |  > AN = FP + TN
    # | AN | FP : TN |  > PP = TP + FP
    # |--------------|  > PN = TN + FN
    FN = AP - TP # false negative
    FP = PP - TP # false positive
    TN = PN - FN # true negative
    return(list(
      X = array(c(TP,FP,FN,TN),c(2,2),dimnames=dn),
      sens = TP / PP,
      spec = TN / PN,
      ppv  = TP / (TP + FP),
      npv  = TN / (TN + FN)
    ))
  }
  obj.fun = function(TP){
    alloc = confusion.alloc(TP)
    j = 0
    if (!is.null(sens)){ j = j + (sens - alloc$sens)^2 }
    if (!is.null(spec)){ j = j + (spec - alloc$spec)^2 }
    if (!is.null(ppv)) { j = j + (ppv  - alloc$ppv )^2 }
    if (!is.null(npv)) { j = j + (npv  - alloc$npv )^2 }
    return(j)
  }
  if (PP==0){
    return(confusion.alloc(0))
  } else {
    return(confusion.alloc(optimize(obj.fun,c(0,AP))$minimum))
  }
}
