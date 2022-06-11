
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

