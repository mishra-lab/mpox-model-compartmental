# functions for plotting the model
library('ggplot2')
library('reshape2')

plot.prev = function(R,city,risk,aggr=T,...){
  # TODO: what a mess. maybe time to learn dplyr ...
  dim.strat = c(length(city), length(risk))
  num = R$X[,city,risk,4]
  den = rowSums(R$X[,city,risk,],dim=1+sum(dim.strat>1))
  d.names = dimnames(R$X)[1:3]
  if (aggr && sum(dim.strat>1)){
    prev = rowSums(num,dim=1) / rowSums(den,dim=1)
    d.names$city = paste(d.names$city[city],collapse=' ')
    d.names$risk = paste(d.names$risk[risk],collapse=' ')
    dim.total = c(length(R$t),1,1)
  } else {
    prev = num / den
    d.names$city = d.names$city[city]
    d.names$risk = d.names$risk[risk]
    dim.total = c(length(R$t),dim.strat)
  }
  g.data = melt(array(data = prev, dim = dim.total, dimnames = d.names))
  g = ggplot(g.data,aes(x=t,y=value,color=city,linetype=risk)) + 
    geom_line() +
    theme_light() +
    labs(x = 'Time (days)', y = 'Prevalence')
  return(g)
}