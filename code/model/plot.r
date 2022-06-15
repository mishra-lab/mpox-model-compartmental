# functions for plotting the model
library('ggplot2')

.out.labs = list(
  prev.prop    = 'Prevalence (%)',
  prev.abs     = 'Count',
  inc.rate     = 'Incidence rate (per 1000 person-days)',
  inc.rate.cum = 'Cumulative incidence (%)',
  inc.abs      = 'New infections (per day)',
  inc.abs.cum  = 'Cumulative infections'
)

.aggr.quantile = function(out,x,y,ci=.9,strat=c('city','risk','health')){
  # compute the median & ci quantiles on y within x, maintaining stratifications if available
  qci = c(.5,ci.to.q(ci))
  f = paste(y,'~',x,'+',paste(strat[strat %in% colnames(out)],collapse=' + '))
  return(do.call(data.frame,aggregate(formula(f),out,quantile,qci,names=F)))
}

.aes.quantile = function(x,y,...){
  # corresponding aes for .aggr.quantile using x, y, ymin, ymax
  return(do.call(aes_string,list(x=x,y=paste0(y,'.1'),ymin=paste0(y,'.2'),ymax=paste0(y,'.3'),...)))
}

plot.out = function(out,x='t',y='value',ylabel,ci=.9,...){
  # plot an output
  if (missing(ylabel)){ ylabel = .out.labs[[out$name[1]]] }
  if (length(unique(out$seed)) > 1){ # multiple model runs -> median line & quantile ribbon
    g = ggplot(.aggr.quantile(out,x,y,ci=ci),.aes.quantile(x,y,...)) +
      geom_line() +
      geom_ribbon(alpha=.2,color=NA)
  } else { # one model run -> one line
    out.aes = list(x=x,y=y,...)
    g = ggplot(out,do.call(aes_string,out.aes)) +
      geom_line()
  }
  # clean-up, etc.
   g = g +
    labs(x='Time (days)',y=ylabel)
  return(g)
}