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

.get.brewer = function(n,palette='Spectral',...){
  # interpolate brewer colours for any n, since tidy team is pedantic about discrete vs continuous
  return(suppressWarnings({
    colorRampPalette(RColorBrewer::brewer.pal(99,palette))(n)
  }))
}

.plot.clean = function(g){
  # apply some common adjustments
  g + theme_light() +
    theme(strip.background=element_rect(fill='gray90'),strip.text = element_text(color='black'))
}

.facet.letters = function(data,x,y,fh,fv,pos=c(.1,.9),...){
  data.lab = setNames(expand.grid(unique(data[[fh]]),unique(data[[fv]])),c(fh,fv))
  data.lab$x = quantile(data[[x]],pos[1])
  data.lab$y = quantile(data[[y]],pos[2])
  data.lab$lab = letters[1:nrow(data.lab)]
  return(geom_text(data=data.lab,aes(label=lab,x=x,y=y,...)))
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
    labs(x='Time (days)',y=ylabel) +
    lims(y=c(0,NA))
  .plot.clean(g)
}

show.vax = function(P){
  # draw a rectangle for the period of vax roll-out
  annotate('rect',xmin=P$vax.t0,xmax=P$vax.t0+P$vax.dt,ymin=-Inf,ymax=Inf,alpha=.15)
}

plot.save = function(...,ext='.pdf',w=4,h=3){
  # save a plot to default directory
  ggsave(paste0(root.path('out','fig','model',...,create=TRUE),ext),w=w,h=h)
}
