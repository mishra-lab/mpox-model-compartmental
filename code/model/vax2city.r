source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')

city.colours = .get.brewer(11)[c(2,10)]

v2c.examples = function(){
  # plot some outputs (incidence, cum.infections) for a few selected conditions & vax allocations
  t = def.t()
  P.list = list(
    'default-100A' = def.params()
  )
  for (case in names(P.list)){
    P = P.list[[case]]
    R = sys.run(P,t)
    outs = do.call(rbind,list(
      out.incidence(R,mode='rate',   strat=c('city','risk')),
      out.incidence(R,mode='cum.abs',strat=c('city','risk'))
    ))
    out.names = unique(outs$name)
    outs$name = factor(outs$name,levels=out.names,labels=.out.labs[out.names])
    g = plot.out(outs,color='city',linetype='risk',ylabel=NULL) +
      facet_wrap('name',scales='free_y') +
      scale_color_manual(values=city.colours) +
      labs(color='City',linetype='Risk') +
      show.vax(P)
    plot.save(paste0('eg-',case),w=7,h=3)
  }
}

v2c.examples()