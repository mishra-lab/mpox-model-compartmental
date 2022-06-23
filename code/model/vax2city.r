source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')
source('model/grid.r')

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

v2c.optimize = function(P=NULL){
  # find the optimal allocation of vaccines between cities A & B
  if (is.null(P)){ P = def.params() }
  t = def.t()
  obj.fun = function(x){
    P$vax.x.city = c(A=x,B=1-x)
    P$X.vax.city.risk = def.vax.city.risk(P)
    R = sys.run(P,t)
    out.incidence(R,mode='cum.abs',strat=NULL)$value[length(t)]
  }
  return(optimize(obj.fun,c(0,1))$minimum)
}

grid = grid.run()
plot.grid(grid,x='x0.ei.A',y='R0.A',z='opt.vax.x.A',facet='asso.city~x.A'); plot.save('grid',w=8,h=7)
