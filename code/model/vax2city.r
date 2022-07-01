colours.city = .get.brewer(11)[c(2,10)]
colours.city.risk = c(unname(c(colours.city,sapply(colours.city,colour.adj,.5))),'#000000')

city.risk.factor = function(x){
  # generate a factor from the interaction of city & risk (plus overall)
  factor(interaction(x$city,x$risk),
    levels = c('A.high','B.high','A.low','B.low','A B.high low'),
    labels = c(
      'City A Higher Risk',
      'City B Higher Risk',
      'City A Lower Risk',
      'City B Lower Risk',
      'Overall'))
}

v2c.optimize = function(P=NULL,force=NULL){
  # find the optimal allocation of vaccines between cities A & B
  # option to force (dummy optimization) to easily obtain the same output for a non-optimal value
  if (is.null(P)){ P = def.params() }
  t = def.t()
  obj.fun = function(x){
    P$vax.x.city = c(A=x,B=1-x)
    P$X.vax.city.risk = def.vax.city.risk(P)
    R = sys.run(P,t)
    out.incidence(R,mode='cum.abs',strat=NULL)$value[length(t)]
  }
  if (is.null(force)){
    return(rename.cols(optimize(obj.fun,c(0,1)),minimum='vax.x.A',objective='cum.inf'))
  } else {
    return(list(vax.x.A=force,cum.inf=obj.fun(force)))
  }
}

v2c.plot.vax = function(){
  # plot coverage & numbers of vaccine allocated for an example strategy and set of conditions
  t = def.t()
  P = def.params(X0.ei=0,x.A=.5,vax.x.A=.8)
  R = sys.run(P,t)
  outs = do.call(rbind,list(
    out.prevalence(R,mode='prop',health='vax',strat=c('city','risk')),
    out.prevalence(R,mode='prop',health='vax',strat=NULL),
    out.prevalence(R,mode='abs', health='vax',strat=c('city','risk')),
    out.prevalence(R,mode='abs', health='vax',strat=NULL)
  ))
  outs$city.risk = city.risk.factor(outs)
  outs$name = factor(outs$name,levels=unique(outs$name),labels=c('Coverage (%)','Count'))
  g = plot.out(outs,color='city.risk',linetype='city.risk',ylabel=NULL) +
    facet_wrap('name',scales='free_y') +
    scale_color_manual(values=colours.city.risk) +
    scale_linetype_manual(values=c('solid','solid','62','62','22')) +
    labs(color='City & Risk',linetype='City & Risk') +
    show.vax(P)
  plot.save('vax',w=8,h=3)
}

v2c.plot.incidence = function(mode='cum.abs'){
  # plot incidence (cum.abs, rate, etc.) for three strategies for a set of conditions
  # faceting & coloured by city.risk, linetype by strategy
  P.list = list(
    'No Vaccine'   = def.params(P=def.params.torott(),vax.X=0),
    'Proportional' = def.params(P=def.params.torott(),vax.x.A=.75),
    'Optimal'      = def.params(P=def.params.torott(),vax.x.A='opt'))
  t = def.t()
  outs = do.call(rbind,lapply(names(P.list),function(case){
    P = P.list[[case]]
    R = sys.run(P,t)
    outs = do.call(rbind,list(
      out.incidence(R,mode=mode,strat=c('city','risk')),
      out.incidence(R,mode=mode,strat=NULL)
    ))
    outs$case = case
    return(outs)
  }))
  outs$case  = factor(outs$case,levels=names(P.list))
  outs$city.risk = city.risk.factor(outs)
  g = plot.out(outs,linetype='case',color='city.risk') +
    facet_wrap('city.risk',ncol=2,scales='free_y') +
    labs(linetype='Vaccine\nStrategy') +
    scale_linetype_manual(values=c('22','62','solid')) +
    scale_color_manual(values=colours.city.risk,guide='none') +
    show.vax(P.list[[1]]) +
    theme(legend.position=c(.75,.15),legend.box='horizontal')
  plot.save(paste0('inf.',mode),w=6,h=6)
}
