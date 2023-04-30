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
  P = def.params(X0.eih=0,x.A=.5,vax.x.A=.8)
  R = sys.run(P,t)
  outs = do.call(rbind,list(
    out.prevalence(R,mode='prop',health='vax',strat=c('city','risk')),
    out.prevalence(R,mode='prop',health='vax',strat=NULL),
    out.prevalence(R,mode='abs', health='vax',strat=c('city','risk')),
    out.prevalence(R,mode='abs', health='vax',strat=NULL)
  ))
  outs$city.risk = city.risk.factor(outs)
  outs$name = factor(outs$name,levels=unique(outs$name),labels=c('Coverage (%)','Count (N)'))
  g = plot.out(outs,color='city.risk',linetype='city.risk',ylabel=NULL) +
    facet_wrap('name',scales='free_y') +
    scale_color_manual(values=colours.city.risk) +
    scale_linetype_manual(values=c('solid','solid','62','62','22')) +
    labs(color='City & Risk',linetype='City & Risk') +
    show.vax(P)
  plot.save('vax',w=8,h=3)
}

v2c.plot.incidence = function(params='base'){
  # plot incidence (cum.abs, rate, etc.) for three strategies for a set of conditions
  # faceting & coloured by city.risk, linetype by strategy
  P0 = list(
    'base'   = def.params(),
    'toroth' = def.params.toroth()
  )[[params]]
  P.list = list(
    'No Vaccine'   = def.params(P=P0,vax.X=0),
    'Proportional' = def.params(P=P0,vax.x.A=P0$x.A),
    'Optimal'      = def.params(P=P0,vax.x.A='opt'))
  t = def.t()
  outs = do.call(rbind,lapply(names(P.list),function(case){
    P = P.list[[case]]
    R = sys.run(P,t)
    outs = rbind(
      out.incidence(R,mode='cum.abs',strat=c('city','risk')),
      out.incidence(R,mode='cum.abs',strat=NULL),
      out.incidence(R,mode='rate',strat=c('city','risk')),
      out.incidence(R,mode='rate',strat=NULL)
    )
    outs$case = case
    return(outs)
  }))
  outs$case.lab = factor(outs$case,levels=names(P.list))
  outs$name.lab = factor(outs$name,labels=c('Cumulative infections (N)','Rate (per 1000 person-days)'))
  outs$city.risk = city.risk.factor(outs)
  print(outs[outs$city.risk=='Overall' & outs$t==t[length(t)],]) # MAN
  g = plot.out(outs,linetype='case.lab',color='city.risk') +
    ggh4x::facet_grid2('city.risk~name.lab',scales='free_y',independent='y') +
    labs(y='Incidence',linetype='Vaccine Strategy') +
    scale_linetype_manual(values=c('22','62','solid')) +
    scale_color_manual(values=colours.city.risk,guide='none') +
    show.vax(P.list[[1]]) +
    theme(legend.position='top')
  g = .facet.letters(g,pars='case.lab',color='black')
  plot.save(paste0('inf.',params),w=5,h=8)
}

v2c.grid.run = function(n=11,main.vars=c('R0.A','x0.eih.A')){
  v.fun = function(name,v){ if (name %in% main.vars){ v$main } else { v$facet } }
  grid = expand.grid(
    R0.A      = v.fun('R0.A',     list(main=seq(1.05,1.95,l=n),facet=c(1.2,1.5,1.8))),
    x0.eih.A  = v.fun('x0.eih.A', list(main=seq(0.05,0.95,l=n),facet=c(.25,.50,.75))),
    asso.city = v.fun('asso.city',list(main=seq(0.70,1.00,l=n),facet=c(.80,.90,.95))),
    x.A       = v.fun('x.A',      list(main=seq(0.20,0.80,l=n),facet=c(.25,.50,.75)))
  )
  grid.i.fun = function(i){
    print(i)
    C.sex.high.A = sys.fit.R0(grid$R0.A[i],'C.sex.high.A',c(.50,.05))
    P.i = do.call(def.params,c(grid[i,],list(C.sex.high.A=C.sex.high.A)))
    return(c(
      'C.sex.high.A' = C.sex.high.A,
      unlist(list.prefix(v2c.optimize(P.i),'opt.')),
      unlist(list.prefix(v2c.optimize(list.update(P.i,vax.X=0),force=.5),'none.')),
      unlist(list.prefix(v2c.optimize(P.i,force=.5),'even.')),
      unlist(list.prefix(v2c.optimize(P.i,force=P.i$x.A),'prop.'))
    ))
  }
  grid.out = mclapply(seq(nrow(grid)),grid.i.fun,mc.cores=7)
  grid = cbind(grid,do.call(rbind,grid.out))
  return(grid)
}

v2c.grid.clean = function(grid){
  R0.B = sys.R0(def.params())$B
  # add some simple variable
  grid$rci.none  = grid$opt.cum.inf / grid$none.cum.inf
  grid$dci.none  = grid$none.cum.inf - grid$opt.cum.inf
  grid$rdci.none = 100 * (grid$none.cum.inf - grid$opt.cum.inf) / grid$none.cum.inf
  grid$rci.prop  = grid$opt.cum.inf / grid$prop.cum.inf
  grid$dci.prop  = grid$prop.cum.inf - grid$opt.cum.inf
  grid$rdci.prop = 100 * (grid$prop.cum.inf - grid$opt.cum.inf) / grid$prop.cum.inf
  grid$R0.AvB = grid$R0.A / R0.B
  grid$x0.eih.A.pct = 100 * grid$x0.eih.A
  grid$f.X.AvB = factor(grid$x.A/(1-grid$x.A),levels=c(1/3,1,3),
    labels=c('City B larger','Cities same size','City A larger'))
  grid$f.mix.city = factor(grid$asso.city,levels=unique(grid$asso.city),
    labels=paste0(c('High','Medium','Low'),' inter-city mixing'))
  return(grid)
}

v2c.grid = function(fresh=FALSE,slug='grid',...){
  fname = root.path('data','.rdata',paste0(slug,'.rdata'),create=TRUE)
  if (fresh){
    grid = v2c.grid.run(...)
    save(grid,file=fname)
  } else {
    load(file=fname)
  }
  return(v2c.grid.clean(grid))
}

v2c.grid.plot = function(grid,z,x='x0.eih.A.pct',y='R0.A',fh='f.X.AvB',fv='f.mix.city'){
  .labs = list(
    opt.vax.x.A   = 'Optimal\nvaccine\nallocation',
    asso.city     = 'Fraction of contacts in same city',
    asso.city.pct = 'Contacts in same city (%)',
    x.A           = 'Fraction of overall population in city A',
    x.A.pct       = 'Overall population in city A (%)',
    x0.eih.A      = 'Fraction of initial cases in city A',
    x0.eih.A.pct  = 'Proportion of initial cases in city A (%)',
    R0.A          = 'Ro in city A (fixed 1.5 in city B)',
    R0.AvB        = 'Ratio of Ro in city A vs city B',
    dci.none      = 'Fewer\ninfections vs\nno vaccines',
    rdci.none     = '% Fewer\ninfections vs\nno vaccines',
    dci.prop      = 'Fewer\ninfections vs\nproportional\nto city size',
    rdci.prop     = '% Fewer\ninfections vs\nproportional\nto city size'
  )
  if (z=='opt.vax.x.A'){
    c.breaks = seq(0,1,l=12)
    c.labs = c('> 90% City B',rep('',4),'50 / 50',rep('',4),'> 90% City A')
    c.vals = rev(.get.brewer(length(c.labs),'Spectral'))
  }
  if (grepl('^dci',z)){
    c.breaks = c(seq(0,200,20),1e6)
    c.labs = c(seq(0,180,20),'200+')
    c.vals = .get.brewer(length(c.labs),'RdPu')
  }  
  if (grepl('^rdci',z)){
    c.breaks = c(seq(0,50,5),100)
    c.labs = c(seq(0,45,5),'50+')
    c.vals = .get.brewer(length(c.labs),'YlGnBu')
  }
  g = ggplot(grid,aes_string(x=x,y=y,z=z)) +
    geom_contour_filled(breaks=c.breaks,alpha=.8) +
    scale_fill_manual(values=c.vals,labels=c.labs,drop=FALSE) +
    facet_grid(paste(fv,' ~ ',fh)) +
    labs(x=.labs[[x]],y=.labs[[y]],fill=.labs[[z]])
  .plot.clean(.facet.letters(g,par=z))
}
