
grid.run = function(n=11,main.vars=c('R0.A','x0.ei.A')){
  v.fun = function(name,v){ if (name %in% main.vars){ v$main } else { v$facet } }
  grid = expand.grid( # TODO: double check ranges/values
    R0.A      = v.fun('R0.A',     list(main=seq(1.05,1.95,l=n),facet=c(1.2,1.5,1.8))),
    x0.ei.A   = v.fun('x0.ei.A',  list(main=seq(0.05,0.95,l=n),facet=c(.25,.50,.75))),
    asso.city = v.fun('asso.city',list(main=seq(0.70,1.00,l=n),facet=c(.80,.90,.95))),
    x.A       = v.fun('x.A',      list(main=seq(0.20,0.80,l=n),facet=c(.25,.50,.75)))
  )
  grid.i.fun = function(i){
    print(i)
    C.sex.high.A = sys.fit.R0(grid$R0.A[i],'C.sex.high.A',c(.50,.05))
    P.i = do.call(def.params,c(grid[i,],list(C.sex.high.A=C.sex.high.A)))
    opt.vax.x.A = v2c.optimize(P.i)
    return(c('C.sex.high.A'=C.sex.high.A,'opt.vax.x.A'=opt.vax.x.A))
  }
  grid.out = mclapply(seq(nrow(grid)),grid.i.fun,mc.cores=7)
  grid = cbind(grid,do.call(rbind,grid.out))
  return(grid)
}

grid.clean = function(grid){
  R0.B = sys.R0(def.params())$B
  # simple variable clean-up
  grid$R0.AvB = grid$R0.A / R0.B
  grid$x0.ei.A.pct = 100 * grid$x0.ei.A
  grid$f.X.AvB = factor(grid$x.A/(1-grid$x.A),levels=c(1/3,1,3),
    labels=c('City B larger','Cities same size','City A larger'))
  grid$f.mix.city = factor(grid$asso.city,levels=unique(grid$asso.city),
    labels=paste0(c('Most','Moderate','Least'),' inter-city mixing'))
  return(grid)
}

grid.rdata = function(n=11,slug='grid',grid=NULL){
  # save or load grid data to / from file
  fname = root.path('data','.rdata',paste0(slug,'-n',n,'.rdata'),create=TRUE)
  if (is.null(grid)){
    load(file=fname)
  } else {
    save(grid,file=fname)
  }
  return(grid)
}

plot.grid = function(grid,x,y,z,facet,n=11){
  .labs = list( # TODO: update
    opt.vax.x.A   = 'Optimal\nvaccine\nallocation',
    asso.city     = 'Fraction of contacts in same city',
    asso.city.pct = 'Contacts in same city (%)',
    x.A           = 'Fraction of overall population in city A',
    x.A.pct       = 'Overall population in city A (%)',
    x0.ei.A       = 'Fraction of seed cases in city A',
    x0.ei.A.pct   = 'Seed cases in city A (%)',
    R0.A          = 'Ro in City A',
    R0.AvB        = 'Ro in City A vs City B'
  )
  c.vals = rev(.get.brewer(n,'Spectral'))
  c.labs = c('B 90%+',rep('',n/2-1.5),'50 / 50',rep('',n/2-1.5),'A 90%+')
  breaks = seq(0,1,l=n+1)
  g = ggplot(grid,aes_string(x=x,y=y,z=z)) +
    geom_contour_filled(breaks=breaks,alpha=.8) +
    scale_fill_manual(values=c.vals,labels=c.labs,drop=FALSE) +
    facet_grid(facet) +
    labs(x=.labs[[x]],y=.labs[[y]],fill=.labs[[z]])
  .plot.clean(g)
}


