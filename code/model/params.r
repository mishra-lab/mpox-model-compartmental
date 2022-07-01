# functions for defining parameters
source('utils/stats.r')

.dn = list( # global variable storing dimnames
  city    = c('A','B'),
  risk    = c('high','low'),
  health  = c('sus','vax','exp','inf','rec')
)

def.t = function(t0=0,t1=120,dt=1){
  # wrapper function (mainly for default values)
  return(seq(t0,t1,dt))
}

def.params = function(P=list(),...){
  # both P and/or ... can be used to override any defaults in P.fixed
  P.fixed = def.params.fixed()
  P = list.update(list.merge(P.fixed,P),...)
  P = def.params.cond(P)
  return(P)
}

def.params.fixed = function(){
  P = list()
  # population
  P$X             = 100000 # total population size
  P$x.A           =   0.50 # proportion of total population in city A
  P$x.high.A      =   0.10 # proportion in higher risk in city A
  P$x.high.B      =   0.10 # proportion in higher risk in city B
  P$X0.ei         =     10 # number initially exposed/infected
  P$x0.ei.high    =   1.00 # proportion in higher risk among initially exposed/infected
  P$x0.ei.A       =   0.50 # proportion in city A among initially exposed/infected
  # mixing + contacts
  P$asso.sex      =   0.50 # assort (epsilon) in sexual partnerships: 0 = random; 1 = assortative
  P$asso.com      =   0.00 # assort (epsilon) in community contacts:  0 = random; 1 = assortative
  P$asso.city     =   0.90 # assort (epsilon) between cities:         0 = random; 1 = assortative
  P$C.sex.low     =   1/90 # daily sexual partnerships among low risk
  P$C.sex.high.A  =   .178  # daily sexual partnerships among high risk in city A
  P$C.sex.high.B  =   .178  # daily sexual partnerships among high risk in city B
  P$C.com.all     =   1.00 # daily community contacts among high and low risk
  # transmission + infection
  P$sar.sex       =   0.90 # transmission probability per sex partner
  P$sar.com       =   0.05 # transmission probability per community encounter
  P$dur.exp       =      7 # 1 week incubation
  P$dur.inf       =     21 # 3 weeks infectious
  P$iso.prop      =   0.50 # proportion isolating while infectious
  # vaccination
  P$vax.eff       =   0.85 # vaccine effectiveness
  P$vax.X         =   5000 # number of vaccines to distribute
  P$vax.x.A       =   1.00 # proportion of vaccines to distribute to city A
  P$vax.t0        =     60 # time (days) of starting vaccine roll-out
  P$vax.dt        =     15 # duration (days) of vaccine roll-out
  P$vax.high.sens =   0.90 # sensitivity of high risk among vaccinated
  return(P)
}

def.params.cond = function(P){
  P$X.city           = P$X * c('A'=P$x.A,'B'=1-P$x.A)
  P$x.city.risk      = array(c(P$x.high.A,P$x.high.B,1-P$x.high.A,1-P$x.high.B),c(2,2),.dn[c('city','risk')])
  P$X.city.risk      = P$X.city * P$x.city.risk  # absolute
  P$x0.ei.city       = c('A'=P$x0.ei.A,'B'=1-P$x0.ei.A)
  P$x0.ei.risk       = c('high'=P$x0.ei.high,'low'=1-P$x0.ei.high)
  P$x0.ei.city.risk  = array(outer(P$x0.ei.city,P$x0.ei.risk),c(2,2),.dn[c('city','risk')])
  P$C.sex.city.risk  = array(c(P$C.sex.high.A,P$C.sex.high.B,rep(P$C.sex.low,2)),c(2,2),.dn[c('city','risk')])
  P$C.com.city.risk  = array(rep(P$C.com.all,4),c(2,2),.dn[c('city','risk')])
  P$M.sex = def.mix(P$X.city.risk,P$C.sex.city.risk,P$asso.sex,P$asso.city)
  P$M.com = def.mix(P$X.city.risk,P$C.com.city.risk,P$asso.com,P$asso.city)
  P$C.sex = sweep(P$M.sex,c(1,2),P$C.sex.city.risk,'*')
  P$C.com = sweep(P$M.sex,c(1,2),P$C.com.city.risk,'*')
  # DEBUG print(rowSums(P$M.sex,dim=2)) # == 1
  # DEBUG print(rowSums(P$M.com,dim=2)) # == 1
  # DEBUG print(rowSums(P$C.sex,dim=2)) == P$C.sex.city.risk)
  # DEBUG print(rowSums(P$C.com,dim=2)) == P$C.com.city.risk)
  P$health.sus = array(c(1,1-P$vax.eff),dimnames=list(health=c('sus','vax')))
  P$X0 = def.X0(P)
  if (P$vax.x.A == 'opt'){ P$vax.x.A = v2c.optimize(P)$vax.x.A }
  P$vax.x.city = c('A'=P$vax.x.A,'B'=1-P$vax.x.A)
  P$X.vax.city.risk = def.vax.city.risk(P)
  return(P)
}

def.params.torott = function(){
  P = list()
  P$X   = 80000 + 20000
  P$x.A = .75
  P$x0.ei.A       = 1.00
  P$C.sex.high.A  = .236 # R0 = 2.0
  P$C.sex.high.B  = .178 # R0 = 1.5
  P$asso.city = .9
  P$vax.x.A = 'opt'
  return(P)
}

def.mix = function(X,C,asso.risk,asso.city){
  # define the probability distribution of contacts "M"
  XC = X * C
  xc.y = rowSums(XC,dim=1)/sum(XC) # proportion XC by city (all risk)
  xc.i = colSums(XC,dim=1)/sum(XC) # proportion XC by risk (all city)
  M = array( # initialize
    dim = c(2,2,2,2),
    dimnames = list(
      self.city  = .dn$city,
      self.risk  = .dn$risk,
      other.city = .dn$city,
      other.risk = .dn$risk
    )
  )
  for (y in c(1,2)){ # self.city
    xc.i.y = XC[y,] / sum(XC[y,]) # proportion XC by risk (this city)
    for (i in c(1,2)){ # self.risk
      for (y. in c(1,2)){ # other.city
        for (i. in c(1,2)){ # other.risk
          M[y,i,y.,i.] =
            (asso.city) * (y==y.) * ( # this city
              (asso.risk) * (i==i.) + (1-asso.risk) * xc.i.y[i.]
            ) +
            (1-asso.city) * (xc.y[y.]) * ( # all cities
              (asso.risk) * (i==i.) + (1-asso.risk) * xc.i[i.]
            )
        }
      }
    }
  }
  return(M)
}

def.vax.city.risk = function(P){
  # compute the idstribution of vaccine allocation by city & risk
  # given: X.city.risk, vax.X, vax.x.city, and vax.high.sens
  dn = list(risk=c('high','low'),vax=c('yes','no'))
  X.vax.A = confusion.solve(P$X.city[1],P$X.city.risk[1,1],P$vax.X*P$vax.x.city[1],sens=P$vax.high.sens,dn=dn)$X
  X.vax.B = confusion.solve(P$X.city[2],P$X.city.risk[2,1],P$vax.X*P$vax.x.city[2],sens=P$vax.high.sens,dn=dn)$X
  # DEBUG print(sum(X.vax.A)+sum(X.vax.B)) # == P$X
  # DEBUG print(sum(X.vax.A[,1])+sum(X.vax.B[,1])) # == P$vax.X
  return(array(c(X.vax.A[1,1],X.vax.B[1,1],X.vax.A[2,1],X.vax.B[2,1]),c(2,2),.dn[1:2]))
}

def.X0 = function(P){
  # define initial conditions
  x0.exp.inf      = c(P$dur.exp,P$dur.inf)/(P$dur.exp+P$dur.inf)
  X0.ei.city.risk = P$X0.ei * P$x0.ei.city.risk
  X0.data      = outer(P$X.city.risk,c(1,0,0,0,0)) # expand into new 'health' dimension
  X0.data[,,1] = X0.data[,,1] - X0.ei.city.risk # remove initial exp/inf from sus
  X0.data[,,3] = X0.ei.city.risk * x0.exp.inf[1] # add back initial exp
  X0.data[,,4] = X0.ei.city.risk * x0.exp.inf[2] # add back initial inf
  return(array(X0.data, c(2,2,5), .dn))
}
