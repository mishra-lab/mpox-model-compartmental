# functions for defining parameters

.dn = list( # global variable storing dimnames
  city    = c('A','B'),
  risk    = c('low','high'),
  health  = c('sus','vax','exp','inf','rec')
)

def.t = function(t0=0,t1=120,dt=1){
  # wrapper function (mainly for default values)
  return(seq(t0,t1,dt))
}

def.params = function(){
  # main function
  P = def.params.fitted()
  P = def.params.fixed(P)
  P$X0 = def.X0(P)
  return(P)
}

def.params.fitted = function(){
  P = list() # TBD
  return(P)
}

def.params.fixed = function(P){
  P = c(P,list( # append to fitted params
    X.city        = c(A=50000,B=20000), # population size in cities A,B
    x.city.high   = c(A=.20,B=.15), # proportion in higher risk in cities A,B
    X0.exp        = array(c(A.low=0,B.low=0,A.high=0,B.high=0),c(2,2)), # number initially exposed
    X0.inf        = array(c(A.low=0,B.low=0,A.high=1,B.high=0),c(2,2)), # number initially infected
    asso.sex      =  0.00,  # assort (epsilon) in sexual partnerships: 0 = random; 1 = assortative
    asso.com      =  0.00,  # assort (epsilon) in community contacts:  0 = random; 1 = assortative
    asso.city     =  0.99,  # assort (epsilon) between cities:         0 = random; 1 = assortative
    C.sex.low     =  1/90,  # daily sexual partnerships among low risk
    C.sex.high    =  1/10,  # daily sexual partnerships among high risk
    C.com.low     =     1,  # daily community "encounters" among low risk
    C.com.high    =     1,  # daily community "encounters" among high risk
    beta.sex      =   .90,  # transmission probability per sex partner
    beta.com      =   .05,  # transmission probability per community encounter
    dur.exp       =     7,  # 1 week incubation
    dur.inf       =    21,  # 3 weeks infectious
    # TODO: vax & isolation
  NULL))
  P$x.city.risk = array(c(1-P$x.city.high,P$x.city.high),c(2,2),.dn[c('city','risk')]) # proportions
  P$X.city.risk = P$x.city.risk * P$X.city  # absolute numbers
  P$C.risk.sex  = array(c(P$C.sex.low, P$C.sex.high), dimnames=list(risk=c('low','high')))
  P$C.risk.com  = array(c(P$C.com.low, P$C.com.high), dimnames=list(risk=c('low','high')))
  P$M.sex       = def.mix(P$X.city.risk, P$C.risk.sex, P$asso.sex, P$asso.city); # DEBUG print(rowSums(P$M.sex,dim=2))
  P$M.com       = def.mix(P$X.city.risk, P$C.risk.com, P$asso.com, P$asso.city); # DEBUG print(rowSums(P$M.com,dim=2))
  P$C.sex = sweep(P$M.sex,2,P$C.risk.sex,'*')
  P$C.com = sweep(P$M.sex,2,P$C.risk.com,'*')
  return(P)
}

def.mix = function(X,C,asso.risk,asso.city){
  # define the probability distribution of contacts "M"
  XC = sweep(X,2,C,'*') # total contacts "offered"
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

def.X0 = function(P){
  # define initial conditions
  X0.data = outer(P$X.city.risk,c(1,0,0,0,0)) # expand into new 'health' dimension
  X0.data[,,1] = X0.data[,,1] - P$X0.exp - P$X0.inf # remove initial exp & inf from sus
  X0.data[,,3] = P$X0.exp # add back initial exp
  X0.data[,,4] = P$X0.inf # add back initial inf
  return(array(X0.data, c(2,2,5), .dn))
}
