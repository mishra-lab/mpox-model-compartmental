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

def.params.n = function(n){
  # wrapper to create a list of parameter sets
  return(lapply(seq(n),def.params))
}

def.params = function(seed=NULL){
  # main function to define parameter set
  if (is.null(seed)){
    P = def.params.hand()
  } else {
    P = def.params.fitted(seed)
  }
  P = def.params.fixed(P)
  P$X0 = def.X0(P)
  return(P)
}

def.params.fitted = function(seed=NULL){
  set.seed(seed)
  P = list() 
  P$seed = seed
  P$C.com.all = f.gamma('r',m=1.50,sd=1.0,n=1)
  P$beta.sex  = f.gamma('r',m=0.70,sd=0.2,n=1)
  P$beta.com  = f.gamma('r',m=0.10,sd=0.1,n=1)
  return(P)
}

def.params.hand = function(){
  P = list()
  P$C.com.all = 1.00 # daily community "encounters" among high and low risk
  P$beta.sex  = 0.90 # transmission probability per sex partner
  P$beta.com  = 0.05 # transmission probability per community encounter
  return(P)
}

def.params.fixed = function(P){
  # input values
  P$X.city        = c(A=50000,B=20000) # population size in cities A,B
  P$x.city.high   = c(A=.20,B=.15) # proportion in higher risk in cities A,B
  P$X0.exp        = array(c(A.high=1,B.high=.3,A.low=0,B.low=0),c(2,2)) # number initially exposed
  P$X0.inf        = array(c(A.high=1,B.high=.3,A.low=0,B.low=0),c(2,2)) # number initially infected
  P$asso.sex      =  0.00  # assort (epsilon) in sexual partnerships: 0 = random; 1 = assortative
  P$asso.com      =  0.00  # assort (epsilon) in community contacts:  0 = random; 1 = assortative
  P$asso.city     =  0.99  # assort (epsilon) between cities:         0 = random; 1 = assortative
  P$C.sex.low     =  1/90  # daily sexual partnerships among low risk
  P$C.sex.high    =  1/7   # daily sexual partnerships among high risk
  P$dur.exp       =     7  # 1 week incubation
  P$dur.inf       =    21  # 3 weeks infectious
  P$iso.prop      =  0.50  # proportion isolating while infectious
  P$vax.eff       =  0.85 # vaccine effectiveness
  # combinations / conditional values
  P$X             = sum(P$X.city)
  P$x.city.risk   = array(c(P$x.city.high,1-P$x.city.high),c(2,2),.dn[c('city','risk')]) # proportions
  P$X.city.risk   = P$x.city.risk * P$X.city  # absolute
  P$C.risk.sex    = array(c(P$C.sex.high, P$C.sex.low), dimnames=.dn['risk'])
  P$C.risk.com    = array(c(P$C.com.all,  P$C.com.all), dimnames=.dn['risk'])
  P$M.sex         = def.mix(P$X.city.risk, P$C.risk.sex, P$asso.sex, P$asso.city)
  P$M.com         = def.mix(P$X.city.risk, P$C.risk.com, P$asso.com, P$asso.city)
  P$C.sex         = sweep(P$M.sex,2,P$C.risk.sex,'*')
  P$C.com         = sweep(P$M.sex,2,P$C.risk.com,'*')
  # DEBUG print(rowSums(P$M.sex,dim=2)) # == 1
  # DEBUG print(rowSums(P$M.com,dim=2)) # == 1
  # DEBUG print(rowSums(P$C.sex,dim=2)) # == P$C.risk.sex
  # DEBUG print(rowSums(P$C.com,dim=2)) # == P$C.risk.com
  P$health.sus = array(c(1,1-P$vax.eff),dimnames=list(health=c('sus','vax')))
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
