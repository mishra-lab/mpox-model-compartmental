source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')

t = def.t(t1=365)

# multiple runs
P.n = def.params.n(70)
R.n = sys.run.n(P.n,t,para=T)
out = rbind(
  out.fun.n(R.n,'incidence',aggr=T,rate=F,health='sus',city='A'),
  out.fun.n(R.n,'incidence',aggr=T,rate=F,health='sus',city='B'))
g = plot.out(out,ci=.5,color='city',fill='city'); print(g)

# one run
P = def.params()
R = sys.run(P,t)
g = plot.out(out.incidence(R,health='sus',city='A',aggr=T,rate=F),color='risk'); print(g)
g = plot.out(out.incidence(R,health='sus',city='A',aggr=F,rate=F),color='risk'); print(g)
