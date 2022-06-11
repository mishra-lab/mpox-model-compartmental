source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')

# setup & run the model
P = def.params()
t = def.t(t1=365)
R = sys.run(P,t)
# plotting
g = plot.out(out.prevalence(R,aggr=T),color='city',linetype='risk'); print(g)
g = plot.out(out.prevalence(R,aggr=F),color='city',linetype='risk'); print(g)
g = plot.out(out.prevalence(R,aggr=T,health='rec'),color='city',linetype='risk'); print(g)
g = plot.out(out.prevalence(R,aggr=F,health='rec'),color='city',linetype='risk'); print(g)
g = plot.out(out.incidence(R,health='sus',aggr=T),color='city',linetype='risk'); print(g)
g = plot.out(out.incidence(R,health='sus',aggr=F),color='city',linetype='risk',facet='health'); print(g)
g = plot.out(out.incidence(R,health='sus',aggr=T,rate=F),color='city',linetype='risk'); print(g)
g = plot.out(out.incidence(R,health='sus',aggr=F,rate=F),color='city',linetype='risk'); print(g)
