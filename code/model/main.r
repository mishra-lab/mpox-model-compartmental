source('utils/utils.r')
source('model/plot.r')
source('model/params.r')
source('model/system.r')

# setup & run the model
P = def.params()
t = def.t(t1=365)
R = sys.run(P,t)
# plotting
plot.prev(R,c(1),c(2),aggr=T)
plot.prev(R,c(1,2),c(2),aggr=T)
plot.prev(R,c(1,2),c(1,2),aggr=T)
plot.prev(R,c(1),c(2),aggr=F)
plot.prev(R,c(1,2),c(2),aggr=F)
plot.prev(R,c(1,2),c(1,2),aggr=F)
