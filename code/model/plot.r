# functions for plotting the model
library('ggplot2')

.out.labs = list(
  prevalence = 'Prevalence',
  incidence = 'Incidence'
)

plot.out = function(out,x='t',y='value',ylabel,...){
  if (missing(ylabel)){ ylabel = .out.labs[[out$name[1]]] }
  g = ggplot(out,aes_string(x=x,y=y,...)) + 
    geom_line() +
    labs(x='Time (days)',y=ylabel)
  return(g)
}