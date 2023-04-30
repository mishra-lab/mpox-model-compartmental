source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')
source('model/vax2city.r')

v2c.main = function(fresh=FALSE){
  v2c.plot.vax()
  v2c.plot.incidence('toroth')
  v2c.plot.incidence('base')
  grid = v2c.grid(fresh=fresh)
  v2c.grid.plot(grid,z='opt.vax.x.A'); plot.save('grid.opt',      w=8,h=7)
  v2c.grid.plot(grid,z='dci.none');    plot.save('grid.dci.none', w=8,h=7)
  v2c.grid.plot(grid,z='rdci.none');   plot.save('grid.rdci.none',w=8,h=7)
  v2c.grid.plot(grid,z='dci.prop');    plot.save('grid.dci.prop', w=8,h=7)
  v2c.grid.plot(grid,z='rdci.prop');   plot.save('grid.rdci.prop',w=8,h=7)
}

# print(sys.R0(def.params())) # DEBUG
v2c.main()
