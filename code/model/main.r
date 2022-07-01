source('utils/utils.r')
source('model/params.r')
source('model/system.r')
source('model/output.r')
source('model/plot.r')
source('model/vax2city.r')

v2c.main = function(){
  v2c.plot.vax()
  v2c.plot.incidence('cum.abs')
  v2c.plot.incidence('rate')
}

v2c.main()