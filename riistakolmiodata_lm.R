library(devtools)
#install_github("statguy/SpaceTime")
library(SpaceTime)

# Load and manipulate data for SpaceTime
data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
columns <- c("vuosi","X","Y","PITUUS","triangletype","interpolated_conevalue","naata1km","naataPrevYear","naata2YearsAgo","kernel_new","kernel_lagged")
completeIndex <- complete.cases(data[,columns])
oravat <- data[completeIndex, c("scivultracks", columns)]
oravat$scivultracks <- round(oravat$scivultracks)
covariates <- oravat[,c("triangletype","interpolated_conevalue","naata1km","naataPrevYear","naata2YearsAgo","kernel_new","kernel_lagged")]
oravat$PITUUS <- oravat$PITUUS * 1000

#reload(inst("SpaceTime"))
model <- DiscreteTimeContinuousSpaceModel$new()$
  setData(oravat[,c("X","Y")], oravat$vuosi, oravat$scivultracks, covariates, oravat$PITUUS)$
  #constructMesh(cutoff=1e5, maxEdge=c(2e5, 10e5), convex=0.12)$ # sparse mesh
  constructMesh(cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)$ # dense mesh
  plotMesh()
model$getMesh()$n

model$setSpatialPrior()$
  setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + naata2YearsAgo + kernel_new + kernel_lagged + triangletype)$
  buildObservationStack()$
  setLikelihoodModel("nbinomial")$
  estimate(verbose=T)
model$summary()
model$summarySpatial()
