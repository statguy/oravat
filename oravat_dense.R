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
# Set data and construct mesh for random effect
model <- DiscreteTimeContinuousSpaceModel$new()$
  constructMesh(coords=oravat[,c("X","Y")], cutoff=5e3, maxEdge=c(3.7e4, 2e5), offset=c(1e4, 4e4), convex=0.04)$ # dense mesh
  plotMesh()$
  setSpatialPrior()$
  setLikelihoodModel("nbinomial")
model$getMesh()$n

# Define model and estimate
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + naata2YearsAgo + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

# running




