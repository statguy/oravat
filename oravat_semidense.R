if (F) {
  library(devtools)
  install_github("statguy/SpaceTime")
  install_github("statguy/oravat")
  reload(inst("SpaceTime"))
}

library(SpaceTime)

data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
columns <- c("Year","X","Y","Length","marmar1km","marmarPrevYear","marmar2YearsAgo","kernel_new","kernel_lagged","triangletype","interpolated_conevalue")
completeIndex <- complete.cases(data[,columns])
oravat <- data[completeIndex, c("scivultracks", columns)]

# cutoff=5e3, maxEdge=c(3.7e4, 2e5), offset=c(1e4, 4e4), convex=0.04 # dense mesh
# cutoff=1e5, maxEdge=c(2e5, 10e5), convex=0.12 # sparse mesh
mesh <- NonConvexHullMesh$new(knots=oravat[,c("X","Y")], knotsScale=1e6)$construct(cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)
mesh$getINLAMesh()$n
mesh$plot()

model <- ContinuousSpaceDiscreteTimeModel$new()
model$setSpatialMesh(mesh)
model$setSpatialPrior()
model$setLikelihood("nbinomial")

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + kernel_lagged + triangletype, covariates=oravat)
model$getLinearModel()
model$addObservationStack(coordinates=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=oravat, offset=oravat$Length)
model$estimate(verbose=T)
model$summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + naataPrevYear + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
    estimate(verbose=T)$
  summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + naataPrevYear + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + naataPrevYear + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()

model$setCovariatesModel(~ interpolated_conevalue + naata1km + triangletype + ycoord, oravat)$
  addObservationStack(coordinates=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=oravat, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()

model$setSmoothingModel()$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$Year, response=oravat$scivultracks, covariates=covariates, offset=oravat$Length)$
  estimate(verbose=T)$
  summary()
