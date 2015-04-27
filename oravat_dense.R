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
  setLikelihood("nbinomial")
model$getMesh()$n

# Define model and estimate
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + naata2YearsAgo + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6273 3.4648   -14.5633  -7.6521    -0.5498 -7.6926
#interpolated_conevalue  0.0046 0.0010     0.0026   0.0046     0.0065  0.0046
#naata1km                0.5825 0.0794     0.4279   0.5820     0.7397  0.5810
#kernel_new             -0.0016 0.0749    -0.1487  -0.0016     0.1452 -0.0016
#triangletypewlt        -0.7501 0.0815    -0.9101  -0.7501    -0.5903 -0.7501
#Watanabe-Akaike information criterion (WAIC) ...: 38272.72

# *** BEST MODEL (of all estimated models) ***
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6251 3.4821   -14.5949  -7.6497    -0.5167 -7.6903
#interpolated_conevalue  0.0046 0.0010     0.0026   0.0046     0.0065  0.0046
#naata1km                0.5823 0.0794     0.4278   0.5818     0.7395  0.5808
#triangletypewlt        -0.7501 0.0813    -0.9098  -0.7501    -0.5906 -0.7501
#Watanabe-Akaike information criterion (WAIC) ...: 38271.57
#                                                       mean     sd 0.025quant
#size for the nbinomial observations (overdispersion) 1.0081 0.0260     0.9578
#Theta1 for spatial                                   1.5934 0.1496     1.3081
#Theta2 for spatial                                   1.1761 0.1339     0.9194
#GroupRho for spatial                                 0.9947 0.0017     0.9908
#                                                     0.5quant 0.975quant   mode
#size for the nbinomial observations (overdispersion)   1.0078     1.0601 1.0073
#Theta1 for spatial                                     1.5898     1.8952 1.5789
#Theta2 for spatial                                     1.1734     1.4453 1.1654
#GroupRho for spatial                                   0.9949     0.9974 0.9954

model$setSmoothingModel()$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

