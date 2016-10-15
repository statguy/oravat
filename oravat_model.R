if (F) {
  library(devtools)
  install_github("statguy/SpaceTimeModels")
  reload(inst("SpaceTimeModels"))
}

#INLA:::inla.dynload.workaround() # Needed for older systems
library(SpaceTimeModels)
library(zoo)
library(splancs)

data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")

# Filter out missing values
# Add ycoordinate as a covariate
# Scale and center covariates
complete.columns <- c("Year","X","Y","Length","marmar1km","marmarPrevYear","marmar2YearsAgo","kernel_new","kernel_lagged","triangletype","interpolated_conevalue")
oravat.raw <- data %>%
  select(one_of(c(complete.columns, "scivultracks"))) %>%
  filter(complete.cases(.[complete.columns])) %>%
  mutate(ycoord = Y / 1e6) %>%
  mutate_each(funs(scale), marmar1km, marmarPrevYear, marmar2YearsAgo, kernel_new, kernel_lagged, interpolated_conevalue, ycoord)

# High correlation for kernel_lagged and kernel_new, leave out kernel_lagged
cor(oravat.raw[,!colnames(oravat.raw) %in% c("Year","X","Y","Length","triangletype")])

# Convert data to STIDF object
oravat <- spacetime::STIDF(sp::SpatialPoints(oravat.raw[,c("X","Y")]), zoo::as.yearmon(oravat.raw$Year), oravat.raw)

# Make a mesh
mesh <- SpaceTimeModels::NonConvexHullMesh$new(knots=oravat@sp, knotsScale=1e6, cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)
mesh$getINLAMesh()$n
mesh$plot()

# Null model
model0 <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model0$setSpatialMesh(mesh)
model0$setSpatialPrior()
model0$setLikelihood("nbinomial")
model0$setLinkFunction(poisson()$link)
model0$setSmoothingModel()
model0$getLinearModel()
model0$addObservationStack(sp=oravat, response=oravat@data$scivultracks)
model0$addPredictionStack(sp=oravat)
model0$estimate(verbose=T)
save(model0, file="oravat0.RData")
model0$summary()
model0$summarySpatialParameters()
model0$summaryTemporalVariation(timeIndex=time(oravat))

# Best fit model
model1 <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model1$setSpatialMesh(mesh)
model1$setSpatialPrior()
model1$setLikelihood("nbinomial")
model1$setLinkFunction(poisson()$link)
formula <- ~ interpolated_conevalue + marmar1km + marmarPrevYear + marmar2YearsAgo + triangletype + ycoord
model1$setCovariatesModel(formula, oravat@data)
model1$getLinearModel()
model1$addObservationStack(sp=oravat, response=oravat@data$scivultracks, covariates=oravat@data)
#model$addValidationStack(sp=val, covariates=val@data)
model1$addPredictionStack(sp=oravat)
model1$estimate(verbose=T)
save(model1, file="oravat1.RData")
model1$summary()
model1$summarySpatialParameters()
model1$summaryTemporalVariation(timeIndex=time(oravat))

# Validation

# Split data to observation and validation data (30% of the data)
set.seed(1)
validationIndex <- sample(nrow(oravat.raw), nrow(oravat.raw) * .3)
oravat.raw.obs <- oravat.raw[-validationIndex,]
oravat.raw.val <- oravat.raw[validationIndex,]
oravat.obs <- spacetime::STIDF(sp::SpatialPoints(oravat.raw.obs[,c("X","Y")]), zoo::as.yearmon(oravat.raw.obs$Year), oravat.raw.obs)
oravat.val <- spacetime::STIDF(sp::SpatialPoints(oravat.raw.val[,c("X","Y")]), zoo::as.yearmon(oravat.raw.val$Year), oravat.raw.val)

mesh <- SpaceTimeModels::NonConvexHullMesh$new(knots=oravat.obs@sp, knotsScale=1e6, cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)

model1v <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model1v$setSpatialMesh(mesh)
model1v$setSpatialPrior()
model1v$setLikelihood("nbinomial")
model1v$setLinkFunction(poisson()$link)
formula <- ~ interpolated_conevalue + marmar1km + marmarPrevYear + marmar2YearsAgo + triangletype + ycoord
model1v$setCovariatesModel(formula, oravat.obs@data)
model1v$getLinearModel()
model1v$addObservationStack(sp=oravat.obs, response=oravat.obs@data$scivultracks, covariates=oravat.obs@data)
model1v$addValidationStack(sp=oravat.val, covariates=oravat.val@data)
model1v$addPredictionStack(sp=oravat.obs)
model1v$estimate(verbose=T)
save(model1v, file="oravat1v.RData")
model1v$summary()
model1v$summarySpatialParameters()
model1v$summaryTemporalVariation(timeIndex=time(oravat.obs))
