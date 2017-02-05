INLA:::inla.dynload.workaround() # Needed for older systems
library(arm)
library(zoo)
library(SpaceTimeModels)
library(splancs)

data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
finland1 <- spTransform(getData(country = "FI", level = 1), CRS("+init=epsg:2393"))
oravat.raw <- data

# Spatial range of cone index

oravat.raw.sp <- spacetime::STIDF(sp::SpatialPoints(oravat.raw[,c("X","Y")]), zoo::as.yearmon(oravat.raw$Year), oravat.raw)
coord.scale <- 1e-6
mesh <- SpaceTimeModels::NonConvexHullMesh$new(knots = oravat.raw.sp@sp, knotsScale = 1/coord.scale, cutoff = 3e4, maxEdge = c(5e4, 6e5), offset = c(1e4, 1e6), innerConvex = 0.05, outerConvex = 1.5)
model <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model$setSpatialMesh(mesh)
model$setSpatialPrior()
model$setLikelihood("gaussian")
model$setSmoothingModel()
model$addObservationStack(sp = oravat.raw.sp, response = sqrt(oravat.raw.sp@data$interpolated_conevalue))
model$estimate(verbose = T)
save(model, file = "model_cone_index.RData")
model$summary()
model$summarySpatialParameters()
model$summaryTemporalVariation(timeIndex = time(oravat))
