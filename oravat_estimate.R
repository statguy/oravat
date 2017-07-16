if (F) {
  library(devtools)
  install_github("statguy/SpaceTimeModels")
  reload(inst("SpaceTimeModels"))
}

INLA:::inla.dynload.workaround() # Needed for older systems
library(arm)
library(zoo)
library(SpaceTimeModels)
library(splancs)

data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
finland1 <- spTransform(getData(country = "FI", level = 1), CRS("+init=epsg:2393"))

# Add new columns, remove missing values and scale covariates to same scale
complete.columns <- c("Year","X","Y","id","Length","marmar1km","marmarPrevYear","marmar2YearsAgo","kernel_new","kernel_lagged","triangletype","interpolated_conevalue")
oravat.raw <- data %>%
  dplyr::mutate(id.tmp = paste0(X, Y), id = factor(as.numeric(factor(id.tmp)))) %>% dplyr::select(-id.tmp) %>%
  dplyr::select(one_of(c(complete.columns, "scivultracks"))) %>%
  dplyr::filter(complete.cases(.[complete.columns])) %>%
  dplyr::mutate(xcoord = X, ycoord = Y) %>%
  #dplyr::mutate(scivultracks = 10 * scivultracks) %>%
  dplyr::mutate(density = scivultracks / Length) %>%
  dplyr::mutate(yearmodel = Year - min(Year))
oravat.sp <- oravat.raw
coordinates(oravat.sp) <- ~ X + Y
proj4string(oravat.sp) <- finland1@proj4string
oravat.raw$Province <- factor(oravat.sp %over% polygons(finland1), labels = finland1$NAME_1)
xscale <- function(x) arm::rescale(x)
oravat.scaled <- oravat.raw %>%
  dplyr::mutate_each(funs(xscale), marmar1km, marmarPrevYear, marmar2YearsAgo, kernel_new, kernel_lagged, interpolated_conevalue, triangletype, xcoord, ycoord, yearmodel)

oravat.raw %>% group_by(Year, triangletype) %>% summarise(density = mean(density)) %>% 
  ggplot(aes(Year, density, color = triangletype)) + geom_path()

# Remove "outliers" from the upper 5% quantile
coord.scale <- 1e-6
oravat.filtered <- oravat.scaled %>%
  dplyr::group_by(Province) %>% 
  dplyr::mutate(q95 = density > quantile(density, .95)) %>%
  dplyr::filter(q95 == FALSE) %>% 
  dplyr::select(-q95) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(xs = X * coord.scale, ys = Y * coord.scale)
coord <- cbind(oravat.filtered$xs, oravat.filtered$ys)
oravat.filtered %>% dplyr::group_by(Province) %>% summarise(min(density), mean(density), median(density), max(density))


# Convert data to STIDF object
oravat <- spacetime::STIDF(sp::SpatialPoints(oravat.filtered[,c("X","Y")]), zoo::as.yearmon(oravat.filtered$Year), oravat.filtered)

# Construct mesh
mesh <- SpaceTimeModels::NonConvexHullMesh$new(knots = oravat@sp, knotsScale = 1/coord.scale, cutoff = 3e4, maxEdge = c(5e4, 1e6), offset = c(1e4, 1e6), innerConvex = 0.05, outerConvex = 1.5)
#mesh <- SpaceTimeModels::SpatialMesh$new(knots = oravat@sp, knotsScale = 1/coord.scale, cutoff = 3e4, maxEdge = c(5e4, 1e6), offset = c(1e4, 3e6))
mesh$getINLAMesh()$n
plot(mesh$getINLAMesh(), asp = 1)

# Spatial replicate model
##reload(inst("SpaceTimeModels"))
modelr <- SpaceTimeModels::ReplicatedContinuousSpaceModel$new()
modelr$setSpatialMesh(mesh)
modelr$setSpatialPrior(rho = 800e3)
modelr$setLikelihood("nbinomial")
modelr$setLinkFunction(poisson()$link)
formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + ycoord
modelr$setCovariatesModel(formula, oravat@data)
modelr$getLinearModel()
modelr$addObservationStack(sp = oravat, response = oravat@data$scivultracks, covariates = oravat@data, offset = oravat$Length)
modelr$addPredictionStack(sp = oravat)
modelr$estimate(verbose = T)
save(modelr, file = "model1-4-r.RData")
modelr$summary()


# Best fit model
model1 <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
#model1$temporalModel <- "rw1"
model1$setSpatialMesh(mesh)
model1$setSpatialPrior(rho = 800e3)
model1$setLikelihood("nbinomial")
model1$setLinkFunction(poisson()$link)
#formula <- ~ marmar1km + kernel_new + interpolated_conevalue + triangletype + xcoord + ycoord # model1.RData 35541.93
#formula <- ~ marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + interpolated_conevalue + triangletype + ycoord # model1-1.RData 35538.42
#formula <- ~ marmar1km + marmarPrevYear + marmar2YearsAgo + interpolated_conevalue + triangletype + ycoord # model1-2.RData 35537.11
#formula <- ~ marmar1km + marmarPrevYear + kernel_new + interpolated_conevalue + triangletype + ycoord # model1-3.RData 35538.16
formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + ycoord # model1-4.RData 35535.08
#formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + ycoord + yearmodel # model1-13.RData
#formula <- ~ marmar1km + interpolated_conevalue + triangletype + ycoord # model1-5.RData 35538.28
#formula <- ~ marmar1km + marmarPrevYear + marmar2YearsAgo + interpolated_conevalue + triangletype + ycoord + xcoord # model1-6.RData 35535.32
#formula <- ~ marmar1km + marmarPrevYear + kernel_new + interpolated_conevalue + triangletype # model1-7.RData 35538.65
#formula <- ~ marmar1km + marmar2YearsAgo + kernel_new + interpolated_conevalue + triangletype # model1-8.RData 35540.47
#formula <- ~ marmar1km + marmar2YearsAgo + interpolated_conevalue + triangletype + ycoord # model1-9.RData 35536.94
#formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype # model1-10.RData 35535.89
#formula <- ~ marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + interpolated_conevalue + triangletype # model1-11.RData 35538.55
#formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + xcoord + ycoord # model1-12.RData 35535.34
#formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + ycoord # model1-4-rw.RData 39643.33
model1$setCovariatesModel(formula, oravat@data)
model1$getLinearModel()
model1$addObservationStack(sp = oravat, response = oravat@data$scivultracks, covariates = oravat@data, offset = oravat$Length)
#model$addValidationStack(sp = val, covariates = val@data)
model1$addPredictionStack(sp = oravat)
model1$estimate(verbose = T)
save(model1, file = "model1-4.RData")
model1$summary()
model1$summarySpatialParameters()
model1$summaryTemporalVariation(timeIndex = time(oravat))

# Null model
model0 <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model0$setSpatialMesh(mesh)
model0$setSpatialPrior(rho = 800e3)
model0$setLikelihood("nbinomial")
model0$setLinkFunction(poisson()$link)
model0$setSmoothingModel()
model0$getLinearModel()
model0$addObservationStack(sp = oravat, response = oravat@data$scivultracks, offset = oravat$Length)
model0$addPredictionStack(sp = oravat)
model0$estimate(verbose = T)
save(model0, file = "model0.RData")
model0$summary()
model0$summarySpatialParameters()
model0$summaryTemporalVariation(timeIndex = time(oravat))

# Full model
modelf <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
modelf$setSpatialMesh(mesh)
modelf$setSpatialPrior(rho = 800e3)
modelf$setLikelihood("nbinomial")
modelf$setLinkFunction(poisson()$link)
formula <- ~ marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + interpolated_conevalue + triangletype + xcoord + ycoord
modelf$setCovariatesModel(formula, oravat@data)
modelf$getLinearModel()
modelf$addObservationStack(sp = oravat, response = oravat@data$scivultracks, covariates = oravat@data, offset = oravat$Length)
#model$addValidationStack(sp = val, covariates = val@data)
modelf$addPredictionStack(sp = oravat)
modelf$estimate(verbose = T)
save(modelf, file = "modelF.RData")
modelf$summary()
modelf$summarySpatialParameters()
modelf$summaryTemporalVariation(timeIndex = time(oravat))

# Non-spatio-temporal model
formula <- ~ marmar1km + marmarPrevYear + interpolated_conevalue + triangletype + ycoord
modelm <- inla(formula = reformulate(attr(terms(formula), "term.labels"), "scivultracks"), data = oravat@data, family = "nbinomial", offset = oravat@data$Length, control.compute = list(waic = TRUE))
summary(modelm)
