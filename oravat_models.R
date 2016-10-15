if (F) {
  library(devtools)
  install_github("statguy/SpaceTimeModels")
  reload(inst("SpaceTimeModels"))
}

library(SpaceTimeModels)
library(zoo)
library(splancs)

data <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
columns <- c("Year","X","Y","Length","marmar1km","marmarPrevYear","marmar2YearsAgo","kernel_new","kernel_lagged","triangletype","interpolated_conevalue")
completeIndex <- complete.cases(data[,columns])
oravat.raw <- data[completeIndex, c("scivultracks", columns)]
#scaleColumns <- !colnames(oravat) %in% c("Year","X","Y","Length","triangletype")
#scaleColumns <- scale(oravat[,scaleColumns])
oravat.raw$ycoord <- oravat.raw$Y / 1e6

# High correlation for kernel_lagged and kernel_new, leave out kernel_lagged
cor(oravat.raw[,!colnames(oravat.raw) %in% c("Year","X","Y","Length","triangletype")])

#oravat.raw <- oravat.raw[sample(1:nrow(oravat.raw), 250),] # Take sample for testing only
# Convert data to STIDF object
oravat <- spacetime::STIDF(sp::SpatialPoints(oravat.raw[,c("X","Y")]), zoo::as.yearmon(oravat.raw$Year), oravat.raw)

# cutoff=5e3, maxEdge=c(3.7e4, 2e5), offset=c(1e4, 4e4), convex=0.04 # dense mesh
# cutoff=1e5, maxEdge=c(2e5, 10e5), convex=0.12 # sparse mesh
mesh <- SpaceTimeModels::NonConvexHullMesh$new(knots=oravat@sp, knotsScale=1e6, cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)
mesh$getINLAMesh()$n
mesh$plot()

model <- SpaceTimeModels::ContinuousSpaceDiscreteTimeModel$new()
model$setSpatialMesh(mesh)
model$setSpatialPrior()
model$setLikelihood("nbinomial")

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + triangletype + ycoord, covariates=oravat@data)
model$getLinearModel()
#model$addObservationStack(time=oravat$Year, response=oravat$scivultracks, covariates=oravat, offset=oravat$Length)
model$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)
model$estimate(verbose=T)
model$summary()


offset <- oravat$Length
tag <- "obs"
index <- inla.stack.index(model$getFullStack(), tag)$data
nodes <- model$getSpatialMesh()$getINLAMesh()$n
spatial.mean <- inla.vector2matrix(model$getResult()$summary.random$spatial$mean, nrow = nodes, ncol = length(unique(time(oravat))))
spatial.sd <- inla.vector2matrix(model$getResult()$summary.random$spatial$sd, nrow = nodes, ncol = length(unique(time(oravat))))

projector <- inla.mesh.projector(model$getSpatialMesh()$getINLAMesh())
year.index <- 1
projection.mean <- inla.mesh.project(projector, spatial.mean[,year.index])
projection.sd <- inla.mesh.project(projector, spatial.sd[,year.index])
image(projector$x, projector$y, projection.mean)
image(projector$x, projector$y, projection.sd)


# Fixed effects:
#   mean      sd 0.025quant 0.5quant 0.975quant    mode
# intercept               9.5020 24.8508   -39.2863   9.5006    58.2539  9.4997
# interpolated_conevalue  0.0047  0.0010     0.0028   0.0047     0.0066  0.0047
# marmar1km               0.5478  0.0782     0.3957   0.5472     0.7026  0.5462
# marmarPrevYear          0.1068  0.0735    -0.0361   0.1063     0.2525  0.1052
# marmar2YearsAgo         0.1399  0.0695     0.0049   0.1394     0.2775  0.1384
# kernel_new              0.0121  0.0666    -0.1186   0.0121     0.1428  0.0120
# triangletypewlt        -0.5810  0.0729    -0.7240  -0.5810    -0.4379 -0.5810
# ycoord                 -1.4800  3.4736    -8.3002  -1.4800     5.3338 -1.4797
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# marmarPrevYear           0
# marmar2YearsAgo          0
# kernel_new               0
# triangletypewlt          0
# ycoord                   0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model 
# 
# Model hyperparameters:
#   mean    sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.9409 7e-04     0.9394
# Theta1 for spatial                                   1.2763 2e-04     1.2759
# Theta2 for spatial                                   1.1212 4e-04     1.1202
# GroupRho for spatial                                 0.9905 0e+00     0.9905
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.9409     0.9423 0.9409
# Theta1 for spatial                                     1.2763     1.3224 1.2764
# Theta2 for spatial                                     1.1212     1.1220 1.1212
# GroupRho for spatial                                   0.9905     0.9908 0.9905
# 
# Expected number of effective parameters(std dev): 548.40(3.28)
# Number of equivalent replicates : 14.70 
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40708.37
# Effective number of parameters .................: 571.50
# 
# Marginal log-Likelihood:  -20730.26 
# Posterior marginals for linear predictor and fitted values computed

model$summaryTemporalVariation()


model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmarPrevYear + marmar2YearsAgo + kernel_new + triangletype, covariates=oravat@data)$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
#   mean     sd 0.025quant 0.5quant 0.975quant    mode
# intercept              -1.0558 3.0848    -7.2660  -1.0567     5.1599 -1.0578
# interpolated_conevalue  0.0047 0.0010     0.0026   0.0047     0.0067  0.0047
# marmar1km               0.5471 0.0802     0.3912   0.5465     0.7059  0.5454
# marmarPrevYear          0.1088 0.0754    -0.0377   0.1082     0.2582  0.1071
# marmar2YearsAgo         0.1416 0.0712     0.0033   0.1410     0.2826  0.1400
# kernel_new              0.0077 0.0687    -0.1273   0.0077     0.1425  0.0077
# triangletypewlt        -0.5807 0.0747    -0.7273  -0.5808    -0.4342 -0.5808
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# marmarPrevYear           0
# marmar2YearsAgo          0
# kernel_new               0
# triangletypewlt          0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model 
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8852 0.0208     0.8451
# Theta1 for spatial                                   1.4483 0.1390     1.1859
# Theta2 for spatial                                   1.2150 0.1243     0.9781
# GroupRho for spatial                                 0.9929 0.0021     0.9881
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8849     0.9269 0.8844
# Theta1 for spatial                                     1.4438     1.7313 1.4303
# Theta2 for spatial                                     1.2121     1.4662 1.2031
# GroupRho for spatial                                   0.9931     0.9963 0.9935
# 
# Expected number of effective parameters(std dev): 533.25(30.85)
# Number of equivalent replicates : 15.12 
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40699.54
# Effective number of parameters .................: 540.17
# 
# Marginal log-Likelihood:  -20711.54 
# Posterior marginals for linear predictor and fitted values computed

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmar2YearsAgo + kernel_new + triangletype, covariates=oravat)$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
#   mean     sd 0.025quant 0.5quant 0.975quant    mode
# intercept              -1.0486 3.0679    -7.2233  -1.0495     5.1319 -1.0507
# interpolated_conevalue  0.0047 0.0010     0.0026   0.0047     0.0067  0.0047
# marmar1km               0.5626 0.0795     0.4081   0.5620     0.7201  0.5609
# marmar2YearsAgo         0.1541 0.0706     0.0170   0.1535     0.2941  0.1525
# kernel_new              0.0096 0.0688    -0.1254   0.0096     0.1445  0.0097
# triangletypewlt        -0.5767 0.0746    -0.7231  -0.5767    -0.4302 -0.5768
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# marmar2YearsAgo          0
# kernel_new               0
# triangletypewlt          0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model 
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8850 0.0208     0.8447
# Theta1 for spatial                                   1.4440 0.1376     1.1806
# Theta2 for spatial                                   1.2123 0.1237     0.9750
# GroupRho for spatial                                 0.9928 0.0021     0.9879
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8849     0.9266 0.8845
# Theta1 for spatial                                     1.4412     1.7209 1.4328
# Theta2 for spatial                                     1.2101     1.4602 1.2035
# GroupRho for spatial                                   0.9931     0.9962 0.9935
# 
# Expected number of effective parameters(std dev): 532.73(30.79)
# Number of equivalent replicates : 15.14 
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40699.18
# Effective number of parameters .................: 539.47
# 
# Marginal log-Likelihood:  -20706.54 
# Posterior marginals for linear predictor and fitted values computed

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmar2YearsAgo + triangletype, covariates=oravat@data)$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
# mean     sd 0.025quant 0.5quant 0.975quant    mode
# intercept              -1.0457 2.9955    -7.1012  -1.0465     5.0173 -1.0475
# interpolated_conevalue  0.0047 0.0010     0.0026   0.0047     0.0067  0.0047
# marmar1km               0.5628 0.0795     0.4083   0.5622     0.7204  0.5611
# marmar2YearsAgo         0.1536 0.0706     0.0164   0.1530     0.2936  0.1520
# triangletypewlt        -0.5760 0.0745    -0.7222  -0.5760    -0.4297 -0.5761
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# marmar2YearsAgo          0
# triangletypewlt          0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model 
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8864 0.0209     0.8463
# Theta1 for spatial                                   1.4470 0.1542     1.1799
# Theta2 for spatial                                   1.2020 0.1289     0.9671
# GroupRho for spatial                                 0.9928 0.0024     0.9878
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8861     0.9282 0.8855
# Theta1 for spatial                                     1.4335     1.7791 1.3895
# Theta2 for spatial                                     1.1946     1.4718 1.1718
# GroupRho for spatial                                   0.9930     0.9969 0.9934
# 
# Expected number of effective parameters(std dev): 536.19(32.79)
# Number of equivalent replicates : 15.04 
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40697.52
# Effective number of parameters .................: 542.25
# 
# Marginal log-Likelihood:  -20700.60 
# Posterior marginals for linear predictor and fitted values computed

# !!

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + marmarPrevYear + triangletype, covariates=oravat@data)$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
#   mean     sd 0.025quant 0.5quant 0.975quant    mode
# intercept              -1.0530 3.0053    -7.1059  -1.0537     5.0049 -1.0544
# interpolated_conevalue  0.0047 0.0010     0.0027   0.0047     0.0067  0.0047
# marmar1km               0.5596 0.0801     0.4039   0.5591     0.7182  0.5580
# marmarPrevYear          0.1276 0.0750    -0.0180   0.1270     0.2762  0.1260
# triangletypewlt        -0.5710 0.0745    -0.7172  -0.5711    -0.4248 -0.5711
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# marmarPrevYear           0
# triangletypewlt          0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8859 0.0209     0.8457
# Theta1 for spatial                                   1.4361 0.1392     1.1768
# Theta2 for spatial                                   1.1995 0.1248     0.9660
# GroupRho for spatial                                 0.9928 0.0021     0.9879
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8857     0.9276 0.8852
# Theta1 for spatial                                     1.4302     1.7225 1.4125
# Theta2 for spatial                                     1.1947     1.4557 1.1801
# GroupRho for spatial                                   0.9930     0.9963 0.9934
# 
# Expected number of effective parameters(std dev): 534.86(31.01)
# Number of equivalent replicates : 15.07
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40697.77
# Effective number of parameters .................: 541.75
# 
# Marginal log-Likelihood:  -20701.35
# Posterior marginals for linear predictor and fitted values computed

model$setCovariatesModel(~ interpolated_conevalue + marmar1km + triangletype, covariates=oravat@data)$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, covariates=oravat@data, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
#   mean     sd 0.025quant 0.5quant 0.975quant    mode
# intercept              -1.0435 3.0020    -7.0850  -1.0443     5.0030 -1.0452
# interpolated_conevalue  0.0047 0.0010     0.0027   0.0047     0.0067  0.0047
# marmar1km               0.5798 0.0793     0.4258   0.5793     0.7369  0.5781
# triangletypewlt        -0.5650 0.0744    -0.7111  -0.5650    -0.4189 -0.5651
# kld
# intercept                0
# interpolated_conevalue   0
# marmar1km                0
# triangletypewlt          0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8851 0.0208     0.8452
# Theta1 for spatial                                   1.4337 0.1365     1.1707
# Theta2 for spatial                                   1.1995 0.1230     0.9637
# GroupRho for spatial                                 0.9927 0.0022     0.9877
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8848     0.9269 0.8840
# Theta1 for spatial                                     1.4316     1.7068 1.4251
# Theta2 for spatial                                     1.1972     1.4461 1.1905
# GroupRho for spatial                                   0.9929     0.9961 0.9934
# 
# Expected number of effective parameters(std dev): 534.17(30.83)
# Number of equivalent replicates : 15.09
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40697.89
# Effective number of parameters .................: 540.72
# 
# Marginal log-Likelihood:  -20696.74
# Posterior marginals for linear predictor and fitted values computed

# Null models

model$clearStack()$setSmoothingModel()$
  clearStack()$addObservationStack(sp=oravat, response=oravat$scivultracks, offset=oravat$Length)$
  estimate(verbose=T)$summary()

# Fixed effects:
#   mean   sd 0.025quant 0.5quant 0.975quant    mode kld
# intercept -1.3465 3.89    -9.1542  -1.3478     6.4674 -1.3495   0
# 
# Random effects:
#   Name      Model
# spatial   SPDE2 model
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.8659 0.0201     0.8268
# Theta1 for spatial                                   1.5719 0.1320     1.3220
# Theta2 for spatial                                   1.3346 0.1154     1.1139
# GroupRho for spatial                                 0.9934 0.0019     0.9891
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.8658     0.9059 0.8657
# Theta1 for spatial                                     1.5680     1.8399 1.5560
# Theta2 for spatial                                     1.3322     1.5667 1.3248
# GroupRho for spatial                                   0.9935     0.9964 0.9939
# 
# Expected number of effective parameters(std dev): 525.69(29.03)
# Number of equivalent replicates : 15.34
# 
# Watanabe-Akaike information criterion (WAIC) ...: 40804.08
# Effective number of parameters .................: 533.97
# 
# Marginal log-Likelihood:  -20738.01
# Posterior marginals for linear predictor and fitted values computed

summary(inla(scivultracks ~ interpolated_conevalue + marmar1km + triangletype, data=oravat@data, family="nbinomial", control.compute=list(waic=TRUE)))

# Fixed effects:
# mean     sd 0.025quant 0.5quant 0.975quant    mode
# (Intercept)             1.8926 0.0493     1.7968   1.8923     1.9904  1.8916
# interpolated_conevalue  0.0054 0.0004     0.0047   0.0054     0.0062  0.0054
# marmar1km               0.5371 0.0836     0.3755   0.5362     0.7036  0.5344
# triangletypewlt        -0.4848 0.0509    -0.5857  -0.4845    -0.3857 -0.4838
# kld
# (Intercept)              0
# interpolated_conevalue   0
# marmar1km                0
# triangletypewlt          0
# 
# The model has no random effects
# 
# Model hyperparameters:
#   mean     sd 0.025quant
# size for the nbinomial observations (overdispersion) 0.5334 0.0098     0.5144
# 0.5quant 0.975quant   mode
# size for the nbinomial observations (overdispersion)   0.5333      0.553 0.5332
# 
# Expected number of effective parameters(std dev): 4.065(9e-04)
# Number of equivalent replicates : 1983.55 
# 
# Watanabe-Akaike information criterion (WAIC) ...: 43259.15
# Effective number of parameters .................: 7.715
# 
# Marginal log-Likelihood:  -21654.49 
# Posterior marginals for linear predictor and fitted values computed
