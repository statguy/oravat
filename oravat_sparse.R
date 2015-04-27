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
# Construct mesh for random effect, set prior and likelihood models
model <- DiscreteTimeContinuousSpaceModel$new()$
  constructMesh(coords=oravat[,c("X","Y")], cutoff=1e5, maxEdge=c(2e5, 10e5), convex=0.12)$ # sparse mesh
  plotMesh()$
  setSpatialPrior()$
  setLikelihood("nbinomial")
model$getMesh()$n

# Define model and estimate
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + naata2YearsAgo + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode kld
#intercept              -7.7587 0.4374    -8.6424  -7.7564    -6.8867 -7.7516   0
#interpolated_conevalue  0.0044 0.0012     0.0021   0.0044     0.0067  0.0044   0
#naata1km                0.5150 0.0829     0.3543   0.5144     0.6795  0.5130   0
#naataPrevYear           0.0887 0.0765    -0.0593   0.0879     0.2409  0.0864   0
#naata2YearsAgo          0.0459 0.0744    -0.0982   0.0452     0.1939  0.0437   0
#kernel_new              0.3810 0.3325    -0.2713   0.3809     1.0337  0.3805   0
#kernel_lagged          -0.0621 0.3319    -0.7137  -0.0621     0.5891 -0.0622   0
#triangletypewlt        -0.8904 0.0771    -1.0419  -0.8904    -0.7392 -0.8904   0
#Watanabe-Akaike information criterion (WAIC) ...: 39386.59

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$  
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode kld
#intercept              -7.7568 0.4352    -8.6360  -7.7546    -6.8893 -7.7497   0
#interpolated_conevalue  0.0044 0.0012     0.0021   0.0044     0.0067  0.0044   0
#naata1km                0.5215 0.0823     0.3619   0.5208     0.6849  0.5195   0
#naataPrevYear           0.0955 0.0758    -0.0513   0.0948     0.2464  0.0933   0
#kernel_new              0.3850 0.3324    -0.2672   0.3849     1.0376  0.3845   0
#kernel_lagged          -0.0660 0.3319    -0.7175  -0.0660     0.5851 -0.0660   0
#triangletypewlt        -0.8887 0.0771    -1.0401  -0.8887    -0.7375 -0.8886   0
#Watanabe-Akaike information criterion (WAIC) ...: 39384.69

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + kernel_new + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode kld
#intercept              -7.7570 0.4357    -8.6370  -7.7547    -6.8885 -7.7499   0
#interpolated_conevalue  0.0044 0.0012     0.0021   0.0044     0.0067  0.0044   0
#naata1km                0.5218 0.0823     0.3622   0.5211     0.6851  0.5197   0
#naataPrevYear           0.0950 0.0758    -0.0517   0.0943     0.2458  0.0928   0
#kernel_new              0.3201 0.0542     0.2138   0.3201     0.4265  0.3201   0
#triangletypewlt        -0.8886 0.0771    -1.0400  -0.8886    -0.7374 -0.8885   0
#Watanabe-Akaike information criterion (WAIC) ...: 39382.36

# *** BEST MODEL (of all estimated models) ***
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode kld
#intercept              -7.7519 0.4333    -8.6269  -7.7496    -6.8881 -7.7449   0
#interpolated_conevalue  0.0044 0.0012     0.0021   0.0044     0.0067  0.0045   0
#naata1km                0.5405 0.0810     0.3834   0.5398     0.7014  0.5385   0
#kernel_new              0.3219 0.0542     0.2156   0.3219     0.4283  0.3219   0
#triangletypewlt        -0.8848 0.0770    -1.0360  -0.8847    -0.7337 -0.8847   0
#Watanabe-Akaike information criterion (WAIC) ...: 39381.54

model$setCovariatesModel(~ 1 + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(coords=oravat[,c("X","Y")], time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                   mean     sd 0.025quant 0.5quant 0.975quant    mode kld
#intercept       -7.6203 0.4729    -8.5745  -7.6186    -6.6739 -7.6146   0
#naata1km         0.5389 0.0810     0.3819   0.5382     0.6997  0.5369   0
#kernel_new       0.3196 0.0542     0.2132   0.3195     0.4259  0.3195   0
#triangletypewlt -0.8841 0.0771    -1.0356  -0.8841    -0.7329 -0.8841   0
#Watanabe-Akaike information criterion (WAIC) ...: 39384.39
