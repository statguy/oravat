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
  constructMesh(cutoff=1e4, maxEdge=c(5e4, 2e5), offset=c(1e4, 4e4), convex=0.05)$ # semidense mesh
  plotMesh()$
  setSpatialPrior()$
  setLikelihoodModel("nbinomial")
model$getMesh()$n

# Define model and estimate
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + naata2YearsAgo + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#model$save("test.RData")
#model$load("test.RData")

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6071 2.1207   -11.8492  -7.6234    -3.2759 -7.6512
#interpolated_conevalue  0.0046 0.0010     0.0026   0.0046     0.0065  0.0046
#naata1km                0.5286 0.0808     0.3714   0.5280     0.6885  0.5270
#naataPrevYear           0.1068 0.0763    -0.0413   0.1062     0.2579  0.1051
#naata2YearsAgo          0.0909 0.0755    -0.0558   0.0904     0.2404  0.0893
#kernel_new             -0.0828 0.3277    -0.7257  -0.0829     0.5605 -0.0832
#kernel_lagged           0.1000 0.3278    -0.5437   0.1000     0.7431  0.1000
#triangletypewlt        -0.7779 0.0803    -0.9356  -0.7779    -0.6204 -0.7779
#Watanabe-Akaike information criterion (WAIC) ...: 38573.05
#                                                       mean     sd 0.025quant
#size for the nbinomial observations (overdispersion) 0.9389 0.0232     0.8940
#Theta1 for spatial                                   1.2178 0.1432     0.9373
#Theta2 for spatial                                   1.0709 0.1321     0.8114
#GroupRho for spatial                                 0.9883 0.0037     0.9795
#                 mean           sd   0.025quant     0.5quant   0.975quant
#kappa    6.867194e-06 8.939577e-07 5.276056e-06 6.806154e-06 8.786939e-06
#tau      1.231603e-02 8.760565e-04 1.068354e-02 1.228140e-02 1.412593e-02
#range    4.188361e+05 5.448800e+04 3.215699e+05 4.152385e+05 5.355542e+05
#variance 1.188175e+01 3.393522e+00 6.568157e+00 1.140620e+01 1.985505e+01

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
    estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6169 2.0173   -11.6552  -7.6333    -3.4816 -7.6589
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5375 0.0806     0.3808   0.5370     0.6971  0.5360
#naataPrevYear           0.1149 0.0762    -0.0331   0.1144     0.2658  0.1133
#kernel_new             -0.0728 0.3279    -0.7162  -0.0730     0.5709 -0.0733
#kernel_lagged           0.0898 0.3280    -0.5543   0.0898     0.7333  0.0898
#triangletypewlt        -0.7735 0.0803    -0.9311  -0.7735    -0.6160 -0.7735
#Watanabe-Akaike information criterion (WAIC) ...: 38571.05

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + kernel_new + kernel_lagged + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6079 2.0207   -11.6487  -7.6235    -3.4778 -7.6489
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5536 0.0799     0.3982   0.5531     0.7118  0.5520
#kernel_new             -0.0904 0.3276    -0.7332  -0.0906     0.5527 -0.0909
#kernel_lagged           0.1089 0.3277    -0.5346   0.1089     0.7519  0.1089
#triangletypewlt        -0.7673 0.0802    -0.9248  -0.7673    -0.6100 -0.7673
#Watanabe-Akaike information criterion (WAIC) ...: 38569.94

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + kernel_lagged + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6123 2.0621   -11.7364  -7.6279    -3.4008 -7.6541
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5367 0.0805     0.3801   0.5362     0.6961  0.5351
#naataPrevYear           0.1157 0.0761    -0.0321   0.1151     0.2664  0.1140
#kernel_lagged           0.0183 0.0693    -0.1178   0.0183     0.1541  0.0183
#triangletypewlt        -0.7736 0.0802    -0.9311  -0.7736    -0.6162 -0.7736
#Watanabe-Akaike information criterion (WAIC) ...: 38569.15

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + naataPrevYear + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS, covariates)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6086 2.0593   -11.7269  -7.6241    -3.4029 -7.6499
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5366 0.0805     0.3800   0.5361     0.6960  0.5351
#naataPrevYear           0.1160 0.0761    -0.0317   0.1155     0.2667  0.1144
#triangletypewlt        -0.7723 0.0801    -0.9295  -0.7723    -0.6152 -0.7723
#Watanabe-Akaike information criterion (WAIC) ...: 38566.27

model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + kernel_new + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS, covariates)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6098 2.0052   -11.6209  -7.6256    -3.5069 -7.6508
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5532 0.0799     0.3978   0.5526     0.7113  0.5516
#kernel_new              0.0162 0.0693    -0.1199   0.0163     0.1521  0.0163
#triangletypewlt        -0.7675 0.0802    -0.9249  -0.7675    -0.6102 -0.7675
#Watanabe-Akaike information criterion (WAIC) ...: 38567.69
#                                                       mean     sd 0.025quant
#size for the nbinomial observations (overdispersion) 0.9416 0.0233     0.8962
#Theta1 for spatial                                   1.1982 0.1464     0.9306
#Theta2 for spatial                                   1.0420 0.1333     0.7937
#GroupRho for spatial                                 0.9879 0.0038     0.9793

# *** BEST MODEL (of all estimated models) ***
model$setCovariatesModel(~ 1 + interpolated_conevalue + naata1km + triangletype, covariates)$
  addObservationStack(time=oravat$vuosi, response=oravat$scivultracks, covariates=covariates, offset=oravat$PITUUS, covariates)$
  estimate(verbose=T)$
  summary()

#                          mean     sd 0.025quant 0.5quant 0.975quant    mode
#intercept              -7.6031 2.0306   -11.6640  -7.6184    -3.4552 -7.6438
#interpolated_conevalue  0.0046 0.0010     0.0027   0.0046     0.0065  0.0046
#naata1km                0.5530 0.0799     0.3977   0.5525     0.7111  0.5514
#triangletypewlt        -0.7662 0.0800    -0.9233  -0.7662    -0.6093 -0.7662
#Watanabe-Akaike information criterion (WAIC) ...: 38565.18
#                                                       mean     sd 0.025quant
#size for the nbinomial observations (overdispersion) 0.9407 0.0233     0.8961
#Theta1 for spatial                                   1.2003 0.1423     0.9308
#Theta2 for spatial                                   1.0469 0.1315     0.7969
#GroupRho for spatial                                 0.9880 0.0037     0.9794
#                 mean           sd   0.025quant     0.5quant   0.975quant
#kappa    7.033697e-06 9.048730e-07 5.369018e-06 6.995756e-06 8.918410e-06
#tau      1.226437e-02 8.637541e-04 1.066082e-02 1.222757e-02 1.405493e-02
#range    4.088615e+05 5.334758e+04 3.168307e+05 4.039862e+05 5.262862e+05
#variance 1.147136e+01 3.307680e+00 6.475636e+00 1.093259e+01 1.942412e+01

