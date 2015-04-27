oravat <- read.csv2("SquirrelData_without_weather.csv", fileEncoding="latin1")
oravat$scivultracks <- round(oravat$scivultracks)

x <- table(oravat$scivultracks)
x[1] / sum(x[-1]) # prop of zeros in the data
x
