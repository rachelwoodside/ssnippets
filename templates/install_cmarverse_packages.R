#Update R without IT help
install.packages("installr")
library(installr)
updateR()



#Install sensorstrings package
#You may be prompted to install supporting packages first
library(devtools)
install_github("dempsey-CMAR/sensorstrings", force = TRUE, dependencies = TRUE)
library(sensorstrings)


#Install qaqcmar package
library(devtools)
install_github("dempsey-CMAR/qaqcmar", force = TRUE, dependencies = TRUE)
library(qaqcmar)


#Install calval package
library(devtools)
install_github("ntorrie/calval", force = TRUE, dependencies = TRUE)
library(calval)


#Install strings package (old)
library(devtools)
install_github("Centre-for-Marine-Applied-Research/strings",force = TRUE)
library(strings)



