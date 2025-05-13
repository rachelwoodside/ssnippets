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
install_github("dempsy-cmar/calval", force = TRUE, dependencies = TRUE)
library(calval)


#install adcp package
library(devtools)
install_github("dempsey-CMAR/adcp", force = TRUE, dependencies = TRUE)
library(adcp)


#install waves package
library(devtools)
install_github("dempsey-CMAR/waves", force = TRUE, dependencies = TRUE)
library(waves)


#install summaryplots package
library(devtools)
install_github("dempsey-CMAR/summaryplots", force = TRUE, dependencies = TRUE)
library(summaryplots)


#Install strings package (old)
library(devtools)
install_github("Centre-for-Marine-Applied-Research/strings",force = TRUE)
library(strings)



