#Install and load supporting libraries.
print(Sys.info()[4])

R.Version()$version.string
library(rmarkdown, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2)
library(reshape2)
library(MASS)
print("list of loaded packages: ")
print((.packages()))

#tom epa windows
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  gdr_root<-path.expand("d:/git/glinski_dermal_routes/")
}
if(Sys.info()[4]=="Coiles-MBP"){
  gdr_root<-path.expand("~/git/glinski_dermal_routes/")
}
print(paste("Root directory location: ", dag_root, sep=""))

gdr_csv_in <- paste(gdr_root, "inputs/", sep="")
gdr_graphics <- paste(gdr_root, "graphics/", sep="")

#check to see if directories are accessible
boo = file.exists(paste(gdr_csv_in,"Water_soil.csv",sep=""))
print(paste("check to see if R can access files OK: ", boo))

#cleaned up data set, manually reshaped
gdr_data <- read.csv(paste(gdr_csv_in,"Water_soil.csv",sep=""))

summary(gdr_data)
colnames(gdr_data)
class(gdr_data$Sample.ID)
class(gdr_data$Analyte)
class(gdr_data$Media)
class(gdr_data$Matrix)
class(gdr_data$Concentration)
dim(gdr_data)

