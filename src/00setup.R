#Install and load supporting libraries.
print(Sys.info()[4])

R.Version()$version.string
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(MASS)
library(FSA)
library(car)
library(rcompanion)
library(moments)

print("list of loaded packages: ")
print((.packages()))

#tom epa windows
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  gdr_root <- file.path("c:", "git", "glinski_dermal_routes")
  gdr_graphics <- file.path(gdr_root, "graphics")
}
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  gdr_root <- file.path("c:","git","glinski_dermal_routes")
  gdr_graphics <- file.path(gdr_root, "graphics")
}

print(paste("Root directory location: ", gdr_root, sep=""))

gdr_csv_in <- file.path(gdr_root, "inputs")
gdr_graphics <- file.path(gdr_root, "graphics")

#check to see if directories are accessible
boo = file.exists(file.path(gdr_csv_in,"/Water_soil.csv"))
print(paste("check to see if R can access files OK: ", boo))

#cleaned up data set, manually reshaped
gdr_data <- read.csv(file.path(gdr_csv_in,"/Water_soil.csv"), stringsAsFactors = TRUE)

summary(gdr_data)
colnames(gdr_data)
class(gdr_data$Sample.ID)
unique(gdr_data$Sample.ID)
class(gdr_data$Analyte)
unique(gdr_data$Analyte)
class(gdr_data$Media)
unique(gdr_data$Media)
class(gdr_data$Matrix)
unique(gdr_data$Matrix)
class(gdr_data$Concentration)
dim(gdr_data)

View(gdr_data)
