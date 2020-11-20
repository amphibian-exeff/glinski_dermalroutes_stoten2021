# subset grd_data where Matrix == Amphibian
amphib_data <- gdr_data[which(gdr_data$Matrix=='Amphibian'),]
dim(amphib_data)
colnames(amphib_data)
# [1] "Sample.ID"     "Analyte"       "Media"         "Matrix"        "Concentration"
levels(amphib_data$Matrix)

levels(amphib_data$Analyte)
#[1] "4-OH" "BIF"  "CPF"  "CPO"  "TFS"  "TFSa"

levels(amphib_data$Media)
#[1] "Soil"  "Water"


# one way blocked anova
# https://www.quality-control-plan.com/StatGuide/oneway_b_anova.htm
# https://rcompanion.org/handbook/I_06.html
Summarize(Concentration ~ Media + Analyte, data=amphib_data)
#Media Analyte n      mean        sd      min        Q1    median        Q3      max percZero
#1   Soil    4-OH 8 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.000000      100
#2  Water    4-OH 8 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.000000      100
#3   Soil     BIF 8 0.1076140 0.0468643 0.073592 0.0806762 0.0946415 0.1100598 0.217679        0
#4  Water     BIF 8 0.4867179 0.0826436 0.406398 0.4467638 0.4675720 0.4890795 0.678046        0
#5   Soil     CPF 8 0.7051681 0.2965263 0.276119 0.4905368 0.7619715 0.9491637 1.019965        0
#6  Water     CPF 8 2.4167515 0.6096283 1.362499 2.1925238 2.3514455 2.6614310 3.407591        0
#7   Soil     CPO 8 0.2880295 0.1756229 0.056939 0.1810142 0.2622280 0.3898405 0.587436        0
#8  Water     CPO 8 0.5298190 0.3523058 0.094086 0.3738482 0.4758010 0.6201100 1.138863        0
#9   Soil     TFS 8 0.1170782 0.0431794 0.065756 0.0879935 0.1054965 0.1379120 0.194161        0
#10 Water     TFS 8 0.9692211 0.2320592 0.558139 0.8198502 1.0046130 1.1596585 1.208225        0
#11  Soil    TFSa 8 0.0314065 0.0153188 0.013626 0.0195385 0.0338985 0.0357350 0.060937        0
#12 Water    TFSa 8 0.0618348 0.0368426 0.013512 0.0265292 0.0694975 0.0863000 0.117612        0

conc_model <- lm(Concentration ~ Media + Analyte, data=amphib_data)

summary(conc_model)
#Call:
#  lm(formula = Concentration ~ Media + Analyte, data = amphib_data)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.01692 -0.24156 -0.04834  0.25704  1.57871 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.26792    0.10443  -2.566 0.011971 *  
#  MediaWater   0.53584    0.07894   6.788 1.22e-09 ***
#  AnalyteBIF   0.29717    0.13673   2.173 0.032403 *  
#  AnalyteCPF   1.56096    0.13673  11.417  < 2e-16 ***
#  AnalyteCPO   0.40892    0.13673   2.991 0.003598 ** 
#  AnalyteTFS   0.54315    0.13673   3.973 0.000144 ***
#  AnalyteTFSa  0.04662    0.13673   0.341 0.733924    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.3867 on 89 degrees of freedom
#Multiple R-squared:  0.7123,	Adjusted R-squared:  0.6929 
#F-statistic: 36.73 on 6 and 89 DF,  p-value: < 2.2e-16

Anova(conc_model, type = "II")
#Anova Table (Type II tests)
#
#Response: Concentration
#Sum Sq Df F value    Pr(>F)    
#Media      6.891  1  46.078 1.224e-09 ***
#  Analyte   26.065  5  34.858 < 2.2e-16 ***
#  Residuals 13.310 89                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

x <- residuals(conc_model)
plotNormalHistogram(x)

plot(fitted(conc_model), residuals(conc_model))

plot(conc_model)


# the one way blocked anova is a generalization of the individual two sample t-tests below

### two sample t-test to compare the difference in means between water and soil
# individual analytes
meth_soil <- amphib_data[intersect(which(amphib_data$Analyte=="4-OH"), which(amphib_data$Media=="Soil")),]$Concentration
meth_water <- amphib_data[intersect(which(amphib_data$Analyte=="4-OH"), which(amphib_data$Media=="Water")),]$Concentration

#bifenthrin
bif_soil <- amphib_data[intersect(which(amphib_data$Analyte=="BIF"), which(amphib_data$Media=="Soil")),]$Concentration
bif_water <- amphib_data[intersect(which(amphib_data$Analyte=="BIF"), which(amphib_data$Media=="Water")),]$Concentration
bif_soil
shapiro.test(bif_soil) #reject normality, p = 0.002889
bif_water
shapiro.test(bif_water) #reject normality, p = 0.01051
t.test(bif_soil, bif_water)

#Welch Two Sample t-test
#data:  bif_soil and bif_water
#t = -11.286, df = 11.08, p-value = 2.037e-07
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4529696 -0.3052382
#sample estimates:
#  mean of x mean of y 
#0.1076140 0.4867179 

#chlorpyrifos
cpf_soil <- amphib_data[intersect(which(amphib_data$Analyte=="CPF"), which(amphib_data$Media=="Soil")),]$Concentration
cpf_water <- amphib_data[intersect(which(amphib_data$Analyte=="CPF"), which(amphib_data$Media=="Water")),]$Concentration
cpf_soil
shapiro.test(cpf_soil) # do not reject normality, p = 0.2399
cpf_water
shapiro.test(cpf_water) #do not reject normality, p = 0.8812
t.test(cpf_soil, cpf_water)

#Welch Two Sample t-test
#data:  cpf_soil and cpf_water
#t = -7.1411, df = 10.137, p-value = 2.921e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.244651 -1.178516
#sample estimates:
#  mean of x mean of y 
#0.7051681 2.4167515 

#trifloxystrobin
tfs_soil <- amphib_data[intersect(which(amphib_data$Analyte=="TFS"), which(amphib_data$Media=="Soil")),]$Concentration
tfs_water <- amphib_data[intersect(which(amphib_data$Analyte=="TFS"), which(amphib_data$Media=="Water")),]$Concentration
tfs_soil
shapiro.test(tfs_soil) #do not reject normality, p = 0.5574
tfs_water
shapiro.test(tfs_water) #do not reject normality, p = 0.3485
t.test(tfs_soil, tfs_water)
#Welch Two Sample t-test
#data:  tfs_soil and tfs_water
#t = -10.211, df = 7.4841, p-value = 1.171e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.0469207 -0.6573651
#sample estimates:
#  mean of x mean of y 
#0.1170782 0.9692211 
