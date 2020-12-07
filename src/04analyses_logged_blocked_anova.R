### SELECTIVE USE OF RECIPROCAL TRANSFORM


### DID NOT USE -- LOGGING DID NOT HELP NORMALITY
### DID NOT USE -- SQUARE ROOT DID NOT HELP NORMALITY

# drop methanol
dim(gdr_data)
dropped_methanol <- gdr_data[-which(gdr_data$Analyte=='4-OH'),]
dim(dropped_methanol)

# subset grd_data where Matrix == Amphibian
amphib_data <- dropped_methanol[which(dropped_methanol$Matrix=='Amphibian'),]
dim(amphib_data)
colnames(amphib_data)
# [1] "Sample.ID"     "Analyte"       "Media"         "Matrix"        "Concentration"

# transformations
#amphib_data$Concentration <- log(amphib_data$Concentration)
#amphib_data$Concentration <- sqrt(amphib_data$Concentration)
#amphib_data$Concentration <- amphib_data$Concentration^0.33333
#amphib_data$Concentration <- log10(amphib_data$Concentration)
#amphib_data$Concentration <- 1/amphib_data$Concentration
amphib_data$Concentration <- amphib_data$Concentration

levels(amphib_data$Matrix)
# [1] "Amphibian" "Soil"      "Water"  

levels(amphib_data$Analyte)
#[1] "4-OH" "BIF"  "CPF"  "CPO"  "TFS"  "TFSa"

levels(amphib_data$Media)
#[1] "Soil"  "Water"


# one way blocked anova
# https://www.quality-control-plan.com/StatGuide/oneway_b_anova.htm
# https://rcompanion.org/handbook/I_06.html
Summarize(Concentration ~ Media + Analyte, data=amphib_data)
#Media Analyte n       mean        sd        min         Q1     median         Q3        max
#1   Soil     BIF 8 -2.2906833 0.3488566 -2.6092190 -2.5183235 -2.3576806 -2.2070062 -1.5247338
#2  Water     BIF 8 -0.7310069 0.1527176 -0.9004223 -0.8057547 -0.7605779 -0.7152940 -0.3885401
#3   Soil     CPF 8 -0.4475368 0.5036663 -1.2869233 -0.7290302 -0.2843453 -0.0527505  0.0197683
#4  Water     CPF 8  0.8519112 0.2721503  0.3093205  0.7847116  0.8544464  0.9764583  1.2260056
#5   Soil     CPO 8 -1.4569335 0.7641165 -2.8657748 -1.7407723 -1.3385455 -0.9458289 -0.5319880
#6  Water     CPO 8 -0.8845017 0.8307462 -2.3635460 -1.0615952 -0.7437407 -0.5130756  0.1300304
#7   Soil     TFS 8 -2.2017559 0.3577016 -2.7218044 -2.4308943 -2.2504595 -1.9862885 -1.6390676
#8  Water     TFS 8 -0.0601428 0.2668400 -0.5831472 -0.1986962  0.0010642  0.1478834  0.1891523
#9   Soil    TFSa 8 -3.5700612 0.5120175 -4.2957755 -3.9470733 -3.3844123 -3.3323356 -2.7979147
#10 Water    TFSa 8 -3.0009201 0.7723593 -4.3041771 -3.6310187 -2.6715257 -2.4499407 -2.1403642

conc_model <- lm(Concentration ~ Media + Analyte, data=amphib_data)

# Figner-Killeen Test of Homogeneity of Variances
bartlett.test(Concentration ~ Media, data=amphib_data) #sensitive to departures from normality
fligner.test(Concentration ~ Media, data=amphib_data)
leveneTest(Concentration ~ Media, data=amphib_data)

summary(conc_model)
#Call:
#  lm(formula = Concentration ~ Media + Analyte, data = amphib_data)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.80706 -0.30420  0.07489  0.43226  1.25296 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -2.1251     0.1651 -12.873  < 2e-16 ***
#  MediaWater    1.2285     0.1348   9.114 1.01e-13 ***
#  AnalyteCPF    1.7130     0.2131   8.038 1.10e-11 ***
#  AnalyteCPO    0.3401     0.2131   1.596   0.1148    
#AnalyteTFS    0.3799     0.2131   1.783   0.0788 .  
#AnalyteTFSa  -1.7746     0.2131  -8.327 3.11e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.6028 on 74 degrees of freedom
#Multiple R-squared:  0.8289,	Adjusted R-squared:  0.8174 
#F-statistic: 71.72 on 5 and 74 DF,  p-value: < 2.2e-16

Anova(conc_model, type = "II")
#Anova Table (Type II tests)

#Response: Concentration
#Sum Sq Df F value    Pr(>F)    
#Media      30.182  1  83.069 1.013e-13 ***
#  Analyte   100.114  4  68.885 < 2.2e-16 ***
#  Residuals  26.887 74                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#x <- residuals(conc_model)
#plotNormalHistogram(x)

plot(fitted(conc_model), residuals(conc_model))

plot(conc_model)


# the one way blocked anova is a generalization of the individual two sample t-tests below

### two sample t-test to compare the difference in means between water and soil
# individual analytes
#bifenthrin
bif_soil <- amphib_data[intersect(which(amphib_data$Analyte=="BIF"), which(amphib_data$Media=="Soil")),]$Concentration
bif_water <- amphib_data[intersect(which(amphib_data$Analyte=="BIF"), which(amphib_data$Media=="Water")),]$Concentration
bif_soil
shapiro.test(bif_soil) #reject normality, p = 0.002889
bif_water
shapiro.test(bif_water) #reject normality, p = 0.01051
t.test(bif_soil, bif_water) #reject equality

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
skewness(cpf_soil)
shapiro.test(cpf_soil) # do not reject normality, p = 0.2399
cpf_water
shapiro.test(cpf_water) #do not reject normality, p = 0.8812
t.test(cpf_soil, cpf_water) #reject equality

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
t.test(tfs_soil, tfs_water) #reject equality
#Welch Two Sample t-test
#data:  tfs_soil and tfs_water
#t = -10.211, df = 7.4841, p-value = 1.171e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.0469207 -0.6573651
#sample estimates:
#  mean of x mean of y 
#0.1170782 0.9692211 

#chlorpyrifos O
cpo_soil <- amphib_data[intersect(which(amphib_data$Analyte=="CPO"), which(amphib_data$Media=="Soil")),]$Concentration
cpo_water <- amphib_data[intersect(which(amphib_data$Analyte=="CPO"), which(amphib_data$Media=="Water")),]$Concentration
cpo_soil
skewness(cpo_soil)
shapiro.test(cpf_soil) # reject normality, p = 0.039
cpo_water
shapiro.test(cpo_water) #reject normality, p = 0.004
t.test(cpo_soil, cpo_water) # do not reject equality
