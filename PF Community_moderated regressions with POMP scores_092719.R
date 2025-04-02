#Activate packages
library(foreign)
library(foreign)
library(psych)
library(car)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(funModeling)
library(dlookr)
library(dplyr)
library(psych)
library(lme4)
library(lmerTest)
library(sjstats)
library(lmerTest)

#Import dataset
library(haven)
GMU_Community_for_regressions_with_POMP_scores_092719 <- read_sav("C:/Users/jddoo/Dropbox/PF manuscript/Primary Datasets/PF Community Sample - GMU/GMU Community_for regressions with POMP scores_092719.sav")
View(GMU_Community_for_regressions_with_POMP_scores_092719)

#rename dataset
PFModerations <- read.spss ("C:/Users/jddoo/Dropbox/PF manuscript/Primary Datasets/PF Community Sample - GMU/GMU Community_for regressions with POMP scores_092719.sav", 
                            to.data.frame = TRUE, use.value.labels = FALSE)

#Allow searching of variables by name 
attach(PFModerations)
detach(PFModerations)

########Moderated regressions with PFI Harnessing and intensity of negative emotions during goal pursuit (goal #1)

#PF Harnessing predicting BFI Conscientiousness
model1 <- lm(B_BFI_CONSC_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model1) 
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       62.120976   6.962096   8.923   <2e-16 ***
#  B_PF1_H5_mean_POMP                 0.077338   0.139185   0.556    0.579    
#  B_PF1_o_4_POMP                    -0.167178   0.101472  -1.648    0.101    
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP  0.001820   0.002083   0.874    0.383    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting BFI Negative Emotionality
model2 <- lm(B_BFI_NE_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model2) 
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       25.328771   6.995886   3.621 0.000347 ***
#  B_PF1_H5_mean_POMP                 0.091027   0.139860   0.651 0.515668    
#  B_PF1_o_4_POMP                     0.404852   0.101964   3.971 9.07e-05 ***
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP -0.001807   0.002093  -0.863 0.388613    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting BFI Open-Mindedness
model3 <- lm(B_BFI_OM_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model3)
#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                        7.755e+01  5.947e+00  13.039   <2e-16 ***
#  B_PF1_H5_mean_POMP                -4.244e-02  1.189e-01  -0.357    0.721    
#  B_PF1_o_4_POMP                    -5.716e-02  8.668e-02  -0.659    0.510    
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP  9.147e-05  1.779e-03   0.051    0.959    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting Subjective Happiness
model4 <- lm(B_SHS_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model4)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       78.333324   7.136967  10.976  < 2e-16 ***
#  B_PF1_H5_mean_POMP                -0.088152   0.143113  -0.616 0.538415    
#  B_PF1_o_4_POMP                    -0.373486   0.103976  -3.592 0.000386 ***
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP  0.003632   0.002139   1.698 0.090583 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting Satisfaction With Life Scale
model5 <- lm(B_SWLS_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model5)
#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       59.3532706  7.6359846   7.773  1.4e-13 ***
#  B_PF1_H5_mean_POMP                 0.1929323  0.1531199   1.260  0.20870    
#  B_PF1_o_4_POMP                    -0.2997425  0.1112465  -2.694  0.00747 ** 
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP -0.0001358  0.0022882  -0.059  0.95271    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting distress intolerance
model6 <- lm(B_DI_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model6)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)   
#  (Intercept)                       24.050417   7.663569   3.138  0.00188 **
#  B_PF1_H5_mean_POMP                -0.068210   0.154212  -0.442  0.65861   
#  B_PF1_o_4_POMP                     0.231812   0.111393   2.081  0.03835 * 
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP  0.000925   0.002301   0.402  0.68795   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting Depression
model7 <- lm(B_PHQ_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model7)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       11.774449   6.515739   1.807 0.071843 .  
#  B_PF1_H5_mean_POMP                 0.020110   0.131116   0.153 0.878217    
#  B_PF1_o_4_POMP                     0.340930   0.094714   3.600 0.000378 ***
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP -0.001442   0.001956  -0.737 0.461776    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#PF Harnessing predicting Social Anxiety
model8 <- lm(B_SIAS_mean_POMP ~ B_PF1_H5_mean_POMP + B_PF1_o_4_POMP + B_PF1_H5_mean_POMP*B_PF1_o_4_POMP, data=PFModerations)
summary(model8)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       17.345107   6.180257   2.807 0.005366 ** 
#  B_PF1_H5_mean_POMP                 0.029869   0.124365   0.240 0.810378    
#  B_PF1_o_4_POMP                     0.329575   0.089838   3.669 0.000293 ***
#  B_PF1_H5_mean_POMP:B_PF1_o_4_POMP -0.001924   0.001855  -1.037 0.300671    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



#########################
# Other useful functions
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics