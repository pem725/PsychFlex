#Load packages
library(foreign)
library(psych)
library(car)
library(lme4)
library(lmerTest)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(funModeling)
library(dlookr)
library(dplyr)
library(sjstats)
library(lmerTest)
library(jtools)
library(sjmisc)
library(sjPlot)
library(lmSupport)

#Import dataset
library(haven)
Merged_GMU_Baseline_FollowUp_102819 <- read_sav("./Merged_GMU_Baseline&FollowUp_102819.sav")
View(Merged_GMU_Baseline_FollowUp_102819)

#rename dataset
PFMerged <- Merged_GMU_Baseline_FollowUp_102819

#Allow searching of variables by name 
attach(PFMerged)
detach(PFMerged)

###################### SIGNIFICANT INTERACTION EFFECTS

#### Subjective LE intensity predicting Baseline Autonomy Satisfaction - moderated by HARNESSING

model237 <- lm(ZB_BMPNS_A_tot ~ ZSubjIntensityAvg + ZB_PF1_H5_mean + ZSubjIntensityAvg:ZB_PF1_H5_mean, data=PFMerged)
summary(model237)
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)   
#  (Intercept)                       0.04177    0.05891   0.709  0.47885   
#  ZSubjIntensityAvg                -0.19129    0.05961  -3.209  0.00149 **
#  ZB_PF1_H5_mean                   -0.02313    0.06104  -0.379  0.70501   
#  ZSubjIntensityAvg:ZB_PF1_H5_mean  0.19993    0.06093   3.281  0.00117 ** #### SIG MODERATION
plot_model(model237, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_H5_mean"))
vcov(model237)

#Simple Intercepts and Slopes at Conditional Values of Z
#=======================================================
#  At Z = cv1...
#  simple intercept = 0.0649(0.0862), t=0.7531, p=0.452
#  simple slope     = -0.3912(0.0879), t=-4.4509, p=0
#  At Z = cv2...
#  simple intercept = 0.0418(0.0589), t=0.7092, p=0.4788
#  simple slope     = -0.1913(0.0596), t=-3.2092, p=0.0015
#  At Z = cv3...
#  simple intercept = 0.0186(0.0834), t=0.2234, p=0.8234
#  simple slope     = 0.0086(0.0825), t=0.1047, p=0.9167

#### Subjective LE intensity predicting Baseline Competence Satisfaction - moderated by HARNESSING

model292 <- lm(ZB_BMPNS_C_tot ~ ZSubjIntensityAvg + ZB_PF1_H5_mean + ZSubjIntensityAvg:ZB_PF1_H5_mean, data=PFMerged)
summary(model292)
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)  
#  (Intercept)                       0.01695    0.05946   0.285   0.7758  
#  ZSubjIntensityAvg                -0.08258    0.06017  -1.373   0.1711  
#  ZB_PF1_H5_mean                    0.06440    0.06161   1.045   0.2968  
#  ZSubjIntensityAvg:ZB_PF1_H5_mean  0.13296    0.06150   2.162   0.0315 * SIG MODERATION
plot_model(model292, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_H5_mean"))
vcov(model292)

#Simple Intercepts and Slopes at Conditional Values of Z
#=======================================================
#  At Z = cv1...
#  simple intercept = -0.0474(0.087), t=-0.5455, p=0.5859
#  simple slope     = -0.2155(0.0887), t=-2.4296, p=0.0158
#  At Z = cv2...
#  simple intercept = 0.017(0.0595), t=0.2851, p=0.7758
#  simple slope     = -0.0826(0.0602), t=-1.3725, p=0.171
#  At Z = cv3...
#  simple intercept = 0.0814(0.0842), t=0.9659, p=0.335
#  simple slope     = 0.0504(0.0833), t=0.605, p=0.5457

#### Subjective LE intensity predicting Baseline Relatedness Satisfaction - moderated by PF Total Score

model269 <- lm(ZB_BMPNS_R_tot ~ ZSubjIntensityAvg + ZB_PF1_Total + ZSubjIntensityAvg:ZB_PF1_Total, data=PFMerged)
summary(model269)
#Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                     0.02969    0.05864   0.506  0.61300    
#  ZSubjIntensityAvg              -0.18205    0.05932  -3.069  0.00237 ** 
#  ZB_PF1_Total                    0.27222    0.06204   4.388 1.65e-05 ***
#  ZSubjIntensityAvg:ZB_PF1_Total  0.13813    0.05787   2.387  0.01769 * #### SIG MODERATION
plot_model(model269, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_Total"))
vcov(model269)

#Simple Intercepts and Slopes at Conditional Values of Z
#=======================================================
#  At Z = cv1...
#  simple intercept = -0.2425(0.0873), t=-2.778, p=0.0059
#  simple slope     = -0.3202(0.0854), t=-3.7513, p=0.0002
#  At Z = cv2...
#  simple intercept = 0.0297(0.0586), t=0.5064, p=0.613
#  simple slope     = -0.182(0.0593), t=-3.0693, p=0.0024
#  At Z = cv3...
#  simple intercept = 0.3019(0.0834), t=3.6215, p=0.0004
#  simple slope     = -0.0439(0.0803), t=-0.5469, p=0.5849

#### Subjective LE intensity predicting Baseline Relatedness Satisfaction - moderated by PF Avoidance

model270 <- lm(ZB_BMPNS_R_tot ~ ZSubjIntensityAvg + ZB_PF1_Av5_mean_NR + ZSubjIntensityAvg:ZB_PF1_Av5_mean_NR, data=PFMerged)
summary(model270)
#Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                           0.02537    0.05884   0.431  0.66670    
#  ZSubjIntensityAvg                    -0.18936    0.05959  -3.178  0.00166 ** 
#  ZB_PF1_Av5_mean_NR                   -0.26691    0.06077  -4.392 1.62e-05 ***
#  ZSubjIntensityAvg:ZB_PF1_Av5_mean_NR -0.12191    0.06126  -1.990  0.04762 *  #### SIG MODERATION
plot_model(model270, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_Av_5_mean"))
vcov(model270)

#  Simple Intercepts and Slopes at Conditional Values of Z
#  =======================================================
#  At Z = cv1...
#  simple intercept = 0.2923(0.0826), t=3.5397, p=0.0005
#  simple slope     = -0.0674(0.0819), t=-0.824, p=0.4107
#  At Z = cv2...
#  simple intercept = 0.0254(0.0588), t=0.4312, p=0.6666
#  simple slope     = -0.1894(0.0596), t=-3.1781, p=0.0017
#  At Z = cv3...
#  simple intercept = -0.2415(0.0865), t=-2.7909, p=0.0056
#  simple slope     = -0.3113(0.0889), t=-3.5009, p=0.0005

#### Subjective LE intensity predicting FOLLOW-UP Relatedness Satisfaction - moderated by PPFI total score and each subscale
# NOTE: The results below are looking at change from baseline to 6-month FU (controlling for outcome at baseline). 
# We I didn't look at change, all of the interactions below were significant with larger effect sizes.

### Moderated by PF total
model279 <- lm(Zfu1_BMPNS_R_tot ~ ZB_BMPNS_R_tot + ZSubjIntensityAvg + ZB_PF1_Total + ZSubjIntensityAvg:ZB_PF1_Total, data=PFMerged)
summary(model279)
#Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                     0.002159   0.063230   0.034   0.9728    
#  ZB_BMPNS_R_tot                  0.442503   0.069681   6.350 1.63e-09 ***
#  ZSubjIntensityAvg              -0.110810   0.065557  -1.690   0.0927 .  
#  ZB_PF1_Total                   -0.038429   0.072832  -0.528   0.5984    
#  ZSubjIntensityAvg:ZB_PF1_Total  0.179916   0.074321   2.421   0.0165 *  #### SIG MODERATION
plot_model(model279, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_Total"))
vcov(model279)

### MOderated by PF Avoidance 
model280 <- lm(Zfu1_BMPNS_R_tot ~ ZB_BMPNS_R_tot + ZSubjIntensityAvg + ZB_PF1_Av5_mean_NR + ZSubjIntensityAvg:ZB_PF1_Av5_mean_NR, data=PFMerged)
summary(model280)
#Coefficients:
#                                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                          -0.005186   0.063619  -0.082   0.9351    
#  ZB_BMPNS_R_tot                        0.446448   0.069486   6.425  1.1e-09 ***
#  ZSubjIntensityAvg                    -0.108152   0.066148  -1.635   0.1038    
#  ZB_PF1_Av5_mean_NR                   -0.041007   0.069228  -0.592   0.5543    
#  ZSubjIntensityAvg:ZB_PF1_Av5_mean_NR -0.122963   0.070906  -1.734   0.0846 .  
plot_model(model280, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_Av5_mean_NR"))
vcov(model280)

### Moderated by PF Acceptance 
model281 <- lm(Zfu1_BMPNS_R_tot ~ ZB_BMPNS_R_tot + ZSubjIntensityAvg + ZB_PF1_Ac5_mean + ZSubjIntensityAvg:ZB_PF1_Ac5_mean, data=PFMerged)
summary(model281)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                        0.001038   0.064224   0.016    0.987    
#  ZB_BMPNS_R_tot                     0.465778   0.069998   6.654 3.17e-10 ***
#  ZSubjIntensityAvg                 -0.104823   0.066065  -1.587    0.114    
#  ZB_PF1_Ac5_mean                   -0.052236   0.070220  -0.744    0.458    
#  ZSubjIntensityAvg:ZB_PF1_Ac5_mean  0.124385   0.077087   1.614    0.108  
plot_model(model281, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_Ac5_mean"))
vcov(model281)

### Moderated by PF Harnessing
model282 <- lm(Zfu1_BMPNS_R_tot ~ ZB_BMPNS_R_tot + ZSubjIntensityAvg + ZB_PF1_H5_mean + ZSubjIntensityAvg:ZB_PF1_H5_mean, data=PFMerged)
summary(model282)
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                      -0.01615    0.06302  -0.256   0.7980    
#  ZB_BMPNS_R_tot                    0.46638    0.06591   7.076 3.01e-11 ***
#  ZSubjIntensityAvg                -0.09477    0.06541  -1.449   0.1491    
#  ZB_PF1_H5_mean                   -0.07712    0.06735  -1.145   0.2536    
#  ZSubjIntensityAvg:ZB_PF1_H5_mean  0.15414    0.07495   2.057   0.0411 * #### SIG MODERATION
plot_model(model282, type = "int", mdrt.values = "meansd", terms ("ZSubjIntensityAvg","ZB_PF1_H5_mean"))
vcov(model282)
