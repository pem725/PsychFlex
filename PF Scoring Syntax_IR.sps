*******************************************BASELINE SCORING********************************************

****Baseline - Psychological Flexibility Index -> **FIRST GOAL**

*Reverse code PF avoidance scale

RECODE B_PF1_av_5_NR B_PF1_av_6_NR B_PF1_av_7_NR B_PF1_av_8_NR B_PF1_av_9_NR B_PF1_av_10_NR (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) 
INTO B_PF1_av5_R B_PF1_av6_R B_PF1_av7_R B_PF1_av8_R B_PF1_av9_R B_PF1_av10_R.
EXECUTE.

*Compute new PF subscales from new 15-item measure (5-item subscales)

*5-item Avoidance (excluding original item av6)

COMPUTE B_PF1_Av5_mean=MEAN(B_PF1_av5_R, B_PF1_av7_R, B_PF1_av8_R, B_PF1_av9_R, B_PF1_av10_R).
EXECUTE.

*5-item Acceptance (excluding original item ac11)

COMPUTE B__PF1_Ac5_mean=MEAN(B_PF1_ac_12, B_PF1_ac_13, B_PF1_ac_14, B_PF1_ac_15, B_PF1_ac_16).
EXECUTE.

*5-item Harnessing (excluding original item h17)

COMPUTE B_PF1_H5_mean=MEAN(B_PF1_h_18, B_PF1_h_19, B_PF1_h_20, B_PF1_h_21, B_PF1_h_22).
EXECUTE.

_____________________________________________________

****Baseline- Psychological Flexibility Index -> **SECOND GOAL**

*Reverse code PF avoidance scale

RECODE B_PF2_av_5_NR B_PF2_av_6_NR B_PF2_av_7_NR B_PF2_av_8_NR B_PF2_av_9_NR B_PF2_av_10_NR (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) 
INTO B_PF2_av5_R B_PF2_av6_R B_PF2_av7_R B_PF2_av8_R B_PF2_av9_R B_PF2_av10_R.
EXECUTE.

*Compute new PF subscales from new 15-item measure (5-item subscales)

*5-item Avoidance (excluding original item av6)

COMPUTE B_PF2_Av5_mean=MEAN(B_PF2_av5_R, B_PF2_av7_R, B_PF2_av8_R, B_PF2_av9_R, B_PF2_av10_R).
EXECUTE.

*5-item Acceptance (excluding original item ac11)

COMPUTE B_PF2_Ac5_mean=MEAN(B_PF2_ac_12, B_PF2_ac_13, B_PF2_ac_14, B_PF2_ac_15, B_PF2_ac_16).
EXECUTE.

*5-item Harnessing (excluding original item h17)

COMPUTE B_PF2_H5_mean=MEAN(B_PF2_h_18, B_PF2_h_19, B_PF2_h_20, B_PF2_h_21, B_PF2_h_22).
EXECUTE.


_____________________________________________________

**Emotion Prejudice scale






_____________________________________________________

***Multidimensional curiosity scale scoring

*5DC JE  mean 

COMPUTE B_5DC_JE_mean=MEAN(B_je_1, B_je_2, B_je_3, B_je_4, B_je_5).
EXECUTE.

*5DC DS mean 

COMPUTE B_5DC_DS_mean=MEAN(B_ds_6, B_ds_7, B_ds_8, B_ds_9, B_ds_10).
EXECUTE.

*5DC ST reverse code items

RECODE B_st_11_NR, B_st_12_NR, B_st_13_NR, B_st_14_NR, B_st_15_NR 
(0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) INTO B_st_11_R, B_st_12_R, B_st_13_R, B_st_14_R, B_st_15_R.
EXECUTE.

*5DC ST mean 

COMPUTE B_5DC_st_mean=MEAN(B_st_11_R, B_st_12_R, B_st_13_R, B_st_14_R, B_st_15_R).
EXECUTE.

*5DC SC  mean 

COMPUTE B_5DC_SC_mean=MEAN(B_sc_16, B_sc_17, B_sc_18, B_sc_19, B_sc_20).
EXECUTE.

*5DC TS  mean 

COMPUTE B_5DC_TS_mean=MEAN(B_ts_21, B_ts_22, B_ts_23, B_ts_24, B_ts_25).
EXECUTE.

_____________________________________________________

**Big Five Inventory - short, 30 items

******BFI30 reverse score items

RECODE BFI_1_NR BFI_3_NR BFI_7_NR BFI_8_NR BFI_10_NR BFI_14_NR BFI_17_NR BFI_19_NR BFI_20_NR BFI_21_NR BFI_24_NR BFI_26_NR
BFI_27_NR BFI_28_NR BFI_30_NR (0=4) (1=3) (2=2) (3=1) (4=0) 
INTO BFI_1_R BFI_3_R BFI_7_R BFI_8_R BFI_10_R BFI_14_R BFI_17_R BFI_19_R BFI_20_R BFI_21_R BFI_24_R BFI_26_R
BFI_27_R BFI_28_R BFI_30_R.
EXECUTE.

***Extraversion

COMPUTE BFI_EX_mean = MEAN(BFI_1_R,BFI_6,BFI_11,BFI_16,BFI_21_R,BFI_26_R).
EXECUTE.

 ***Sociability

COMPUTE BFI_SOC_mean = MEAN(BFI_1_R,BFI_16).
EXECUTE.

***Assertiveness

COMPUTE BFI_ASSERT_mean = MEAN(BFI_6,BFI_21_R).
EXECUTE.

***Energy level

COMPUTE BFI_EL_mean = MEAN(BFI_11,BFI_26_R).
EXECUTE.

***Agreeableness

COMPUTE BFI_AGREE_mean = MEAN(BFI_2,BFI_7_R,BFI_12,BFI_17_R,BFI_22,BFI_27_R).
EXECUTE.

***Compassion

COMPUTE BFI_COMP_mean = MEAN(BFI_2,BFI_17_R).
EXECUTE.

***Respectfulness

COMPUTE BFI_RESP_mean = MEAN(BFI_7_R,BFI_22).
EXECUTE.

***Trust

COMPUTE BFI_TRUST_mean = MEAN(BFI_12,BFI_27_R).
EXECUTE.

***Conscientiousness

COMPUTE BFI_CONSC_mean = MEAN(BFI_3_R,BFI_8_R,BFI_13,BFI_18,BFI_23,BFI_28_R).
EXECUTE.

***Organization

COMPUTE BFI_ORGAN_mean = MEAN(BFI_3_R,BFI_18).
EXECUTE.

***Productiveness

COMPUTE BFI_PROD_mean = MEAN(BFI_8_R,BFI_23).
EXECUTE.

***Responsibility

COMPUTE BFI_RSPON_mean = MEAN(BFI_13,BFI_28_R).
EXECUTE.

***Negative Emotionaility

COMPUTE BFI_NE_mean = MEAN(BFI_4,BFI_9,BFI_14_R,BFI_19_R,BFI_24_R,BFI_29).
EXECUTE.

***Anxiety

COMPUTE BFI_ANX_mean = MEAN(BFI_4,BFI_19_R).
EXECUTE.

***Depression

COMPUTE BFI_DEP_mean = MEAN(BFI_9,BFI_24_R).
EXECUTE.

***Emotional Votatality

COMPUTE BFI_EV_mean = MEAN(BFI_14_R,BFI_29).
EXECUTE.

***Open Mindedness

COMPUTE BFI_OM_mean = MEAN(BFI_5,BFI_10_R,BFI_15,BFI_20_R,BFI_25,BFI_30_R).
EXECUTE.

***Aesthetic Sensitivity

COMPUTE BFI_AS_mean = MEAN(BFI_5,BFI_20_R).
EXECUTE.

***Intellectual curiosity

COMPUTE BFI_IC_mean = MEAN(BFI_10_R,BFI_25).
EXECUTE.

***Creative Imagination

COMPUTE BFI_CI_mean = MEAN(BFI_15,BFI_30_R).
EXECUTE.
_____________________________________________________


***Baseline: Brief Mood Inspection scale

***Recode items for Pleasant-unpleasant scale

RECODE  BMIS_3_NR BMIS_4_NBFIR BMIS_7_NR BMIS_8_NR BMIS_9_NR BMIS_10_NR BMIS_12_NR BMIS_15_NR (0=3) (1=2) (2=1) (3=0) INTO BMIS_3_R BMIS_4_R BMIS_7_R BMIS_8_R BMIS_9_R BMIS_10_R BMIS_12_R BMIS_15_R.
EXECUTE.

***Pleasant-Unpleasant Scale mean total

COMPUTE B_BMIS_PU_mean = MEAN(B_BMIS_1, B_BMIS_2, B_BMIS_3_R, B_BMIS_4_R, B_BMIS_5, B_BMIS_6, B_BMIS_7_R, B_BMIS_8_R, B_BMIS_9_R, B_BMIS_10_R, B_BMIS_11, B_BMIS_12_R, B_BMIS_13_R, B_BMIS_14, B_BMIS_15_R, B_BMIS_16).
EXECUTE.

***Arousal-Calm Mood scale mean total

COMPUTE B_BMIS_AC_mean = MEAN(B_BMIS_1, B_BMIS_3_R, B_BMIS_4_R, B_BMIS_5, B_BMIS_7_R, B_BMIS_8_R, B_BMIS_11, B_BMIS_12_R, B_BMIS_13_R, B_BMIS_14, B_BMIS_15_R, B_BMIS_16).
EXECUTE.

***Positive Relaxed Mood

COMPUTE B_BMIS_PR_mean = MEAN(B_BMIS_1, B_BMIS_4_R, B_BMIS_5, B_BMIS_9_R, B_BMIS_11, B_BMIS_14, B_BMIS_16).
EXECUTE.

***Negative Tired mood

COMPUTE B_BMIS_NT_mean = MEAN(B_BMIS_3_R, B_BMIS_7_R, B_BMIS_8_R, B_BMIS_12_R, B_BMIS_15_R).
EXECUTE.
_____________________________________________________


****Baseline: Subjective Happiness Scale

*Reverse score item 4:

RECODE B_SHS_4_NR (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) INTO B_SHS_4_R.
VARIABLE LABELS B_SHS_4_R 'B_SHS_4_reverse scored'.
EXECUTE.

*Calculate mean total

COMPUTE B_SHS_mean = MEAN(B_SHS_1, B_SHS_2, B_SHS_3, B_SHS_4_R).
EXECUTE.


_____________________________________________________


***Baseline: Balanced Measure of Psychological Needs Scale

***BMPNS Relatedness Satisfaction sum

COMPUTE B_BMPNS_RS_mean=MEAN(B_BMPNS_rs_1, B_BMPNS_rs_7, B_BMPNS_rs_13).
EXECUTE.

***BMPN Relatedness Dissatisfaction sum

COMPUTE B_BMPNS_RD_mean=MEAN(B_BMPNS_rd_4, B_BMPNS_rd_10, B_BMPNS_rd_16).
EXECUTE.

***BMPN Aggregate Relateness difference

COMPUTE B_BMPNS_R_diff=(B_BMPNS_RS_mean-B_BMPNS_RD_mean).
EXECUTE.

***BMPN Competence Satisfaction sum

COMPUTE B_BMPNS_CS_mean=MEAN(B_BMPNS_cs_2, B_BMPNS_cs_8, B_BMPNS_cs_14).
EXECUTE.

***BMPN Competence Dissatisfaction sum

COMPUTE B_BMPNS_CD_mean=MEAN(B_BMPNS_cd_5, B_BMPNS_cd_11, B_BMPNS_cd_17).
EXECUTE.

***BMPN Aggregate Competence difference

COMPUTE B_BMPNS_C_diff=(B_BMPNS_CS_mean-B_BMPNS_CD_mean).
EXECUTE.

***BMPN Autonomy Satisfaction sum

COMPUTE B_BMPNS_AS_mean=MEAN(B_BMPNS_as_3, B_BMPNS_as_9, B_BMPNS_as_15).
EXECUTE.

***BMPN Autonomy Dissatisfaction sum

COMPUTE B_BMPNS_AD_mean=MEAN(B_BMPNS_ad_6, B_BMPNS_ad_12,B_BMPNS_ad_18).
EXECUTE.

***BMPN Aggregate Autonomy difference

COMPUTE B_BMPNS_A_diff=(B_BMPNS_AS_mean-B_BMPNS_AD_mean).
EXECUTE.

***Reverse scored dissatisfaction for mean totals of automony, relatedness, and competence

RECODE B_BMPNS_rd_4 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_rd_4_R.
RECODE B_BMPNS_cd_5 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_cd_5_R.
RECODE B_BMPNS_ad_6 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_ad_6_R.
RECODE B_BMPNS_rd_10 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_rd_10_R.
RECODE B_BMPNS_cd_11 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_cd_11_R.
RECODE B_BMPNS_ad_12 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_ad_12_R.
RECODE B_BMPNS_rd_16 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_rd_16_R.
RECODE B_BMPNS_cd_17 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_cd_17_R.
RECODE B_BMPNS_ad_18 (0=4) (1=3) (2=2) (3=1) (4=0) INTO B_BMPNS_ad_18_R.
EXECUTE.

***Calculate total autonomy, relatedness, and competence scores - containing satisfaction items and reverse-scored 
disatisfaction items

*Autonomy

COMPUTE B_BMPNS_A_tot=MEAN(B_BMPNS_as_3, B_BMPNS_as_9,
B_BMPNS_as_15, B_BMPNS_ad_6_R, B_BMPNS_ad_12_R, B_BMPNS_ad_18_R).
EXECUTE.

*Competence

COMPUTE B_BMPNS_C_tot=MEAN(B_BMPNS_cs_2, B_BMPNS_cs_8, B_BMPNS_cs_14, B_BMPNS_cd_5_R, 
B_BMPNS_cd_11_R, B_BMPNS_cd_17_R).
EXECUTE.

*Relatedness

COMPUTE B_BMPNS_R_tot=MEAN(B_BMPNS_rs_1, B_BMPNS_rs_7, B_BMPNS_rs_13, B_BMPNS_rd_4_R, 
B_BMPNS_rd_10_R, B_BMPNS_rd_16_R).
EXECUTE.

_____________________________________________________


***Baseline: Satisfaction With Life Scale total mean score

COMPUTE B_SWLS_mean=MEAN(B_SWLS_1, B_SWLS_2, B_SWLS_3, B_SWLS_4, B_SWLS_5).
EXECUTE.

_____________________________________________________


***Baseline Portrait Values Questionnaire 

*Conformity

COMPUTE B_PVQ_con_mean=MEAN(B_PVQ_con_1, B_PVQ_con_11).
VARIABLE LABELS B_PVQ_con_mean 'Baseline - Portrait Values Questionnaire - Conformity total score (mean)'.
EXECUTE.

*Tradition

COMPUTE B_PVQ_tra_mean=MEAN(B_PVQ_tra_2, B_PVQ_tra_12).
VARIABLE LABELS B_PVQ_tra_mean 'Baseline - Portrait Values Questionnaire - Tradition total score (mean)'.
EXECUTE.

*Benevolence

COMPUTE B_PVQ_ben_mean=MEAN(B_PVQ_ben_3 ,B_PVQ_ben_13).
VARIABLE LABELS B_PVQ_ben_mean 'Baseline - Portrait Values Questionnaire - Benevolence total score (mean)'.
EXECUTE.

*Universalism

COMPUTE B_PVQ_uni_mean=MEAN(B_PVQ_uni_4, B_PVQ_uni_14).
VARIABLE LABELS B_PVQ_uni_mean 'Baseline - Portrait Values Questionnaire - Universalism total score (mean)'.
EXECUTE.

*Self-direction

COMPUTE B_PVQ_sd_mean=MEAN(B_PVQ_sd_5, B_PVQ_sd_15).
VARIABLE LABELS B_PVQ_sd_mean 'Baseline - Portrait Values Questionnaire - Self-direction total score (mean)'.
EXECUTE.

*Stimulation

COMPUTE B_PVQ_stim_mean=MEAN(B_PVQ_stim_6, B_PVQ_stim_16).
VARIABLE LABELS B_PVQ_stim_mean  'Baseline - Portrait Values Questionnaire - Stimulation total score (mean)'.
EXECUTE.

*Hedonism

COMPUTE B_PVQ_hed_mean=MEAN(B_PVQ_hed_7, B_PVQ_hed_17).
VARIABLE LABELS B_PVQ_hed_mean  'Baseline - Portrait Values Questionnaire - Hedonism total score (mean)'.
EXECUTE.

*Achievement

COMPUTE B_PVQ_ach_mean=MEAN(B_PVQ_ach_8, B_PVQ_ach_18).
VARIABLE LABELS B_PVQ_ach_mean 'Baseline - Portrait Values Questionnaire - Achievement total score (mean)'.
EXECUTE.

*Power

COMPUTE B_PVQ_pow_mean=MEAN(B_PVQ_pow_9, B_PVQ_pow_19).
VARIABLE LABELS B_PVQ_pow_mean  'Baseline - Portrait Values Questionnaire - Power total score (mean)'.
EXECUTE.

*Security

COMPUTE B_PVQ_sec_mean=MEAN(B_PVQ_sec_10, B_PVQ_sec_20).
VARIABLE LABELS B_PVQ_sec_mean  'Baseline - Portrait Values Questionnaire - Security total score (mean)'.
EXECUTE.

_____________________________________________________


***Kaufman domains of creativity scale



_____________________________________________________

***Baseline: Brief Purpose Measure (4 items) total mean score


COMPUTE B_BPURP_mean = MEAN(B_BPURP_1, B_BPURP_2,B_BPURP_3, B_BPURP_4).
EXECUTE.


_____________________________________________________


****Baseline: Purpose In Life Extneral Aims total mean score

COMPUTE B_PILEA_mean = MEAN(B_PILEA_1, B_PILEA_2, B_PILEA_3, B_PILEA_4, B_PILEA_5, B_PILEA_6).
VARIABLE LABELS B_PILEA_mean 'Baseline - Purpose In Life Extneral Aims total score (mean)'.
EXECUTE.


_____________________________________________________


***Baseline: BMEAQ Brief Experiential avoidance measure
***BMEAQ reverse score item 6

RECODE B_BMEAQ_6_NR (1=6) (2=5) 
    (3=4) (4=3) (5=2) (6=1) INTO B_BMEAQ_6_R.
EXECUTE.

***BMEAQ total mean score

COMPUTE B_BMEAQ_mean=MEAN(B_BMEAQ_1, B_BMEAQ_2, B_BMEAQ_3, B_BMEAQ_4, B_BMEAQ_5, B_BMEAQ_6_R, B_BMEAQ_7, B_BMEAQ_8, B_BMEAQ_9,
B_BMEAQ_10, B_BMEAQ_11, B_BMEAQ_12, B_BMEAQ_13, B_BMEAQ_14, B_BMEAQ_15).
EXECUTE.


_____________________________________________________

***Baseline: Acceptance and Action Questionnaire total mean score

COMPUTE B_AAQII_mean=MEAN(B_AAQII_1, B_AAQII_2, B_AAQII_3, B_AAQII_4, B_AAQII_5, B_AAQII_6, B_AAQII_7). 
EXECUTE.

_____________________________________________________

***Baseline: Distress Intolerance total mean score
***higher scores indicate higher distress intolerance (lower distress tolerance)


COMPUTE B_DI_mean=MEAN(B_DI_1, B_DI_2, B_DI_3, B_DI_4, B_DI_5, B_DI_6, B_DI_7, B_DI_8, B_DI_9, B_DI_10).
EXECUTE.

_____________________________________________________


***Baseline: Goal-Specific Hope Scale 

***total score

COMPUTE B_GSHS_tot_mean=MEAN(B_GSHS_1, B_GSHS_2, B_GSHS_3, B_GSHS_4, B_GSHS_5, B_GSHS_6).
EXECUTE.

***pathways subscale

COMPUTE B_GSHS_path_mean=MEAN(B_GSHS_1, B_GSHS_3, B_GSHS_4).
EXECUTE.

***agency subscale

COMPUTE B_GSHS__agency_mean=MEAN(B_GSHS_2, B_GSHS_5, B_GSHS_6).
EXECUTE.
_____________________________________________________

****Baseline: Inventory for Cognitive and Somatic Anxiety (ICSA) - Trait Version

*Trait-Cognitive

*Trait-Somatic 

*Total Mean Score

COMPUTE B_ICSA_mean=MEAN(B_ICSA_1, B_ICSA_2, B_ICSA_3, B_ICSA_4, B_ICSA_5, B_ICSA_6, B_ICSA_7, B_ICSA_8, B_ICSA_9, B_ICSA_10, B_ICSA_11,
B_ICSA_12, B_ICSA_13, B_ICSA_14, B_ICSA_15, B_ICSA_16, B_ICSA_17, B_ICSA_18, B_ICSA_19, B_ICSA_20, B_ICSA_21).
EXECUTE. 
_____________________________________________________


****Baseline: Implicit Beliefs About Emotions Scale total mean score

***Reverse score 3,4.
***Mean score: higher scores indicate more incremental theories, lower scores indicate more entity theories

RECODE B_IBAES_3_NR B_IBAES_4_NR (1=5) (2=4) (3=3) (4=2) (5=1) INTO 
B_IBAES_3_R B_IBAES_4_R.
EXECUTE.

COMPUTE B_IBAES_mean = MEAN(B_IBAES_1, B_IBAES_2, B_IBAES_3_R, B_IBAES_4_R).
EXECUTE.
_____________________________________________________

***Patient Health questionnaire

COMPUTE B_PHQ_mean = MEAN(B_PHQ_1, B_PHQ_2, B_PHQ_3, B_PHQ_4, B_PHQ_5, B_PHQ_6, B_PHQ_7, B_PHQ_8).
EXECUTE.

_____________________________________________________

***UTRECHT Work engagement scale

**UTRECHT - changed this to reflect the short version measure UTRECHT-9

***UTRECHT mean

COMPUTE UWES_mean=MEAN(UWES_1,UWES_2,UWES_3,UWES_4,UWES_5,UWES_6,UWES_7,UWES_8,UWES_9).
EXECUTE.

***UTRECHT Vigor mean

COMPUTE UWES_V_mean=MEAN(UWES_1,UWES_2,UWES_5).
EXECUTE.

***UTRECHT Dedication mean

COMPUTE UWES_D_mean=MEAN(UWES_3,UWES_4,UWES_7).
EXECUTE.

***UTRECHT Absorption mean

COMPUTE UWES_A_mean=MEAN(UWES_6,UWES_8,UWES_9).
EXECUTE.
_____________________________________________________

***Work related acceptance and action questionnaire

COMPUTE B_WAAQ_mean = MEAN(B_WAAQ_1, B_WAAQ_2, B_WAAQ_3, B_WAAQ_4, B_WAAQ_5, B_WAAQ_6, B_WAAQ_7).
EXECUTE.
_____________________________________________________

***Baseline: Social Interaction Anxiety Scale (Sum and Mean scores)
***summative scoring. 34 or more = Social Phobia is probable, 43 or more = Social Anxiety is probable.

RECODE B_SIAS_5_NR, B_SIAS_9_NR, B_SIAS_11_NR (0=4) (1=3) (2=2) (3=1) (4=0) INTO 
B_SIAS_5R, B_SIAS_9R, B_SIAS_11R.
EXECUTE.

COMPUTE B_SIAS_sum = SUM(B_SIAS_1, B_SIAS_2, B_SIAS_3, B_SIAS_4, B_SIAS_5R, B_SIAS_6, B_SIAS_7, B_SIAS_8, B_SIAS_9R, B_SIAS_10, B_SIAS_11R, B_SIAS_12, B_SIAS_13, B_SIAS_14, B_SIAS_15, B_SIAS_16, B_SIAS_17, B_SIAS_18, B_SIAS_19, B_SIAS_20).
EXECUTE.

****Mean scoring

COMPUTE B_SIAS_mean = MEAN(B_SIAS_1, B_SIAS_2, B_SIAS_3, B_SIAS_4, B_SIAS_5R, B_SIAS_6, B_SIAS_7, B_SIAS_8, B_SIAS_9R, B_SIAS_10, B_SIAS_11R, B_SIAS_12, B_SIAS_13, B_SIAS_14, B_SIAS_15, B_SIAS_16, B_SIAS_17, B_SIAS_18, B_SIAS_19, B_SIAS_20).
EXECUTE.
_____________________________________________________


****Brief Portrait values questionnaire - consistency

COMPUTE PVQ_C_conform=MEAN(PVQ_C_1,PVQ_C_11).
COMPUTE PVQ_C_tradition=MEAN(PVQ_C_2,PVQ_C_12).
COMPUTE PVQ_C_benev=MEAN(PVQ_C_3,PVQ_C_13).
COMPUTE PVQ_C_univ=MEAN(PVQ_C_4,PVQ_C_14).
COMPUTE PVQ_C_selfd=MEAN(PVQ_C_5,PVQ_C_15).
COMPUTE PVQ_C_stim=MEAN(PVQ_C_6,PVQ_C_16).
COMPUTE PVQ_C_hed=MEAN(PVQ_C_7,PVQ_C_17).
COMPUTE PVQ_C_achv=MEAN(PVQ_C_8,PVQ_C_18).
COMPUTE PVQ_C_power=MEAN(PVQ_C_9,PVQ_C_19).
COMPUTE PVQ_C_sec=MEAN(PVQ_C_10,PVQ_C_20).
EXECUTE.

______________________________________________________

****Baseline - Self-Control Scale ****NOTE: Is the original scale on a 1-5 Likert scale? This one appears to be 0-3?

*Reverse scoring (items 1, 2, 3, 7, 8, 9, 10)

RECODE B_SCS_1_NR, B_SCS_2_NR, B_SCS_3_NR, B_SCS_7_NR, B_SCS_8_NR, B_SCS_9_NR, B_SCS_10_NR (0=3) (1=2) (2=1) (3=0) INTO 
B_SCS_1_R, B_SCS_2_R, B_SCS_3_R, B_SCS_7_R, B_SCS_8_R, B_SCS_9_R, B_SCS_10_R.
EXECUTE.

COMPUTE B_SCS_mean = MEAN(B_SCS_1_R, B_SCS_2_R, B_SCS_3_R, B_SCS_4, B_SCS_5, B_SCS_6, B_SCS_7_R, B_SCS_8_R, B_SCS_9_R, B_SCS_10_R).
EXECUTE. 

*******************************************6-MONTH FOLLOW-UP SCORING********************************************

***6 Month Follow Up - Brief Mood Inspection scale

***Recode items for Pleasant-unpleasant scale

RECODE fu1_BMIS_3_NR fu1_BMIS_4_NR fu1_BMIS_7_NR fu1_BMIS_8_NR fu1_BMIS_9_NR fu1_BMIS_10_NR fu1_BMIS_12_NR fu1_BMIS_13_NR fu1_BMIS_15_NR (0=3) (1=2) (2=1) (3=0) 
INTO fu1_BMIS_3_R fu1_BMIS_4_R fu1_BMIS_7_R fu1_BMIS_8_R fu1_BMIS_9_R fu1_BMIS_10_R fu1_BMIS_12_R fu1_BMIS_13_R fu1_BMIS_15_R.
EXECUTE.

***Pleasant-Unpleasant Scale

COMPUTE fu1_BMIS_PU_mean = MEAN(fu1_BMIS_1,fu1_BMIS_2,fu1_BMIS_3_R,fu1_BMIS_4_R,fu1_BMIS_5,fu1_BMIS_6,fu1_BMIS_7_R,fu1_BMIS_8_R,fu1_BMIS_9_R,fu1_BMIS_10_R,fu1_BMIS_11,fu1_BMIS_12_R,fu1_BMIS_13_R,fu1_BMIS_14,fu1_BMIS_15_R,fu1_BMIS_16).
EXECUTE.

***Arousal-Calm Mood scale

COMPUTE fu1_BMIS_AC_mean = MEAN( fu1_BMIS_1,fu1_BMIS_3_R,fu1_BMIS_4_R,fu1_BMIS_5,fu1_BMIS_7_R,fu1_BMIS_8_R,fu1_BMIS_11,fu1_BMIS_12_R,fu1_BMIS_13_R,fu1_BMIS_14,fu1_BMIS_15_R,fu1_BMIS_16).
EXECUTE.

***Positive Relaxed Mood

COMPUTE fu1_BMIS_PR_mean = MEAN(fu1_BMIS_1,fu1_BMIS_4_R,fu1_BMIS_5,fu1_BMIS_9_R,fu1_BMIS_11,fu1_BMIS_14,fu1_BMIS_16).
EXECUTE.

***Negative Tired mood

COMPUTE fu1_BMIS_NT_mean = MEAN(fu1_BMIS_3_R,fu1_BMIS_7_R,fu1_BMIS_8_R,fu1_BMIS_12_R,fu1_BMIS_15_R).
EXECUTE.
_____________________________________________________

****6 Month Follow Up - Subjective Happiness Scale

*Reverse score item 4:

RECODE fu1_SHS_4_NR (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) INTO fu1_SHS_4_R.
VARIABLE LABELS fu1_SHS_4_R 'fu1_SHS_4_reverse scored'.
EXECUTE.

*Calculate mean total

COMPUTE fu1_SHS_mean = MEAN(fu1_SHS_1, fu1_SHS_2, fu1_SHS_3, fu1_SHS_4_R).
EXECUTE.

_____________________________________________________

****6 Month Follow Up - Balanced Measure of Psychological Needs Scale


***BMPNS Relatedness Satisfaction mean

COMPUTE fu1_BMPNS_RS_mean=MEAN(fu1_BMPNS_rs_1,fu1_BMPNS_rs_7,fu1_BMPNS_rs_13).
VARIABLE LABELS fu1_BMPNS_RS_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Relatedness Satisfaction (mean)'.
EXECUTE.

***BMPN Relatedness Dissatisfaction mean

COMPUTE fu1_BMPNS_RD_mean=MEAN(fu1_BMPNS_rd_4,fu1_BMPNS_rd_10,fu1_BMPNS_rd_16).
VARIABLE LABELS fu1_BMPNS_RD_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale -Relatedness Dissatisfaction (mean)'.
EXECUTE.

***BMPN Aggregate Relateness difference

COMPUTE fu1_BMPNS_R_diff=(fu1_BMPNS_RS_mean-fu1_BMPNS_RD_mean).
VARIABLE LABELS fu1_BMPNS_R_diff '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Aggregate Relatedness Difference (mean)'.
EXECUTE.

***BMPN Competence Satisfaction mean

COMPUTE fu1_BMPNS_CS_mean=MEAN(fu1_BMPNS_cs_2,fu1_BMPNS_cs_8,fu1_BMPNS_cs_14).
VARIABLE LABELS fu1_BMPNS_CS_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Competemce Satisfaction (mean)'.
EXECUTE.

***BMPN Competence Dissatisfaction mean

COMPUTE fu1_BMPNS_CD_mean=MEAN(fu1_BMPNS_cd_5,fu1_BMPNS_cd_11,fu1_BMPNS_cd_17).
VARIABLE LABELS fu1_BMPNS_CD_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Competence Dissatisfaction (mean)'.
EXECUTE.

***BMPN Aggregate Competence difference

COMPUTE fu1_BMPNS_C_diff=(fu1_BMPNS_CS_mean-fu1_BMPNS_CD_mean).
VARIABLE LABELS fu1_BMPNS_C_diff '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Aggregate Competence Difference (mean)'.
EXECUTE.

***BMPN Autonomy Satisfaction mean

COMPUTE fu1_BMPNS_AS_mean=MEAN(fu1_BMPNS_as_3,fu1_BMPNS_as_9,fu1_BMPNS_as_15).
VARIABLE LABELS fu1_BMPNS_AS_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Autonomy Satisfaction (mean)'.
EXECUTE.

***BMPN Autonomy Dissatisfaction mean

COMPUTE fu1_BMPNS_AD_mean=MEAN(fu1_BMPNS_ad_6,fu1_BMPNS_ad_12,fu1_BMPNS_ad_18).
VARIABLE LABELS fu1_BMPNS_AD_mean '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Autonomy Dissatisfaction (mean)'.
EXECUTE.

***BMPN Aggregate Autonomy difference

COMPUTE fu1_BMPNS_A_diff=(fu1_BMPNS_AS_mean-fu1_BMPNS_AD_mean).
VARIABLE LABELS fu1_BMPNS_A_diff '6 Month Follow Up - Balanced Measure of Psychological Needs Scale - Aggregate Autonomy Difference'.
EXECUTE.

_____________________________________________________

***6-Month Follow Up - Satisfaction with life scale
***SWLS mean

COMPUTE fu1_SWLS_mean=MEAN(fu1_SWLS_1,fu1_SWLS_2,fu1_SWLS_3,fu1_SWLS_4,fu1_SWLS_5).
VARIABLE LABELS fu1_SWLS_mean '6 Month Follow Up - Satisfaction With Life Scale total score (mean)'.
EXECUTE.

_____________________________________________________

***6 Month Follow Up - Portrait Values Questionnaire 

*Conformity

COMPUTE fu1_PVQ_con_mean=MEAN(fu1_PVQ_con_1,fu1_PVQ_con_11).
VARIABLE LABELS fu1_PVQ_con_mean '6 Month Follow Up - Portrait Values Questionnaire - Conformity total score (mean)'.
EXECUTE.

*Tradition

COMPUTE fu1_PVQ_tra_mean=MEAN(fu1_PVQ_tra_2,fu1_PVQ_tra_12).
VARIABLE LABELS fu1_PVQ_tra_mean '6 Month Follow Up - Portrait Values Questionnaire - Tradition total score (mean)'.
EXECUTE.

*Benevolence

COMPUTE fu1_PVQ_ben_mean=MEAN(fu1_PVQ_ben_3,fu1_PVQ_ben_13).
VARIABLE LABELS fu1_PVQ_ben_mean '6 Month Follow Up - Portrait Values Questionnaire - Benevolence total score (mean)'.
EXECUTE.

*Universalism

COMPUTE fu1_PVQ_uni_mean=MEAN(fu1_PVQ_uni_4,fu1_PVQ_uni_14).
VARIABLE LABELS fu1_PVQ_uni_mean '6 Month Follow Up - Portrait Values Questionnaire - Universalism total score (mean)'.
EXECUTE.

*Self-direction

COMPUTE fu1_PVQ_sd_mean=MEAN(fu1_PVQ_sd_5,fu1_PVQ_sd_15).
VARIABLE LABELS fu1_PVQ_sd_mean '6 Month Follow Up - Portrait Values Questionnaire - Self-direction total score (mean)'.
EXECUTE.

*Stimulation

COMPUTE fu1_PVQ_stim_mean=MEAN(fu1_PVQ_stim_6,fu1_PVQ_stim_16).
VARIABLE LABELS fu1_PVQ_stim_mean  '6 Month Follow Up - Portrait Values Questionnaire - Stimulation total score (mean)'.
EXECUTE.

*Hedonism

COMPUTE fu1_PVQ_hed_mean=MEAN(fu1_PVQ_hed_7,fu1_PVQ_hed_17).
VARIABLE LABELS fu1_PVQ_hed_mean  '6 Month Follow Up - Portrait Values Questionnaire - Hedonism total score (mean)'.
EXECUTE.

*Achievement

COMPUTE fu1_PVQ_ach_mean=MEAN(fu1_PVQ_ach_8,fu1_PVQ_ach_18).
VARIABLE LABELS fu1_PVQ_ach_mean '6 Month Follow Up - Portrait Values Questionnaire - Achievement total score (mean)'.
EXECUTE.

*Power

COMPUTE fu1_PVQ_pow_mean=MEAN(fu1_PVQ_pow_9,fu1_PVQ_pow_19).
VARIABLE LABELS fu1_PVQ_pow_mean  '6 Month Follow Up - Portrait Values Questionnaire - Power total score (mean)'.
EXECUTE.

*Security

COMPUTE fu1_PVQ_sec_mean=MEAN(fu1_PVQ_sec_10,fu1_PVQ_sec_20).
VARIABLE LABELS fu1_PVQ_sec_mean  '6 Month Follow Up - Portrait Values Questionnaire - Security total score (mean)'.
EXECUTE.

_____________________________________________________

****6 Month Follow Up - Inventory for Cognitive and Somatic Anxiety (ICSA) - Trait Version

*Trait-Cognitive

*Trait-Somatic 

*Total Mean Score

COMPUTE fu1_ICSA_mean=MEAN(fu1_ICSA_1, fu1_ICSA_2, fu1_ICSA_3, fu1_ICSA_4, fu1_ICSA_5, fu1_ICSA_6, fu1_ICSA_7, fu1_ICSA_8, fu1_ICSA_9, fu1_ICSA_10, fu1_ICSA_11,
fu1_ICSA_12, fu1_ICSA_13, fu1_ICSA_14, fu1_ICSA_15, fu1_ICSA_16, fu1_ICSA_17, fu1_ICSA_18, fu1_ICSA_19, fu1_ICSA_20, fu1_ICSA_21).
VARIABLE LABELS fu1_ICSA_mean '6 Month Follow Up - Inventory for Cognitive and Somatic Anxiety total score (mean)'.
EXECUTE. 

_____________________________________________________

***6 Month Follow Up - Patient Health Questionnaire

COMPUTE fu1_PHQ_mean = MEAN(fu1_PHQ_1, fu1_PHQ_2, fu1_PHQ_3, fu1_PHQ_4, fu1_PHQ_5, fu1_PHQ_6, fu1_PHQ_7, fu1_PHQ_8).
VARIABLE LABELS fu1_PHQ_mean '6 Month Follow Up - Patient Health Questionnaire total score (mean)'.
EXECUTE.

_____________________________________________________

***6 Month Follow Up - Work-related Acceptance and Action Questionnaire

COMPUTE fu1_WAAQ_mean = MEAN(fu1_WAAQ_1, fu1_WAAQ_2, fu1_WAAQ_3, fu1_WAAQ_4, fu1_WAAQ_5, fu1_WAAQ_6, fu1_WAAQ_7).
VARIABLE LABELS  fu1_WAAQ_mean '6 Month Follow Up - Work-related Acceptance and Action Questionnaire total score (mean)'.
EXECUTE.

_____________________________________________________

***6 Month Follow Up - Utrecht Work Engagement Scale (9-item short version)

***UWES total mean score

COMPUTE fu1_UWES_mean=MEAN(fu1_UWES_1, fu1_UWES_2, fu1_UWES_3, fu1_UWES_4, fu1_UWES_5, fu1_UWES_6, fu1_UWES_7, fu1_UWES_8, fu1_UWES_9).
VARIABLE LABELS fu1_UWES_mean '6 Month Follow Up - Utrecht Work Engagement Scale total score (mean)'.
EXECUTE.

***UWES Vigor mean

COMPUTE fu1_UWES_V_mean=MEAN(fu1_UWES_1, fu1_UWES_2, fu1_UWES_5).
VARIABLE LABELS fu1_UWES_V_mean '6 Month Follow Up - Utrecht Work Engagement Scale - Vigor subscale (mean)'.
EXECUTE.

***UWES Dedication mean

COMPUTE fu1_UWES_D_mean=MEAN(fu1_UWES_3, fu1_UWES_4, fu1_UWES_7).
VARIABLE LABELS fu1_UWES_D_mean '6 Month Follow Up - Utrecht Work Engagement Scale - Dedication subscale (mean)'.
EXECUTE.

***UWES Absorption mean

COMPUTE fu1_UWES_A_mean=MEAN(fu1_UWES_6, fu1_UWES_8, fu1_UWES_9).
VARIABLE LABELS fu1_UWES_A_mean '6 Month Follow Up - Utrecht Work Engagement Scale - Absorption subscale (mean)'.
EXECUTE.

_____________________________________________________

***6 Month Follow Up - Brief Purpose Measure (4 items)

COMPUTE fu1_BPURP_mean = MEAN(fu1_BPURP_1, fu1_BPURP_2, fu1_BPURP_3, fu1_BPURP_4).
VARIABLE LABELS fu1_BPURP_mean '6 Month Follow Up - Brief Purpose Measure total score (mean)'.
EXECUTE.

_____________________________________________________

****6 Month Follow Up - Purpose In Life Extneral Aims total mean score

COMPUTE fu1_PILEA_mean = MEAN(fu1_PILEA_1, fu1_PILEA_2, fu1_PILEA_3, fu1_PILEA_4, fu1_PILEA_5, fu1_PILEA_6).
VARIABLE LABELS fu1_PILEA_mean '6 Month Follow Up - Purpose In Life Extneral Aims total score (mean)'.
EXECUTE.

_____________________________________________________


****6 Month Follow Up - Goal Specific Hope Scale

***goal specific hope total score

COMPUTE fu1_GSHS_mean=MEAN(fu1_GSHS_1, fu1_GSHS_2,fu1_GSHS_3, fu1_GSHS_4, fu1_GSHS_5, fu1_GSHS_6).
VARIABLE LABELS fu1_GSHS_mean '6 Month Follow Up - Goal-Specific Hope Scale total score (mean)'.
EXECUTE.

***goal specific hope pathways subscale

COMPUTE fu1_GSHS_path_mean=MEAN(fu1_GSHS_1, fu1_GSHS_3, fu1_GSHS_4).
VARIABLE LABELS fu1_GSHS_path_mean '6 Month Follow Up - Goal-Specific Hope Scale Pathways subscale (mean)'.
EXECUTE.

***goal specific hope agency subscale

COMPUTE fu1_GSHS_agen_mean=MEAN(fu1_GSHS_2, fu1_GSHS_5, fu1_GSHS_6).
VARIABLE LABELS fu1_GSHS_agen_mean '6 Month Follow Up - Goal-Specific Hope Scale Agency subscale (mean)'.
EXECUTE.
_____________________________________________________

****6 Month Follow Up - Psychological Flexibility Index -> **FIRST GOAL**

*Reverse code PF avoidance scale

RECODE fu1_PF1_av_5 fu1_PF1_av_6 fu1_PF1_av_7 fu1_PF1_av_8 fu1_PF1_av_9 fu1_PF1_av_10 (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) 
INTO fu1_PF1_av5_R fu1_PF1_av6_R fu1_PF1_av7_R fu1_PF1_av8_R fu1_PF1_av9_R fu1_PF1_av10_R.
VARIABLE LABELS  fu1_av5_R 'fu1_PF1-av5 (Reverse Coded)' /fu1_av6_R 'fu1_PF1-av6 (Reverse Coded)' /fu1_av7_R 
    'fu1_PF1-av7 (Reverse Coded)' /fu1_av8_R 'fu1_PF1-av8 (Reverse Coded)' /fu1_av9_R 'fu1_PF1-av9 (Reverse Coded)' /fu1_av10_R 
    'fu1_PF1-av10 (Reverse Coded)'.
EXECUTE.

*Compute new PF subscales from new 15-item measure (5-item subscales)

*5-item Avoidance (excluding original item av6)

COMPUTE fu1_PF1_Av5_mean=MEAN(fu1_PF1_av5_R, fu1_PF1_av7_R, fu1_PF1_av8_R, fu1_PF1_av9_R, fu1_PF1_av10_R).
VARIABLE LABELS fu1_PF1_Av5_mean '6 Month Follow Up - PF Avoidance 5-item subscale (mean)'.
EXECUTE.

*5-item Acceptance (excluding original item ac11)

COMPUTE fu1__PF1_Ac5_mean=MEAN(fu1_PF1_ac_12, fu1_PF1_ac_13, fu1_PF1_ac_14, fu1_PF1_ac_15, fu1_PF1_ac_16).
VARIABLE LABELS fu1_PF1_Ac5_mean '6 Month Follow Up - PF Acceptance 5-item subscale (mean)'.
EXECUTE.

*5-item Harnessing (excluding original item h17)

COMPUTE fu1_PF1_H5_mean=MEAN(fu1_PF1_h_18, fu1_PF1_h_19, fu1_PF1_h_20, fu1_PF1_h_21, fu1_PF1_h_22).
VARIABLE LABELS fu1_PF1_H5_mean '6 Month Follow Up - PF Harnessing 5-item subscale (mean)'.
EXECUTE.

_____________________________________________________

****6 Month Follow Up - Psychological Flexibility Index -> **SECOND GOAL**

*Reverse code PF avoidance scale

RECODE fu1_PF2_av_5 fu1_PF2_av_6 fu1_PF2_av_7 fu1_PF2_av_8 fu1_PF2_av_9 fu1_PF2_av_10 (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) 
INTO fu1_PF2_av5_R fu1_PF2_av6_R fu1_PF2_av7_R fu1_PF2_av8_R fu1_PF2_av9_R fu1_PF2_av10_R.
VARIABLE LABELS  fu1_PF2_av5_R 'fu1_PF2-av5 (Reverse Coded)' /fu1_PF2_av6_R 'fu1_PF2-av6 (Reverse Coded)' /fu1_PF2_av7_R 
    'fu1_PF2-av7 (Reverse Coded)' /fu1_PF2_av8_R 'fu1_PF2-av8 (Reverse Coded)' /fu1_PF2_av9_R 'fu1_PF2-av9 (Reverse Coded)' /fu1_PF2_av10_R 
    'fu1_PF2-av10 (Reverse Coded)'.
EXECUTE.

*Compute new PF subscales from new 15-item measure (5-item subscales)

*5-item Avoidance (excluding original item av6)

COMPUTE fu1_PF2_Av5_mean=MEAN(fu1_PF2_av5_R, fu1_PF2_av7_R, fu1_PF2_av8_R, fu1_PF2_av9_R, fu1_PF2_av10_R).
VARIABLE LABELS fu1_PF2_Av5_mean '6 Month Follow Up - PF Avoidance 5-item subscale - Second Goal (mean)'.
EXECUTE.

*5-item Acceptance (excluding original item ac11)

COMPUTE fu1__PF2_Ac5_mean=MEAN(fu1_PF2_ac_12, fu1_PF2_ac_13, fu1_PF2_ac_14, fu1_PF2_ac_15, fu1_PF2_ac_16).
VARIABLE LABELS fu1_PF2_Ac5_mean '6 Month Follow Up - PF Acceptance 5-item subscale - Second Goal (mean)'.
EXECUTE.

*5-item Harnessing (excluding original item h17)

COMPUTE fu1_PF2_H5_mean=MEAN(fu1_PF2_h_18, fu1_PF2_h_19, fu1_PF2_h_20, fu1_PF2_h_21, fu1_PF2_h_22).
VARIABLE LABELS fu1_PF2_H5_mean '6 Month Follow Up - PF Harnessing 5-item subscale - Second Goal (mean)'.
EXECUTE.

*******************************************INFORMANT SCORING********************************************

*Reverse code PF avoidance scale

RECODE INF_PF_av_5_nr, INF_PF_av_6_nr, INF_PF_av_7_nr, INF_PF_av_8_nr, INF_PF_av_9_nr, INF_PF_av_10_nr (0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) 
INTO INF_PF_av_5_r, INF_PF_av_6_r, INF_PF_av_7_r, INF_PF_av_8_r, INF_PF_av_9_r, INF_PF_av_10_r.
EXECUTE.

__________________________________________________


******Compute 5-item PF subscales 

*Avoidance (excludes av6)

COMPUTE INF_PF_Av5_mean = MEAN(INF_PF_av_5_r, INF_PF_av_7_r, INF_PF_av_8_r, INF_PF_av_9_r, INF_PF_av_10_r).
EXECUTE.

*Acceptance (excludes item 11)

COMPUTE INF_PF_Ac5_mean = MEAN(INF_PF_ac_12, INF_PF_ac_13, INF_PF_ac_14, INF_PF_ac_15,  INF_PF_ac_16).
EXECUTE.

*Harnessing (excludes item 17)

COMPUTE INF_PF_H5_mean = MEAN(INF_PF_h_18, INF_PF_h_19, INF_PF_h_20, INF_PF_h_21, INF_PF_h_22).
EXECUTE.

__________________________________________________


******Goal Specific Hope Scale

*goal specific hope total score

COMPUTE INF_GSHS_mean=MEAN(INF_GSHS_1, INF_GSHS_2, INF_GSHS_3, INF_GSHS_4, INF_GSHS_5, INF_GSHS_6).
EXECUTE.

*goal specific hope pathways subscale

COMPUTE INF_GSHS_path_mean=MEAN(INF_GSHS_1, INF_GSHS_3, INF_GSHS_4).
EXECUTE.

***goal specific hope agency subscale

COMPUTE INF_GSHS_agen_mean=MEAN(INF_GSHS_2, INF_GSHS_5, INF_GSHS_6).
EXECUTE.

__________________________________________________


******Subjective Happiness Scale

COMPUTE INF_SHS_mean = MEAN(INF_SHS_1, INF_SHS_2, INF_SHS_3, INF_SHS_4).
EXECUTE.

__________________________________________________


******Satisfaction with life scale

COMPUTE INF_SWLS_mean=MEAN(INF_SWLS_1,INF_SWLS_2,INF_SWLS_3,INF_SWLS_4,INF_SWLS_5).
EXECUTE.

__________________________________________________

***6 Month Follow Up - Brief Mood Inspection scale

***Recode items for Pleasant-unpleasant scale

RECODE INF_BMIS_3_nr, INF_BMIS_4_nr, INF_BMIS_7_nr, INF_BMIS_8_nr, INF_BMIS_9_nr, INF_BMIS_10_nr, INF_BMIS_12_nr, INF_BMIS_13_nr, INF_BMIS_15_nr (0=3) (1=2) (2=1) (3=0) 
INTO INF_BMIS_3_r, INF_BMIS_4_r, INF_BMIS_7_r, INF_BMIS_8_r, INF_BMIS_9_r, INF_BMIS_10_r, INF_BMIS_12_r, INF_BMIS_13_r, INF_BMIS_15_r.
EXECUTE.

***Pleasant-Unpleasant Scale

COMPUTE INF_BMIS_PU_mean = MEAN(INF_BMIS_1,INF_BMIS_2,INF_BMIS_3_r,INF_BMIS_4_r,INF_BMIS_5,INF_BMIS_6,INF_BMIS_7_r,INF_BMIS_8_r,INF_BMIS_9_r,INF_BMIS_10_r,INF_BMIS_11,INF_BMIS_12_r,INF_BMIS_13_r,INF_BMIS_14,INF_BMIS_15_r,INF_BMIS_16).
EXECUTE.

***Arousal-Calm Mood scale

COMPUTE INF_BMIS_AC_mean = MEAN(INF_BMIS_1,INF_BMIS_3_r,INF_BMIS_4_r,INF_BMIS_5,INF_BMIS_7_r,INF_BMIS_8_r,INF_BMIS_11,INF_BMIS_12_r,INF_BMIS_13_r,INF_BMIS_14,INF_BMIS_15_r,INF_BMIS_16).
EXECUTE.

***Positive Relaxed Mood

COMPUTE INF_BMIS_PR_mean = MEAN(INF_BMIS_1,INF_BMIS_4_R,INF_BMIS_5,INF_BMIS_9_R,INF_BMIS_11,INF_BMIS_14,INF_BMIS_16).
EXECUTE.

***Negative Tired mood

COMPUTE INF_BMIS_NT_mean = MEAN(INF_BMIS_3_r,INF_BMIS_7_r,INF_BMIS_8_r,INF_BMIS_12_r,INF_BMIS_15_r).
EXECUTE.
__________________________________________________

**Big Five Inventory - short, 30 items

******BFI30 reverse score items

RECODE INF_BFI_1_nr, INF_BFI_3_nr, INF_BFI_7_nr, INF_BFI_8_nr, INF_BFI_10_nr, INF_BFI_14_nr, INF_BFI_17_nr, INF_BFI_19_nr, INF_BFI_20_nr, INF_BFI_21_nr, INF_BFI_24_nr, INF_BFI_26_nr,
INF_BFI_27_nr, INF_BFI_28_nr, INF_BFI_30_nr (0=4) (1=3) (2=2) (3=1) (4=0) 
INTO BFI_1_R BFI_3_R BFI_7_R BFI_8_R BFI_10_R BFI_14_R BFI_17_R BFI_19_R BFI_20_R BFI_21_R BFI_24_R BFI_26_R
BFI_27_R BFI_28_R BFI_30_R.
EXECUTE.

***Extraversion

COMPUTE INF_BFI_E_mean = MEAN(INF_BFI_1_R, INF_BFI_6, INF_BFI_11, INF_BFI_16, INF_BFI_21_R, INF_BFI_26_R).
EXECUTE.

 ***Sociability

COMPUTE INF_BFI_soc_mean = MEAN(INF_BFI_1_R, INF_BFI_16).
EXECUTE.

***Assertiveness

COMPUTE INF_BFI_assert_mean = MEAN(INF_BFI_6, INF_BFI_21_R).
EXECUTE.

***Energy level

COMPUTE INF_BFI_el_mean = MEAN(INF_BFI_11, INF_BFI_26_R).
EXECUTE.

***Agreeableness

COMPUTE INF_BFI_A_mean = MEAN(INF_BFI_2, INF_BFI_7_R, INF_BFI_12, INF_BFI_17_R, INF_BFI_22, INF_BFI_27_R).
EXECUTE.

***Compassion

COMPUTE INF_BFI_comp_mean = MEAN(INF_BFI_2, INF_BFI_17_R).
EXECUTE.

***Respectfulness

COMPUTE INF_BFI_resp_mean = MEAN(INF_BFI_7_R, INF_BFI_22).
EXECUTE.

***Trust

COMPUTE INF_BFI_trust_mean = MEAN(INF_BFI_12, INF_BFI_27_R).
EXECUTE.

***Conscientiousness

COMPUTE INF_BFI_C_mean = MEAN(INF_BFI_3_R, INF_BFI_8_R, INF_BFI_13, INF_BFI_18, INF_BFI_23,INF_BFI_28_R).
EXECUTE.

***Organization

COMPUTE INF_BFI_organ_mean = MEAN(INF_BFI_3_R, INF_BFI_18).
EXECUTE.

***Productiveness

COMPUTE INF_BFI_prod_mean = MEAN(INF_BFI_8_R,INF_BFI_23).
EXECUTE.

***Responsibility

COMPUTE INF_BFI_respon_mean = MEAN(INF_BFI_13, INF_BFI_28_R).
EXECUTE.

***Negative Emotionaility

COMPUTE INF_BFI_N_mean = MEAN(INF_BFI_4, INF_BFI_9, INF_BFI_14_R, INF_BFI_19_R, INF_BFI_24_R, INF_BFI_29).
EXECUTE.

***Anxiety

COMPUTE INF_BFI_anx_mean = MEAN(INF_BFI_4, INF_BFI_19_R).
EXECUTE.

***Depression

COMPUTE INF_BFI_dep_mean = MEAN(INF_BFI_9, INF_BFI_24_R).
EXECUTE.

***Emotional Votatality

COMPUTE INF_BFI_ev_mean = MEAN(INF_BFI_14_R, INF_BFI_29).
EXECUTE.

***Open Mindedness

COMPUTE INF_BFI_O_mean = MEAN(INF_BFI_5, INF_BFI_10_R, INF_BFI_15, INF_BFI_20_R, INF_BFI_25, INF_BFI_30_R).
EXECUTE.

***Aesthetic Sensitivity

COMPUTE INF_BFI_as_mean = MEAN(INF_BFI_5, INF_BFI_20_R).
EXECUTE.

***Intellectual curiosity

COMPUTE INF_BFI_ic_mean = MEAN(INF_BFI_10_R, INF_BFI_25).
EXECUTE.

***Creative Imagination

COMPUTE INF_BFI_ci_mean = MEAN(INF_BFI_15, INF_BFI_30_R).
EXECUTE.

_____________________________________________________

***Multidimensional curiosity scale scoring

*5DC JE  mean 

COMPUTE INF_5DC_JE_mean=MEAN(INF_je_1, INF_je_2, INF_je_3, INF_je_4, INF_je_5).
EXECUTE.

*5DC DS mean 

COMPUTE INF_5DC_DS_mean=MEAN(INF_ds_6, INF_ds_7, INF_ds_8, INF_ds_9, INF_ds_10).
EXECUTE.

*5DC ST reverse code items

RECODE INF_st_11_nr, INF_st_12_nr, INF_st_13_nr, INF_st_14_nr, INF_st_15_nr 
(0=6) (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) INTO INF_st_11_r, INF_st_12_r, INF_st_13_r, INF_st_14_r, INF_st_15_r.
EXECUTE.

*5DC ST mean 

COMPUTE INF_5DC_ST_mean=MEAN(INF_st_11_r, INF_st_12_r, INF_st_13_r, INF_st_14_r, INF_st_15_r).
EXECUTE.

*5DC SC  mean 

COMPUTE INF_5DC_SC_mean=MEAN(INF_sc_16, INF_sc_17, INF_sc_18, INF_sc_19, INF_sc_20).
EXECUTE.

*5DC TS  mean 

COMPUTE INF_5DC_TS_mean=MEAN(INF_ts_21, INF_ts_22, INF_ts_23, INF_ts_24, INF_ts_25).
EXECUTE.
___________________________________________________________________________________________________

******Balanced Measure of Psychological Needs Scale

*BMPNS Relatedness Satisfaction mean

COMPUTE INF_BMPNS_RS_mean=MEAN(INF_BMPNS_rel_1_s, INF_BMPNS_rel_3_s, INF_BMPNS_rel_5_s).
EXECUTE.

***BMPN Relatedness Dissatisfaction mean

COMPUTE INF_BMPNS_RD_mean=MEAN(INF_BMPNS_rel_2_d, INF_BMPNS_rel_4_d, INF_BMPNS_rel_6_d).
EXECUTE.

***BMPN Aggregate Relateness difference

COMPUTE INF_BMPNS_R_diff=(INF_BMPNS_RS_mean-INF_BMPNS_RD_mean).
EXECUTE.

***BMPN Competence Satisfaction mean

COMPUTE INF_BMPNS_CS_mean=MEAN(INF_BMPNS_comp_7_s, INF_BMPNS_comp_9_s, INF_BMPNS_comp_11_s, INF_BMPNS_comp_13_s).
EXECUTE.

***BMPN Competence Dissatisfaction mean

COMPUTE INF_BMPNS_CD_mean=MEAN(INF_BMPNS_comp_8_d, INF_BMPNS_comp_10_d, INF_BMPNS_comp_12_d). 
EXECUTE.

***BMPN Aggregate Competence difference

COMPUTE INF_BMPNS_C_diff=(INF_BMPNS_CS_mean-INF_BMPNS_CD_mean).
EXECUTE.

***BMPN Autonomy Satisfaction mean

COMPUTE INF_BMPNS_AS_mean=MEAN(INF_BMPNS_auto_15_s, INF_BMPNS_auto_17_s). 
EXECUTE.

***BMPN Autonomy Dissatisfaction mean

COMPUTE INF_BMPNS_AD_mean=MEAN(INF_BMPNS_auto_14_d, INF_BMPNS_auto_16_d, INF_BMPNS_auto_18_d).
EXECUTE.

***BMPN Aggregate Autonomy difference

COMPUTE INF_BMPNS_A_diff=(INF_BMPNS_AS_mean-INF_BMPNS_AD_mean).
EXECUTE.

**************************Other in-lab and interview-based measures (life events, strivings, emotion regulation interview, MSCEIT, etc.

***Inter-item correlations for strivings assessment (for each striving)

*Striving 1

CORRELATIONS
  /VARIABLES=cen_1 org_1 aim_1 pur_1 eff_1 dif_1 suc_1 sup_1 joy_1 aut_1 gen_1
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Striving 2

CORRELATIONS
  /VARIABLES=cen_2 org_2 aim_2 pur_2 eff_2 dif_2 suc_2 sup_2 joy_2 aut_2 gen_2
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Striving 3

CORRELATIONS
  /VARIABLES=cen_3 org_3 aim_3 pur_3 eff_3 dif_3 suc_3 sup_3 joy_3 aut_3 gen_3
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Striving 4

CORRELATIONS
  /VARIABLES=cen_4 org_4 aim_4 pur_4 eff_4 dif_4 suc_4 sup_4 joy_4 aut_4 gen_4
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Striving 5

CORRELATIONS
  /VARIABLES=aim_5 pur_5 eff_5 dif_5 suc_5 sup_5 joy_5 aut_5 gen_5
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Striving 6

CORRELATIONS
  /VARIABLES=cen_6 org_6 aim_6 pur_6 eff_6 dif_6 suc_6 sup_6 joy_6 aut_6 gen_6
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

_________

*************************** Computing Baseline POMP Scores *******************************
*PF1 Avoidance.
COMPUTE B_PFI_Av5_mean_POMP=B_PF1_Av5_mean / 6 * 100.
EXECUTE.

*PF1 Acceptance.
COMPUTE B_PFI_Ac5_mean_POMP=B_PF1_Ac5_mean / 6 * 100.
EXECUTE.

*PF1 Harnessing.
COMPUTE B_PFI_H5_mean_POMP=B_PF1_H5_mean / 6 * 100.
EXECUTE.

*BFI-C.
COMPUTE B_BFI_CONSC_mean_POMP=B_BFI_CONSC_mean / 4 * 100.
EXECUTE. 

*BFI-N.
COMPUTE B_BFI_NE_mean_POMP=B_BFI_NE_mean / 4 * 100.
EXECUTE. 

*BFI-OM.
COMPUTE B_BFI_OM_mean_POMP=B_BFI_OM_mean / 4 * 100.
EXECUTE. 

*BFI-E.
COMPUTE B_BFI_EX_mean_POMP=B_BFI_EX_mean / 4 * 100.
EXECUTE. 

*BFI-A.
COMPUTE B_BFI_AGREE_mean_POMP=B_BFI_AGREE_mean / 4 * 100.
EXECUTE. 

*BMIS Pleasant-Unpleasant mood.
COMPUTE B_BMIS_PU_mean_POMP= B_BMIS_PU_mean / 3 * 100.
EXECUTE.

*Subjective Happiness Scale.
COMPUTE B_SHS_mean_POMP= B_SHS_mean / 6 * 100.
EXECUTE.

*Balanced Measure of Psychological Needs - Autonomy total score.
COMPUTE B_BMPNS_A_tot_POMP=B_BMPNS_A_tot / 4 * 100.
EXECUTE.

*Balanced Measure of Psychological Needs - Competence total score.
COMPUTE B_BMPNS_C_tot_POMP=B_BMPNS_C_tot / 4 * 100.
EXECUTE.

*Balanced Measure of Psychological Needs - Relatedness total score.
COMPUTE B_BMPNS_R_tot_POMP=B_BMPNS_R_tot / 4 * 100.
EXECUTE.

*Satisfaction With Life Scale.
COMPUTE B_SWLS_mean_POMP=B_SWLS_mean / 6 * 100.
EXECUTE.

*Brief Purpose Scale.
COMPUTE B_BPURP_mean_POMP=B_BPURP_mean / 4 * 100.
EXECUTE.

*Distress Intolerance.
COMPUTE B_DI_mean_POMP=B_DI_mean / 5 * 100.
EXECUTE.

*AAQ-II.
COMPUTE B_AAQII_mean_POMP=B_AAQII_mean / 6 * 100.
EXECUTE.

*BMEAQ.
COMPUTE B_BMEAQ_mean_POMP=B_BMEAQ_mean / 6 * 100.
EXECUTE.

*Inventory for Cognitivce and Somatic Anxiety.
COMPUTE B_ICSA_mean_POMP=B_ICSA_mean / 3 * 100.
EXECUTE.

