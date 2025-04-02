### Model 1: Testing the Bifactor model as specified in our exchanged figure

## data needed for model1:
##   PF - AV, AC, and H
##   BFI NE
##   BMIS PU
##   PHQ
##   AAQ
##   BMEAQ

## model:  general factor (g) causes PHQ, BFI NE, BMIS PU, AV, AC, H, AAQ, and BMEAQ
##         negative emotionality (NE) causes PHQ, BFI NE, and BMIS PU
##         psychological flexibility (PF) causes AV, AC, and H
##         NE correlates with AAQ and BMEAQ but does not with PF


mod1.dat <- read.csv("./Model1Dat.csv",header = T)
ids4sel <- mod1.dat[mod1.dat$id %in% dat$ID[dat$B_PF1_o_4 < 2],]

library(psych)
library(sem)
library(lavaan)

tryW <- mod1.dat[,c(47,37,39,17:19,48,49)]
tryunW <- mod1.dat[,c(47,37,39,12:14,48,49)]
names(tryW)
names(tryunW)
names(tryunW) <- c("PHQ","BFI-NE","BMIS-Pleasant","Avoidance","Acceptance","Harnessing","AAQ-II","BMEAQ")
unWnoH <- tryunW[,-6]

corW <- cor(tryW,use="pairwise.complete.obs")
corunW <- cor(tryunW,use="pairwise.complete.obs")
corunWnoH <- cor(unWnoH,use="pairwise.complete.obs")

round(corunW,2)

SampSize <- nrow(mod1.dat)

mod1.om.sem <- omegaSem(corW,n.obs=SampSize)
summary(mod1.om.sem)

mod1.om.sem2 <- omegaSem(corunW,n.obs=SampSize)
summary(mod1.om.sem2)

mod1.om.sem3 <- omegaSem(unWnoH,n.obs=SampSize)
summary(mod1.om.sem3)


### whoa!  let's step back

f1 <- fa(corW,2)
summary(f1)
fa.diagram(f1)
f1

f2 <- fa(corunW,2)
colnames(f2$loadings) <- c("Factor 1","Factor 2")
summary(f2)
f2
fa.diagram(f2)

#structure.graph(f2)



f2.b <- fa(tryunW,2,n.iter=1000)
summary(f2.b)
f2.b

### even further step back

f3 <- fa(cor(tryunW[,4:6]),1)
summary(f3)
fa.diagram(f3)
f3

### in response to reviewers to drop H from model

f4 <- fa(corunWnoH,2)
colnames(f4$loadings) <- c("Factor 1","Factor 2")
summary(f4)
f4
fa.diagram(f4)


## Study 1: Convergent validity and construct specificity: Global self-reports

## Convergent validity: 

## The PFI will be positively associated with the following personality traits:
## conscientiousness, open-mindedness, and (lower) negative emotionality
## (BFI-2-S), (lower) distress intolerance (DI), self-control (SCS), Grit-
## Perseverance Subscale, (Sample C - professionals), Mindfulness (MAAS; Sample
## C - professionals).

## The PFI will be positively associated with the following indices of
## well-being: subjective happiness (SHS), satisfaction with life (SWLS),
## psychological needs satisfaction (belonging, competence, and autonomy) (BMPN)

## The PFI will be negatively associated with the following indices of
## psychopathology: depression (PHQ-9), cognitive and somatic anxiety (STICSA),
## and social anxiety (SIAS)

## Construct specificity:

## In CFAs, the PFI scales will have lower factor loadings onto a negative
## affect factor (BFI-2-S Negative Emotionality, BMIS unpleasant emotions,
## STICSA trait cognitive anxiety) compared to the AAQ-II and BEAQ.


