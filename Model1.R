### Model 1: Testing the Bifactork model as specified in our exchanged figure

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

library(psych)
library(sem)
library(lavaan)

try1 <- mod1.dat[,c(47,37,39,17:19,48,49)]
try2 <- mod1.dat[,c(47,37,39,12:14,48,49)]

cor1 <- cor(try1,use="pairwise.complete.obs")
cor2 <- cor(try2,use="pairwise.complete.obs")
SampSize <- nrow(mod1.dat)

mod1.om.sem <- omegaSem(cor1,n.obs=SampSize)
summary(mod1.om.sem)

mod1.om.sem2 <- omegaSem(cor2,n.obs=SampSize)
summary(mod1.om.sem2)

### whoa!  let's step back

f1 <- fa(cor1,2)
summary(f1)
f1

f2 <- fa(cor2,2)
summary(f2)
f2
fa.diagram(f2)
structure.graph(f2)
