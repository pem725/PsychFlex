## Develop a scoring algorithm first before doing much of anything else

##library(foreign)
library(haven) ## WHOA!  New way to read in SPSS data - never foreign again!!!!
dat <- read_sav("./Merged.sav")
dat2 <- read_sav("./Merged_GMU_Baseline&FollowUp_102819.sav")

nm1 <- names(dat)
nm2 <- names(dat2)
match(nm1,nm2)
#datB <- read.spss("./At-home-survey.edited_JD_052819.sav",F,T)
#datBPF <- read.spss("./BLPF.sav",F,T)
#datFU <- read.spss("./PFFU.sav",F,T)
names(dat)
names(dat)[1001:1965]
#names(datB)
#names(datFU)

#GenValues <- datB[,c(78:97)]
#str(GenValues)

library(psych)

## PVQ scale - VALUES
valuesB <- dat[,941:960]
names(valuesB)
describe(valuesB)
valuesFU <- dat[,1609:1628]
names(valuesFU)
describe(valuesFU)

## Id varaible - used later
id <- dat$ID

## Psych Flex data for two goals at two times
PF.B.1 <- dat[,c(7:25)]
names(PF.B.1)
describe(PF.B.1)
alpha(PF.B.1, check.keys = T)
PF.B.1.b <- dat2[,names(PF.B.1)]
PF.B.1.b <- PF.B.1.b[,-19]
alpha(PF.B.1.b, check.keys = T)

fa(PF.B.1.b,3,n.iter = 1000)
iclust(PF.B.1.b,3,n.iterations = 1000)
omega(PF.B.1.b,nfactors = 3)


PF.B.2 <- dat[,c(31:49)]
names(PF.B.2)
describe(PF.B.2)
PF.FU.1 <- dat[,c(1385:1403)]
names(PF.FU.1)
describe(PF.FU.1)
PF.FU.2 <- dat[,c(1409:1427)]
names(PF.FU.2)
describe(PF.FU.2)





PF.B <- dat[,c(7:25,31:49)]
PF.FU1 <- dat[,c(1385:1403,1409:1427)]

WtPFScores <- function(id,Values,PF){
  ## score the core values from the PF
  id <- id
  Values <- cbind(Values,id)
  PF1 <- cbind(PF[,1:19],id)
  PF2 <- cbind(PF[,20:38],id)
  
  ScoreValues <- function(x){
    id <- x$id
    conf <- rowMeans(x[,c(1,11)])/7 # conformity
    trad <- rowMeans(x[,c(2,12)])/7 # tradition
    bene <- rowMeans(x[,c(3,13)])/7 # benevolence
    univ <- rowMeans(x[,c(4,14)])/7 # universalism
    self <- rowMeans(x[,c(5,15)])/7 # self-direction
    stim <- rowMeans(x[,c(6,16)])/7 # stimulation
    hedo <- rowMeans(x[,c(7,17)])/7 # hedonism
    ach <- rowMeans(x[,c(8,18)])/7 # achievement
    pow <- rowMeans(x[,c(9,19)])/7 # power
    sec <- rowMeans(x[,c(10,20)])/7 # security
    out <- data.frame(id,conf,trad,bene,univ,self,stim,hedo,ach,pow,sec)
    #out$openness <- rowMeans(out[,5:6])
    #out$selftrans <- rowMeans(out[,3:4])
    #out$conservative <- rowMeans(out[,1:2])
    #out$selfenhance <- rowMeans(out[,7:8])
    #out$max <- apply(out,1,max,na.rm=TRUE)
    #out2 <- out[,c("openness","selftrans","conservative","selfenhance","hedo","sec","max")]
    return(out)
  }
  
  
  PFscore <- function(x){
    av <- (rowMeans(x[,1:6])/6)*100
    ac <- (rowMeans(x[,7:12])/6)*100
    h <- (rowMeans(x[,13:18])/6)*100
    value <- x[,19]
    id <- x[,20]
    out <- data.frame(id,av,ac,h,value)
    return(out)
  }
  
  ## need to break up the PF into the two goals for later averaging
  tmp1 <- merge(ScoreValues(Values),PFscore(PF1),by="id")
  tmp2 <- merge(ScoreValues(Values),PFscore(PF2),by="id")
  ## Now, evaluate the tmp dataset for missing value variable and omit since we
  ## cannot score without it.
  tmp1 <- tmp1[!is.na(tmp1$value),]
  tmp2 <- tmp2[!is.na(tmp2$value),]

  WeightPF <- function(x){
    x$mult <- 0
    for (i in 1:nrow(x)){
      x$mult[i] <- x[i,x$value[i]+1] ## note: had to shift by one col due to id
    }
    x$av.wt <- x$av*x$mult
    x$ac.wt <- x$ac*x$mult
    x$h.wt <- x$h*x$mult
    x$av.wt[x$av.wt==0] <- NA
    x$ac.wt[x$ac.wt==0] <- NA
    x$h.wt[x$h.wt==0] <- NA
    return(x)
  }
  
  PFwt1 <- WeightPF(tmp1)
  PFwt2 <- WeightPF(tmp2)
  
  out <- merge(PFwt1,PFwt2,by="id")
  out$Xbar.av <- rowMeans(out[,c(12,30)],na.rm=T)
  out$Xbar.ac <- rowMeans(out[,c(13,31)],na.rm=T)
  out$Xbar.h <- rowMeans(out[,c(14,32)],na.rm=T)
  out$Xbar.av.wt <- rowMeans(out[,c(17,35)],na.rm=T)
  out$Xbar.ac.wt <- rowMeans(out[,c(18,36)],na.rm=T)
  out$Xbar.h.wt <- rowMeans(out[,c(19,37)],na.rm=T)
  out <- out[,c(1:19,30:43)]
  names(out) <- c("id","conf","trad","bene","univ","self","stim","hedo","ach","pow","sec","av1","ac1","h1","value1","mult1","av1.wt","ac1.wt","h1.wt","av2","ac2","h2","value2","mult2","av2.wt","ac2.wt","h2.wt","Xbar.av","Xbar.ac","Xbar.h","Xbar.av.wt","Xbar.ac.wt","Xbar.h.wt")
  return(out)
}

PF.Ball <- WtPFScores(id,valuesB,PF.B)

#round(cor(test[,c(12:14)],test[,c(30:32)],use="pairwise.complete.obs"),2)
#round(cor(test[,c(17:19)],test[,c(35:37)],use="pairwise.complete.obs"),2)


PFscored.B1 <- WtPFScores(id,valuesB,PF.B.1)
PFscored.B2 <- WtPFScores(id,valuesB,PF.B.2)
PFscored.FU1 <- WtPFScores(id,valuesFU,PF.FU.1)
PFscored.FU2 <- WtPFScores(id,valuesFU,PF.FU.2)
describe(PFscored.B1)
PFscored.B1$time <- "B1"
PFscored.B2$time <- "B2"
PFscored.FU1$time <- "FU1"
PFscored.FU2$time <- "FU2"
PFall <- rbind(PFscored.B1,PFscored.B2,PFscored.FU1,PFscored.FU2)
round(cor(PFall[,12:14],PFall[,17:19],use="pairwise.complete.obs"),2)

PFscored.B <- merge(PFscored.B1[,c(1,12:14,17:19)],PFscored.B2[,c(1,12:14,17:19)],by="id")
PFscored.FU <- merge(PFscored.FU1[,c(1,12:14,17:19)],PFscored.FU2[,c(1,12:14,17:19)],by="id")
names(PFscored.B)

tab.df <- data.frame(round(cor(PFall[,12:14],PFall[,17:19],use="pairwise.complete.obs"),2))
str(tab.df)
tab.df

## How to setup GitHub for yourself AFTER creating a project
## 1.  Setup project in Rstudio using Git
## 2.  Ensure usethis package is installed
## 3.  Make sure you have setup a PAT from GitHub and created the .Renviron file
## 4.  At the terminal, type:  usethis::use_github()
## 5.  Follow the propmpts and use HTTPS as the auth method
## 6.  Viola!  All done now.

## convergent validity information
AAQ <- dat[,c(1,1088:1094,1717:1723,1362)]

scoreAAQ <- function(x){
  id <- x[,1]
  AAQ <- (rowMeans(x[,2:8])/6)*100 ## pomp scores
  out <- data.frame(id,AAQ)
  return(out)
}

AAQ.B <- scoreAAQ(AAQ[,1:8])
names(AAQ.B) <- c("id","AAQ")
#AAQ.B$time <- "B"
#AAQ.F1 <- scoreAAQ(AAQ[,c(1,9:15)])
#AAQ.F1$time <- "FU"

BMEAQ <- dat[,c(1,1073:1087,1361)] # reverse score item 6
names(BMEAQ)

scoreBMEAQ <- function(x){
  id <- x[,1]
  BMEAQ <- (rowMeans(x[,2:16])/6)*100
  out <- data.frame(id,BMEAQ)
  return(out)
}
BMEAQ.B <- scoreBMEAQ(BMEAQ)
names(BMEAQ.B) <- c("id","BMEAQ")


BFI <- dat[,c(1,1265,1269,1273,1277,1281)] # already scored - use in SEM model
names(BFI) <- c("id","BFI.B.EX","BFI.B.AGREE","BFI.B.CONSC","BFI.B.NE","BFI.B.OM")

PHQ <- dat[,c(1,1138:1145)]

scorePHQ <- function(x){
  id <- x[,1]
  PHQ <- (rowMeans(x[2:9])/3)*100
  out <- data.frame(id,PHQ)
  return(out)
}

PHQ.B <- scorePHQ(PHQ)
names(PHQ.B) <- c("id","PHQ")


BMIS <- dat[,c(1,1334:1337,1889:1892)]
names(BMIS) <- c("id","BMIS.B.PU","BMIS.B.AC","BMIS.B.PR","BMIS.B.NT","BMIS.FU1.PU","BMIS.FU1.AC","BMIS.FU1.PR","BMIS.FU1.NT")

## data needed for model1:
##   PF - AV, AC, and H
##   BFI NE
##   BMIS PU
##   PHQ
##   AAQ
##   BMEAQ

Model1.dat <- merge(PF.Ball,BFI,by="id")
Model1.dat <- merge(Model1.dat,BMIS,by="id")
Model1.dat <- merge(Model1.dat,PHQ.B,by="id")
Model1.dat <- merge(Model1.dat,AAQ.B,by="id")
Model1.dat <- merge(Model1.dat,BMEAQ.B,by="id")
write.csv(Model1.dat,"Model1Dat.csv",row.names = F)


DI <- dat[,c(1,1095:1104)] 
GSHS <- dat[,c(1,1106:1111,1639:1644)] # two subscales - agency and pathways
SCS <- dat[,c(1,1146:1155)] # reverse score 1-3 and 7-10