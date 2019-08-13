## Develop a scoring algorithm first before doing much of anything else

##library(foreign)
library(haven) ## WHOA!  New way to read in SPSS data - never foreign again!!!!
dat <- read_sav("./Merged.sav")
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
PF.B.2 <- dat[,c(31:49)]
names(PF.B.2)
describe(PF.B.2)
PF.FU.1 <- dat[,c(1385:1403)]
names(PF.FU.1)
describe(PF.FU.1)
PF.FU.2 <- dat[,c(1409:1427)]
names(PF.FU.2)
describe(PF.FU.2)


WtPFScores <- function(id,Values,PF){
  ## score the core values from the PF
  id <- id
  Values <- cbind(Values,id)
  PF <- cbind(PF,id)
  
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
  
  tmp <- merge(ScoreValues(Values),PFscore(PF),by="id")
  
  ## Now, evaluate the tmp dataset for missing value variable and omit since we
  ## cannot score without it.
  tmp <- tmp[!is.na(tmp$value),]
  
  tmp$mult <- 0

  for (i in 1:nrow(tmp)){
      tmp$mult[i] <- tmp[i,tmp$value[i]+1] ## note: had to shift by one col due to id
  }
  
  tmp$av.wt <- tmp$av*tmp$mult
  tmp$ac.wt <- tmp$ac*tmp$mult
  tmp$h.wt <- tmp$h*tmp$mult
  tmp$av.wt[tmp$av.wt==0] <- NA
  tmp$ac.wt[tmp$ac.wt==0] <- NA
  tmp$h.wt[tmp$h.wt==0] <- NA
  return(tmp)
}


PFscored.B1 <- WtPFScores(id,valuesB,PF.B.1)
PFscored.B2 <- WtPFScores(id,valuesB,PF.B.2)
PFscored.FU1 <- WtPFScores(id,valuesFU,PF.FU.1)
PFscored.FU2 <- WtPFScores(id,valuesFU,PF.FU.2)
describe(PFscored.B1)
