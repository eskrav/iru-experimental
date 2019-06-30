# Power by simulation for a normally distributed continuous outcome with 
# subjects, items, and residual variability

# Population parameters
# mu: underlying mean of the outcome in the reference group
# betaN: effect size of predictor or interaction
# sdItem: sd of random effect at the item level
# sdSubject: sd of random effect at the subject level
# sdResid: sd of residual error

# Design parameters
# nSubjects: number of subjects in simulation
# nIterations: number of iterations in simulation

rm(list=ls())
setwd("~/Shared/informationally-redundant-utterances/code/")
library(Hmisc)
library(rms)
library(lme4)
library(lmerTest)
fnPower <-
  function(mu,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,
           beta10,beta11,sdItem,sdSubject,sdResid,nSubjects,nIterations,
           dots=TRUE){
    start.time <- Sys.time()
    if(dots) cat("Simulations (",nIterations,
           ") \n----|--- 1 ---|--- 2 ---|--- 3 ---|--- 4 ---| --- 5 \n",
           sep="")
# objects to store pvalue, beta, and standard error from each iteration 
# of simulation
    pVals <- betaVals <- seVals <- matrix(NA,nrow=nIterations,ncol=11)
# build design matrices
    m <- matrix(NA,nrow=nSubjects*4,ncol=7)
    colnames(m) <- c("worker","exp.alike","exp.diff","story","condition",
                     "context","slider")
    m[,1] <- rep(1:nSubjects,each=4)
    m[,2] <- rep(c(-0.5,0.5,0),each=4,length.out=length(m[,2]))
    m[,3] <- rep(c(-0.3333333,-0.3333333,0.6666667),each=4,
                 length.out=length(m[,3]))
    i <- 1
    while(i < (length(m[,4]))){ m[i:(i+3),4] <- sample(1:24,size=4,
                                                       replace=FALSE)
    i <- i+4 }
    m[,5] <- rep(c(-0.5, 0.5))
    m[,6] <- rep(c(-0.5, 0.5),each=2)
    # i <- 1 # (when testing for-loop)
    for(i in 1:nIterations){
      
  # draw random effects
      itemRE <- rnorm(24,0,sdItem)
      subjRE <- rnorm(nSubjects,0,sdSubject)
      residRE <- rnorm(nrow(m),0,sdResid)
      
  # create outcome
      y <- mu + beta1*m[,2] + beta2*m[,3] + beta3*m[,6] + beta4*m[,5] + 
        beta5*m[,2]*m[,6] + beta6*m[,3]*m[,6] + beta7*m[,2]*m[,5] + 
        beta8*m[,3]*m[,5] + beta9*m[,6]*m[,5] + beta10*m[,2]*m[,6]*m[,5] + 
        beta11*m[,3]*m[,6]*m[,5] + itemRE[m[,4]] + subjRE[m[,1]] + residRE
      m[,7] <- y
      dm <- as.data.frame(m)
      dm$worker <- as.factor(dm$worker)
      dm$story <- as.factor(dm$story)
  # fit model, store p-value, beta and standard error
      o <- lmer(slider ~ exp.alike + exp.diff + context + condition + 
                  exp.alike:context + exp.diff:context + 
                  exp.alike:condition + exp.diff:condition + 
                  context:condition + exp.alike:context:condition + 
                  exp.diff:context:condition + (1|story) + (1|worker),dm)
      pVals[i,] <- coef(summary(o))[2:12,5]
      betaVals[i,] <- coef(summary(o))[2:12,1]
      seVals[i,] <- coef(summary(o))[2:12,2]
      
      if(dots) cat(".",sep="")
      if(dots && i %% 50 == 0) cat(i,"\n")
      
    }
    
    if(dots) cat("\nSimulation Run Time:",round(difftime(Sys.time(),
                 start.time,units="hours"),3)," Hours \n")
    
# calculate power
    powerOut <- apply(pVals,2,function(x) length(x[x<0.05])/length(x))
    return(list(power=powerOut,p=pVals,beta=betaVals,se=seVals))
  }


# calibrate by setting betas = 0; histograms should all look level, 
# with about 5% of results significant by chance
outCalibrate <- fnPower(mu=61.0444,beta1=0,beta2=0,beta3=0,beta4=0,
                        beta5=0,beta6=0,beta7=0,beta8=0,beta9=0,beta10=0,
                        beta11=0,sdItem=10.138,sdSubject=9.285,
                        sdResid=21.839,nSubjects=1200,nIterations=10000,
                        dots=TRUE)
outCalibrate$power
hist(outCalibrate$p[,1],main="beta1",xlab="p value")
hist(outCalibrate$p[,2],main="beta2",xlab="p value")
hist(outCalibrate$p[,3],main="beta3",xlab="p value")
hist(outCalibrate$p[,4],main="beta4",xlab="p value")
hist(outCalibrate$p[,5],main="beta5",xlab="p value")
hist(outCalibrate$p[,6],main="beta6",xlab="p value")
hist(outCalibrate$p[,7],main="beta7",xlab="p value")
hist(outCalibrate$p[,8],main="beta8",xlab="p value")
hist(outCalibrate$p[,9],main="beta9",xlab="p value")
hist(outCalibrate$p[,10],main="beta10",xlab="p value")
hist(outCalibrate$p[,11],main="beta11",xlab="p value")

nSubjects <- seq(1200,2400,100)

# Random effects:
#   Groups   Name        Variance Std.Dev. Corr       
# workerid (Intercept)  86.20    9.285              
# cWorld      176.63   13.290   -0.76      
# cCondition  133.32   11.546    0.60  0.06
# story    (Intercept) 102.78   10.138              
# cWorld      319.53   17.875   -0.54      
# cCondition   35.91    5.993   -0.48  0.11
# Residual             476.93   21.839              
# Number of obs: 4800, groups:  workerid, 1200; story, 24
# 
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     61.2216     2.1107   23.4841  29.006  < 2e-16
# experiment1                      1.3018     1.0187 1242.2603   1.278   0.2015
# experiment2                      4.3355     0.8807 1235.7362   4.923 9.70e-07
# cWorld                          38.0383     3.7233   23.1500  10.216 4.76e-10
# cCondition                      -0.4559     1.4181   23.5152  -0.322   0.7507
# experiment1:cWorld              -0.8669     1.8205 1315.8596  -0.476   0.6340
# experiment2:cWorld               2.4416     1.5741 1309.6529   1.551   0.1211
# experiment1:cCondition           0.6141     1.7583 1314.0411   0.349   0.7270
# experiment2:cCondition           6.6776     1.5207 1307.4592   4.391 1.22e-05
# cWorld:cCondition              -12.6572     1.2699 2378.7656  -9.967  < 2e-16
# experiment1:cWorld:cCondition    0.4207     3.1134 2382.8498   0.135   0.8925
# experiment2:cWorld:cCondition    6.1601     2.6937 2374.7469   2.287   0.0223

st <- c()
for(i in 1:length(nSubjects)){
  st[[paste("subj",nSubjects[i])]] <-
    fnPower(mu=61.2216,beta1=1.3018,beta2=4.3355,beta3=38.0383,
            beta4=-0.4559,beta5=-0.8669,beta6=2.4416,beta7=0.6141,
            beta8=6.6776,beta9=-12.6572,beta10=0.4207,beta11=6.1601,
            sdItem=10.138,sdSubject=9.285,sdResid=21.839,
            nSubjects=nSubjects[i],nIterations=1000,dots=TRUE)
}

powerVals <- lapply(st,function(x) x[[1]])
plot(nSubjects,unlist(lapply(powerVals,function(x) x[[9]])),type='l',
     ylab='Power',xlab="Number of subjects",ylim=c(0,1),lty=1)
lines(nSubjects,unlist(lapply(powerVals,function(x) x[[10]])),lty=2)
lines(nSubjects,unlist(lapply(powerVals,function(x) x[[11]])),lty=3)
legend(x=min(nSubjects),y=1.05,legend=c("Context*Condition",
     "Ex. 1 vs. 2 C*C","Ex. 1+2 vs. 3 C*C"),lty=c(1,2,3),bty='n',cex=0.8)
