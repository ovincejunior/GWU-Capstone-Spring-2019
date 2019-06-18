train <- read.table("df_train.csv", header = TRUE, sep = ",")
test <- read.table("df_test.csv", header = TRUE, sep = ",")
val <- read.table("df_val.csv", header = TRUE, sep = ",")


str(train)

# P7.1.1 Specifying and estimating a two - level model

library(lme4)
library(tidyverse)
library(dplyr)
library(caTools)
library(caret)
#library(e1071)

fit <- glmer(sexphys ~ (1 | country), family = binomial("logit"), data = train)

summary(fit)

# P7.1.2 Interpretation of the null two - level model

fita <- glm(sexphys ~ 1, data = train, family = binomial("logit"))

logLik(fita)-logLik(fit)

anova(fit, fita)


u0 <- ranef(fit, postVar = TRUE) 

u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

commid <- as.numeric(rownames(u0[[1]]))

u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)

colnames(u0tab)[2] <- "u0"

u0tab <- u0tab[order(u0tab$u0), ]

u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))

u0tab <- u0tab[order(u0tab$commid), ]

colnames(u0tab)[4] <- "u0rank"

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for comm_id:_cons", ylim = c(-0.5, 0.5))

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")


#______________________________________________________________________________


fitall <- glmer(sexphys ~ Q515R + Q513R + mcv1006 + mcv1006a + mcv1006b + mcv1008 + 
                  men_alcohol_all + mcv1009 + Fam_support + Q516R + rQ701d + Island + 
                  rQ701b + rQ701a + rQ701c + sQ601c + sQ601d + tQ602d + justify + tQ602c + 
                  CONTROLNUM_1 + CONTROLNUM_2 + CONTROLNUM_3 + Q702R_2 + Q702R_3 + 
                  earlymarriage_0 + earlymarriage_1 + edpart_1 + edpart_2 + sumdiffage_1 + 
                  sumdiffage_2 + sumdiffage_3 + edresp_1 + edresp_2 + LAWDV1_0 + LAWDV1_2 + 
                  HDI1_0 + HDI1_1 + Governance1_0 + Governance1_1 + EP3_2 + EP3_3 + EP3_4 + 
                  FCMAR_0 + FCMAR_1 + SourceIncome_1 + SourceIncome_2 + SourceIncome_4 + 
                  SourceIncome_5 + ageyr10_2 + ageyr10_3 + ageyr10_4 + ageyr10_5 + 
                  (1 | country), family = binomial("logit"), data = train)

summary(fitall)

#______________________________________________________________________________

fitall2 <- glmer(sexphys ~ Q515R + Q513R + mcv1006 + mcv1006a + mcv1006b + mcv1008 + 
                  men_alcohol_all + mcv1009 + Fam_support + sQ601d +  
                  CONTROLNUM_1 + CONTROLNUM_2 + CONTROLNUM_3 + Q702R_2 + Q702R_3 + 
                  earlymarriage_0 + earlymarriage_1 + edpart_1 + sumdiffage_1 + 
                  EP3_2 + EP3_3 +  SourceIncome_2 + 
                   ageyr10_2 + ageyr10_3 + ageyr10_4 + ageyr10_5 + 
                  (1 | country), family = binomial("logit"), data = train)

summary(fitall2)

anova(fitall2, fit)

#______________________________________________________________________________

#predict(fitall2, newdata = , allow.new.levels = TRUE)
##Will try to select on R see link
#https://www.datanovia.com/en/lessons/select-data-frame-columns-in-r/

#______________________________________________________________________________

colsf <- c ('Q515R', 'Q513R', 'mcv1006', 'mcv1006a', 'mcv1006b', 'mcv1008',  
  'men_alcohol_all', 'mcv1009', 'Fam_support', 'sQ601d',  
  'CONTROLNUM_1', 'CONTROLNUM_2', 'CONTROLNUM_3', 'Q702R_2', 'Q702R_3', 
  'earlymarriage_0', 'earlymarriage_1', 'edpart_1', 'sumdiffage_1',  
  'EP3_2', 'EP3_3',  'SourceIncome_2',  
  'ageyr10_2', 'ageyr10_3', 'ageyr10_4', 'ageyr10_5', 'country')

#predict(fitall2, newdata = test[, colsf], allow.new.levels = TRUE)

p <- predict(fitall2, newdata = val[, colsf], allow.new.levels = TRUE, type='response')
summary(p)

p_class<- ifelse(p>0.5, 1, 0)
table(p_class)


table(p_class, val[, c('sexphys')])


#confusionMatrix(factor(p_class), factor(test[, c('sexphys')]))

#confusionMatrix(factor(p_class), factor(test[, c('sexphys')]), positive = '1')

confusionMatrix(factor(p_class), factor(val[, c('sexphys')]), positive = '1', 
                mode = 'everything')


colAUC(p, val[, c('sexphys')], plotROC=TRUE)


#______________________________________________________________________________

fitall3 <- glmer(sexphys ~ Q515R + Q513R + mcv1006 + mcv1006b + mcv1008 + 
                    Fam_support + sQ601d +  
                    CONTROLNUM_2 + CONTROLNUM_3 + Q702R_2 + Q702R_3 + 
                   earlymarriage_0 + earlymarriage_1  + sumdiffage_1 + 
                   EP3_2 + ageyr10_3 + ageyr10_5 + 
                   (1 | country), family = binomial("logit"), data = train)

summary(fitall3)

#______________________________________________________________________________
fitall4 <- glmer(sexphys ~ Q515R + Q513R + mcv1006 + mcv1006b + mcv1008 + 
                   Fam_support + sQ601d +  
                   CONTROLNUM_2 + CONTROLNUM_3 + Q702R_2 + Q702R_3 + 
                    earlymarriage_1  + sumdiffage_1 + 
                    + 
                   (1 | country), family = binomial("logit"), data = train)

summary(fitall4)

colsf2 <- c('Q515R', 'Q513R', 'mcv1006', 'mcv1006b', 'mcv1008', 
              'Fam_support', 'sQ601d', 
              'CONTROLNUM_2', 'CONTROLNUM_3', 'Q702R_2', 'Q702R_3',
              'earlymarriage_1', 'sumdiffage_1', 'country')

p1 <- predict(fitall4, newdata = val[, colsf2], allow.new.levels = TRUE, type='response')
summary(p1)

p_class1<- ifelse(p1>0.5, 1, 0)
table(p_class1)


table(p_class1, val[, c('sexphys')])


#confusionMatrix(factor(p_class), factor(test[, c('sexphys')]))

confusionMatrix(factor(p_class1), factor(val[, c('sexphys')]), positive = '1', 
                mode = 'everything')


colAUC(p1, val[, c('sexphys')], plotROC=TRUE)

#___________________________________________________________________

library("MCMCglmm")
library("plotMCMC")

cap_MG0 <- MCMCglmm(sexphys~Q515R,
                     random=~country,data=train,
                     family="categorical",
                     verbose=FALSE)

allChains22 <- as.mcmc(cbind(cap_MG0$Sol,cap_MG0$VCV))
plotTrace(allChains22,axes=TRUE,las=1)


cap_MG1 <- MCMCglmm(sexphys~Q515R,
                     random=~country,data=train,
                     family="categorical",verbose=FALSE,
                     nitt=5e5,burnin=5e4,thin=100)

allChains23 <- as.mcmc(cbind(cap_MG1$Sol,cap_MG1$VCV))
plotTrace(allChains23,axes=TRUE,las=1)

pMG1 <- predict(cap_MG1, newdata = test[, c('Q515R', 'country', 'sexphys')],  type='response')
summary(pMG1)

#__________________________________________________________________

#load("data/culcita.RData")
summary(culcita_dat)

with(culcita_dat,table(ttt,block))

cmod_MG0 <- MCMCglmm(predation~ttt,
                     random=~block,data=culcita_dat,
                     family="categorical",
                     verbose=FALSE)

cmod_MG1 <- MCMCglmm(predation~ttt,
                     random=~block,data=culcita_dat,
                     family="categorical",verbose=FALSE,
                     nitt=5e5,burnin=5e4,thin=100)




#____
prior.c <- list(R=list(V=1,fix=1),
                G=list(G1=list(V=1, nu=1, alpha.mu=0, alpha.V=1000)))

cmod_MG <- MCMCglmm(predation~ttt,
                    random=~block,data=culcita_dat,
                    slice=TRUE, ## slice sampling: for binary trials
                    ## with independent residuals
                    pl=TRUE,    ## save posterior distributions of latent
                    ## variables (= conditional modes)
                    prior=prior.c,
                    family="categorical",verbose=FALSE,
                    nitt=13000*10,burnin=3000*10,thin=5*10)

(cmod_MGsum <- summary(cmod_MG))

#_________
allChains <- as.mcmc(cbind(cmod_MG0$Sol,cmod_MG0$VCV))
plotTrace(allChains)

allChains2 <- as.mcmc(cbind(cmod_MG1$Sol,cmod_MG1$VCV))
plotTrace(allChains2,axes=TRUE,las=1)


allChains3 <- as.mcmc(cbind(cmod_MG$Sol,cmod_MG$VCV))
## units variance is fixed to 1 anyway, we don't need
## to examine it
allChains3 <- allChains3[,colnames(allChains3)!="units"]
plotTrace(allChains3,axes=TRUE,las=1)

vcChain <- log10(cmod_MG$VCV)[,1]
plotTrace(vcChain)


