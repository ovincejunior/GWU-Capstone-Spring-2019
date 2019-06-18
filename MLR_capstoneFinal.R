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




