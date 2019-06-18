
train <- read.table("df_train1.csv", header = TRUE, sep = ",")
test <- read.table("df_test1.csv", header = TRUE, sep = ",")
val <- read.table("df_val1.csv", header = TRUE, sep = ",")


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


fitall <- glmer(sexphys ~ . + 
                  (1 | country), family = binomial("logit"), data = train)

summary(fitall)

#______________________________________________________________________________
#______________________________________________________________________________

fitall2 <- glmer(sexphys ~ emotvio + econviol + Q515R + Q513R +
                   mcv1006 + mcv1008 + 
                   Fam_support + sQ601d +  
                    CONTROLNUM_2 + CONTROLNUM_3 + Q702R_3 + 
                   earlymarriage_0 + earlymarriage_1 + edpart_1 + sumdiffage_1 + 
                   EP3_2 +  SourceIncome_2 + SourceIncome_5 +
                   ageyr10_2 + ageyr10_3 + ageyr10_4  + 
                   (1 | country), family = binomial("logit"), data = train)

summary(fitall2)
#____________________#
fitall2 <- glmer(sexphys ~ emotvio + econviol + Q515R + Q513R +
                   mcv1006 + mcv1008 + 
                   Fam_support + sQ601d +  
                   CONTROLNUM_2 + CONTROLNUM_3 + Q702R_3 + 
                   earlymarriage_0 + earlymarriage_1 + edpart_1 + sumdiffage_1 + 
                   EP3_2  + 
                   ageyr10_2 + ageyr10_3  + 
                   (1 | country), family = binomial("logit"), data = train)

summary(fitall2)

#______________________________________________________________________________

colsf <- c ('emotvio', 'econviol', 'Q515R', 'Q513R', 'mcv1006', 'mcv1008',  
             'Fam_support', 'sQ601d',  
             'CONTROLNUM_2', 'CONTROLNUM_3', 'Q702R_3', 
            'earlymarriage_0', 'earlymarriage_1', 'edpart_1', 'sumdiffage_1',  
            'EP3_2',   
            'ageyr10_2', 'ageyr10_3',  'country')

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

