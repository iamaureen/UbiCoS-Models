library(dplyr)
library(glmnet)
# library(caret)
# library(pROC)

ubicos <- read.csv("~/Documents/PythonCode/Data/UbiCoS/ubicos_model_data_v2.csv", stringsAsFactors=FALSE)
ubicos$ff <- as.factor(ifelse(ubicos$Fam==1, "Fam", "noFam"))
ubicos$pp <- as.factor(ifelse(ubicos$Public==1, "Private", "Public"))

ubicos$hh <- as.factor(ifelse(ubicos$HSC==1, "High", "Low"))
ubicos$ss <- as.factor(ifelse(ubicos$Sych==1, "Sync", "Async"))

ubicos$mm <- as.factor(ifelse(ubicos$MSC==1, "High", "Low"))
ubicos$platform <- as.factor(ubicos$platform)

ubicos$CP <- as.factor(ifelse(ubicos$constructive.participation==0, "no", "yes"))
ubicos$CP <- relevel(ubicos$CP, ref="yes")

# main effects as well as interaction effects
F1model_full <- glm(F1 ~ 0 + hh*ss, data=ubicos)
summary(F1model_full)


# main effects as well as interaction effects
F2model <- glm(F2 ~ 0 + ff*pp, data=ubicos)
summary(F2model)

# main effects as well as interaction effects
F3model <- glm(F3 ~ 0 + mm*platform, data=ubicos)
summary(F3model)

## F1 full model: interaction + main effects
f1model_full <- glm(CP ~ hh*ss, family=binomial, data=ubicos)
summary(f1model_full)

# F2 full model: interaction + main effects
f2model_full <- glm(CP ~ ff*pp, family=binomial, data=ubicos)
summary(f2model_full)

# F3 full model: interaction + main effects
f3model_full <- glm(CP ~ mm*platform, family=binomial, data=ubicos)
summary(f3model_full)

# F1 + F2 + F3 full model
f123model_full_1 <- glm(CP ~ F1 + F2 + F3, family=binomial, data=ubicos)
summary(f123model_full_1)

# F1 + F2 + F3 full model
f123model_full_2 <- glm(CP ~ 0 + hh*ss + ff*pp + mm*platform, family=binomial, data=ubicos)
summary(f123model_full_2)


### ignore below for now

# unscaledfit <- glm(CP ~ F1 + F2 + F3, data=reducedat, family="binomial")
# simplefit <- glm(CP ~ scale(F1) + scale(F2) + scale(F3), data=reducedat, family="binomial")
# classpred <- (predict(simplefit, type="response") > 0.5) + 0
# table(data.frame(pred=classpred, actual=reducedat$CP))
# 
# 
# logitmodel <- glm(CP ~ F1 + F2 + F3, 
#                            data = ubicos, 
#                            family="binomial")
# 
# summary(logitmodel)
# ubicos$CP
# 
# 
# # probs <- predict(logitmodel, ubicos, type="prob")
# # predlogistic <- predict(logitmodel, reducedat[,predictors], type="prob")
# # auclogistic <- roc(ubicos$CP, predlogistic[,1])
# # plot(auclogistic, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auclogistic$auc[[1]],2)))
# # 
# # cutoff <- 0.75
# # confusionMatrix(as.factor((classpred > 0.5) + 0 ), reducedat$CP)