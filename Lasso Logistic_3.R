# Building a logistic regression model
# Use variable reduction techniques like Lasso to deal with 
# high dimensionality and collinearity problems
# Be sure to upsample on training set
library(caret)
library(tidyverse)
library(glmnet)
library(ipflasso) # for repeated cv.glmnet
setwd("~/Desktop/Capstone Project/Logistic Lasso Models")
df <- read.csv("df_sca_scaled.csv")
df$sca <- as.factor(df$sca)
df$auop_avg <- as.factor(df$auop_avg)
df$restate_bin <- as.factor(df$restate_bin)
df$lt_cat <- as.factor(df$lt_cat)
# remove Kellogg


#----------------------------------
# Based on t-test results,  we select these Baneish ratios...
# Removing redundant features

df <- select(df, -c(DSRI_AVG,
                    GMI_AVG,
                    AQI_AVG,
                    SGI_AVG,
                    DEPI_MAX,
                    SGAI_AVG,
                    LVGI_AVG))

#----------------------------------
# The non-ratio financial metrics have massive collinearity
# As they are all metrics for company size
# select those metrics best correlated with target
# add interactions for all debt related features to control for company size (assets)

df$revt_lct_mean <- df$revt_mean*df$lct_mean
df$revt_dvt_mean <- df$revt_mean*df$dvt_mean
df$revt_intan_mean <- df$revt_mean*df$intan_mean

df <- select(df, -c(at_log_mean, 
                    act_mean, 
                    lct_mean, 
                    ch_mean, 
                    teq_mean,
                    capx_mean,
                    dvt_mean,
                    intan_mean))

df <- df %>%  # move categoricals
  relocate(sca, .after = revt_intan_mean)

df.k <- df
df <- filter(df, tic!="K")

#-----------------------------------
# Penalized Logistic Regression
# Create Outer Folds - 4

set.seed(1234)
folds <- createFolds(df$sca, k = 4)
fold1 <- folds[[1]]
fold2 <- folds[[2]]
fold3 <- folds[[3]]
fold4 <- folds[[4]]

#-----------------------------------
# Build Model for Fold 1
# Data split and upsample

df_fold1_train <- df[-fold1,]
df_fold1_test <- df[fold1,]
                        
# upsample training data
df_fold1_trainUP <- upSample(x = df_fold1_train [, c(4:53)],
                     y = df_fold1_train [, 54],
                     list = FALSE,
                     yname = "sca")

# Dummify X in matrix form / upsampled training data
x <- model.matrix(sca~., df_fold1_trainUP)[,-1]
y <- df_fold1_trainUP$sca

#------------------------------------------
# Optimize model and find best lambda value
# Find best lambda value with cv.glmnet

cv.lasso.fold1 <- cv.glmnet(x, y, 
                      alpha = 1, 
                      family = "binomial",
                      type.measure="auc",
                      nfolds = 5)

plot(cv.lasso.fold1)
cv.lasso.fold1

#------------------------------------------
# Build Model - Fold 1

model_fold1 <- glmnet(x, y, alpha = 1, family = "binomial", 
                      lambda = cv.lasso.fold1$lambda.1se) 

# Evaluate Performance on Outer Fold
x.test <- model.matrix(sca ~., df_fold1_test[,c(4:54)])[,-1]
y.test <- df_fold1_test$sca
probabilities <- model_fold1 %>% predict(newx = x.test, type = "response")
predicted.classes <- as.factor(ifelse(probabilities > 0.5, 1, 0))
confusionMatrix(predicted.classes,
                reference = y.test,
                positive = "1")

df_fold1_test <- cbind(df_fold1_test, probabilities, predicted.classes)
df_fold1_test <- rename(df_fold1_test, predicted.probs=s0)
assess.glmnet(model_fold1,
              newx = x.test,
              newy = y.test)

#------------------------------------------
# Build Model - Fold 2
# Data split and upsample

df_fold2_train <- df[-fold2,]
df_fold2_test <- df[fold2,]

# upsample training data
df_fold2_trainUP <- upSample(x = df_fold2_train [, c(4:53)],
                             y = df_fold2_train [, 54],
                             list = FALSE,
                             yname = "sca")

# Dummify X in matrix form / upsampled training data
x <- model.matrix(sca~., df_fold2_trainUP)[,-1]
y <- df_fold2_trainUP$sca

#------------------------------------------
# Optimize model and find best lambda value
# Find best lambda value with cv.glmnet

cv.lasso.fold2 <- cv.glmnet(x, y, 
                            alpha = 1, 
                            family = "binomial",
                            type.measure="auc",
                            nfolds = 5)

plot(cv.lasso.fold2)
cv.lasso.fold2

#------------------------------------------
# Build Final Model - Fold 2

model_fold2 <- glmnet(x, y, alpha = 1, family = "binomial", 
                      lambda = cv.lasso.fold2$lambda.1se) 

# Evaluate Performance on Outer Fold
x.test <- model.matrix(sca ~., df_fold2_test[,c(4:54)])[,-1]
y.test <- df_fold2_test$sca
probabilities <- model_fold2 %>% predict(newx = x.test, type = "response")
predicted.classes <- as.factor(ifelse(probabilities > 0.5, 1, 0))
confusionMatrix(predicted.classes,
                reference = y.test,
                positive = "1")

df_fold2_test <- cbind(df_fold2_test, probabilities, predicted.classes)
df_fold2_test <- rename(df_fold2_test, predicted.probs=s0)


#------------------------------------------
# Build Model - Fold 3
# Data split and upsample

df_fold3_train <- df[-fold3,]
df_fold3_test <- df[fold3,]

# upsample training data
df_fold3_trainUP <- upSample(x = df_fold3_train [, c(4:53)],
                             y = df_fold3_train [, 54],
                             list = FALSE,
                             yname = "sca")

# Dummify X in matrix form / upsampled training data
x <- model.matrix(sca~., df_fold3_trainUP)[,-1]
y <- df_fold3_trainUP$sca

#------------------------------------------
# Optimize model and find best lambda value
# Find best lambda value with cv.glmnet

cv.lasso.fold3 <- cv.glmnet(x, y, 
                            alpha = 1, 
                            family = "binomial",
                            type.measure="auc",
                            nfolds = 5)

plot(cv.lasso.fold3)
cv.lasso.fold3

#------------------------------------------
# Build Final Model - Fold 3

model_fold3 <- glmnet(x, y, alpha = 1, family = "binomial", 
                      lambda = cv.lasso.fold3$lambda.1se) 

# Evaluate Performance on Outer Fold
x.test <- model.matrix(sca ~., df_fold3_test[,c(4:54)])[,-1]
y.test <- df_fold3_test$sca
probabilities <- model_fold3 %>% predict(newx = x.test, type = "response")
predicted.classes <- as.factor(ifelse(probabilities > 0.5, 1, 0))
confusionMatrix(predicted.classes,
                reference = y.test,
                positive = "1")

df_fold3_test <- cbind(df_fold3_test, probabilities, predicted.classes)
df_fold3_test <- rename(df_fold3_test, predicted.probs=s0)

#--------------------------------------------
# Fold 4

df_fold4_train <- df[-fold4,]
df_fold4_test <- df[fold4,]

# upsample training data
df_fold4_trainUP <- upSample(x = df_fold4_train [, c(4:53)],
                             y = df_fold4_train [, 54],
                             list = FALSE,
                             yname = "sca")

# Dummify X in matrix form / upsampled training data
x <- model.matrix(sca~., df_fold4_trainUP)[,-1]
y <- df_fold4_trainUP$sca

#------------------------------------------
# Optimize model and find best lambda value
# Find best lambda value with cv.glmnet

cv.lasso.fold4 <- cv.glmnet(x, y, 
                            alpha = 1, 
                            family = "binomial",
                            type.measure="auc",
                            nfolds = 5)

plot(cv.lasso.fold4)
cv.lasso.fold4

#------------------------------------------
# Build Final Model - Fold 4

model_fold4 <- glmnet(x, y, alpha = 1, family = "binomial", 
                      lambda = cv.lasso.fold4$lambda.1se) 

# Evaluate Performance on Outer Fold
x.test <- model.matrix(sca ~., df_fold4_test[,c(4:54)])[,-1]
y.test <- df_fold4_test$sca
probabilities <- model_fold4 %>% predict(newx = x.test, 
                                         type = "response")
predicted.classes <- as.factor(ifelse(probabilities > 0.5, 1, 0))
confusionMatrix(predicted.classes,
                reference = y.test,
                positive = "1")

df_fold4_test <- cbind(df_fold4_test, probabilities, predicted.classes)
df_fold4_test <- rename(df_fold4_test, predicted.probs=s0)

# End of model building...

cv.lasso.fold1$lambda.1se # 0.0105, 30 coefficients
cv.lasso.fold2$lambda.1se # 0.003, 39 coefficients
cv.lasso.fold3$lambda.1se # 0.002, 41 coefficients
cv.lasso.fold4$lambda.1se # 0.007, 41 coefficients 

plot(cv.lasso.fold1)

#---------------------------------
# Plots of CV for Lambda Determination
fold1.lambda <- cbind(Loglambda = log(cv.lasso.fold1$lambda),
                       AUC = cv.lasso.fold1$cvm,
                      SD = cv.lasso.fold1$cvsd)

fold2.lambda <- cbind(Loglambda = log(cv.lasso.fold2$lambda),
                      AUC = cv.lasso.fold2$cvm,
                      SD = cv.lasso.fold2$cvsd)

fold3.lambda <- cbind(Loglambda = log(cv.lasso.fold3$lambda),
                      AUC = cv.lasso.fold3$cvm,
                      SD = cv.lasso.fold3$cvsd)

fold4.lambda <- cbind(Loglambda = log(cv.lasso.fold4$lambda),
                      AUC = cv.lasso.fold4$cvm,
                      SD = cv.lasso.fold4$cvsd)

write.csv(fold1.lambda, "fold1.lambda.csv")
write.csv(fold2.lambda, "fold2.lambda.csv")
write.csv(fold3.lambda, "fold3.lambda.csv")
write.csv(fold4.lambda, "fold4.lambda.csv")

plot(model_fold1$glmnet.fit, "norm",   label=TRUE)

#---------------------------------
# Coefficients and Values
model1_coef <- as.matrix(coef(model_fold2))
model2_coef <- as.matrix(coef(model_fold2))
model3_coef <- as.matrix(coef(model_fold3))
model4_coef <- as.matrix(coef(model_fold4))
write.csv(model1_coef, "model1_coef.csv")
write.csv(model2_coef, "model2_coef.csv")
write.csv(model3_coef, "model3_coef.csv")
write.csv(model2_coef, "model4_coef.csv")

#---------------------------------
# ROC Curves
library(ROCR)
rocr_pred1 <- prediction(df_fold1_test$predicted.probs,
                         df_fold1_test$sca)
rocr_roc1 <- performance(rocr_pred1, measure = "tpr", x.measure = "fpr")
rocr_auc1 <- performance(rocr_pred1, measure = "auc")
auc1 <- rocr_auc1@y.values[[1]] #0.596

# Fold 2
rocr_pred2 <- prediction(df_fold2_test$predicted.probs,
                         df_fold2_test$sca)
rocr_roc2 <- performance(rocr_pred2, measure = "tpr", x.measure = "fpr")
rocr_auc2 <- performance(rocr_pred2, measure = "auc")
auc2 <- rocr_auc2@y.values[[1]] #0.526

# Fold 3
rocr_pred3 <- prediction(df_fold3_test$predicted.probs,
                         df_fold3_test$sca)
rocr_roc3 <- performance(rocr_pred3, measure = "tpr", x.measure = "fpr")
rocr_auc3 <- performance(rocr_pred3, measure = "auc")
auc3 <- rocr_auc3@y.values[[1]] #0.662

# Fold 4
rocr_pred4 <- prediction(df_fold4_test$predicted.probs,
                         df_fold4_test$sca)
rocr_roc4 <- performance(rocr_pred4, measure = "tpr", x.measure = "fpr")
rocr_auc4 <- performance(rocr_pred4, measure = "auc")
auc4 <- rocr_auc4@y.values[[1]] #5473

# roc curve for submodels
plot(rocr_roc1,
     col = "red",
     text.adj = c(-0.5, 1),
     lwd = 2)
plot(rocr_roc2,
     col = "blue",
     lwd = 2,
     add = TRUE)
plot(rocr_roc3,
     col = "green",
     lwd = 2,
     add = TRUE)
plot(rocr_roc4,
     col = "black",
     lwd = 2,
     add = TRUE)
abline(a = 0, b = 1)

#------------------------------
# AUC and ROC for ensembled model

full_results <- rbind(df_fold1_test, df_fold2_test, 
                      df_fold3_test, df_fold4_test) %>%
                select(sca,predicted.probs,predicted.classes)

confusionMatrix(data=full_results$predicted.classes,
                reference = full_results$sca,
                positive = "1")  

rocr_pred_full <- prediction(full_results$predicted.probs,
                             full_results$sca)
rocr_roc_full <- performance(rocr_pred_full, measure = "tpr", x.measure = "fpr")
rocr_auc_full <- performance(rocr_pred_full, measure = "auc")
auc_full <- rocr_auc_full@y.values[[1]] #0.578

plot(rocr_roc_full,
     colorize = TRUE,
     print.cutoffs.at = seq(0,1, by = 0.1),
     text.adj = c(-0.5, 1),
     lwd = 2)
abline(a = 0, b = 1)

write.csv(full_results, "full_results.csv")

full_results1 <- read.csv("full_results.csv")

confusionMatrix(data = as.factor(full_results1$predicted.classes),
                reference = as.factor(full_results1$sca),
                positive = "1")

# END
# Final Model for Logistic Lasso Regression

#------------------------------------------
# Prediction for Kellogg Company
df_kellogg <- filter(df.k, tic=="K")
df_kellogg_y <- df_kellogg$sca
df_kellogg_x <- model.matrix(sca~., df_kellogg[,c(4:54)])[,-1]

x <- predict(model_fold1, newx=df_kellogg_x, type="response") #0.035
y <- predict(model_fold2, newx=df_kellogg_x, type="response") #0.365
z <- predict(model_fold3, newx=df_kellogg_x, type="response") #0.103
aa <- predict(model_fold4, newx=df_kellogg_x, type="response") #0.407


sum_auc <- sum(0.55, 0.6375, 0.677, 0.533)

avg_prob <- 0.55/sum_auc*x + 0.6375/sum_auc*y + 
  0.677/sum_auc*z + 0.533/sum_auc*aa  #22.5%

#Fold1 Model

quants <- quantile(full_results$predicted.probs,
                   c(0.1,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90))

quant1 <- quantile(full_results$predicted.probs, probs = 22.5)

View(quants)

probs_plot <- ggplot(full_results, aes(x=predicted.probs))+
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="steel blue",
                 bins=20)+
  labs(y=NULL,
       x ="\nPredicted Probability of SCA Litigation")+
  theme_bw()+
  theme(axis.title = element_text(size=22, face="bold"),
        axis.text = element_text(size=20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_density(trim=TRUE, size=1) +
  geom_vline(xintercept = 0.225,
             color = "dark red", size=1.5)

ggsave("probs_plot.png", probs_plot, dpi=400, height = 6, width = 9 )


