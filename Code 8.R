## Question 11.1


# Clean workspace
rm(list=ls())
cat("\014")
graphics.off()
set.seed(123)

library(glmnet)

uscrime <- read.csv("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 7 - Advanced Regression/Homework 7/Data/uscrime.csv", sep="")
head(uscrime)

############### Stepwise Regression ###################

summary(uscrime)

# We need to scale the predictor variables, except So and Crime. 
# The variable So is a binary factor variable indicating 1 whether a state is in South, O otherwise. Thus no scale is needed.
# Crime is the response and therefore no scale is needed as well.
uscrime.scaled = scale(uscrime[,-2][1:14])
uscrime.scaled = cbind(uscrime.scaled, uscrime[,2], uscrime[,16])
colnames(uscrime.scaled)[15] <- "So"
colnames(uscrime.scaled)[16] <- "Crime"
colnames(uscrime)
col_order <- c(colnames(uscrime))
uscrime.scaled <- uscrime.scaled[, col_order]
uscrime.scaled = as.data.frame(uscrime.scaled)
head(uscrime.scaled)
summary(uscrime.scaled)

# Perform a backwards stepwise regression with cross-validation.
model.step.back <- lm(Crime ~ ., data = uscrime.scaled)

step(model.step.back, direction = "backward", trace=TRUE,steps =1000, k = log(nrow(uscrime.scaled)))


#Step:  AIC=517.74
#Crime ~ M + Ed + Po1 + U2 + Ineq + Prob

# Develop a new model with the 6 variables found with stepwise regression. 
# This yields an adjusted R-squared of 0.7307.
model.step.back.1 = lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = uscrime.scaled)
summary(model.step.back.1)

# Next use leave-one-out cross-validation to assess the quality of this model. 

sst <- sum((uscrime.scaled$Crime - mean(uscrime.scaled$Crime))^2)
sse <- 0

for(i in 1:nrow(uscrime.scaled)) {
  model.step.back.cv = lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = uscrime.scaled[-i,])
  predict.step.back <- predict(model.step.back.cv, newdata=uscrime.scaled[i,])
  sse <- sse + ((predict.step.back - uscrime.scaled[i,16])^2)
}

r.squared <- 1 - sse/sst
r.squared
# We obtain an R squared of 0.666. 
# The model seletced by stepwise regression is the following:Crime ~ M + Ed + Po1 + U2 + Ineq + Prob.
# The R-squared obtained is about 0.7307 and AIC=517.74. 
# The r-squared of the leave-one-out cross-validation to assess the quality of this model is 0.666

############### LASSO Regression ###################
set.seed(123)
# We build a lasso regression
# Given the glmnet explanation alpha=1 is the lasso (default) and alpha=0 is the ridge
model.lasso=cv.glmnet(x=as.matrix(uscrime.scaled[,-16]), y=as.matrix(uscrime.scaled$Crime), alpha=1, nfolds = log(nrow(uscrime.scaled)), type.measure="mse", family="gaussian")
coef(model.lasso, s=model.lasso$lambda.min)
plot(model.lasso)

# The output of the Lasso model selected the following 6 variables: M + Ed + Po1 + M.F + Ineq + Prob

model.lasso = lm(Crime ~M + Ed + Po1 + M.F + Ineq + Prob, data = uscrime.scaled)
summary(model.lasso)
# We obtain an adjusted R-squared of 0.713 which is slightly lower to the one obtained with stepwise regression.

# Use cross-validation to evaluate the model. 

sst <- sum((uscrime.scaled$Crime - mean(uscrime.scaled$Crime))^2)
sse <- 0
for(i in 1:nrow(uscrime.scaled)) {
  model.lasso.cv = lm(Crime ~ M + Ed + Po1 + M.F + Ineq + Prob, data = uscrime.scaled[-i,])
  predict.lasso <- predict(model.lasso.cv, newdata=uscrime.scaled[i,])
  sse <- sse + ((predict.lasso - uscrime.scaled[i,16])^2)
}

r.squared <- 1 - sse/sst
r.squared

# We obtain 0.639 which is slightly lower to the one obtained with stepwise regression.
# In the lasso regression, we obtain the following model: M + Ed + Po1 + M.F + Ineq + Prob.
# In stepwise regression, we obtain the following model: M + Ed + Po1 + U2 + Ineq + Prob.
# In conclusion, both selected 6 variables, however M.F variable was selected as siginficant only in the lasso model 
# and U2 was selected only in stepwise model. 

############### Elastic Net ###################
set.seed(123)
# We discretize alpha values by steps of 0.1, from 0 to 1, and we compute the R-Squared.

r.squared=c()

for (i in 0:10) {
  model.elastic.cv = cv.glmnet(x=as.matrix(uscrime.scaled[,-16]),y=as.matrix(uscrime.scaled$Crime),
                          alpha=i/10,nfolds = log(nrow(uscrime.scaled)),type.measure="mse",family="gaussian")

  r.squared = cbind(r.squared, model.elastic.cv$glmnet.fit$dev.ratio[which(model.elastic.cv$glmnet.fit$lambda == model.elastic.cv$lambda.min)])
  
}

r.squared

alpha_best = (which.max(r.squared)-1)/10
alpha_best

# Use the ???best??? alpha value to build the model.
elastic.net=cv.glmnet(x=as.matrix(uscrime.scaled[,-16]),y=as.matrix(uscrime.scaled$Crime),alpha=alpha_best,
                      nfolds = log(nrow(uscrime.scaled)),type.measure="mse",family="gaussian")
plot(elastic.net)
#Output the coefficients of the variables selected by Elastic Net

coef(elastic.net, s=elastic.net$lambda.min)

# The Elastic Net selects 14 variables compared to 6 in Lasso and 6 in stepwise. 
# This is the following model selected by elastic net regression: M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob
# Next, compare how this new model performs compared to the Lasso and Stepwise models.

model.elastic.net = lm(Crime ~M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = uscrime.scaled)
summary(model.elastic.net)

# We obtain an r-squared of 0.7148 which appears to be similar to that obtained with Lasso and Stepwise regression. 
# Let???s use cross-validation to evaluate the model.

sst <- sum((uscrime.scaled$Crime - mean(uscrime.scaled$Crime))^2)
sse <- 0

for(i in 1:nrow(uscrime.scaled)) {
  model.elastic = lm(Crime ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = uscrime.scaled[-i,])
  predict.elastic <- predict(model.elastic,newdata=uscrime.scaled[i,])
  sse <- sse + ((predict.elastic - uscrime.scaled[i,16])^2)
}
r.squared <- 1 - sse/sst
r.squared

# We obtain an r-squared of 0.5361 

# We observe the p-values of So, Po2, LF, M.F, Pop, NW, U1, Wealth is higher than 0.10.
# When we remove the above variables we end up with the following model: M + Ed + Po1 + U2 + Ineq + Prob. Similar to stepwise regression model.
# In the lasso regression, we obtain the following model: M + Ed + Po1 + M.F + Ineq + Prob.
# In stepwise regression, we obtain the following model: M + Ed + Po1 + U2 + Ineq + Prob.

