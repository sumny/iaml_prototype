library(mvtnorm)
library(rpart)
library(rpart.plot)
library(iml)
X = rmvnorm(10000, c(0, 0, 0), sigma = matrix(c(1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1), nrow = 3L, ncol = 3L, byrow = TRUE))

y = t(c(1, 1, 1) %*% t(X))

dat = cbind(y, X)
colnames(dat) = c("y", "x1", "x2", "x3")
dat = as.data.frame(dat)

model = rpart(y ~ ., data = dat)

pred = Predictor$new(model, data = dat)

inter = Interaction$new(pred, feature = "x2")
eff = FeatureEffects$new(pred)

library(mlr3)
library(mlr3learners)
library(xgboost)
library(EIX)

task = TaskRegr$new("test", target = "y", backend = dat)
learner = lrn("regr.xgboost")
learner$param_set$values$nrounds = 10L
learner$param_set$values$interaction_constraints = list(c(0, 1), c(1, 2))
learner$train(task)

xgb.model.dt.tree(task$feature_names, learner$model)
pairs = interactions(learner$model, task$data(cols = task$feature_names), option = "pairs")
lolli = lollipop(learner$model, task$data(cols = task$feature_names))
