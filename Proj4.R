dat = read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj4/data.csv")
library(randomForest)
library(fdm2id)
dat = na.omit(dat)
dat$X21 = as.numeric(dat$X2 == "A")
dat$X22 = as.numeric(dat$X2 == "B")
dat$X41 = as.numeric(dat$X4 == "A")
dat$X42 = as.numeric(dat$X4 == "B")
dat = subset(dat, select = -c(X2, X4))
dat$Z = dat$Z.1
dat$Y = dat$Y.3 + dat$Y.4 - dat$Y.1 - dat$Y.2
dat$Z = as.factor(dat$Z)
dat = subset(dat, 
             select = -c(Z.1, Z.2, Z.3, Z.4, Y.1, Y.2, Y.3, Y.4, 
                         post.1, post.2, post.3, post.4, id.practice,
                         X.1, X))
N = dim(dat)[1]
dat1 = dat[dat$Z == 1, ]
dat0 = dat[dat$Z == 0, ]
N1 = dim(dat1)[1]
N0 = dim(dat0)[1]
# Propensity Score Model
## Random Forest 
### tuning parameters
nt = c(400, 450, 500, 550, 600) #number of trees
Error_Rate = rep(0, length(nt))
for (i in 1:length(nt)){
  mod = randomForest(Z~., data = dattrain[, -41], ntree = nt[i])
  mod.predict = predict(mod, newdata = datvalid[, -c(40, 41)])
  error_rate = sum(mod.predict != datvalid[, 40])/dim(datvalid)[1]
  Error_Rate[i] = error_rate
}

modR = randomForest(formula = Z ~ ., data = dat[sample(1:N*0.8, N*0.1), -41], ntree = 450)
modR.predict = predict(modR, newdata = dat[(N*0.8+1):N, -c(40, 41)])
error_rateR = sum(modR.predict != dat[(N*0.8+1):N, 40])/(N*0.2)
# error rate 0.3044526

## Logistic Regression
modL = glm(formula = Z ~ ., data = dat[1:N*0.8, -41], family = binomial(link = "logit"))
modL.predict = predict(modL, newdata = dat[(N*0.8+1):N, -c(40, 41)], type = "response")
modL.predict = as.factor(ifelse(modL.predict >= 0.5, 1, 0))
error_rateL = sum(modL.predict != dat[(N*0.8+1):N, 40])/(N*0.2)
# error rate 0.3003933

# outcome model
## Random Forest
modR1 = randomForest(Y~., data = dat1[1:20000, -44], importance = TRUE, ntree = 400)
modR1$importance
modR1.predict = predict(modR1, newdata = dat1[20001:25000, -c(44, 45)])
sqrt(mean((modR1.predict-dat1[20001:25000, 45])^2))
modR0 = randomForest(Y~., data = dat0[1:20000, -44], importance = TRUE, ntree = 400)
modR0$importance
modR0.predict = predict(modR0, newdata = dat0[20001:25000, -c(44, 45)])
sqrt(mean((modR0.predict-dat0[20001:25000, 45])^2))
## MLP
modM1 = MLPREG(dat1[1:N1*0.8, -c(44, 45)], dat1[1:N1*0.8, 45], size = 6, decay = 0.01)
modM1.predict = predict(modM1, dat1[(N1*0.8+1):N1, -c(44, 45)])

modM0 = MLPREG(dat0[1:N0*0.8, -c(44, 45)], dat0[1:N0*0.8, 45], size = 6, decay = 0.01)
modM0.predict = predict(modM0, dat0[(N0*0.8+1):N0, -c(44, 45)])
# Doubly Robust
ObsCausal.est = function(z, y, x, out.family = gaussian, 
                         truncpscore = c(0, 1))
{
  x = as.matrix(x)
  ## fitted propensity score
  pscore   = glm(z ~ x, family = binomial)$fitted.values
  pscore   = pmax(truncpscore[1], pmin(truncpscore[2], pscore))
  
  ## fitted potential outcomes
  outcome1 = glm(y ~ x, weights = z, 
                 family = out.family)$fitted.values
  outcome0 = glm(y ~ x, weights = (1 - z), 
                 family = out.family)$fitted.values
  
  ## regression imputation estimator
  ace.reg  = mean(outcome1 - outcome0) 
  ## propensity score weighting estimator
  #ace.ipw0 = mean(z*y/pscore - (1 - z)*y/(1 - pscore))
  #ace.ipw  = mean(z*y/pscore)/mean(z/pscore) - 
  #               mean((1 - z)*y/(1 - pscore))/mean((1 - z)/(1 - pscore))
  ## doubly robust estimator
  res1     = y - outcome1
  res0     = y - outcome0
  ace.dr   = ace.reg + mean(z*res1/pscore - (1 - z)*res0/(1 - pscore))
  
  return(ace.dr)     
}

ObsCausal = function(z, y, x, n.boot = 10^2,
                     out.family = gaussian, truncpscore = c(0, 1))
{
  point.est  = ObsCausal.est(z, y, x, out.family, truncpscore)
  
  ## nonparametric bootstrap
  n.sample   = length(z)
  x          = as.matrix(x)
  boot.est   = replicate(n.boot, 
                         {id.boot = sample(1:n.sample, n.sample, replace = TRUE)
                         ObsCausal.est(z[id.boot], y[id.boot], x[id.boot, ], 
                                       out.family, truncpscore)})
  
  boot.se    = sd(boot.est)
  
  res        = rbind(point.est, boot.se)
  rownames(res) = c("est", "se")
  colnames(res) = c("DR")
  
  return(res)
}

ind = sample(1:N, 400000)
causaleffect = ObsCausal(as.numeric(dat[ind, 44])-1, dat[ind, 45], dat[ind, -c(44, 45)])
ACE = causaleffect[1, ]
ACE
SEboot = causaleffect[2, ]
SEboot