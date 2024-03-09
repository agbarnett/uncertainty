# 99_check_xval.R
# checking the cross-validation from nimble
# Jan 2024
library(nimble, warn.conflicts = FALSE)
library(dplyr)
library(ggplot2)
calc.p = function(x){1/(1 + exp(-x))}

# loss function for cross-validation for a binary outcome
#CrossEntropylossFunction <- function(simulatedDataValues, actualDataValues){ # original
CrossEntropylossFunction <- function(actualDataValues, simulatedDataValues){ # switched as a test
    dataLength <- length(simulatedDataValues) # 
  simulatedDataValues <- simulatedDataValues / 100 # change % to probability
  simulatedDataValues = pmax(0.001, simulatedDataValues) # avoid logging impossible values
  simulatedDataValues = pmin(0.999, simulatedDataValues)
  actualDataValues <- actualDataValues / 100
  loss <- 0
  for (i in 1:dataLength){
    est1 <- actualDataValues[i] * log(simulatedDataValues[i])
    est2 <- (1-actualDataValues[i]) * log(1-simulatedDataValues[i])
    est <- (est1 + est2)*(-1)
    loss <- loss + est
  }
  loss <- loss / dataLength
  return(loss)
}

# generate the logistic data
set.seed(3)
p <- 15    # number of explanatory variables
n <- 400   # number of observations
X <- matrix(rnorm(p*n), nrow = n, ncol = p) # explanatory variables
true_betas <- c(c(0.3, 0.3, 0.3, 0.3, 0.3), rep(0, p-5)) # coefficients
mu = X %*% true_betas
p = calc.p(mu)
y <- rbinom(n = n, prob = p, size = 1)

#
code3 <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta1 ~ dnorm(0, sd = 100)
  beta2 ~ dnorm(0, sd = 100)
  beta3 ~ dnorm(0, sd = 100)
  for(i in 1:n) {
    y[i] ~ dbern(mu[i])
    logit(mu[i]) <- beta0 + beta1*x1[i] + beta2*x2[i] + beta4*x4[i]
  }
})
#
code2 <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta1 ~ dnorm(0, sd = 100)
  beta2 ~ dnorm(0, sd = 100)
  beta3 ~ dnorm(0, sd = 100)
  for(i in 1:n) {
    y[i] ~ dbern(mu[i])
    logit(mu[i]) <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i]
  }
})
#
#
code1 <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta1 ~ dnorm(0, sd = 100)
  beta2 ~ dnorm(0, sd = 100)
  for(i in 1:n) {
    y[i] ~ dbern(mu[i])
    logit(mu[i]) <- beta0 + beta1*x1[i] + beta2*x2[i]
  }
})
#
code0 <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta1 ~ dnorm(0, sd = 100)
  for(i in 1:n) {
    y[i] ~ dbern(mu[i])
    logit(mu[i]) <- beta0 + beta1*x1[i]
  }
})


## extract data for two predictors and center for better MCMC performance
x1 <- X[,1] - mean(X[,1])
x2 <- X[,2] - mean(X[,2])
x3 <- X[,3] - mean(X[,3])
x4 <- X[,4] - mean(X[,4])

constants0 <- list(n = n, x1 = x1)
constants1 <- list(n = n, x1 = x1, x2 = x2)
constants2 <- list(n = n, x1 = x1, x2 = x2, x3 = x3)
constants3 <- list(n = n, x1 = x1, x2 = x2, x4 = x4)
data <- list(y = y)
inits0 <- list(beta0 = 0, beta1 = 0)
inits1 <- list(beta0 = 0, beta1 = 0, beta2 = 0)
inits2 <- list(beta0 = 0, beta1 = 0, beta2 = 0, beta3 = 0)
inits3 <- list(beta0 = 0, beta1 = 0, beta2 = 0, beta4 = 0)
model0 <- nimbleModel(code0, constants = constants0, data = data, inits = inits0) # build model
model1 <- nimbleModel(code1, constants = constants1, data = data, inits = inits1) # build model
model2 <- nimbleModel(code2, constants = constants2, data = data, inits = inits2) # build model
model3 <- nimbleModel(code3, constants = constants3, data = data, inits = inits3) # build model
#
mcmcConf0 <- configureMCMC(model0) # assign default samplers to nodes
mcmcConf1 <- configureMCMC(model1) # assign default samplers to nodes
mcmcConf2 <- configureMCMC(model2) # assign default samplers to nodes
mcmcConf3 <- configureMCMC(model3) # assign default samplers to nodes

### run the cross-validations
n_folds = 10
xval0 = runCrossValidate(MCMCconfiguration = mcmcConf0,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = 'random',
                         lossFunction = CrossEntropylossFunction,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
xval1 = runCrossValidate(MCMCconfiguration = mcmcConf1,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = 'random',
                         lossFunction = CrossEntropylossFunction,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
xval2 = runCrossValidate(MCMCconfiguration = mcmcConf2,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = 'random',
                         lossFunction = CrossEntropylossFunction,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
xval3 = runCrossValidate(MCMCconfiguration = mcmcConf3,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = 'random',
                         lossFunction = CrossEntropylossFunction,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))

# plot results
f1 = data.frame(xval = xval0$CVvalue, se = xval0$CVstandardError )
f2 = data.frame(xval = xval1$CVvalue, se = xval1$CVstandardError )
f3 = data.frame(xval = xval2$CVvalue, se = xval2$CVstandardError )
f4 = data.frame(xval = xval3$CVvalue, se = xval3$CVstandardError )
to_plot = bind_rows(f1, f2, f3, f4, .id = 'model')%>%
  mutate(model = as.numeric(model),
         z = qnorm(0.975),
         lower = xval - (z*se),
         upper = xval + (z*se))
xplot = ggplot(data = to_plot, aes(x = model, y = xval, ymin=lower, ymax=upper))+
  geom_point()+
  geom_errorbar(width=0)+
  theme_bw()+
  xlab('Model')+
  ylab('Mean square error')
xplot

## box plot of all folds
process_xval = function(inxval){
  outxval = matrix(unlist(inxval$foldCVinfo), nrow=n_folds, ncol=2, byrow = TRUE) %>%
      as.data.frame()
    names(outxval) = c('xval','se')
    outxval = mutate(outxval, 
                    fold = 1:n())
}
this_xval0 = process_xval(xval0)
this_xval1 = process_xval(xval1)
this_xval2 = process_xval(xval2)
this_xval3 = process_xval(xval3)
to_plot = bind_rows(this_xval0, this_xval1, this_xval2, this_xval3, .id = 'model')%>%
  mutate(model = as.numeric(model))
fplot = ggplot(data = to_plot, aes(x = factor(model), y = xval))+
  geom_boxplot()+
  theme_bw()+
  xlab('Model')+
  ylab('Mean square error')
fplot
