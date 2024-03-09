# 99_bayes_models_nimble.R
# Bayesian models fitted using nimble
# version with generic predictor and mean only
# February 2024

# a) model without predictors (Model 0)
code_model_no_predictors <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through people
    percentages[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){ # loop through recommendations
      log(e[i,j]) <- alpha[j] + beta[j]*recommendation[i,j] # probabilities dependent on reviewers' recommendation
      pi[i,j] <- e[i,j] / sum(e[i,1:ncat])
    }
  }
  alpha[1] <- 0 # corner-point
  beta[1] ~ dnorm(0, hyper[1]) 
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, hyper[k])
    beta[k] ~ dnorm(0, hyper[k])
  }
  # fitted probabilities
  for (i in 1:ncat){ # loop through each recommendation
    for (j in 1:ncat){
      log(e.fit[i,j]) <- alpha[j] + beta[j]*recommendation_fit[i,j]
      pi.fit[i,j] <- e.fit[i,j] / sum(e.fit[i,1:ncat])
    }
  }
})

## b) model with predictors on mean but no uncertainty (Model 1, mean only)
code_model_mean <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through people
    percentages[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){ # loop through recommendations
#      log(e[i,j]) <- alpha[j] + beta[j]*recommendation[i,j] + zeta[j]*predictor[i] # zeta influences all 3 categories
      log(e[i,j]) <- alpha[j] + beta[j]*recommendation[i,j] + zeta[j]*recommendation[i,j]*predictor[i] # zeta influences chosen category only; predictor is 0/1 or 0,1,2,...
      pi[i,j] <- e[i,j] / sum(e[i,1:ncat])
    }
  }
  alpha[1] <- 0 # corner-point
  beta[1] ~ dnorm(0, hyper[1]) 
  zeta[1] ~ dnorm(0, hyper[1]) 
  #zeta[1] <- 0 # corner-point
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, hyper[k])
    beta[k] ~ dnorm(0, hyper[k])
    zeta[k] ~ dnorm(0, hyper[k])
  }
  # fitted probabilities
  for (i in 1:(ncat*2)){ # *2 for two levels of the predictor
    for (j in 1:ncat){
      log(e.fit[i,j]) <- alpha[j] + beta[j]*recommendation_fit[i,j] + zeta[j]*recommendation_fit[i,j]*predictor_fit[i]
      pi.fit[i,j] <- e.fit[i,j] / sum(e.fit[i,1:ncat])
    }
  }
})

## c) model with predictors on variance but not on mean (Model 2, uncertainty only)
code_model_variance <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through people
    percentages[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){ # loop through recommendations
      log(e[i,j]) <- alpha[j] + beta[j]*recommendation[i,j]
      f[i,j] <- gamma*predictor[i] # not logged 
      pi[i,j] <- (e[i,j]+f[i,j]) / (sum(e[i,1:ncat]) + sum(f[i,1:ncat]))
    }
  }
  gamma ~ dunif(0,10000) # must be positive
  alpha[1] <- 0 # corner-point
  beta[1] ~ dnorm(0, hyper[1]) 
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, hyper[k])
    beta[k] ~ dnorm(0, hyper[k])
  }
  # fitted probabilities
  for (i in 1:(ncat*2)){ # *2 for both levels of the predictor
    for (j in 1:ncat){
      log(e.fit[i,j]) <- alpha[j] + beta[j]*recommendation_fit[i,j]
      f.fit[i,j] <- gamma*predictor_fit[i]
      pi.fit[i,j] <- (e.fit[i,j] + f.fit[i,j]) / (sum(e.fit[i,1:ncat]) + sum(f.fit[i,1:ncat])) 
    }
  }
})
