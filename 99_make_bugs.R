## 99_make_bugs.R
# make winbugs file for ordinal model depending on number of independent variables
# September 2021

# model 1: no variation
if(type=='no variation'){
bugs = file(model.file, 'w')
cat('model{
  ## Likelihood
  for (i in 1:N){
    recommendation[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){
      log(e[i,j]) <- alpha[j]
      pi[i,j] <- e[i,j] / sum(e[i,1:ncat])
    }
  }
  alpha[1] <- 0
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, 0.001)
  }
}\n', file=bugs)
close(bugs)
}

# model 2: with variation
if(type=='with variation'){
  bugs = file(model.file, 'w')
  cat('model{
  ## Likelihood
  for (i in 1:N){
    recommendation[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){
      log(e[i,j]) <- alpha[j]
      f[i,j] <- beta*X[i] # uncertainty equation (not-logged)
      pi[i,j] <- (e[i,j] + f[i,j])/ (sum(e[i,1:ncat])+sum(f[i,1:ncat]))
    }
  }
  ## Priors
  beta ~ dnorm(0, 0.001)
  alpha[1] <- 0
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, 0.001)
  }
}\n', file=bugs)
  close(bugs)
}

# model 2 (alternative): with predictors
if(type=='with predictors'){
  bugs = file(model.file, 'w')
  cat('model{
  ## Likelihood
  for (i in 1:N){
    recommendation[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){
      log(e[i,j]) <- alpha[j] + beta[j]*X[i]
      pi[i,j] <- e[i,j] / sum(e[i,1:ncat])
    }
  }
  alpha[1] <- 0
  beta[1] <- 0
  for (k in 2:ncat){
    alpha[k] ~ dnorm(0, 0.001)
    beta[k] ~ dnorm(0, 0.001)
  }
}\n', file=bugs)
  close(bugs)
}

# spike and slab version - not used
if(type=='spike and slab'){
  bugs = file(model.file, 'w')
cat('model{
  ## Likelihood
  for (i in 1:N){
    recommendation[i, 1:ncat] ~ dmulti(pi[i, 1:ncat], M)
    # make probabilities
    for (j in 1:ncat){
      pi[i,j] <- (e[i,j] + f[i,j])/ (sum(e[i,1:ncat])+sum(f[i,1:ncat]))
    }
    # uncertainty equation (not-logged, but always positive)
    for (k in 1:ncat){
      f[i,k] <- U*X[i]
    }
    # regression equation and uncertainty (no regression equation for cat 1)
    log(e[i, 1]) <- 0
    for (k in 2:ncat){
      log(e[i,k]) <- alpha[k-1]
    }
  }
  ## Priors
  U <- spike*(1-delta) + slab*delta
  spike <- 0
  slab ~ dunif(0,10)
  delta ~ dbern(p_precision)
  p_precision ~ dbeta(1,1)
  for (k in 1:(ncat-1)){
    alpha[k] ~ dnorm(0, 0.0001)
  }
}\n', file=bugs)
close(bugs)
#slab ~ dnorm(0, 0.0001)I(0,)
}

