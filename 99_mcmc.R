## 99_mcmc.R
# MCMC options for nimble; called by 4_bayes_models.R
# January 2024

debug = FALSE
n.chains = 2
thin = 5
MCMC = 10000
seed = rep(0,2)
seed[1] = TeachingDemos::char2seed('lincoln')
seed[2] = TeachingDemos::char2seed('cheltenham') # one per chain
