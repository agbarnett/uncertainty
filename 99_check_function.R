# 99_check_function.R
# check the link function used to model uncertainty, see 99_bayes_models.R
# March 2023

ncat = 3
decision = toeplitz(c(1,rep(0,ncat-1)))
alpha = c(0, 1, 1)
beta = c(0, 1, 1)
gamma = -100

e1 = matrix(nrow = 3, ncol = 3)
e2 = matrix(nrow = 3, ncol = 3)
pi1 = matrix(nrow = 3, ncol = 3)
pi2 = matrix(nrow = 3, ncol = 3)
for (i in 1:3){ # loop responses
  for (j in 1:ncat){ # loop through recommendations
    e1[i,j] = exp(alpha[j] + beta[j]*decision[i,j])
    e2[i,j] = exp(alpha[j] + beta[j]*decision[i,j]) + gamma # general uncertainty must be outside the exp
  }
  for (j in 1:ncat){ # loop through recommendations
    pi1[i,j] = e1[i,j] / sum(e1[i,1:ncat])
    pi2[i,j] = e2[i,j] / sum(e2[i,1:ncat])
  }
}
# rowSums(pi)  # check