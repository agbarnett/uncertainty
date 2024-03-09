# 99_understand_model.R
# understanding the limitations of the multinomial model; version with three categories
# Feb 2024
library(dplyr)
library(ggplot2)
source('3_colours.R')

## using parameters for F1000 protocol as an example

## parameters
# model 0
alpha0 = c(0,1.4,-0.5)
beta0 = c(3.17,-0.09,3.48) # only impacts the same category, allows for smaller or larger results for that category
# model 1
alpha1 = c(0,0.73,-1.35)
beta1 = c(3.18,-0.18,3.6) # only impacts the same category
zeta1 = c(0,0.88,0.97)
zeta1 = c(0,0,10) # shows that zeta increases probability across all categories
# checking F1000 time_diff results for accept
alpha = c(0, 1.41,-0.50)
beta = c(3.98, -0.11, 4.13)
zeta = c(-0.47, 0.01,-0.20)

# checking Epidemiology gender results
alpha = c(0, 1.14, 1.34, 1.72, 0.92)
beta = c(4.11, 2.73, 2.06, 1.14, 3.23)
zeta = c(-0.31, 0.23, 0.32, 0.11, 0.63)

# number of categories
ncat = length(alpha)
toe = rep(0, ncat); toe[1] = 1
recommendation = toeplitz(toe)
predictor = rep(0,ncat)

# loop
pi0 = pi1 = pi2 = e0 = e1 = e2 = matrix(data = 0, nrow = ncat, ncol = ncat)
for (i in 1:ncat){
  for (j in 1:ncat){
    
    # change parameters to alpha1 alpha0 etc depending on above
    
    e0[i,j] = exp(alpha[j] + beta[j]*recommendation[i,j]) 
  #  e1[i,j] = exp(alpha1[j] + beta1[j]*recommendation[i,j]) + zeta1[j]*0 # predictor off
   # e2[i,j] = exp(alpha1[j] + beta1[j]*recommendation[i,j]) + zeta1[j]*1 # predictor on and effects all categories
    e2[i,j] = exp(alpha[j] + beta[j]*recommendation[i,j] + zeta[j]*recommendation[i,j]) # predictor on and just effects that category
  }
}

# now make probabilities
for (i in 1:ncat){
  for (j in 1:ncat){
    pi0[i,j] = e0[i,j] / sum(e0[i,1:ncat])
    pi1[i,j] = e1[i,j] / sum(e1[i,1:ncat])
    pi2[i,j] = e2[i,j] / sum(e2[i,1:ncat])
  }
}
pi1

#
to_plot0 = reshape2::melt(pi0)
#to_plot1 = reshape2::melt(pi1)
to_plot2 = reshape2::melt(pi2)
to_plot = bind_rows(to_plot0, to_plot2, .id = 'model')
ggplot(data = to_plot, aes(x=Var1, fill=factor(Var2), y=value))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  ylab('Probability')+
  xlab('')+
#  scale_fill_manual(NULL, labels=breaks_f1000, values=colours_f1000)+
  scale_fill_manual(NULL, labels=breaks_epi, values=colours_epi)+
  theme_bw()+
  facet_wrap(~model)
