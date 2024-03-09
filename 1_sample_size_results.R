# 1_sample_size_results.R
# results of sample size
# Oct 2021
library(dplyr)
library(ggplot2)
library(binom)

# get results from lyra
setwd("//hpc-fs/barnetta/uncertainty/results")
d = dir()
all_dic = all_beta = NULL
for (l in d){
  load(l)
  all_dic = bind_rows(all_dic, dic_results)
  all_beta = bind_rows(all_beta, sim_results)
}
setwd("U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/uncertainty")

## results summary
# average difference
all_dic = mutate(all_dic, 
                     diff2 = DIC2 - DIC1,
                     diff3 = DIC3 - DIC1)
group_by(all_dic, variance) %>%
  summarise(n=n(), mean(diff2), mean(diff3))
# model selection based on DIC
power = mutate(all_dic, 
       sig2 = DIC2 - DIC1 < -10,
       sig3 = DIC3 - DIC1 < -10) %>% # 
  pivot_longer(cols=starts_with('sig')) %>%
  group_by(variance, name, value) %>%
  tally() %>%
  pivot_wider(values_from = 'n', names_from = 'value') %>%
  mutate(
    r = `TRUE`,
    n = `TRUE` + `FALSE`,
    ci = binom.asymp(r, n),
    p = ci$mean,
    lower = ci$lower,
    upper = ci$upper)
power
# plot power by variance - to do
pplot = ggplot(data=filter(power, name=='sig3'), aes(x=variance, y=p, ymin=lower, ymax=upper))+
  geom_point(color='dodgerblue', size=4)+
  geom_errorbar(width=0, color='dodgerblue')+
  geom_line(color='dodgerblue', size=1.05)+
  xlab('Percent increase in variance')+
  scale_x_continuous(breaks=c(1,1.05,1.1,1.15,1.2), labels=c('0','5','10','15','20'))+
  ylab('Probability of choosing model 2')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
pplot
postscript('protocol/powercurve.eps', width=5, height=4)
print(pplot)
dev.off()


# plot DIC
ggplot(data=all_dic, aes(x=factor(variance), y=diff2)) +
  geom_boxplot()

# using statistical significance for parameters
mutate(all_beta, 
       sig = pvalue < 0.05) %>%
  group_by(variance, model, row) %>%
  summarise(n=n(), r = sum(sig)) %>%
  mutate(p = r/ n)

