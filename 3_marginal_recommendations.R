# 3_marginal_recommendations.R
# Look at the marginal distribution of recommendations
# no longer needed used for protocol
# Sep 2021
library(ggplot2)
library(dplyr)
library(janitor)

# from 0_read_api.R
load('data/historic.RData') 

#
table <- all_reviews %>%
  tabyl(recommendation) %>%
  mutate(percent = round(percent*100),
         label = paste(percent, '%', sep=''))
#
bplot = ggplot(data=all_reviews, aes(x=factor(recommendation)))+
  geom_bar(fill='dodgerblue')+
  geom_text(data=table, aes(x=recommendation, y=n, label=label), nudge_y = -250, col='white', size=5)+
  xlab('')+
  ylab('Count')+
  theme_bw()
bplot

# export for protocol
postscript('protocol/recommendationBar.eps', width=4, height=4)
print(bplot)
dev.off()
