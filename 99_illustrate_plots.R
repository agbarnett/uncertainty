# 99_illustrate_plots.R
# illustrate how maximum and minimum uncertainty would look like in fitted values
# Jan 2024
library(dplyr)
library(ggplot2)
library(gridExtra)
source('99_functions.R')

# can vary this to get different plots
this_journal = 'BMJ Open'
return = TRUE
xlab = 'Actual recommendation'

# labels dependent on journal
source('3_colours.R') # colour-schemes for each journal
margin = c(2,0,0,0) # top, right, bottom, left; scale = pt; reduce gaps between panels
if(this_journal=='F1000'){
  alabels = breaks_f1000
  if(return==TRUE){alabels = breaks_f1000_return}
  colours = colours_f1000
}
if(this_journal=='BMJ Open'){
  alabels = breaks_bmj
  if(return==TRUE){alabels = breaks_bmj_return}
  colours = colours_bmj
}
if(this_journal=='Epidemiology'){
  alabels = breaks_epi_no_none
  if(return==TRUE){alabels = breaks_epi_no_none_return}
  colours = colours_epi_no_none
}
ncat = length(alabels) # number of recommendation categories for this journal

## data for no uncertainty
no_uncertainty = NULL
for (r in 1:ncat){
  for (u in 1:ncat){
    prob = ifelse(r==u, 1, 0)
    f = data.frame(recommendation = r,
                   uncertainty = u,
                   prob = prob)
    no_uncertainty = bind_rows(no_uncertainty, f)
  }
}
## data for maximum uncertainty
max_uncertainty = NULL
for (r in 1:ncat){
  for (u in 1:ncat){
    prob = 1 / ncat
    f = data.frame(recommendation = r,
                   uncertainty = u,
                   prob = prob)
    max_uncertainty = bind_rows(max_uncertainty, f)
  }
}


## plots
#
plot_no_uncertainty = ggplot(data = no_uncertainty, aes(x=recommendation, y=prob, fill=factor(uncertainty)))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  ylab('Probability')+
  xlab(xlab)+
  scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(NULL, labels=alabels, values=colours)+
  theme_bw()+
  ggtitle('Minimum uncertainty')+
  theme(panel.grid.minor = element_blank(), 
        plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
        legend.position = 'none')
#
plot_max_uncertainty = ggplot(data = max_uncertainty, aes(x=recommendation, y=prob, fill=factor(uncertainty)))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  ylab('')+ # blank here, in other panel
  xlab(xlab)+
  scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Considered recommendations:", labels=alabels, values=colours)+
  theme_bw()+
  ggtitle('Maximum uncertainty')+
  theme(panel.grid.minor = element_blank(), 
        plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
        legend.position = 'top')
# legend
glegend = g_legend(plot_max_uncertainty) # get legend ... 
plot_max_uncertainty = plot_max_uncertainty + theme(legend.position = 'none')  # ... now turn off legend

# export to figure
lay = rbind(c(1,1), # for legend
            c(2,3))
jpeg('figures/example_uncertainty.jpg', width=7, height=5, units='in', res=400)
grid.arrange(glegend, plot_no_uncertainty, plot_max_uncertainty, heights=c(0.1,1),layout_matrix = lay)
dev.off()
