# 5_plot_summary.R
# called by 5_analyses.Rmd
# Feb 2024

# from 3_summary.Rmd (after journal stuff is added)
load('data/3_not_excluded_data.RData')
# switch to long, get average by journal and recommendation
percent_summary = select(data_not_excluded, journal, q1_3, starts_with('q1_1_')) %>%
  pivot_longer(-c(journal, q1_3)) %>%
  filter(!is.na(value),
         name != 'q1_1_6') %>% # exclude 'none'
  group_by(journal, q1_3, name) %>%
  summarise(mean = mean(value)) %>%
  mutate(
    q1_3_n = case_when(
      q1_3 == 'Accept' ~ 1,
      q1_3 == 'Approved' ~ 2,
      q1_3 == 'Approved with reservations' ~ 3,
      q1_3 == 'Accept with minor revisions' ~ 4,
      q1_3 == 'Minor revisions' ~ 5,
      q1_3 == 'Reconsider after revisions' ~ 6,
      q1_3 == 'Uncertain, needs major revision' ~ 7,
      q1_3 == 'Major revisions' ~ 8,
      q1_3 == 'Reject' ~ 9,
      q1_3 == 'Not approved' ~ 10
    ),
    namef = case_when(
      journal == 'F1000' & name == 'q1_1_1' ~ 1, # 'Approved / Accept
      journal == 'F1000' & name == 'q1_1_2' ~ 2, # 'Approved with reservations',
      journal == 'F1000' & name == 'q1_1_3' ~ 5, # 'Not approved',
      journal == 'Epidemiology' & name == 'q1_1_1' ~ 1, #  Approved / Accept
      journal == 'Epidemiology' & name == 'q1_1_2' ~ 2, #  Accept with minor revisions / Minor revisions
      journal == 'Epidemiology' & name == 'q1_1_3' ~ 3, #  Reconsider after revisions
      journal == 'Epidemiology' & name == 'q1_1_4' ~ 4, #  'Uncertain, needs major revision / Major revision'
      journal == 'Epidemiology' & name == 'q1_1_5' ~ 5, #  
      journal == 'BMJ Open' & name == 'q1_1_1' ~ 1, #  'Approved / Accept
      journal == 'BMJ Open' & name == 'q1_1_2' ~ 2, # Accept with minor revisions / Minor revisions
      journal == 'BMJ Open' & name == 'q1_1_3' ~ 4, # 'Uncertain, needs major revision / Major revision'
      journal == 'BMJ Open' & name == 'q1_1_5' ~ 5 # 
    ),
    namef = factor(as.numeric(namef)))
#
combined_breaks = c('Accept', 
                    'Approved', 
                    'Approved with\nreservations',
                    'Accept with minor\nrevisions', 
                    'Minor revisions', 
                    'Reconsider after\ revisions',
                    'Uncertain, needs\nmajor revision',
                    'Major revisions', 
                    'Reject',
                    'Not approved')
combined_colours = c('darkseagreen3','yellow2','orange','darkorange3','indianred4')
combined_labels = c('Approved / Accept', 
                    'Approved with reservations /\nAccept with minor revisions /\nMinor revisions',
                    'Reconsider after revisions',
                    'Uncertain, needs major revision /\nMajor revision',
                    'Reject')
pplot = ggplot(data = percent_summary, aes(x=factor(q1_3_n), y=mean, fill=namef))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  scale_fill_manual(NULL, breaks=1:5, values = combined_colours, labels= combined_labels)+
  ylab('Percentage')+
  xlab('')+
  scale_x_discrete(expand = c(0,0), breaks = 1:10, labels= combined_breaks)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  facet_wrap(~journal, scales='free')

# export
jpeg('figures/summary_recommendations.jpg', width = 7, height = 5.5, units = 'in', res = 600, quality = 100)
print(pplot)
invisible(dev.off())

#### alternative version ####

# split per journal and with modal percentage in bar
mode = group_by(percent_summary, journal, q1_3, q1_3_n) %>%
  arrange(journal, q1_3, q1_3_n , desc(mean)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(label = paste(round(mean),'%',sep='')) %>% # change shade of grey by bars
  select(journal, q1_3_n, name, label) 
# add back labels, needed for geom_text
percent_summary = left_join(percent_summary, mode, by=c('journal', 'q1_3_n', 'name')) 
  

#
xlab = 'Actual recommendation'
low_colour = 'grey88'
high_colour = 'black'
data_f1000 = data = filter(percent_summary, journal=='F1000')
plot_f1000 = ggplot(data = data_f1000, aes(x=factor(q1_3_n), y=mean, fill=name))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  geom_text(aes(label=label, color=q1_3_n), position = position_stack(vjust=0.5))+
  scale_fill_manual("Considered\nrecommendations:", values = colours_f1000, labels= breaks_f1000_return)+
  scale_colour_gradient(NULL, low=low_colour, high=high_colour)+
  ylab('Percentage')+
  xlab(xlab)+
  scale_x_discrete(expand = c(0,0), breaks=c(2,3,10), labels= breaks_f1000_return)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ggtitle('F1000')+ 
  guides(color="none") # suppress colour legend
#
data_bmj = data = filter(percent_summary, journal=='BMJ Open')
plot_bmj = ggplot(data = data_bmj, aes(x=factor(q1_3_n), y=mean, fill=name))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  geom_text(aes(label=label, color=q1_3_n), position = position_stack(vjust=0.5))+
  scale_fill_manual("Considered\nrecommendations:", values = colours_bmj, labels= breaks_bmj_return)+
  scale_colour_gradient(NULL, low=low_colour, high=high_colour)+
  ylab('Percentage')+
  xlab(xlab)+
  scale_x_discrete(expand = c(0,0), breaks = c(1,5,8,9), labels= breaks_bmj_return)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ggtitle('BMJ Open')+ 
  guides(color="none") # suppress colour legend
#
data_epi= data = filter(percent_summary, journal=='Epidemiology')
plot_epi = ggplot(data = data_epi, aes(x=factor(q1_3_n), y=mean, fill=name))+
  geom_bar(stat='identity', position='stack', width = 0.99)+
  geom_text(aes(label=label, color=q1_3_n), position = position_stack(vjust=0.5))+
  scale_fill_manual("Considered\nrecommendations:", values = colours_epi_no_none, labels= breaks_epi_no_none_return)+
  scale_colour_gradient(NULL, low=low_colour, high=high_colour)+ # for text colour
  ylab('Percentage')+
  xlab(xlab)+
  scale_x_discrete(expand = c(0,0), breaks=c(1,4,6,7,9), labels= breaks_epi_no_none_return)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ggtitle('Epidemiology')+ 
  guides(color="none") # suppress colour legend

# export, 
lmat = matrix(c(1,1,1,2,2,2,3,3,3,3,NA,NA), byrow=T, ncol=6) # bit more room for epi
jpeg('figures/summary_recommendations_split.jpg', width = 9, height = 7, units = 'in', res = 600, quality = 100)
grid.arrange(plot_bmj, plot_f1000, plot_epi, layout_matrix = lmat)
invisible(dev.off())
