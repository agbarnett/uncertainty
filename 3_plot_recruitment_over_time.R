# 3_plot_recruitment_over_time.R
# plot the recruitment by journal over time
# called from 3_summary.Rmd
# November 2022
library(ggplot2)
colours = c('indianred1','skyblue','darkorange')
library(dplyr)
#load('data/0_Reviewers_Responses.RData') # from 0_read_qualtrics_api.R
load('data/2_data.RData') # from 2_read_journal_data.R

# use today as last date
#censor.date = Sys.Date() # only during live recruitment
censor.date = as.Date('2023-04-20') # one day after last survey

# cumulative counts by journal
counts = group_by(data, journal, recorded_date) %>%
  tally() %>%
  arrange(journal, recorded_date) %>%
  group_by(journal) %>%
  mutate(csum = cumsum(n)) %>%
  ungroup()
# add zero at start
zeros = group_by(counts, journal) %>%
  arrange(journal, recorded_date) %>%
  slice(1) %>% # earliest date
  ungroup() %>%
  mutate(csum=0)
# add final date at end
final = group_by(counts, journal) %>%
  arrange(journal, desc(recorded_date)) %>%
  slice(1) %>%
  mutate(recorded_date = censor.date)

#
to_plot = bind_rows(zeros, counts, final) %>% 
  unique() # in case of duplicates due to adding final date

# plot
gplot = ggplot(data=to_plot, aes(x=recorded_date, y=csum, col=journal)) +
  geom_step(size=1.05)+
  scale_color_manual(NULL, values=colours)+
  ylab('Cumulative responses')+
  xlab('Date')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position=c(0.2,0.85),
        plot.margin = margin(0, 5, 0, 0, "mm")) # trbl
#
jpeg('figures/recruitment.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(gplot)
dev.off()

## for text
# average number per week in recent weeks
three_week_cut = Sys.Date() - (3*7)
av_per_week = filter(to_plot, recorded_date >= three_week_cut) %>%
              mutate(year = as.numeric(format(recorded_date, '%Y')),
                 week = as.numeric(format(recorded_date, '%U')),
                 yr_week = year + (week/53)) %>%
  group_by(yr_week) %>%
  summarise(sum = sum(n)) %>%
  ungroup() %>%
  summarise(av = mean(sum)) %>%
  pull(av)
# number of weeks recruitment
recruitment_weeks = summarise(to_plot, min = min(recorded_date)) %>%
  mutate(today = as.Date(Sys.Date()),
         days = today - min,
         weeks = floor(as.numeric(days)/7)) %>%
  pull(weeks)
# weeks until end
weeks_until_end = (400 - sum(counts$n)) / av_per_week
weeks_until_end = ceiling(weeks_until_end)
                                
