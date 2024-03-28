# 5_plot_tile_summary_model.R
# tile plot show the best model for each journal/predictor/imputed combination
# called by 5_analysis.Rmd
# Feb 2024
library(tidyverse)

# 1: choose best version of model 2
model_1 = filter(xval, model == 1, imputed =='Complete case') # do not use imputed
model_2 = filter(xval, model == 2, imputed =='Complete case')
best_3_4 = filter(xval, model >= 3, imputed =='Complete case') %>%
  group_by(journal, predictor) %>%
  arrange(journal, predictor, xval) %>%
  slice(1) %>% # take the model with the smallest mean square error
  ungroup()
# 2: select if mean and/or variance are better than null model   
models_2_3_4 = bind_rows(model_2, best_3_4)
# best null model
best_null = mutate(model_1, best = xval - se) %>%
  select(journal, predictor, best)
best = left_join(models_2_3_4, best_null, by=c('journal','predictor')) %>%
  mutate(better = xval < best, # does model MSE beat null?
         pred_nice = nice_rename(predictor))

# switch to wide
best_wide = select(best, journal, pred_nice, model, better) %>%
  group_by(journal, pred_nice) %>%
  pivot_wider(names_from = model, values_from = better) %>%
  ungroup() %>%
  mutate(order = case_when( # create variable for ordering on y-axis (highest number top); match table in appendix
    pred_nice == "Male reviewers" ~ 10, # reviewer variables
    pred_nice == "Female reviewers" ~ 9, # reviewer variables
    pred_nice == "Reviewer's experience" ~ 8,
    pred_nice == "Time spent on review" ~ 7,
    pred_nice == "Version" ~ 6, # paper variables
    pred_nice == "Time to peer review" ~ 5,
    pred_nice == "Protocol" ~ 4,
    pred_nice == "Number of words" ~ 3,
    pred_nice == "Flesch readability" ~ 2,
    pred_nice == "Dale-Chall readability" ~ 1
  )) %>%
  arrange(order, journal)

# Make the triangular data for the tiles
newcoord_up <- make_triangles(best_wide$journal, best_wide$order)
newcoord_down <- make_triangles(best_wide$journal, best_wide$order, point = "down")
# just a dirty trick for renaming
newcoord_down <- newcoord_down %>% select(xdown = x, ydown = y)
# repeat each row of your previous data frame 3 times
repdata <- map_df(1:nrow(best_wide), function(i) best_wide[rep(i, 3), ])
newdata <- bind_cols(repdata, newcoord_up, newcoord_down) %>%
  mutate(two = as.numeric(`2`)+1,
         `3` = ifelse(is.na(`3`),FALSE, `3`),
         `4` = ifelse(is.na(`4`),FALSE, `4`),
         three = 1 + as.numeric(`3`)+as.numeric(`4`), # just do either uncertainty
         two = as.factor(two),
         three = as.factor(three),
         journal = factor(journal),
         pred_nice= factor(pred_nice))

## get the best model based on the cross-validation for the text label
very_best = filter(best, better==TRUE) %>%
  arrange(journal, predictor, best) %>%
  group_by(journal, predictor) %>%
  slice(1) %>% # best out of mean or variance
  ungroup() %>%
  select(journal, predictor, model) %>%
  rename('best' = 'model') %>%
  mutate(pred_nice = nice_rename(predictor))
# merge with plot data
plot_median = group_by(newdata, journal, pred_nice) %>% # try to get the coordinates at the middle of the triangle
  summarise(xleft = mean(x), # for (x,y) coordinates
            xright = mean(xdown), # for (x,y) coordinates
            yleft = mean(y), # for (x,y) coordinates
            yright = mean(ydown)) %>%
  ungroup()
very_best_plot = left_join(very_best, plot_median, by = c('journal','pred_nice')) %>%
  mutate(x = ifelse(best == 2, xleft, xright),
         y = ifelse(best == 2, yleft, yright),
         label = '*') # label to highlight best model

## plot
ylevels = select(newdata, order, pred_nice) %>%
  unique() %>%
  pull(pred_nice) # get ordered y-levels
#
tplot = ggplot(newdata) +
  geom_polygon(aes(x = x, y = y, fill = two, group = interaction(journal, order)), color = "black") +
  scale_fill_manual("Effect on\nmean", values = c("grey92", "darkseagreen3"), labels=c('No','Yes')) +
  ggnewscale::new_scale_fill() +
  geom_polygon(aes(x = xdown, y = ydown, fill = three, group = interaction(journal, order)), color = "black") +
  scale_fill_manual("Effect on\nuncertainty", values = c("grey92", "skyblue"), labels=c('No','Yes')) +
#  geom_text(data = very_best_plot, aes(x = x,y = y,label = label), size=12, col='white') + # no longer needed
  coord_equal(xlim = c(0.5, 3.5), clip = "off") + # needed for annotate outside plot region
  g.theme+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))+
  xlab('')+
  ylab('')+
  annotate('text', x=-1.5, y=5, label='--- Reviewer ---', angle=90)+ # trial and error with x
  annotate('text', x=-1.5, y=2, label='--- Journal ---', angle=90)+
  scale_x_continuous(expand=c(0,0), breaks=1:3, labels = levels(newdata$journal))+
  scale_y_continuous(expand=c(0,0), breaks=1:10, labels = ylevels)

# export
jpeg('figures/tile_plot_best_model.jpg', width=6.5, height=5.5, units='in', res=500, quality=100)
print(tplot)
dev.off()
