# 5_crossval_table.R
# table of cross-validated errors; used in appendix of paper
# March 2024
library(tidyr)
library(xtable)
library(dplyr)
options(dplyr.summarise.inform = FALSE)

# combine the data sets of results from 4_run_bayes_models.R
source('5_combine_results.R')

#
xval = filter(xval, imputed == 'Complete case') %>%
  select(-imputed, -innum)

# models 0 and 1 (numbered 1 and 2)
not_2 = filter(xval, model <= 2)
# get best model 2 (numbered 3 and 4)
best_2 = filter(xval, model >= 3) %>%
  group_by(journal, predictor) %>%
  arrange(journal, predictor, xval) %>%
  slice(1) %>% # take best out of 2
  ungroup() %>%
  mutate(model = 3) # over-write

#
to_table = bind_rows(not_2, best_2) %>%
  mutate(model = model - 1) # match numbers in paper

# switch to wide
wide = select(to_table, model, journal, predictor, xval) %>%
  pivot_wider(names_from = 'model', values_from = 'xval') %>%
  mutate(`diff1` = `1` - `0`, # calculate difference
         `diff2` = `2` - `0`) %>%
  select(journal, predictor, diff1, diff2)
  
# output to latex
print(xtable(wide, digits=4), include.rownames=FALSE, math.style.negative=TRUE)
