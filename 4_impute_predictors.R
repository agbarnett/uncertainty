# 4_impute_predictors.R
# impute missing predictors using chained regressions
# called by 4_run_bayes_models.R
# Jan 2024
seed = TeachingDemos::char2seed('portsmouth')
library(mice)
predictors_to_impute = c('version','protocol','experience','gender','review_time','time_diff')

## add index to original data for merging below
for_models = mutate(for_models, id = 1:n())

## how many imputations should we run? Answer: equal to the percentage of incomplete cases
n_impute = select(for_models, all_of(predictors_to_impute)) %>%
  mutate(is.complete = complete.cases(.)) %>%
  summarise(n = n(), 
            r = sum(!is.complete),
            perc = ceiling(100*r/n)) %>%
  pull(perc)

# run imputation
imp_mice = mice(for_models, maxit = 20, m = n_impute, seed = seed)


# graphical check
# stripplot(imp_mice, review_time, pch = 19, xlab = "Imputation number")

# transform complete imputations to a list (works better with previous programming)
imputed = list()
for (k in 1:n_impute){
  # chained regressions only worked when imputing all missing, but we can keep original q1_1_x variables
  this_complete = complete(imp_mice, k)
  just_missing = select(this_complete, 'id', all_of(predictors_to_impute))
  not_missing = select(for_models, 'id', !all_of(predictors_to_impute))
  # now merge the two
  imputed[[k]] = full_join(not_missing, just_missing, by='id') %>% select(-id)
}

# check that imputation worked
sapply(for_models, function(x) sum(is.na(x)))
sapply(imputed[[1]], function(x) sum(is.na(x)))

# no longer need index
for_models = select(for_models, -id)