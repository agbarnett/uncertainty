# 5_combine_results.R
# combine the results from 4_run_bayes_models.R
# called by 5_analysis.Rmd
# Feb 2024

# start with empty data
res_xval = res_tables = res_fitted = res_differences = NULL

# find all files to combine
here = getwd()
setwd('data')
to_combine = dir(pattern = '^4_results')
for (f in to_combine){
  load(f)
  res_xval = bind_rows(res_xval, xval)
  res_tables = bind_rows(res_tables, tables)
  res_fitted = bind_rows(res_fitted, fitted)
  if(any(ls() == 'differences')){res_differences = bind_rows(res_differences, differences)}
}
setwd(here)

# rename
fitted = res_fitted
tables = res_tables
xval = res_xval
differences = res_differences
differences = unique(differences) # getting repeats because only available for some journal-predictor combinations
rm(res_fitted, res_xval, res_tables, res_differences)

# add CIs to x-validated results
xval = mutate(xval, 
              imputed = ifelse(imputed=='TRUE','Imputed','Complete case'),
              model = as.numeric(model),
              z = qnorm(0.975),
              lower = xval - (z*se),
              upper = xval + (z*se)) %>%
  select(-z)
