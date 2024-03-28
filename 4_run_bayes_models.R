# 4_run_bayes_models.R
# Bayesian regression models to examine uncertainty; using nimble
# complete case and imputed versions
# ran separate models for each journal because of difference in ordered decision categories
# January 2024
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(nimble)
library(mitools) # for imputation
library(ggplot2)
library(gridExtra)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
source('99_functions.R')

# from 3_summary.Rmd (after journal stuff is added)
load('data/3_not_excluded_data.RData')

# use data from 3_analyses.Rmd that has excluded missing and all probability sums add to 100
# data selections and edits
for_models = select(data_not_excluded, 
                    journal,
                    paper_type, # paper type, examine `protocol`
                    time_diff, # delay in peer review
                    version_number, # version
                    n_words,
                    dale_chall,
                    flesch,
                    q2_2, # time_spent on review
                    q2_3, # gender
                    q2_4, # experience
                    q1_3, # recommendation
                    starts_with('q1_1')) %>%
  mutate(review_time = log2(q2_2 + 1), # log-transformed due to large positive skew, plus one to keep positive
         q2_3 = ifelse(q2_3 == 'Prefer not to say', NA, q2_3), # treat this gender response as missing
         gender = as.numeric(q2_3 == 'Male'), # simplified gender
         female = as.numeric(q2_3 == 'Female'), # simplified gender
         experience = as.numeric(q2_4) - 1, # experience, must be kept positive, so start at zero
         version = as.numeric(version_number == 1), # simplify to version 1 versus 2+
         time_diff = log2(time_diff), # log-transformed due to large positive skew
         protocol = as.numeric(paper_type == 'Protocol'),
         n_words = (n_words-1000)/2000, # scale to per 2000 words
         dale_chall = (dale_chall + 8)/5, # make minimum positive, and scale
         flesch = (flesch-5)/10 ) %>% # scale
  select(-starts_with('q2_')) # thin out columns

# impute missing predictors, created `imputed`
source('4_impute_predictors.R')

# big loop through journals, predictors and imputations
all_journals = c('BMJ Open','F1000','Epidemiology')
all_predictors = c('version','review_time','time_diff','gender','experience','protocol','n_words','flesch','dale_chall','female')
# version to run single models
for (this_journal in all_journals){
  for (this_predictor in all_predictors){ # loop through predictors
    
    # set up files    
    file = paste('4_results_', str_replace(this_journal, ' ', ''), '_', this_predictor, '.RData', sep='')
    outfile = paste('data/', file, sep='')
    if(length(dir('data', pattern = file)) > 0){next} # if file exists then skip
    cat('Predictor = ', this_predictor, '\n\n')
    
    # do not fit combinations where there's little or no variation
    if(this_journal=='Epidemiology' & this_predictor == 'version'){next} # no version difference at Epidemiology (all version 1)
    if(this_journal=='Epidemiology' & this_predictor == 'protocol'){next} # no protocols at Epidemiology
    # do not fit reading data in Epidemilogy or BMJ Open 
    if(this_journal %in% c('BMJ Open','Epidemiology') & this_predictor %in% c('n_words','flesch','dale_chall')){next} 
    
    # change hyper-parameters for models that did not converge with all vague priors
    hyper = NULL
    if(this_journal=='Epidemiology' & this_predictor == 'female'){hyper = c(0.5,0.0001,0.0001,0.0001,0.0001)}
    if(this_journal=='Epidemiology' & this_predictor == 'review_time'){hyper = c(1,0.0001,0.0001,0.0001,0.0001)}
    if(this_journal=='Epidemiology' & this_predictor == 'experience'){hyper = c(1,0.0001,0.0001,0.0001,0.0001)}
    if(this_journal=='Epidemiology' & this_predictor == 'time_diff'){hyper = c(1,0.0001,0.0001,0.0001,0.0001)}
    if(this_journal=='F1000' & this_predictor == 'review_time'){hyper = c(0.0001,0.0001,1)}
    if(this_journal=='F1000' & this_predictor == 'dale_chall'){hyper = c(0.0001,0.0001,1)}
    
    # for storing the results, restart for each journal and predictor combination
    xval = tables = fitted = differences = NULL 
    
    # complete case
    model_results_complete = run_bayes_model(indata = for_models,
                                             this_journal = this_journal,
                                             predictor = this_predictor,
                                             priors = hyper,
                                             imputed = FALSE,
                                             innum = 0) # empty imputation number
    xval = bind_rows(xval, model_results_complete$xval)
    tables = bind_rows(tables, model_results_complete$tables)
    fitted = bind_rows(fitted, model_results_complete$fitted)
    differences = bind_rows(differences, model_results_complete$differences)
    # loop through imputations
    if(!this_predictor %in% c('n_words','flesch','dale_chall')){ # nothing to impute
      coef_m0 = vcov_m0 = coef_m1 = vcov_m1 = coef_m2a = vcov_m2a = coef_m2b = vcov_m2b = NULL
      for (imp in 1:n_impute){ # loop through imputations
        model_results = run_bayes_model(indata = imputed[[imp]],
                                        this_journal = this_journal,
                                        predictor = this_predictor,
                                        priors = hyper,
                                        imputed = TRUE,
                                        innum = imp)
        fitted = bind_rows(fitted, model_results$fitted)
        coef_m0[[imp]] = model_results$coef_m0 # model 0
        vcov_m0[[imp]] = model_results$vcov_m0
        coef_m1[[imp]] = model_results$coef_m1 # model 1
        vcov_m1[[imp]] = model_results$vcov_m1
        coef_m2a[[imp]] = model_results$coef_m2a # model 2a
        vcov_m2a[[imp]] = model_results$vcov_m2a
        coef_m2b[[imp]] = model_results$coef_m2b # model 2b
        vcov_m2b[[imp]] = model_results$vcov_m2b
      } # end of imputation loop
      
      # combine imputed results
      combined_imputed1 = my_combine_imputed(coef_m0, vcov_m0, modelnum = 1, this_journal = this_journal, this_predictor = this_predictor) 
      combined_imputed2 = my_combine_imputed(coef_m1, vcov_m1, modelnum = 2, this_journal = this_journal, this_predictor = this_predictor) 
      combined_imputed3 = my_combine_imputed(coef_m2a, vcov_m2a, modelnum = 3, this_journal = this_journal, this_predictor = this_predictor) 
      combined_imputed4 = my_combine_imputed(coef_m2b, vcov_m2b, modelnum = 4, this_journal = this_journal, this_predictor = this_predictor) 
      # add tables to data
      tables = bind_rows(tables, combined_imputed1, combined_imputed2, combined_imputed3, combined_imputed4) %>%
        filter(!str_detect(parameter, 'pi.fit')) # remove fitted values
    } # end of if to imputation
    
    # store the results
    save(xval, tables, fitted, differences, file = outfile)
    
  } # end of predictor loop
} # end of journal loop

## run model 0 in every journal to give the largest sample size with no missing predictor data
for_models = mutate(for_models, dummy = 1) # dummy predictor that is not missing
for (this_journal in all_journals){
  
  this_predictor = 'no_predictor' # model 0
  
  # set up files    
  file = paste('4_results_', str_replace(this_journal, ' ', ''), '_', this_predictor, '.RData', sep='')
  outfile = paste('data/', file, sep='')
  if(length(dir('data', pattern = file)) > 0){next} # if file exists then skip
  
  # for storing the results, restart for each journal and predictor combination
  xval = tables = fitted = NULL 
  
  this_predictor = 'dummy'
  
  # complete case
  model_results_complete = run_bayes_model(indata = for_models,
                                           this_journal = this_journal,
                                           predictor = this_predictor,
                                           no_predictors = TRUE, # flag to run model 0 only
                                           #priors = hyper, # not needed
                                           imputed = FALSE,
                                           innum = 99) # empty imputation number, without triggering x-validation
  xval = bind_rows(xval, model_results_complete$xval)
  tables = bind_rows(tables, model_results_complete$tables)
  fitted = bind_rows(fitted, model_results_complete$fitted)
  
  # store the results
  save(xval, tables, fitted, file = outfile)
  
}
