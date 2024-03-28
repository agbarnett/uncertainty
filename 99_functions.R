# 99_functions.R
# November 2022

## make separate legend for plotting
# from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# function for posterior probability
post_pval = function(inres){ # pos must be 0/1 or true/false
  if(class(inres)[1] == 'matrix'){
    MCMC = nrow(inres)
    pos = colMeans(inres)
    pos.dash = 1 - pos
    pval = pmax(2*pmin(pos, pos.dash), 1/MCMC)
  }
  if(class(inres)[1] == 'logical'){
    MCMC = length(inres)
    pos = mean(inres)
    pos.dash = 1 - pos
    pval = max(2*min(pos, pos.dash), 1/MCMC) # cannot be zero
  }
  return(pval)
}

## fiddly edits to time question
q2_edits = function(input){
  out = case_when(
    input == 'Six days' ~ '144',
    input == '3days' ~ '72',
    input == '150 (first and revised version together)' ~ '75', # halved as we only want one paper
    input == '1 day' ~ '24',
    str_detect(input, '^I had a cursory look at the paper') ~ '20', # long screed, but says 20 hours at the end!
    input == '18hrs roughly' ~ '18',
    str_detect(input, '^At least, 15 hours') ~ '15',
    str_detect(input, '^14 including') ~ '14',
    input == '12-15 hours' ~ '13.5',
    str_detect(input, '^It might have been 13.5 hours') ~ '13.5',
    input == 'Six time (12 hours)' ~ '12',
    str_detect(input, '^12 - this is difficult to say') ~ '12',
    input == '10-12 hrs' ~ '11',
    input == '8-10' ~ '9',
    input == '8:30' ~ '8.5',
    input == '8 HOURS' ~ '8',
    str_detect(input, '^8 hours') ~ '8',
    str_detect(input, '^I spent 5 hours reading and jottign down') ~ '8', # detailed text, add to 8
    input == 'About 8 hours' ~ '8',
    input == '6-8' ~ '7',
    input == '6-7' ~ '6.5',
    input == 'about 6 hours since I need to read some recent methodology papers as well' ~ '6',
    input == 'six hours' ~ '6',
    input == '5-6 hrs' ~ '5.5',
    input == '5-6 hours' ~ '5.5',
    input == '4-6 hours' ~ '5',
    input == '4-5 hours' ~ '4.5',
    input == '4 hours 30 minutes' ~ '4.5',
    input == '4 hours 30 minutes' ~ '4.5',
    input == '4h' ~ '4',
    str_detect(input, '^I spent 4 hours') ~ '4',
    input == '4 hours in total for original and revision' ~ '2', # halved as we only want one paper
    input == '3.5-4' ~ '3.75',
    input == '3-4 hours' ~ '3.5',
    input == '3-4' ~ '3.5',
    input == '3 hours.' ~ '3',
    input == '3 hours, over 2 days' ~ '3',
    input == '3 HRS' ~ '3',
    input == '3 h' ~ '3',
    input == 'THREE' ~ '3',
    input == 'Around 3 hours' ~ '3',
    str_detect(input, '^around 3 hours') ~ '3',
    input == '3h' ~ '3',
    input == 'Three hours' ~ '3',
    input == '2.5-3 hours' ~ '2.75',
    input == '2-3 hours' ~ '2.5',
    str_detect(input, '2,5 uur') ~ '2.5',
    input == 'two hours' ~ '2',
    input == 'about 2h' ~ '2',
    str_detect(input, '2 hours on the second version') ~ '2',
    input == '2h' ~ '2',
    input == '2,5' ~ '2.5',
    input == 'Two - would usually be more' ~ '2',
    input == 'About 90 minutes.' ~ '1.5',
    input == '90 minutes' ~ '1.5',
    input == '1-2 hours' ~ '1.5',
    input == '1-2' ~ '1.5',
    input == '1-1.5 hours' ~ '1.25',
    input == '1 hour on this the second review, several hours on prior first review' ~ '1',
    input == '1 hour' ~ '1',
    input == '60 min' ~ '1',
    str_detect(input, '^1 hour-ish') ~ '1',
    input == '1 h' ~ '1',
    str_detect(input, '^Since this was the revised version') ~ '1',
    input == '40 minutes' ~ '0.66',
    input == 'Ist review 1.5 hrs. 2nd review 0.5 hours' ~ '0.5', # assuming this must be the second review
    input == '20 minutes' ~ '0.33',
    #input == '' ~ '',
    TRUE ~ as.character(input)
  )
  return(out)
}


# redistribute for those whose numbers did not add to 100
redistribute = function(indata){
  ## can process all data, regardless of redistribute = TRUE/FALSE, as the function does nothing to rows with "TRUE"
  ## depends on journal because of different questions for uncertainty
  #
  f1000 = filter(indata, journal =='F1000') %>%
    mutate(total = q1_1_1 + q1_1_2 + q1_1_3,
           q1_1_1 = round(100 * q1_1_1 / total), # must be intergers
           q1_1_2 = round(100 * q1_1_2 / total),
           q1_1_3 = round(100 * q1_1_3 / total))
  #
  epi = filter(indata, journal =='Epidemiology') %>%
    mutate(total = q1_1_1 + q1_1_2 + q1_1_3 + q1_1_4 + q1_1_5, # exclude none for this, just one person with 5% in none, q1_1_6
           q1_1_1 = round(100 * q1_1_1 / total),
           q1_1_2 = round(100 * q1_1_2 / total),
           q1_1_3 = round(100 * q1_1_3 / total),
           q1_1_4 = round(100 * q1_1_4 / total),
           q1_1_5 = round(100 * q1_1_5 / total))
  #
  bmj = filter(indata, journal =='BMJ Open') %>%
    mutate(total = q1_1_1 + q1_1_2 + q1_1_3 + q1_1_5,  # 4 not used
           q1_1_1 = round(100 * q1_1_1 / total),
           q1_1_2 = round(100 * q1_1_2 / total),
           q1_1_3 = round(100 * q1_1_3 / total),
           q1_1_5 = round(100 * q1_1_5 / total)) # 4 not used
  # return
  outdata = bind_rows(f1000, epi, bmj)
  return(outdata)
}
# checks, over 100, dead on, under 100
#redistribute(data.frame(journal='F1000',q1_1_1=50, q1_1_2=50, q1_1_3=50,q1_1_4=NA, q1_1_5=NA, q1_1_6=NA))
#redistribute(data.frame(journal='F1000',q1_1_1=50, q1_1_2=0, q1_1_3=50,q1_1_4=NA, q1_1_5=NA, q1_1_6=NA))
#redistribute(data.frame(journal='F1000',q1_1_1=20, q1_1_2=0, q1_1_3=20,q1_1_4=NA, q1_1_5=NA, q1_1_6=NA))

## rename variables (columns) from labels, e.g., `q1_3`, to names
rename_q = function(indata){
  names = names(indata)
  if(any(names == 'q1_1_1')){indata = rename(indata, 'Uncertainty 1' = 'q1_1_1')}
  if(any(names == 'q1_1_2')){indata = rename(indata, 'Uncertainty 2' = 'q1_1_2')}
  if(any(names == 'q1_1_3')){indata = rename(indata, 'Uncertainty 3' = 'q1_1_3')}
  if(any(names == 'q1_1_4')){indata = rename(indata, 'Uncertainty 4' = 'q1_1_4')}
  if(any(names == 'q1_1_5')){indata = rename(indata, 'Uncertainty 5' = 'q1_1_5')}
  if(any(names == 'q1_1_6')){indata = rename(indata, 'Uncertainty 6' = 'q1_1_6')}
  if(any(names == 'q1_3')){indata = rename(indata, 'Recommendation' = 'q1_3')}
  if(any(names == 'q1_4')){indata = rename(indata, 'Paper title' = 'q1_4')}
  if(any(names == 'q2_2')){indata = rename(indata, 'Time spent on review' = 'q2_2')}
  if(any(names == 'q2_3')){indata = rename(indata, 'Gender' = 'q2_3')}
  if(any(names == 'q2_4')){indata = rename(indata, 'Years in research' = 'q2_4')}
  if(any(names == 'q2_5')){indata = rename(indata, 'Country' = 'q2_5')}
  if(any(names == 'time_diff')){indata = rename(indata, 'Time between submission and review' = 'time_diff')}
  if(any(names == 'paper_type')){indata = rename(indata, 'Paper type' = 'paper_type')}
  if(any(names == 'version_number')){indata = rename(indata, 'Version number' = 'version_number')}
  if(any(names == 'decision')){indata = rename(indata, 'Editorial decision' = 'decision')}
  #
  return(indata)
}

## function to make a missing data plot and table
make_missing_plot = function(indata, injournal, inbreaks = miss.breaks){
  # remove some uncertainty variables that were not relevant for that journal
  if(injournal == 'BMJ Open'){
    inbreaks = inbreaks[!inbreaks %in% c('Uncertainty 4', 'Uncertainty 6')]
  }
  if(injournal == 'F1000'){
    inbreaks = inbreaks[!inbreaks %in% c('Uncertainty 4', 'Uncertainty 5', 'Uncertainty 6')]
  }
  # data to use
  for_missing = filter(indata, journal == injournal) %>%
    select(starts_with('q'), 'version_number', 'decision', 'time_diff', 'paper_type') %>%
    select(-q1_5) # do not need comments
  # rename variables 
  for_missing = rename_q(for_missing) %>%
    select(all_of(inbreaks)) # just variables that can be missing
  
  ## create plot
  v = vis_miss(for_missing) # get data from vis_miss function ... 
  # ... now plot as tile, had to do this in order to use breaks in scale_x_continuous
  for_plot = mutate(v$data, variable = factor(variable, levels = rev(inbreaks))) %>%
    filter(!is.na(variable))
  vplot = ggplot(data=for_plot, aes(x = variable, y=rows, fill=value))+  
    geom_tile()+
    scale_fill_manual('Missing', values=c('grey88','black'))+
    xlab('')+
    ylab('Review number') +
    coord_flip()+
    theme(axis.text.x = element_text(angle=0))+
    scale_x_discrete(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(legend.position = 'bottom')
  
  ## table
  table = group_by(for_plot, variable, value) %>%
    tally() %>%
    mutate(value = ifelse(value==FALSE, 'Not missing','Missing')) %>%
    pivot_wider(names_from = 'value', values_from = 'n') %>%
    mutate(journal = injournal,
           `Missing` = ifelse(is.na(`Missing`), 0, `Missing`),
           `Not missing` = ifelse(is.na(`Not missing`), 0, `Not missing`),
           perc = round(100*Missing / (Missing + `Not missing`)),
           Missing = paste(Missing, ' (', perc, '%)', sep='')) %>%
    select(journal, variable, `Not missing`, Missing) %>%
    arrange(`Not missing`)
  
  ## return
  to_return = list()
  to_return$plot = vplot
  to_return$table = table
  return(to_return)
}


#### process results from nimble ####
process_nimble = function(indata, 
                          inmcmc, 
                          difference = FALSE, # calculate difference in probabilities
                          ncat, # number of categories for the peer review response
                          predictions = FALSE){
  
  ## table
  # extract summaries
  table = as.data.frame(inmcmc$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]|\\.')) 
  
  # add posterior p-values
  pos = rbind(inmcmc$samples$chain1,inmcmc$samples$chain2) > 0 # both chains
  pval = post_pval(pos)
  table = bind_cols(table, pval)
  names(table) = c('parameter','mean','median','sd','lower','upper','row','post_prob')

  ## simple predictions for marginal means
  marginal = filter(table, str_detect(parameter, 'fit')) %>%
    separate(col=row, into=c('recommendation','uncertainty'), remove = TRUE, sep=',', convert=TRUE)
  #
  table = filter(table, !str_detect(parameter, 'fit')) # remove fitted values from table
  
  ## predictions and residuals (only if mu added to monitor)
  residuals = NULL
  if(predictions == TRUE){
    ## predictions and residuals
    fitted = filter(table, str_detect(parameter, pattern='^mu')) %>% # model predictions
      select(row, mean) %>%
      mutate(row = as.numeric(row)) %>%
      bind_cols(indata)
    names(fitted)[3] = 'observed'
    
    #
    residuals = mutate(fitted, residual = observed - mean)
    
    # now remove fitted values from table
    table = filter(table, 
                   !str_detect(parameter, pattern='^mu'))
  }
  
  ## difference in fitted probabilities
  p_results = NULL
  if(difference==TRUE){
    chains = rbind(inmcmc$samples$chain1,inmcmc$samples$chain2)
    index = str_detect(colnames(chains),'pi\\.fit')
    chains = chains[,index]
    for (j in 1:ncat){
      for (k in 1:ncat){
        index1 = colnames(chains) == paste('pi.fit[',j, ', ', k, ']', sep ='') # from the prediction matrix with predictor = 0
        index2 = colnames(chains) == paste('pi.fit[',j+ncat, ', ', k, ']', sep ='') # from the prediction matrix with predictor = 1
        if(sum(index1)>1 | sum(index2)>1){cat("error, multiple matches\n")}
        p_diff = chains[,index1] - chains[,index2] # predictor = 0 minus predictor = 1
        pval = post_pval(p_diff > 0) # calculate posterior probability
        frame = data.frame(recommendation = j, uncertainty = k, mean = mean(p_diff), lower = quantile(p_diff, 0.025), upper = quantile(p_diff, 0.975), post_prob = pval)
        p_results = bind_rows(p_results, frame)
      }
    }
  }
  rownames(p_results) = NULL
  
  ## model fit, now using cross-validation, not MCMC
  #model_fit = data.frame(WAIC = inmcmc$WAIC$WAIC,
  #                       pWAIC = inmcmc$WAIC$pWAIC)
  model_fit = 0
  
  ##
  to_return = list()
  to_return$table = table
  to_return$marginal = marginal
  to_return$residuals = residuals
  to_return$model_fit = model_fit
  to_return$p_diff = p_results
  return(to_return)
}


#### function to run Bayes models in nimble; runs three models ####
run_bayes_model = function(indata = for_models,
                     this_journal = 'F1000',
                     predictor = 'gender',
                     imputed = FALSE,
                     priors = NULL, # hyper priors for inverse variance for non-converging models
                     no_predictors = FALSE, # run model with no predictors only (model 0)
                     innum) # imputation number
  {

  # journal-specific recommendations and labels
  if(this_journal == 'F1000'){
    recommendations = c('q1_1_1','q1_1_2','q1_1_3')
    categories = c('Approved','Approved with reservations','Not approved')
  }
  if(this_journal == 'Epidemiology'){
    recommendations = c('q1_1_1', 'q1_1_2', 'q1_1_3', 'q1_1_4', 'q1_1_5') # without `none`
    categories = c('Accept', 
                   'Accept with minor revisions',
                   'Reconsider after revisions',
                   'Uncertain, needs major revision',
                   'Reject')
  }
  if(this_journal == 'BMJ Open'){
    recommendations = c('q1_1_1','q1_1_2','q1_1_3','q1_1_5') # skipped q1_1_4
    categories = c('Accept', 
                   'Minor revisions',
                   'Major revisions',
                   'Reject')
  }
  ncat = length(categories) # number of recommendation categories
  
  # rename predictor to generic 'predictor'
  index = which(names(indata) == predictor)
  names(indata)[index] = 'predictor'
  # assess predictor type (binary or continuous)
  pred = indata$predictor
  levels = paste(names(table(pred)), collapse=',')
  predictor_type = ifelse(levels=='0,1', 'binary', 'continuous')
  
  # restrict to this journal 
  indata = filter(indata, journal == this_journal) %>%
    mutate(recommendation = NA) # to fill in below
  # make recommendation number
  for (k in 1:ncat){
    indata$recommendation[indata$q1_3 == categories[k]] = k
  }
  
  # remove missing predictors
  indata = filter(indata, 
                  !is.na(predictor),
                  !is.na(q1_1_1)) # small number with missing uncertainty percentages
  if(any(is.na(indata$recommendation))){cat('Warning, missing recommendation')} # cannot work if decision is missing
  
  # make recommendation into binary matrix, with a yes/no for every person and decision
  N = nrow(indata)
  recommendation_matrix = matrix(data=0, nrow = N, ncol = ncat)
  for (k in 1:ncat){
    index = which(indata$recommendation == k)
    recommendation_matrix[index, k] = 1
  }
  if(any(rowSums(recommendation_matrix) != 1)){cat("Error in matrix\n")}
  
  # simple recommendation matrix needed for fitted values
  recommendation_fit_no_predictors = toeplitz(c(1,rep(0,ncat-1)))
  recommendation_fit_predictors = rbind(recommendation_fit_no_predictors, recommendation_fit_no_predictors) # simple repeat
  
  # recommendation percentages as a matrix suitable for multinomial (dependent variable)
  percentages = as.matrix(select(indata, all_of(recommendations))) # in column order
  M = 100 # assume results are scaled to 100 for multinomial
  #
  constants_no_predictors = list(N = N, 
                                 M = M,
                                 recommendation = recommendation_matrix,
                                 recommendation_fit = recommendation_fit_no_predictors,
                                 ncat = ncat)
  constants_predictors = list(N = N, 
                              M = M,
                              recommendation = recommendation_matrix,
                              recommendation_fit = recommendation_fit_predictors,
                              ncat = ncat,
                              predictor = indata$predictor, 
                              predictor_fit = c(rep(0,ncat),rep(1,ncat)))
  constants_predictors_reverse = constants_predictors # version for uncertainty in reverse direction (model 2b)
  # reverse
  if(predictor_type == 'binary'){constants_predictors_reverse$predictor = 1 - indata$predictor}
  if(predictor_type == 'continuous'){constants_predictors_reverse$predictor = max(indata$predictor) - (indata$predictor)} # reverse; must be >= 0
  # dependent data
  dependent_data = list(percentages = percentages)

  # hyper-priors for non-converging models
  if(is.null(priors)){hyper_priors = rep(0.0001, ncat)} 
  if(!is.null(priors)){hyper_priors = priors}
  constants_no_predictors$hyper = hyper_priors
  constants_predictors$hyper = hyper_priors
  constants_predictors_reverse$hyper = hyper_priors

  # get model codes
  source('99_bayes_models_nimble.R')
  
  # initial values
  inits_no_predictors = list(alpha=rep(0,ncat), beta=rep(0,ncat))
  inits_predictors_mean = list(alpha=rep(0,ncat), beta=rep(0,ncat), zeta=rep(0,ncat))
  inits_predictors_variance = list(alpha=rep(0,ncat), beta=rep(0,ncat), gamma = 0.1)
  # parameters
  parms_no_predictors = c('alpha','beta','pi.fit')
  parms_predictors_mean = c('alpha','beta','zeta','pi.fit')
  parms_predictors_variance = c('alpha','beta','gamma','pi.fit')
  
  ## set up models
  # without predictors (model 0)
  model_no_predictors = nimbleModel(code_model_no_predictors, 
                                    data = dependent_data,
                                    inits = inits_no_predictors,
                                    constants = constants_no_predictors)
  # with predictors influencing mean (model 1)
  model_mean = nimbleModel(code_model_mean, 
                                 data = dependent_data,
                                 inits = inits_predictors_mean,
                                 constants = constants_predictors)
  # with predictors influencing variance (model 2a)
  model_variance = nimbleModel(code_model_variance, 
                           data = dependent_data,
                           inits = inits_predictors_variance,
                           constants = constants_predictors)
  # with predictors influencing variance in reverse direction (model 2b)
  model_variance_reverse = nimbleModel(code_model_variance, 
                               data = dependent_data,
                               inits = inits_predictors_variance,
                               constants = constants_predictors_reverse)
  
  # get the MCMC parameters
  source('99_mcmc.R')
  
  # a) model with no predictors, used as a baseline
  mcmc_no_predictors =  nimbleMCMC(model = model_no_predictors,
                                   inits = inits_no_predictors,
                                   monitors = parms_no_predictors,
                                   niter = MCMC*2*thin, # times 2 for burn-in 
                                   thin = thin,
                                   nchains = n.chains, 
                                   nburnin = MCMC,
                                   summary = TRUE, 
                                   setSeed = seed,
                                   WAIC = FALSE) # do not use WAIC, too many warnings
  
  # b) model with predictors influencing mean
  if(no_predictors == FALSE) { # Model 0 only version
  mcmc_mean =  nimbleMCMC(model = model_mean,
                                     inits = inits_predictors_mean,
                                     monitors = parms_predictors_mean,
                                     niter = MCMC*2*thin, # times 2 for burn-in 
                                     thin = thin,
                                     nchains = n.chains, 
                                     nburnin = MCMC,
                                     summary = TRUE, 
                                     setSeed = seed,
                          WAIC = FALSE) # do not use WAIC, too many warnings
  
  # c) model with predictors influencing variance/uncertainty
  mcmc_variance =  nimbleMCMC(model = model_variance,
                          inits = inits_predictors_variance,
                          monitors = parms_predictors_variance,
                          niter = MCMC*2*thin, # times 2 for burn-in 
                          thin = thin,
                          nchains = n.chains, 
                          nburnin = MCMC,
                          summary = TRUE, 
                          setSeed = seed,
                          WAIC = FALSE) # do not use WAIC, too many warnings
  
  # d) model with predictors influencing variance/uncertainty (reverse direction)
  mcmc_variance_reverse =  nimbleMCMC(model = model_variance_reverse,
                              inits = inits_predictors_variance,
                              monitors = parms_predictors_variance,
                              niter = MCMC*2*thin, # times 2 for burn-in 
                              thin = thin,
                              nchains = n.chains, 
                              nburnin = MCMC,
                              summary = TRUE, 
                              setSeed = seed,
                              WAIC = FALSE) # do not use WAIC, too many warnings
  } # end of `no predictors` if
  
  # run x-validation
  if(innum <= 0){ # only for complete case (to save time)
    source('99_xval.R', local = environment())
  }
  if(innum >= 1){model_xvals = NULL}
    
  # get the tables of parameter estimates and fitted values
  results = process_nimble(indata = dependent_data, 
                           inmcmc = mcmc_no_predictors, 
                           ncat = ncat)
  if(no_predictors == FALSE) { # Model 0 only version?
    results_mean = process_nimble(indata = dependent_data, 
                                           inmcmc = mcmc_mean, 
                                  difference = TRUE,
                                  ncat = ncat)
   results_variance = process_nimble(indata = dependent_data, 
                                inmcmc = mcmc_variance, 
                                difference = TRUE,
                                ncat = ncat)
   results_variance_reverse = process_nimble(indata = dependent_data, 
                                    inmcmc = mcmc_variance_reverse, 
                                    difference = TRUE,
                                    ncat = ncat)
  }
  if(no_predictors == TRUE) { # Model 0 only version?
    results_mean = results_variance = results_variance_reverse = NULL
  }
    
  # combine tables
  tables = bind_rows(results$table,
                     results_mean$table,
                     results_variance$table,
                     results_variance_reverse$table, .id = 'model') %>%
    mutate(model = as.numeric(model),
           journal = this_journal,
           predictor = predictor,
           imputed = imputed,
           innum = innum)
  # combine fitted values
  fitted = bind_rows(results$marginal,
                     results_mean$marginal,
                     results_variance$marginal,
                     results_variance_reverse$marginal, .id = 'model') %>%
    mutate(model = as.numeric(model),
           journal = this_journal,
           predictor = predictor,
           imputed = imputed,
           innum = innum)
  # combine probability differences
  differences = bind_rows(results$p_diff,
                     results_mean$p_diff,
                     results_variance$p_diff,
                     results_variance_reverse$p_diff, .id = 'model')
  if(nrow(differences)>0){
    differences = mutate(differences,
           model = as.numeric(model) + 1, # no differences for null model, so add 1
           journal = this_journal,
           predictor = predictor,
           imputed = imputed,
           innum = innum)
  }
  
  ## Check chain convergence, plots in figures/mcmc folder
  plot_convergence(insamples = mcmc_no_predictors$samples,
                   injournal = this_journal,
                   inimputed = imputed,
                   model = 0,
                   inpredictor = 'none')
  if(no_predictors == FALSE) { # Model 0 only version?
    plot_convergence(insamples = mcmc_mean$samples,
                   injournal = this_journal,
                   inimputed = imputed,
                   model = 1,
                   inpredictor = predictor)
    plot_convergence(insamples = mcmc_variance$samples,
                   injournal = this_journal,
                   inimputed = imputed,
                   model = '2a',
                   inpredictor = predictor) # 
    plot_convergence(insamples = mcmc_variance_reverse$samples,
                   injournal = this_journal,
                   inimputed = imputed,
                   model = '2b',
                   inpredictor = predictor)
  }

  ## need variance-covariance matrix for imputation
  # model 0
  combined = rbind(mcmc_no_predictors$samples$chain1, mcmc_no_predictors$samples$chain2) # use both chains
  coef_m0 = colMeans(combined)
  vcov_m0 = cov(combined)
  if(no_predictors == FALSE) { # Model 0 only version?
    # model 1
    combined = rbind(mcmc_mean$samples$chain1, mcmc_mean$samples$chain2)
    coef_m1 = colMeans(combined)
    vcov_m1 = cov(combined)
    # model 2a
    combined = rbind(mcmc_variance$samples$chain1, mcmc_variance$samples$chain2)
    coef_m2a = colMeans(combined)
    vcov_m2a = cov(combined)
    # model 2b
    combined = rbind(mcmc_variance_reverse$samples$chain1, mcmc_variance_reverse$samples$chain2)
    coef_m2b = colMeans(combined)
    vcov_m2b = cov(combined)
  }
  if(no_predictors == TRUE) { # Model 0 only version?
    coef_m1 = coef_m2a = coef_m2b = vcov_m1 = vcov_m2a = vcov_m2b = NULL
  }
    
  # to avoid DLL error  ... but still get them
  clearCompiled(model_no_predictors)
  clearCompiled(model_mean)
  clearCompiled(model_variance)
  clearCompiled(model_variance_reverse)
  
  # return results
  to_return = list()
  to_return$xval = model_xvals
  to_return$tables = tables
  to_return$fitted = fitted
  to_return$differences = differences
  to_return$coef_m0 = coef_m0 # for imputation
  to_return$vcov_m0 = vcov_m0 # for imputation
  to_return$coef_m1 = coef_m1 # for imputation
  to_return$vcov_m1 = vcov_m1 # for imputation
  to_return$coef_m2a = coef_m2a # for imputation
  to_return$vcov_m2a = vcov_m2a # for imputation
  to_return$coef_m2b = coef_m2b # for imputation
  to_return$vcov_m2b = vcov_m2b # for imputation
  return(to_return)
  
} # end of function



#### function to plot convergence ###
plot_convergence = function(insamples, injournal, inpredictor, inimputed, model){
  
  # convert TRUE/FALSE to character
  inimputed = ifelse(inimputed==TRUE, 'imputed', 'complete_case')
  
  # combine two chains
  chain1 = data.frame(insamples$chain1) %>% mutate(s = 1:n())
  chain2 = data.frame(insamples$chain2) %>% mutate(s = 1:n())
  chains = bind_rows(chain1, chain2, .id = 'chain') %>%
    clean_names()
  
  # remove some variables
  index = apply(chains, 2, var) != 0 # remove corner-point variables
  chains = chains[, index]
  index = !str_detect(colnames(chains), 'fit')
  chains = chains[, index] # remove fitted variables
  
  # switch to long
  long = reshape2::melt(chains, id=c('s','chain'))
  
  # line plot
  cplot = ggplot(data=long, aes(x=s, y=value, col=factor(chain)))+
    geom_line(lty=2)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    xlab('Iteration')+
    ylab('Estimate')+
    scale_color_manual('Chain', values=cbPalette[2:3])+
    facet_wrap(~variable, scales = 'free_y')
  
  # overlapping density plot
  hplot = ggplot(data=long, aes(x=value, fill=factor(chain)))+
    geom_density(alpha=0.5)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    xlab('Estimate')+
    scale_fill_manual('Chain', values=cbPalette[2:3])+
    facet_wrap(~variable, scales = 'free')
  
  ## save plots
  #
  outfile = paste('figures/mcmc/MCMC_line_model', model, '_', injournal, '_', inpredictor, '_', inimputed, '.jpg', sep='')
  jpeg(outfile, width=6, height=5, units='in', res=300, quality = 100)
  print(cplot)
  invisible(dev.off())
  #
  outfile = paste('figures/mcmc/MCMC_density_model', model, '_', injournal, '_', inpredictor, '_', inimputed, '.jpg', sep='')
  jpeg(outfile, width=6, height=5, units='in', res=300, quality = 100)
  print(hplot)
  invisible(dev.off())
  
}


#### hot deck imputation using dplyr, used by 4_impute_predictors ###
hot_deck = function(indata, variable){
  
  # rename variable to impute
  index = which(names(indata) == variable)
  names(indata)[index] = 'value'
  
  # data that does not need imputing (see https://stackoverflow.com/questions/24539488/hot-deck-imputation-in-dplyr)
  myDataOK <- indata %>%
    filter(value %>% is.finite)
  
  # data to impute, use hot deck imputation within journal
  myDataimputed <- indata %>%
    group_by(journal) %>%
    summarise(n_inf = sum(!is.finite(value))) %>% 
    group_by(journal) %>%
    do(sample_n(filter(myDataOK, journal == .$journal), size = .$n_inf, replace = TRUE))
  
  ## combine imputed with complete
  myData2 <- bind_rows(myDataOK, myDataimputed)
  
  ## same size as original?
  if(nrow(myData2) != nrow(indata)){cat('Imputation error')}
  
  # change back name
  index = which(names(myData2) == 'value')
  names(myData2)[index] = variable
  
  return(myData2)
  
}

#### plot the fitted probabilities for all three models ###
plot_fitted = function(indata, # data for plotting fitted values
                       inxval, # x-validation data
                       xlab = 'Actual recommendation',
                       legend_lab = 'Considered\nrecommendations',
                       this_imputed, 
                       this_journal, 
                       this_predictor, 
                       no_predictor = FALSE, # flag for no predictor only results (model 0)
                       predictor_label = NULL, # predictor labels for TRUE and FALSE
                       difference = FALSE, # plot difference (TRUE) or bars (FALSE)
                       return = TRUE){ # carriage return in labels
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
  # figure file name
  if(difference==TRUE){
    file = paste('figures/fitted_difference_', this_journal, '_', this_predictor, '.jpg', sep='')
  }
  if(difference==FALSE){
    file = paste('figures/fitted_', this_journal, '_', this_predictor, '.jpg', sep='')
  }
  
  # choose model 2a or 2b dependent on x-validation
  if(no_predictor == FALSE){ # if there are some predictors
    best_2 = filter(inxval, journal == this_journal, predictor == this_predictor, model >= 3, imputed == 'Complete case') %>%
      arrange(xval) %>%
      slice(1) %>%
      pull(model)
    if(best_2 == 4){predictor_label = rev(predictor_label)} # reverse label as model number 4 is best
  }

  ## plot difference from model 0 ##
  if(difference == TRUE){
    # get model 0 estimates
    baseline = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == 1) %>%
      select(recommendation, uncertainty, mean) %>%
      rename('baseline' = 'mean')
    # get model 1 estimates
    model1 = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == 2) %>%
      select(model, recommendation, uncertainty, mean)
    # get model 2 estimates, which one to use depends on x-validation (see above)
    model2 = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == best_2) %>%
      select(model, recommendation, uncertainty, mean)
    models12 = bind_rows(model1, model2) %>%
      mutate(predictor = recommendation >= ncat + 1, # re-numbering before merge
             recommendation = ifelse(predictor==TRUE, recommendation - ncat, recommendation))
    # calculate difference
    diffs = left_join(baseline, models12, by=c('recommendation','uncertainty')) %>%
      mutate(diff = mean - baseline,
             modelf = case_when(
               model == 2 ~ 'Predictor influences mean (Model 2)',
               model %in% c(3,4) ~ 'Predictor influences uncertainty (Model 3)'
             ) ,
             facet = case_when(
               predictor == FALSE ~ 1, # reference category
               predictor == TRUE ~ 2
             ),
             facet = factor(facet, levels=1:2, labels = predictor_label)) # to give correct ordering

    # plot
    plot_diff = ggplot(data = diffs, aes(x=recommendation, y=diff, ymin=0, ymax=diff, col=factor(uncertainty)))+
      geom_point(position = position_dodge(width=0.3), size=4)+
      geom_errorbar(width=0, lty=1, position = position_dodge(width=0.3), linewidth=1.05)+
      #geom_bar(stat='identity', position='stack', width = 0.99)+
      ylab('Difference in probability')+
      xlab(xlab)+
      scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))+ # avoid overlap in labels
      scale_color_manual(legend_lab, labels=alabels, values=colours)+
      theme_bw()+
      facet_wrap(modelf~facet)+
      theme(panel.grid = element_blank(),
            plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
            legend.position = 'top')
    
    # export
    jpeg(file, width = 8, height = 7, units = 'in', res = 600, quality = 100)
    print(plot_diff)
    invisible(dev.off())
    
    return(plot_diff)
    
  } # end of if for difference =TRUE

  ## plot bars (using median instead of mean due to occasional outliers) ##
  if(difference == FALSE){
  
  # model 0
  to_plot = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == 1)
  fitted_no_predictors = ggplot(data = to_plot, aes(x=recommendation, y=median, fill=factor(uncertainty)))+
    geom_bar(stat='identity', position='stack', width = 0.99)+
    ylab('Probability')+
    xlab(xlab)+
    scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(ylim=c(0,1))+ # because numbers are sometimes fractionally short/over
    scale_fill_manual(legend_lab, labels=alabels, values=colours)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(), 
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
          legend.position = 'top')
  
  ### legend as a separate plot
  legend = g_legend(fitted_no_predictors)
  fitted_no_predictors = fitted_no_predictors + theme(legend.position = 'none') # now remove legend
  
  # model 1
  if(no_predictor == FALSE){
    to_plot = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == 2) %>%
    mutate(predictor = recommendation >= ncat + 1,
           recommendation = ifelse(predictor==TRUE, recommendation - ncat, recommendation),
           facet = case_when(
             predictor == FALSE ~ 1, # reference category
             predictor == TRUE ~ 2
           ),
           facet = factor(facet, levels=1:2, labels = predictor_label)) # to give correct ordering
    fitted_with_predictors = ggplot(data = to_plot, aes(x=recommendation, y=median, fill=factor(uncertainty)))+
    geom_bar(stat='identity', position='stack', width = 0.99)+
    ylab('')+
    xlab(xlab)+
    scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(ylim=c(0,1))+ # because numbers are sometimes fractionally short/over
    scale_fill_manual(legend_lab, labels=alabels, values=colours)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),  
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
          legend.position = 'none')+
    facet_wrap(~facet)

  # model 2
  to_plot = filter(indata, imputed == this_imputed, journal == this_journal, predictor == this_predictor, model == best_2) %>%
    mutate(predictor = recommendation >= ncat + 1,
           recommendation = ifelse(predictor==TRUE, recommendation-ncat, recommendation), 
           facet = case_when(
             predictor == FALSE ~ 1, # reference category
             predictor == TRUE ~ 2
           ),
           facet = factor(facet, levels=1:2, labels = predictor_label)) # to give correct ordering
  fitted_with_variance = ggplot(data = to_plot, aes(x=recommendation, y=median, fill=factor(uncertainty)))+
    geom_bar(stat='identity', position='stack', width = 0.99)+
    ylab('')+
    xlab(xlab)+
    scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(ylim=c(0,1))+ # because numbers are sometimes fractionally short/over
    scale_fill_manual(legend_lab, labels=alabels, values=colours)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(), 
          axis.text.x = element_text(angle=45, hjust=1),
          plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt"),
          legend.position = 'none')+
    facet_wrap(~facet)

  ## export model 1 and 2 alone before adding title
  # model 1
  file1 = paste('figures/model1/fitted_', this_journal, '_', this_predictor, '.jpg', sep='')
  plot1 = fitted_with_predictors + ylab('Probability') + theme(legend.position = 'right') # add back label and legend
  jpeg(file1, width = 7, height = 6, units = 'in', res = 600, quality = 100)
  print(plot1)
  dev.off()
  # model 2
  file2 = paste('figures/model2/fitted_', this_journal, '_', this_predictor, '.jpg', sep='')
  plot2 = fitted_with_variance + ylab('Probability') + theme(legend.position = 'right') #  add back label
  jpeg(file2, width = 6, height = 5, units = 'in', res = 600, quality = 100)
  print(plot2)
  dev.off()
  } # end of 'no predictor' if
  
  # combine plots and export for version that has all models compared
  if(no_predictor == FALSE){
    fitted_no_predictors = fitted_no_predictors + ggtitle('Model 0: no predictor')
    fitted_with_predictors = fitted_with_predictors + ggtitle('Model 1: predictor associated\nwith mean')
    fitted_with_variance = fitted_with_variance + ggtitle('Model 2: predictor associated\nwith uncertainty')
    # layout
    lay = rbind(c(1,3,4), # legend in top-left
                c(2,3,4))
    lay = rbind(c(1,1), # legend at top, new version without model 0
                c(2,3))
    # export
    jpeg(file, width = 9.5, height = 6, units = 'in', res = 600, quality = 100)
    # version with model 0
#    grid.arrange(legend,
#                 fitted_no_predictors, 
#                 fitted_with_predictors,
#                 fitted_with_variance, 
#                 layout_matrix = lay, heights=c(0.09,1))
    # version without model 0
        grid.arrange(legend,
                     fitted_with_predictors,
                     fitted_with_variance, 
                     layout_matrix = lay, heights=c(0.09,1))
    invisible(dev.off())
    
    # output to Word/screen
    grid.arrange(legend,
                        fitted_with_predictors,
                        fitted_with_variance,  
                 layout_matrix = lay, heights=c(0.1,1))
    # save results for bespoke plots for journal
    to_return = fitted_with_variance
    return(to_return)
    
  }
  if(no_predictor == TRUE){

    # make adjustments to plot as it's not a single panel
    margin = c(4,0,-4,0) # top, right, bottom, left; scale = pt; reduce gaps between panels
    fitted_no_predictors = fitted_no_predictors + 
      theme(legend.position = 'right',
            plot.margin = margin(t = margin[1], r = margin[2], b = margin[3], l = margin[4], unit = "pt")) +
      scale_x_continuous(breaks=1:ncat, labels=alabels, expand = c(0,0))
      
    # export
    jpeg(file, width = 6.5, height = 5, units = 'in', res = 600, quality = 100)
    print(fitted_no_predictors)
    invisible(dev.off())
    
    # output to screen
    print(fitted_no_predictors)
  }
    
  
  } # end of if for difference = FALSE
  
}


### combine the estimates from the multiple imputed results ###
my_combine_imputed = function(coef, vcov, this_journal, this_predictor, modelnum){
  table = summary(MIcombine(coef, vcov)) %>% 
  tibble::rownames_to_column() %>%
  mutate(model = modelnum, # model number 
         journal = this_journal,
         predictor = this_predictor,
         imputed = TRUE,
         row = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]|\\.')) %>%
  rename('parameter' = 'rowname',
         'mean' = 'results',
         'lower' = '(lower',
         'upper' = 'upper)',
         'sd' = 'se')
 return(table)
}


#### plot the fitted probabilities for all three models ###
make_table = function(indata,
                       xlab = '',
                       this_imputed, 
                       this_journal, 
                       this_predictor, 
                       predictor_label, # predictor labels for TRUE and FALSE
                      model_labels){ # carriage return in labels
  to_table = filter(indata, 
                    imputed == this_imputed, 
                    journal == this_journal, 
                    predictor == this_predictor,
                    !str_detect(parameter, 'alpha.1.|zeta.1.')) %>% # remove reference categories
    select(model, parameter, mean, lower, upper, post_prob) %>%
    mutate(model = factor(model, levels=1:4, labels = model_labels),
           parameter = nice_rename(parameter),
           post_prob = format.pval(post_prob, digits=4, eps=0.0001))
  ftab = flextable(to_table) %>%
    theme_box() %>%
    merge_v(j=1) %>%
    colformat_double(j=3:5, digits=2) %>%
    autofit() %>%
    width(j=1, width=2) # narrower first column
return(ftab)
}

### rename bugs parameters, and other variables too
nice_rename = function(x, scale=FALSE){ # with or without scale
  y = case_when(
    x == 'alpha[2]' ~ 'Intercept, recommendation 2',
    x == 'alpha[3]' ~ 'Intercept, recommendation 3',
    x == 'alpha[4]' ~ 'Intercept, recommendation 4',
    x == 'alpha[5]' ~ 'Intercept, recommendation 5',
    x == 'beta[1]' ~ 'Slope, recommendation 1',
    x == 'beta[2]' ~ 'Slope, recommendation 2',
    x == 'beta[3]' ~ 'Slope, recommendation 3',
    x == 'beta[4]' ~ 'Slope, recommendation 4',
    x == 'beta[5]' ~ 'Slope, recommendation 5',
    x == 'zeta[2]' ~ 'Predictor, recommendation 2',
    x == 'zeta[3]' ~ 'Predictor, recommendation 3',
    x == 'zeta[4]' ~ 'Predictor, recommendation 4',
    x == 'zeta[5]' ~ 'Predictor, recommendation 5',
    x == 'gamma' ~ 'Uncertainty',
    x == 'review_time' & scale == TRUE~ 'Time spent on review (per doubling)', 
    x == 'review_time' & scale == FALSE ~ 'Time spent on review', 
    x == 'gender' & scale == TRUE ~ "Male reviewers = yes", 
    x == 'gender' & scale == FALSE ~ "Male reviewers", 
    x == 'female' & scale == FALSE ~ "Female reviewers", 
    x == 'time_diff' & scale == TRUE ~ 'Time to peer review (+60 days)', 
    x == 'time_diff' & scale == FALSE ~ 'Time to peer review', 
    x == 'version' & scale == TRUE ~ 'Version = 1', 
    x == 'version' & scale == FALSE ~ 'Version', 
    x == 'protocol' ~ 'Protocol', 
    x == 'n_words' ~ 'Number of words', 
    x == 'flesch' ~ 'Flesch readability', 
    x == 'dale_chall' ~ 'Dale-Chall readability', 
    x == 'experience' ~ "Reviewer's experience", 
    TRUE ~ as.character(x)
  )
  return(y)
}

### plot x-validated mean square error
plot_mse = function(indata, inpredictor){
  to_plot = filter(indata, predictor == inpredictor) %>%
    mutate(imputed == ifelse(imputed==TRUE, 'Imputed', 'Complete case'))
  xplot = ggplot(data = to_plot, aes(x = model, y = xval, ymin=lower, ymax=upper, col=imputed))+
    geom_point()+
    geom_errorbar(linewidth=1.05, width=0)+
    scale_color_manual('Imputed', values = cbPalette[2:3], labels=c('No','Yes'))+
    g.theme +
    theme(legend.position = 'none')+
    xlab('')+
    ylab('Mean square error')+
    scale_x_continuous(breaks=1:4, labels = model_labels_breaks)+
    coord_flip()+
    facet_wrap(~journal, scales='free_x')
  return(xplot)
}

## for tile plot, see 5_plot_tile_summary_model.R
# from https://stackoverflow.com/questions/65887187/how-to-display-two-categorical-variables-on-one-tile-of-a-heatmap-triangle-til
make_triangles <- function(x, y, point = "up") {
  x <- as.integer(as.factor((x)))
  y <- as.integer(as.factor((y)))
  
  if (point == "up") {
    newx <- sapply(x, function(x) {
      c(x - 0.5, x - 0.5, x + 0.5)
    }, simplify = FALSE)
    newy <- sapply(y, function(y) {
      c(y - 0.5, y + 0.5, y + 0.5)
    }, simplify = FALSE)
  } else if (point == "down") {
    newx <- sapply(x, function(x) {
      c(x - 0.5, x + 0.5, x + 0.5)
    }, simplify = FALSE)
    newy <- sapply(y, function(y) {
      c(y - 0.5, y - 0.5, y + 0.5)
    }, simplify = FALSE)
  }
  data.frame(x = unlist(newx), y = unlist(newy))
}
