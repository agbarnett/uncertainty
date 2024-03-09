# 99_xval.R
# run cross-validation for four models; see https://rdrr.io/cran/nimble/man/runCrossValidate.html
# called by 99_functions.R
# January 2024

n_folds = 10
n_boot = 10 # number of bootstrap replications

# use same 10-fold x-validation for each model to improve consistency
same_sets = function(i){
  TeachingDemos::char2seed('wigan') # set seed to get same random samples
  N = constants_no_predictors$N # move to function?
  samples = sample(1:N, size = N, replace=FALSE) # each observation sampled once; numbers are those to leave out
  start = 1 + (i-1)*floor(N/10)
  stop = i*floor(N/10)
  foldNodes_i <- paste0('percentages[', samples[start:stop], ', ]')  # will return 'percentages[1,]' for i = 1 
  return(foldNodes_i)
}

# cross-entropy loss function for multinomial
CrossEntropylossFunction <- function(simulatedDataValues, actualDataValues){
  dataLength <- length(simulatedDataValues) # 
  simulatedDataValues <- simulatedDataValues / 100 # change % to probability
  simulatedDataValues = pmax(0.001, simulatedDataValues) # avoid logging impossible values
  simulatedDataValues = pmin(0.999, simulatedDataValues)
  actualDataValues <- actualDataValues / 100
  loss <- 0
  for (i in 1:dataLength){
    est1 <- actualDataValues[i] * log(simulatedDataValues[i])
    est2 <- (1-actualDataValues[i]) * log(1-simulatedDataValues[i])
    est <- (est1 + est2)*(-1)
    loss <- loss + est
  }
  loss <- loss / dataLength
  return(loss)
}


# configure all four models
model_no_predictors_configured = configureMCMC(model_no_predictors)
model_mean_configured = configureMCMC(model_mean)
model_variance_configured = configureMCMC(model_variance)
model_variance_reverse_configured = configureMCMC(model_variance_reverse)

## run the cross-validations for each model

# model 0
xval1 = runCrossValidate(MCMCconfiguration = model_no_predictors_configured,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = same_sets,
                         lossFunction = CrossEntropylossFunction,
                         nBootReps = n_boot,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
clearCompiled(model_no_predictors_configured) # to avoid DLL
# model 1
xval2 = runCrossValidate(MCMCconfiguration = model_mean_configured,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = same_sets,
                         lossFunction = CrossEntropylossFunction,
                         nBootReps = n_boot,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
clearCompiled(model_mean_configured) # to avoid DLL
# model 2a
xval3 = runCrossValidate(MCMCconfiguration = model_variance_configured,
                         k = n_folds, # number of folds, model is re-run each time
                         foldFunction = same_sets,
                         lossFunction = CrossEntropylossFunction,
                         nBootReps = n_boot,
                         MCMCcontrol = list(niter = 5000,
                                            nburnin = 500))
clearCompiled(model_variance_configured) # to avoid DLL
# model 2b
xval4 = runCrossValidate(MCMCconfiguration = model_variance_reverse_configured,
                        k = n_folds, # number of folds, model is re-run each time
                        foldFunction = same_sets,
                        lossFunction = CrossEntropylossFunction,
                        nBootReps = n_boot,
                        MCMCcontrol = list(niter = 5000,
                                           nburnin = 500))
clearCompiled(model_variance_reverse_configured) # to avoid DLL

# store the results
this_xval1 = data.frame(xval = xval1$CVvalue, se = xval1$CVstandardError)
this_xval2 = data.frame(xval = xval2$CVvalue, se = xval2$CVstandardError)
this_xval3 = data.frame(xval = xval3$CVvalue, se = xval3$CVstandardError)
this_xval4 = data.frame(xval = xval4$CVvalue, se = xval4$CVstandardError)
#
model_xvals = bind_rows(this_xval1, this_xval2, this_xval3, this_xval4, .id = 'model') %>%
  mutate(model = as.numeric(model),
         journal = this_journal,
         predictor = predictor,
         imputed = imputed,
         innum = innum) # imputed number
