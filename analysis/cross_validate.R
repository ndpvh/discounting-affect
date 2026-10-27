# blocked k-fold
## within each person's series, split the time points into k consectutive blocks. Each block is constant while the model is fit on the rest
## drop some observations on either side of the test block 

# goal: want to know how well each model predicts data it was not fitted on
  # cannot use regular k-fold because cannot assume data points are IID (autocorrelation)
    # we know the future information, so we cannot predict past

  # need a contiguous stretch of time: unbroken blocks of consecutive time periods
  # held out data (fold to evaluate) needs to be a contiguous stretch of time - the fitting data must not peek at that stretch

# step 1: assess the dependence in the raw data
  # plot the autocorrelation function of the participant's raw affect series
  # note the lag at which the autocorrelation fades

# step 2: state your prediction objective
  # what is the model supposed to predict - affect at new stretches of time for the same person
  # blocking is required because random splits would be training points right next to test points

# step 3: define the blocks:
  # cut the series into contiguous blocks of time, each at least as long as the range in step 1
  # maybe add larger blocks to account for overfitting?
  # in order, no shuffling

  # each block becomes a fold 
  # "blocked" - each fold is a stretch of time rather than a random point

# step 4: perform the cross validation
  # for each fold:
    #1. hold out one block as the test set
    #2. Fit each model on the remaining blocks
    #3. Predict the held-out block with each fitted model
    #4. Compute teh RMSE on the held-out block
  # take a fold (e.g. 3) and make it the test set
  # the models shouldn't use these points to estimate the parameters

  ## do we need a buffer?

  # average fold RMSE, which gives one cross-validation RMSE per participant and model
  # average across participants, which gives one value per model

  # model with the lower value predicts new stretches of time better

# step 5: final model
  # after comparing the models, refit the chosen model on the full series

# to decide:
  # block length (from raw data autocorrelation)










