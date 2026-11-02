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



###########################################

#### test participant:
df <- read.csv("analysis/test_participant.csv")
df



#### step 1: cut the series into blocks (blueprint)
# returns the trials associated with each fold
make_folds <- function(n, k){
  edges <- round(seq(0, n, length.out = k + 1))
  
  folds_list <- vector("list", length = k)
  
  for(i in 1:k){
    start_index <- edges[i] + 1
    end_index <- edges[i + 1]
    
    folds_list[[i]] <- start_index:end_index
  }
  
  return(folds_list)
}

# test function
n <- nrow(df)
k <- 5

make_folds(n, k)


##########################
###### EXISTING ESTIMATION STRATEGY  - NEED TO ADD

##### step 3: autocorrelation - to determine block and buffer size
# check how far back the autocorrelation stretches in participants
  # helps determine how long time blocks should be
  # helps determine buffer zone 


buffer_size <- 3 ####### need to find a way to determine this or find source that this is a good buffer

run_blocked_cross_validation <- function(affect_series, number_of_folds, buffer_size, estimation_function, predict_function){
  # 1. generate the folds using make_folds function
  total_time_points <- length(affect_series)
  all_folds <- make_folds(n = total_time_points, k = number_of_folds)
  
  # create vector to store RMSE score for each fold
  fold_rmse_vector <- numeric(number_of_folds)
  
  # 2. loop through each fold
  for(fold_idnex in seq_along(all_folds)){
    test_indeces <- all_folds[[fold_index]]
    
    # 3. create a buffer zone around test block to prevent leakage (drop observations immediately before and after test block)
    min_test_index <- min(test_indeces)
    max_test_index <- max(test_indeces)
    
    buffer_start <- max(1, min_test_index - buffer_size)
    buffer_end  <- min(total_time_points, max_test_index + buffer_size)
    buffer_indices <- buffer_start:buffer_end
    
    # 4. define the training indices (all points EXCEPT the test block and its buffer)
    all_indices <- seq_len(total_time_points)
    training_indices <- setdiff(all_indices, buffer_indices)
    
    # extract the actual data subsets
    training_data <- affect_series[training_indices]
    test_data <- affect_series[test_indices]
    
    # 5. fit model on the trainign data using estimation function
    estimated_parameters <- estimation_function(training_data)
    
    # 6. predict the held-out test block using the fitted parameters
    predictions <- predict_function(estimated_parameters, test_data)
    
    # 7. compute the RMSE on the held-out test block
    squared_errors <- (test_data - predictions)^2
    fold_rmse_vector[fold_index] <- sqrt(mean(squared_errors, na.rm = TRUE))
    
  }
  
  mean_participant_rmse <- mean(fold_Rmse_vector, na.rm = TRUE)
  return(mean_participant_rmse)
}

