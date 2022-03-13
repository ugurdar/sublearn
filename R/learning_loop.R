learning_loop <- function(formul, training, testing, factor_variables,response,
                          method=method, type = type,model = model) {
  # count of factor variables.
  nfact <- length(factor_variables)
  # indexes for each factor.
  sayac <- 0
  # dfsayac contains 6 columns (Factor, CVVar, Levels, RMSE, CVDist, Runtime, VIPMeanLoss,Rank).
  dfsayac <- matrix(, nrow = 0, ncol = 4)
  # for Rmse vector in loop,
  Rmse <- NULL
  Acc <- NULL
  liste <- list()
  # for runtime
  time_dif <- NULL
  # The loop works for each factor variable.
  for (fact in factor_variables) {
    # Runtime start here.
    start_time <- Sys.time()
    # if method is sublearn it use k_split function for levels names
    if(method == "sublearn"){
      levels_names <- k_split(response= response,training = training,fact = fact)
    }
    # if method is each levels_names takes all levels names from the factor.
    if(method == "each"){
      levels_names <- levels(training[,paste(fact)])
    }
    # indexes start from 1 to count of factor variables.
    sayac = sayac + 1
    # creating NULL vectors for loop
    subpredict <- rep(NA, dim(testing)[1])
    nlevel <- NULL

    # if task is regression
    if (type == "reg") {
      # the loop works for every levels of the factor.
      for (i in levels_names){

        if(method=="sublearn"){
          # pulls data corresponding to a i.th group names.
          sub_data <- training %>% filter(get(fact) %in% i)
          ind <- which(testing[,fact] %in% i)
        }
        if(method=="each"){
          # pulls data corresponding to a certain level (i).
          sub_data <- training %>% filter(get(fact) == i)
          ind <- which(testing[, fact] == i)
        }

        # Removing choosen factor variable from sub_data.
        sub_data[, fact] <- NULL

        # In this part, sub model, which given as model parameter, training
        # If method is each, it trained by choosen i.th level in sub data.
        # If method is sublearn, it trained by choosen i.th group levels in sub data.
        if (model == "cart") {
          # Model trained with sub_data(sub data of training set)
          sub_model <- rpart(formul, data = sub_data)
          if(method == "each"){
            # Prediction with testing data for i. levels.
            pred <- predict(sub_model, testing[which(testing[, fact] == i), ])
          }
          if(method == "sublearn"){
            # Prediction with testing data for i.th group levels.
            pred <- predict(sub_model, testing[which(testing[, fact] %in% i), ])
          }
          # Filling subpredict vector related indexes with predictions.
          subpredict[ind] <- pred
        }
        if (model == "ranger") {
          sub_model <-
            ranger(formul, data = sub_data)
          if(method == "each"){
            pred <- predict(sub_model, testing[which(testing[, fact] == i), ])
          }
          if(method == "sublearn"){
            pred <- predict(sub_model, testing[which(testing[, fact] %in% i), ])
          }
          subpredict[ind] <- pred$predictions
        }
        if (model == "lm") {
          if(method == "each"){
            test <- testing[which(testing[, fact] == i), ]
            test <- test %>% select(-fact)
            sub_model <- lm(formul, data = sub_data)
            pred <- predict(sub_model,test)
          }
          if(method == "sublearn"){
            test <- testing[which(testing[, fact] %in% i),]
            test <- test %>% select(-fact)
            sub_model <- lm(formul, data = sub_data)
            pred <- predict(sub_model, test)
          }
          subpredict[ind] <- pred
        }

      }
      # End time for modelling and prediction for i.th factor.
      end_time <- Sys.time()
      # Runtime for modelling and prediction for i.th factor.
      time_dif[sayac] <- end_time - start_time


      nlevel[sayac] <- length(levels_names)

      # RMSE calculation for i.th factor on test set.
      Rmse <- RMSE(y_true = testing[, response] , y_pred = subpredict)
      # Table
      dfsayac <-
        rbind(dfsayac,
              c(fact,  nlevel[sayac], Rmse, time_dif[sayac]))

    }
    # if task is classification
    if (type == "class") {
      # the loop works for every levels of the factor.
      for (i in levels_names){
        if(method=="sublearn"){
          # pulls data corresponding to a i.th group names.
          sub_data <- training %>% filter(get(fact) %in% i)
          ind <- which(testing[,fact] %in% i)
        }
        if(method=="each"){
          # pulls data corresponding to a certain level (i).
          sub_data <- training %>% filter(get(fact) == i)
          ind <- which(testing[, fact] == i)
        }

        # Removing choosen factor variable from sub_data.
        sub_data[, fact] <- NULL

        # In this part, sub model, which given as model parameter, training
        # If method is each, it trained by choosen i.th level in sub data.
        # If method is sublearn, it trained by choosen i.th group levels in sub data.
        if (model == "cart") {
          # Model trained with sub_data(sub data of training set)
          sub_model <- rpart(formul, data = sub_data)
          if(method == "each"){
            # Prediction with testing data for i. levels.
            pred <- predict(sub_model, testing[which(testing[, fact] == i), ],type="class")
          }
          if(method == "sublearn"){
            # Prediction with testing data for i.th group levels.
            pred <- predict(sub_model, testing[which(testing[, fact] %in% i), ],type="class")
          }
          # Filling subpredict vector related indexes with predictions.
          subpredict[ind] <- pred
        }
        if (model == "ranger") {
          sub_model <- ranger(formul, data = sub_data)
          if(method == "each"){
            test <- testing[which(testing[, fact] == i), ]
            if(dim(test)[1] == 0){
              next
            }else{
              pred <- predict(sub_model, test)
            }
          }
          if(method == "sublearn"){

            pred <- predict(sub_model, testing[which(testing[, fact] %in% i), ])
          }
          subpredict[ind] <- pred$predictions
        }

      }
      # End time for modelling and prediction for i.th factor.
      end_time <- Sys.time()
      # Runtime for modelling and prediction for i.th factor.
      time_dif[sayac] <- end_time - start_time


      nlevel[sayac] <- length(levels_names)

      # RMSE calculation for i.th factor on test set.
      Acc <- Accuracy(y_true = testing[, response] , y_pred = subpredict)
      # Table
      dfsayac <-
        rbind(dfsayac,
              c(fact,  nlevel[sayac], Acc, time_dif[sayac]))

    }


  }# end of for loop
  return(dfsayac)
}# end of function
