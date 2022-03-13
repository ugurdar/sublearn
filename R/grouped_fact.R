grouped_fact <- function(formul,training,testing,response,factor_variables,
                         type , model ){

  set.seed(1234)
  nfact <- length(factor_variables)
  sayac <- 0
  dfsayac <- matrix(, nrow = 0, ncol = 4)
  Acc <- NULL
  Rmse <- NULL
  liste <- list()
  time_dif <- NULL
  for (fact in factor_variables) {
    set.seed(1234)
    training_1 <- training
    testing_1 <- testing
    # Start time for runtime
    start_time <- Sys.time()
    # Group names with k_split
    group_names <- k_split(response,fact,training_1)

    # In this part, the loop finds the list index in the group_names
    # vector of the relevant level for the relevant factor variable
    a <- 0
    le <- NULL
    for(i in levels(training_1[,fact])){
      a = a+1
      le[a] <- which(sapply(group_names, FUN=function(X) i %in% X))
    }
    # Changing levels on train and test set.
    levels(training_1[,fact]) <- le
    levels(testing_1[,fact]) <- le


    if (length(levels(training[, fact])) != length(levels(testing[, fact]))) {
      warnings("Factor levels are not equal on train and test set.")
    }


    sayac = sayac + 1
    prediction_test <- rep(NA, dim(testing)[1])
    nlevel <- NULL
    # if task is regression
    if (type == "reg") {
      if (model == "cart") {
        full_model <-  rpart(formul, data = training_1)
        prediction_test <- predict(full_model, testing_1)
      }
      if (model == "ranger") {
        full_model <-  ranger(formul, data = training_1)
        prediction_test <- predict(full_model, testing_1)$predictions
      }
      if (model == "lm") {
        full_model <- lm(formul, data = training_1)
        prediction_test <-predict(full_model, testing_1)

      }

      end_time <- Sys.time()
      time_dif[sayac] <- end_time - start_time

      nlevel[sayac] <- length(levels(training_1[,fact]))

      Rmse[sayac] <- RMSE(y_true = testing[, response] , y_pred = prediction_test)
      # Table
      dfsayac <- rbind(dfsayac,
                       c(fact, nlevel[sayac], Rmse[sayac], time_dif[sayac]))

    }
    if (type == "class") {
      if (model == "cart") {
        full_model <-  rpart(formul, data = training_1)
        prediction_test <- predict(full_model, testing_1, type="class")
      }
      if (model == "ranger") {
        full_model <-  ranger(formul, data = training_1)
        prediction_test <- predict(full_model, testing_1)$predictions
      }

      end_time <- Sys.time()
      time_dif[sayac] <- end_time - start_time

      nlevel[sayac] <- length(levels(training_1[,fact]))

      Acc[sayac] <- Accuracy(y_true = testing[, response] , y_pred = prediction_test)
      # Table
      dfsayac <- rbind(dfsayac,
                       c(fact,  nlevel[sayac], Acc[sayac],  time_dif[sayac]))

    }

  }
  return(dfsayac)
}
