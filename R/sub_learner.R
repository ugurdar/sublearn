source("R/load_pkg.R")
source("R/general_model.R")
source("R/to_cat.R")
source("R/k_split.R")
source("R/learning_loop.R")
source("grouped_fact.R")




sub_learner <- function(formul,training, testing, model = "ranger", method = "sublearn",
                        type="reg",list = FALSE,seed=1234){
  set.seed(seed)
  type <- type
  method <- method
  model <- model
  formul <- formul
  response <- all.vars(formul)[1]
  # if(type == "class"){
  #   testing[,response] <- as.factor(testing[,response])
  #   training[,response] <- as.factor(training[,response])
  # }
  # factor_variables contains names of factor variables in the training data.
  factor_variables <- toCat(training)
  factor_variables <- factor_variables[factor_variables!= response]

  # In this part, levels names of factor variables changing to numeric values.
  # Example: Factor A levels "A", "B", "C" -> Factor A levels "1", "2", "3"
  training <- training %>% mutate_if(is.factor,as.numeric)
  testing <- testing %>% mutate_if(is.factor,as.numeric)
  training[,factor_variables] <-sapply(training[,factor_variables],as.factor)
  testing[,factor_variables] <-sapply(testing[,factor_variables],as.factor)
  training <- training %>% mutate_if(is.character,as.factor)
  testing <- testing %>% mutate_if(is.character,as.factor)

  if(type == "class"){
    testing[,response] <- as.factor(testing[,response])
    training[,response] <- as.factor(training[,response])
  }
  # Training base model.
  model_general <- general_model(formul,training,model)
  # Runtime of training the base model.
  general_diff <- model_general$general_diff
  model_ <- model_general$model
  # Creating explanier with base model.
  explanier <- suppressWarnings(DALEX::explain(model_,
                                               data = training,
                                               y = as.numeric(training[,paste(response)]),
                                               colorize = FALSE,
                                               verbose = FALSE))

  vd <- model_parts(explanier, type = "raw")
  vd <- data.frame(vd)
  # Calculation mean loss with dropout loss and sorting.
  sortedvd <- suppressWarnings(vd %>%
                                 group_by(variable) %>%
                                 summarize(mean_loss = mean(dropout_loss)) %>%
                                 arrange(desc(mean_loss)))

  # Calculationg total mean loss
  sumloss <- sum(sortedvd$mean_loss)

  # Pull factor variables importances(mean loss)
  factorimp <- sortedvd[which(sortedvd$variable %in% factor_variables),]
  sumfactorimp <- sum(factorimp$mean_loss)
  sum_all_minus_factors <- sumloss-sumfactorimp

  # Changing names for joining with dfsayac
  colnames(factorimp) <- c("Factor","VIPMeanLoss")
  # Ranking factor variables importances
  factorimp <- cbind(factorimp,Rank = 1:dim(factorimp)[1])
  factorimp$VIPMeanLoss  <- round(as.numeric(factorimp$VIPMeanLoss ),4)

  if(list == FALSE){
    factor_variables <- factorimp[which.min(factorimp$Rank),1]
  }


  # Training according to methods
  # dfsayac is a list contains dfsayac(data.frame) and distances(list)
  if(method== "sublearn" | method == "each"){
    dfsayac <- learning_loop(formul,training, testing, factor_variables,response, method=method, model = model, type = type)

  }
  if(method=="group"){
    dfsayac <-   grouped_fact(formul,training,testing,response = response ,factor_variables, model = model, type = type )
  }


  # Full model predictions
  set.seed(1234)
  if (model == "cart") {
    if(type == "class"){
      predict_general <- predict(model_, testing,type="class")
    }
    if(type == "reg"){
      predict_general <- predict(model_, testing)
    }
  }
  if (model == "ranger") {
    predict_general <- predict(model_, testing)$predictions
  }
  if (model == "lm") {
    predict_general <- predict(model_, testing)
  }
  # Calculation CV for full model.
  if(type == "reg"){
    # Calculating RMSE for full model.
    Rmse <- RMSE(predict_general, testing[, response])
  }
  if(type == "class"){
    Acc <- Accuracy(predict_general, testing[, response])
  }

  # Joining tables.
  if(type == "reg"){
    dfsayac <-
      rbind(dfsayac,
            c("Full", "-", Rmse,  general_diff))
    dfsayac <- as.data.frame(dfsayac)
    colnames(dfsayac) <-
      c("Factor", "Levels", "RMSE",  "Runtime")
    dfsayac$RMSE <- round(as.numeric(dfsayac$RMSE), 4)
  }
  if(type == "class"){
    dfsayac <-
      rbind(dfsayac,
            c("Full",  "-", Acc , general_diff))
    dfsayac <- as.data.frame(dfsayac)
    colnames(dfsayac) <-
      c("Factor", "Levels", "Accuracy", "Runtime")
    dfsayac$Accuracy <- round(as.numeric(dfsayac$Acc), 4)
  }


  dfsayac$Runtime <- suppressWarnings(round(as.numeric(dfsayac$Runtime) , 4))
  dfsayac <- left_join(dfsayac, factorimp, by = "Factor")
  dfsayac[is.na(dfsayac)] <- "-"
  dfsayac[dfsayac$Factor == "Full", "VIPMeanLoss"] <-
    round(sum_all_minus_factors, 4)

  if(model == "ranger"){
    print(paste("Model is:","random forest","|","Method is:",method))
  }else{
    print(paste("Model is:",model,"|","Method is:",method))
  }
  return(dfsayac)


}
