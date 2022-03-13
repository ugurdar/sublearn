general_model <- function(formul,training,model="ranger"){
  set.seed(1234)
  if(model=="cart"){
    general_start <- Sys.time()
    model_ <- rpart(as.formula(formul) , data = training)
    general_end <- Sys.time()
    general_diff <- general_end-general_start
  }
  if(model=="ranger"){
    general_start <- Sys.time()
    model_ <- ranger(as.formula(formul) ,data=training)
    general_end <- Sys.time()
    general_diff <- general_end-general_start
  }
  if(model=="lm"){
    general_start <- Sys.time()
    model_ <- lm(as.formula(formul) ,data=training)
    general_end <- Sys.time()
    general_diff <- general_end-general_start
  }
  return(list(model = model_,general_diff=general_diff))
}
