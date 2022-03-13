toCat <- function(x,na.rm=TRUE){
  if(is.data.frame(x) == TRUE){
    # toCat function remove Na columns when na.rm parameter is default.
    if(na.rm==TRUE){
      x <- x[,colSums(is.na(x))<nrow(x)]
    }
    # It gives factor variables' names.
    factor_variables <- names(x)[sapply(x, is.factor)]
    # It finds factor variables' indexes.
    index <- which(names(x) %in% c(factor_variables))

    if(length(factor_variables) > 0){

      return(factor_variables)
    }else
      message("Data hasn't a factor variable.")
  }
  else
    message("Input class must be data.frame")
}
