#' @name ggESDA
#' @title A symbolic object by R6 class for interval analysis and ggplot
#' @description  This is an object that will be used to make a ggplot
#' object.A ggESDA object contains both classic data that user have
#' and interval data which we transform.More over,some basic statistics
#' from row data will also be recorded in this object,and the interval
#' data which is from RSDA transformation will still contain RSDA
#' properties.
#' @import R6
#' @param raw_data Classical data frame.
#' @param statistics Data frame contained the statistic of raw data.
#' @param symbolic_data Interval-valued data frame.
#' @param cluster_result The clustering result from classical data drame to interval-valued data frame.
#' @export
ggESDA <- R6::R6Class(
  classname = "ggESDA",
  public = list(
    #' @field raw_data the data from user.
    raw_data = "data.frame",

    #' @field statistics contains min max mean median dataframe for each group of symbolic data
    statistics = "list",

    #' @field symbolic_data interval data from RSDA type
    symbolic_data = "data.frame",

    #' @field cluster_result clustering result
    cluster_result = "list",

    #' @description
    #' initialize all data, check whether satisfy theirs form
    initialize = function(raw_data = NULL, statistics = NULL,
                          symbolic_data = NULL, cluster_result = NULL){
      self$raw_data <- raw_data
      self$statistics <- statistics
      self$symbolic_data <- symbolic_data
      self$cluster_result <- cluster_result
      if(!private$test_data_type_legal()){
        stop("Object type error in statistics")
      }
    }
  ),
  private = list(
    test_data_type_legal = function() {
      '
      test_data_type_legal is to test whether the items in statistics are all data.frame type.

      input:

      output: Boolean. if true, all items in statistics are data.frame.
      '
      n <- length(self$statistics)
      is_all_data_frame <- all(unlist(lapply(self$statistics[1:n], FUN = is.data.frame)))
      return(is_all_data_frame)
    }
  )
)


test_data_type <- function(data){
  '
  test_data_type is to test whether input data is type of ggESDA, or RSDA.
  If not, it will automatically convert the data into ggESDA type.

  input:
    - data: The data that user input. Mostly data.frame.

  output: ggESDA object.
  '
  if("ggESDA" %in% class(data)){
    return(data)
  }else{
    if(("symbolic_tbl" %in% class(data))){
      return(RSDA2sym(data))
    }else{
      warning("Automatically transform a classical data to symbolic data")
      return(classic2sym(data))
    }
  }
}


test_univariate <- function(iData, aes_x, aes_y){
  '
  test_univariate is to test whether it is exactly one variables (x or y) in aes() that user input.

  input:
    - iData: Interval-valued data frame.
    - aes_x: x variable in aes().
    - aes_y: y variable in aes().

  output:
  '
  num_of_variables <- dim(iData)[2]
  with(iData, {
    is_x_exist <- any(unlist(lapply(iData[, 1:num_of_variables], FUN = identical, x = eval(aes_x))))
    is_y_exist <- any(unlist(lapply(iData[, 1:num_of_variables], FUN = identical, x = eval(aes_y))))
    if(is_x_exist && is_y_exist){
      stop("ERROR : This plot must have exactly one variable")
    }
  })
}


add_factor_variables <- function(raw_data, iData){
  '
  add_factor_variables is to add original factor variables, which esists
  in raw_data, into interval_valued data frame.

  input:
    - raw_data: Raw data frame before converting into iData.
    - iData: Interval-valued data frame.

  output: Symbolic data frame with interval-valued and factor variables.
  '
  tryCatch({
      test <- unlist(lapply(raw_data, is.factor))
      if(any(test)){
         factorIndex <- which(test)
         if(dim(raw_data)[1] == dim(iData)[1]){
           for(i in factorIndex){
             iData <- dplyr::bind_cols(iData, tmp = raw_data[[i]])
             colnames(iData)[dim(iData)[2]] <- colnames(raw_data)[i]
           }
         }
      }
      return(iData)
  }, error = function(err) {
      return(iData)
  })
}

