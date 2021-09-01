#' @name ggESDA
#' @title A symbolic object by R6 class for interval analysis and ggplot
#' @description  This is an object that will be used to make a ggplot
#' object.A ggESDA object contains both classic data that user have
#' and interval data which we transform.More over,some basic statistics
#' from row data will also be recorded in this object,and the interval
#' data which is from RSDA transformation will still contain RSDA
#' properties.
#' @import R6
#'
#' @export
ggESDA<-R6::R6Class(
  classname = "ggESDA",
  public = list(
    #' @field rawData the data from user.
    rawData = "data.frame",

    #' @field statisticsDF contains min max mean median dataframe for each group of symbolic data
    statisticsDF = "list",

    #' @field intervalData interval data from RSDA type
    intervalData = "data.frame",

    #' @field clusterResult clustering result
    clusterResult = "list",

    #' @description
    #' initialize all data, check whether satisfy theirs form
    initialize = function(rawData=NULL,statisticsDF=NULL,
                          intervalData=NULL,clusterResult=NULL){
      self$rawData <- rawData
      self$statisticsDF <- statisticsDF
      self$intervalData <- intervalData
      self$clusterResult <- clusterResult

      if(private$invalidDataType()){
        stop("Object type error in statisticsDF")
      }
    }
  ),
  private = list(
    invalidDataType = function() {
      n<-length(self$statisticsDF)
      isAllDF<-all(unlist(lapply(self$statisticsDF[1:n],FUN=is.data.frame)))
      return(!isAllDF)
    }
  )
)

#test whether data can be used for ggplot
testData <- function(data){
  if("ggESDA" %in% class(data)){ # if ggESDA class?
    return(data)
  }else{
    if(("symbolic_tbl" %in% class(data))){#if RSDA class?
      return(RSDA2sym(data))
    }else{
      warning("Automatically transform a classical data to symbolic data")
      return(classic2sym(data))
    }
  }
}

testXY <- function(iData,x,y){
  p<-dim(iData)[2]
  with(iData,{
    isVarX<-any(unlist(lapply(iData[,1:p],FUN=identical,x=eval(x))))
    isVarY<-any(unlist(lapply(iData[,1:p],FUN=identical,x=eval(y))))
    if(isVarX&&isVarY){
      stop("ERROR : This plot must have exactly one variable")
    }
  })
}
