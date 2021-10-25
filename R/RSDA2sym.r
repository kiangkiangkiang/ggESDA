#' @name RSDA2sym
#' @title RSDA object to symbolic object for ggplot
#' @description  It  will be a good way to unify all symbolic data
#' object in R that collects all useful symbolic analysis tools
#' such like RSDA into the same class for management.In this way,
#' user who wants to do some study in symbolic data will be more
#' convenient for searching packages.Thus,RSDA2sym collecting RSDA
#' object into ggESDA object will do for plot(ggplot) and
#' RSDA's analysis.
#' @import stats
#' @param data an interval data ,which may transfrom by RSDA::classic.to.sym
#' .Note:data is a necessary parameter,and must have symbolic_tbl class.
#' @param rawData rawData,which can be transformed to interval data,
#' must be a data frame and match to data.
#' @return Return an object of class "ggESDA",which
#' have a interval data and others as follows.
#' \itemize{
#'   \item intervalData - The Interval data after converting also known
#'   as a RSDA object.
#'   \item rawData - Classical data that user input.
#'   \item clusterResult - Cluster results .If the groupby method is
#'   a clustering method then it will exist.
#'   \item statisticsDF - A list contains data frame including some
#'   typically statistics in each group.
#' }#'
#' @usage RSDA2sym(data=NULL,rawData=NULL)
#'
#' @examples
#' r<-RSDA::Cardiological
#' mySym<-RSDA2sym(r)
#' mySym$intervalData
#'
#' @export
RSDA2sym<-function(data=NULL,rawData=NULL){
  if(!("symbolic_tbl" %in% class(data))){
    stop("data must be a RSDA object. Missing \"symbolic_tbl\"")
  }
  pkg.env$intervalData <- data
  #if having row data
  if((!is.null(rawData))){
    if(!is.data.frame(rawData)){
      stop("rawData must be a data frame")
    }
    if(!isMatch(data,rawData)){
      stop("Cannot match data and rawData")
    }
  }
  numericData <- unlist(lapply(as.data.frame(data[,1:dim(data)[2]]) ,FUN = RSDA::is.sym.interval))
  numericData <- data[,numericData]

  pkg.env$statisticsDF <- buildStatsDf(numericData)
  names(pkg.env$statisticsDF) <- c("min","median","max")

  symObj<-ggESDA$new(rawData=rawData,
                         statisticsDF=pkg.env$statisticsDF,
                         intervalData=pkg.env$intervalData,
                         clusterResult = pkg.env$result)
  return(symObj)
}

isMatch<-function(D,rowD){
  a<-dim(rowD)[2]>=dim(D)[2]
  b<-colnames(D)%in%colnames(rowD)
  if(a&&b){
    return(TRUE)
  }
  return(FALSE)
}
pkg.env <- new.env()
pkg.env$statistics <- c("min","median","max","mean")
pkg.env$statisticsDF<-NULL
pkg.env$result<-NULL
pkg.env$intervalData<-NULL
pkg.env$rawData<-NULL
