#' @name scale_sym
#' @title scale for symbolic data table
#' @description scale for symbolic data table
#' @importFrom RSDA is.sym.interval
#' @param data A ggESDA object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggESDA
#' data.
#' @return Return a scale ggESDA object.
#' @usage scale.sym(data = NULL)
#' @examples
#'
#' #For all interval-valued
#' scale.sym(facedata)
#'
#' #For both interval-valued and modal multi-valued
#' scale.sym(mtcars.i)
#'
#' @export
scale_sym <- function(data = NULL){

  #test data illegal
  ggSymData <- testData(data)
  iData <- ggSymData$intervalData


  #get interval-valued col
  interval.index <- lapply(1:ncol(iData), FUN = function(x) RSDA::is.sym.interval(iData[[x]]))
  none.interval.index <- which(!unlist(interval.index))
  interval.index <- which(unlist(interval.index))
  if(length(interval.index) == 0)
    stop("Cannot find interval-valued variables to scale.")
  n <- dim(iData)[1]

  #scale
  temp1 <- sapply(interval.index, FUN = function(x) unlist(data.frame(iData[[x]])))
  temp2 <- apply(temp1, 2, scale)
  newd <- data.frame(temp2[1:n, ], temp2[(n+1):(n*2), ])
  myd <- classic2sym(newd, groupby = "customize",
                     minData = temp2[1:n, ],
                     maxData = temp2[(n+1):(n*2), ])

  #initial result & merge interval-valued and modal
  result <- data.frame(matrix(NA, nrow = nrow(iData), ncol = ncol(iData)))
  result[, interval.index] <- myd$intervalData
  if(length(none.interval.index) != 0)
    result[, none.interval.index] <- iData[, none.interval.index]
  myd$intervalData <- result
  colnames(myd$intervalData) <- colnames(iData)
  rownames(myd$intervalData)<- rownames(iData)

  if(!("symbolic_tbl" %in% class(myd$intervalData))){
    class(myd$intervalData) <- c(class(myd$intervalData), "symbolic_tbl")
  }

  return(myd)
}
