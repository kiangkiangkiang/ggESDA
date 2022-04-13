#' @name scale
#' @aliases scale
#' @title scale for symbolic data table
#' @description scale for symbolic data table
#' @importFrom RSDA is.sym.interval
#' @param x A ggESDA object. It can also be either RSDA object or
#' classical data frame, which will be automatically convert to ggESDA
#' data.
#' @param center same as base::scale, either a logical value or numeric-alike vector of length equal to the number of columns of x, where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.
#' @param scale same as base::scale, either a logical value or a numeric-alike vector of length equal to the number of columns of x.
#' @param ... Used by other R function.
#' @return Return a scale ggESDA object.
#' @examples
#'
#' #For all interval-valued
#' scale(facedata)
#'
#' #For both interval-valued and modal multi-valued
#' scale(mtcars.i)
#'
#' @keywords Symbolic scale
#' @export
scale <- function(x, ...) {
  UseMethod("scale")
}

#' @rdname scale
#' @export
scale.default <- function(x, center = TRUE, scale = TRUE, ...) {
  base::scale.default(x, center, scale)
}

#' @rdname scale
#' @export
scale.symbolic_tbl <- function(x, ...){

  #test data illegal
  ggSymData <- testData(x)
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

#' @rdname scale
#' @export
scale.symbolic_interval <- function(x, ...){
  s <- RSDA::sd(x)
  m <- mean(x)
  m1 <- (min(x) - m)/s
  m2 <- (max(x) - m)/s
  d <- data.frame(m1 = m1, m2 = m2)
  d2 <- classic2sym(d, groupby = "customize",
                    minData = d$m1,
                    maxData = d$m2)

  return(d2$intervalData$V1)
}
