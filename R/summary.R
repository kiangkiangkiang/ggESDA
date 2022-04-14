#' @name summary
#' @aliases summary
#' @title summary for symbolic data table
#' @description summary for symbolic data table
#' @param object an object for which a summary is desired.
#' @param x a result of the default method of summary().
#' @param maxsum integer, indicating how many levels should be shown for factors.
#' @param digits integer, used for number formatting with signif() (for summary.default) or format() (for summary.data.frame). In summary.default, if not specified (i.e., missing(.)), signif() will not be called anymore (since R >= 3.4.0, where the default has been changed to only round in the print and format methods).
#' @param quantile.type integer code used in quantile(*, type=quantile.type) for the default method.
#' @param summary_fun only works when the symbolic_modal class input, it determine which summary function be applied for each modal.
#' @param ... additional arguments affecting the summary produced.
#' @return Return a summary table.
#' @examples
#'
#' #For all interval-valued
#' summary(facedata)
#'
#' #For both interval-valued and modal multi-valued
#' summary(Environment)
#'
#' summary(Environment$URBANICITY, summary_fun = "quantile")
#'
#'
#' @keywords Symbolic summary
#' @export
summary <- function(object, ...) {
  UseMethod("summary")
}

#' @rdname summary
#' @export
summary.default <- function(object, ...) {
  tryCatch({
    eval(parse(text = paste0("base::summary.", class(object)[1])))(object, ...)
  },error = function(err1) {
    tryCatch({
      base::summary(object, ...)
    },error = function(err2) {
      base::summary.default(object, ...)
    })
  })
}

#' @rdname summary
#' @export
summary.symbolic_tbl <- function(object, ...){
  pkg.env$inPackage <- TRUE
  iData.boolean <- unlist(lapply(object, RSDA::is.sym.interval))
  mData.boolean <- unlist(lapply(object, RSDA::is.sym.modal))
  if(!all(iData.boolean|mData.boolean)){
    stop("Non-symbolic object detected. Please use classic2sym() to transform data into symbolic_tbl.")
  }
  result <- list(symbolic_interval = NULL, symbolic_modal = NULL)

  #For interval-valued data
  iData_ind <- which(iData.boolean)
  if(length(iData_ind) > 0){
    tmp <- data.frame(matrix(0, nrow = 7, ncol = 1))#7: summary interval data will return 7 measures
    for(i in iData_ind){
      tmp <- cbind(tmp, summary.symbolic_interval(object[, i][[1]]))
    }
    tmp <- tmp[, -1]
    if(class(tmp)[1] == "symbolic_interval"){
      tmp <- data.frame(tibble(tmp))
      rownames(tmp) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Std.")
    }
    colnames(tmp) <- colnames(object)[iData_ind]
    result$symbolic_interval <- tmp
  }else{
    result <- within(result, rm(symbolic_interval))
  }

  #For modal-multi valued data
  mData_ind <- which(mData.boolean)
  if(length(mData_ind) > 0){
    tmp <- list(NULL)
    for(i in 1:length(mData_ind)){
      tmp[[i]] <- summary.symbolic_modal(object[, mData_ind[i]][[1]], ...)
    }
    myMax <- max(unlist(lapply(tmp, length)))
    myMat <- matrix("", ncol = length(mData_ind), nrow = myMax)

    for(i in 1:length(tmp)){
      myMat[1:nrow(tmp[[i]]), i] <- tmp[[i]]
    }
    colnames(myMat) <- colnames(object)[mData_ind]
    result$symbolic_modal <- noquote(myMat)
  }else{
    result <- within(result, rm(symbolic_modal))
  }
  pkg.env$inPackage <- FALSE
  return(result)

}

#' @rdname summary
#' @export
summary.symbolic_interval <- function(object, ...){
  result <- NULL
  x <- object
  m1 <- min(x)
  m2 <- max(x)
  d <- data.frame(min_ = c(quantile(m1), mean(x), RSDA::sd(x)),
                  max_ = c(quantile(m2), mean(x), RSDA::sd(x)))

  d2 <- classic2sym(d, groupby = "customize",
                    minData = d$min_,
                    maxData = d$max_)
  myNames <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Std.")
  if(pkg.env$inPackage){
    result <- as.data.frame(d2$intervalData[c(1:3, 6, 4:5, 7),])
    rownames(result) <- myNames
    class(result) <- c("symbolic_tbl", class(result))
  }else{
    result <- c(d2$intervalData[c(1:3, 6, 4:5, 7),])[[1]]
    names(result) <- myNames
  }

  return(result)
  #summary.symbolic_modal(object, ...) works
}

#' @rdname summary
#' @export
summary.symbolic_modal <- function(object, summary_fun = "mean", ...){
  x <- object
  d <- data.frame(NULL)
  for(i in 1:length(x)){
    d <- rbind(d, x[[i]]$prop)
  }
  colnames(d) <- x[[1]]$var
  result <- round(apply(d, 2, eval(parse(text = summary_fun))), 2)
  if(pkg.env$inPackage){
    if(!is.null(dim(result))){
      result <- round(apply(d, 2, mean, 2))
      warning("Dimension Error in summary_fun input. Autoadjust to mean summary.")
    }
    result <- paste(names(result), sprintf(result, fmt = "%.2f"), sep = ": ")
    result <- noquote(cbind(result))
  }
  #colnames(result) <- deparse(as.list(match.call())$object)
  return(result)
}
pkg.env <- new.env()
pkg.env$inPackage <- FALSE


