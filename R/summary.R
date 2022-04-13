#' @name summary
#' @aliases summary
#' @title summary for symbolic data table
#' @description summary for symbolic data table
#' @param object an object for which a summary is desired.
#' @param x a result of the default method of summary().
#' @param maxsum integer, indicating how many levels should be shown for factors.
#' @param digits integer, used for number formatting with signif() (for summary.default) or format() (for summary.data.frame). In summary.default, if not specified (i.e., missing(.)), signif() will not be called anymore (since R >= 3.4.0, where the default has been changed to only round in the print and format methods).
#' @param quantile.type integer code used in quantile(*, type=quantile.type) for the default method.
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
  print("in sym tbl")
}

#' @rdname summary
#' @export
summary.symbolic_interval <- function(object, ...){
  print("in summary.symbolic_interval")
  x <- object
  m1 <- min(x)
  m2 <- max(x)
  d <- data.frame(min_ = c(quantile(m1), mean(x), RSDA::sd(x)),
                  max_ = c(quantile(m2), mean(x), RSDA::sd(x)))

  d2 <- classic2sym(d, groupby = "customize",
                    minData = d$min_,
                    maxData = d$max_)

  result <- as.data.frame(d2$intervalData[c(1:3, 6, 4:5, 7),])
  rownames(result) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Std.")
  class(result) <- c("symbolic_tbl", class(result))
  return(result)
  #summary.symbolic_modal(object, ...) works
}

#' @rdname summary
#' @export
summary.symbolic_modal <- function(object, ...){
  print("in symbolic_modal")
}



