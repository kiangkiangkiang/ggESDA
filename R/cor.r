#' Generic function for the correlation
#' @name cor
#' @aliases cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' correlation in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm As in R cor function.
#' @param ... As in R cor function.
#'
#' @return Return a real number.
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @keywords Symbolic correlation
#' @export
cor <- function(x, ...) {
  UseMethod("cor", x)
}

#' @rdname cor
#' @export
cor.default <- function(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"), ...) {
  stats::cor(x, y, use, method)
}

#' @rdname cor
#' @export
cor.symbolic_tbl <- function(x, ...) {
  iData <- x

  isnumericData <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]) ,FUN = is.sym.interval))
  numericData <- data.frame(iData[,which(isnumericData)])
  p <- ncol(numericData)

  d <- sapply(1:p, function(a) sapply(1:p, function(b) cor(numericData[[a]], numericData[[b]], ...)))
  d <- as.data.frame(d)
  colnames(d) <- colnames(iData[,which(isnumericData)])
  rownames(d) <- colnames(iData[,which(isnumericData)])
  return(d)

}

#' @rdname cor
#' @export
cor.symbolic_interval <- function(x, y, method = c("centers", "B", "BD", "BG"), ...) {
  if(method == "centers"){
    out <- stats::cor((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
  }else{
    out <- cov(x, y, method, ...) / (RSDA::sd(x) * RSDA::sd(y))
  }
  return(out)
}

