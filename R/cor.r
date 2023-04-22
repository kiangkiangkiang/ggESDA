#' Generic function for the correlation
#' @name cor
#' @aliases cor
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic correlation
#' @param x First symbolic num_of_variables.
#' @param y Second symbolic num_of_variables.
#' @param use an optional character string giving a method for computing
#' correlation in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
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

  is_interval_data <- unlist(lapply(data.frame(iData[1:dim(iData)[2]]), FUN = is.sym.interval))
  interval_data <- data.frame(iData[, which(is_interval_data)])
  num_of_variables <- ncol(interval_data)

  cor_matrix <- sapply(1:num_of_variables, function(x) sapply(1:num_of_variables, function(y) cor(interval_data[[x]], interval_data[[y]], ...)))
  cor_matrix <- as.data.frame(cor_matrix)
  colnames(cor_matrix) <- colnames(iData[, which(is_interval_data)])
  rownames(cor_matrix) <- colnames(iData[, which(is_interval_data)])
  return(cor_matrix)
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

