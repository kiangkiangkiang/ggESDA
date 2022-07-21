#' Generic function for the covariance
#' @name cov
#' @aliases cov
#' @author Oldemar Rodriguez Rojas
#' @description This function compute the symbolic covariance.
#' @param x First symbolic variables.
#' @param y Second symbolic variables.
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation of)
#'  one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete',
#'  or 'pairwise.complete.obs'.
#' @param method The method to be use.
#' @param na.rm As in R cov function.
#' @param ... As in R cov function.
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
#' @keywords Symbolic Covariance
#' @export
cov <- function(x, ...) {
  UseMethod("cov", x)
}

#' @rdname cov
#' @export
cov.default <- function(x, y = NULL, use = "everything",
                        method = c("pearson", "kendall", "spearman"), ...) {
  stats::cov(x, y, use, method)
}

#' @rdname cov
#' @export
cov.symbolic_interval <- function(x, y, method = c("centers", "BD"),
                                  na.rm = FALSE, ...) {
  Gj <- function(a, b, vmean) {
    if ((a + b) / 2 <= vmean) {
      return(-1)
    } else {
      return(1)
    }
  }
  Qj <- function(a, b, vmean) {
    return((a - vmean)^2 + (a - vmean) * (b - vmean) + (b - vmean)^2)
  }
  method <- match.arg(method)
  if (method == "centers") {
    out <- cov((min(x) + max(x)) / 2, (min(y) + max(y)) / 2)
    return(out)
  }
  if (method == "BD") {
    ss <- 0
    vmean.x <- mean(x, method = "centers")
    vmean.y <- mean(y, method = "centers")

    for (i in seq_len(length(x))) {
      ss <- ss + Gj(min(x[i]), max(x[i]), vmean.x) * Gj(
        min(y[i]),
        max(y[i]), vmean.y
      ) * sqrt(Qj(min(x[i]), max(x[i]), vmean.x) *
                 Qj(min(y[i]), max(y[i]), vmean.y))
    }
    return((1 / (3 * length(x))) * ss)
  }
}
