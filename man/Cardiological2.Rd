% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{Cardiological2}
\alias{Cardiological2}
\title{Cardiological data example}
\format{
An object of class \code{symbolic_tbl} (inherits from \code{tbl_df}, \code{tbl}, \code{data.frame}) with 15 rows and 3 columns.
}
\usage{
data(Cardiological2)
}
\description{
Cardiological interval data example.
}
\examples{
data(Cardiological2)
ggInterval_index(Cardiological2, aes(x = Syst))
}
\references{
Billard L. and  Diday E. (2006).Symbolic data analysis:
Conceptual statistics and data mining. Wiley, Chichester.
}
\keyword{datasets}
