% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSDA2sym.r
\name{RSDA2sym}
\alias{RSDA2sym}
\title{RSDA object to symbolic object for ggplot}
\usage{
RSDA2sym(data=NULL,rawData=NULL)
}
\arguments{
\item{data}{an interval data, which may transfrom by RSDA::classic.to.sym
.Note:data is a necessary parameter,and must have symbolic_tbl class.}

\item{rawData}{rawData, which can be transformed to interval data,
must be a data frame and match to data.}
}
\value{
Return an object of class "ggESDA", which
have a interval data and others as follows.
\itemize{
  \item intervalData - The Interval data after converting also known
  as a RSDA object.
  \item rawData - Classical data that user input.
  \item clusterResult - Cluster results .If the groupby method is
  a clustering method then it will exist.
  \item statisticsDF - A list contains data frame including some
  typically statistics in each group.
}#'
}
\description{
It will be a good way to unify all symbolic data
object in R that collects all useful symbolic analysis tools
such like RSDA into the same class for management. In this way,
user who wants to do some study in symbolic data will be more
convenient for searching packages.Thus,RSDA2sym collecting RSDA
object into ggESDA object will do for plot(ggplot) and
RSDA's analysis.
}
\examples{
r<-ggESDA::Cardiological
mySym<-RSDA2sym(r)
mySym$intervalData

}
