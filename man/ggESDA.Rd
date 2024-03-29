% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggESDA.R
\name{ggESDA}
\alias{ggESDA}
\title{A symbolic object by R6 class for interval analysis and ggplot}
\description{
This is an object that will be used to make a ggplot
object.A ggESDA object contains both classic data that user have
and interval data which we transform.More over,some basic statistics
from row data will also be recorded in this object,and the interval
data which is from RSDA transformation will still contain RSDA
properties.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{rawData}}{the data from user.}

\item{\code{statisticsDF}}{contains min max mean median dataframe for each group of symbolic data}

\item{\code{intervalData}}{interval data from RSDA type}

\item{\code{clusterResult}}{clustering result}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ggESDA-new}{\code{ggESDA$new()}}
\item \href{#method-ggESDA-clone}{\code{ggESDA$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ggESDA-new"></a>}}
\if{latex}{\out{\hypertarget{method-ggESDA-new}{}}}
\subsection{Method \code{new()}}{
initialize all data, check whether satisfy theirs form
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ggESDA$new(
  rawData = NULL,
  statisticsDF = NULL,
  intervalData = NULL,
  clusterResult = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{rawData}}{Classical data frame.}

\item{\code{statisticsDF}}{Data frame contained the statistic of raw data.}

\item{\code{intervalData}}{Interval-valued data frame.}

\item{\code{clusterResult}}{The clustering result from classical data drame to interval-valued data frame.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ggESDA-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ggESDA-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ggESDA$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
