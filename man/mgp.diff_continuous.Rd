% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mgp.diff_continuous.R
\name{mgp.diff_continuous}
\alias{mgp.diff_continuous}
\title{Difference Comparison of Continuous Variables}
\usage{
mgp.diff_continuous(data = data, variables = variables, group = group)
}
\arguments{
\item{data}{a data.frame.}

\item{variables}{a character string indicating the categorical variables.}

\item{group}{a character string indicating the grouping variable.}
}
\value{
a data.frame object df_resul
}
\description{
Difference Comparison of Continuous Variables
}
\examples{
mgp.diff_continuous(data = iris,
                              variables = colnames(iris)[1:4],
                             group = colnames(iris)[5])

}
