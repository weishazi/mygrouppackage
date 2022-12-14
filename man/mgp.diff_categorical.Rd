% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mgp.diff_categorical.R
\name{mgp.diff_categorical}
\alias{mgp.diff_categorical}
\title{Difference Comparison of Categorical Variables}
\usage{
mgp.diff_categorical(
  data = data,
  variables = variables,
  group = group,
  prop = c(1, 2),
  correct = F
)
}
\arguments{
\item{data}{a data.frame.}

\item{variables}{a character string indicating the categorical variables.}

\item{group}{a character string indicating the grouping variable.}

\item{prop}{a number 1 or 2.}

\item{correct}{a logical value. Default is "F". Is the chi-square test corrected?}
}
\value{
a data.frame object df_result
}
\description{
Difference Comparison of Categorical Variables
}
