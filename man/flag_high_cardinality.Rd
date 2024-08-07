% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sTabl3R.v.0.6.0.0.R
\name{flag_high_cardinality}
\alias{flag_high_cardinality}
\title{Flag High Cardinality Function}
\usage{
flag_high_cardinality(df, threshold = NULL, .group_null = "Not_a_group")
}
\arguments{
\item{df}{A \code{\link[=data.frame]{data.frame()}} object that is to be checked for high cardinality columns.}

\item{threshold}{A positive integer that sets the limit for what is considered high
cardinality. If no threshold is provided, it defaults to 5\% of the number of
observations in the data frame.}
}
\value{
A character vector containing the names of the high cardinality columns.
}
\description{
This function identifies and returns the names of the columns in a data frame
that have a high cardinality (i.e., the number of unique values in the column
exceeds a specified threshold). This is function is intended to scan a
matrix-like object (arranged with variables in columns and observations in
rows) for categorical variables that may require further attention and cleaning.
}
\examples{
df <- data.frame(A = seq(1,26), B = letters[1:26], C = c(rep("foo", 25), "bar"))
flag_high_cardinality(df)
}
