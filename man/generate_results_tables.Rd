% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sTabl3R.v.0.6.0.0.R
\name{generate_results_tables}
\alias{generate_results_tables}
\title{Generate results tables from \code{\link[=generate_statistics]{generate_statistics()}} output.}
\usage{
generate_results_tables(
  results,
  font_size = 12,
  print_categorical = TRUE,
  print_continuous = TRUE
)
}
\arguments{
\item{results}{A list from \code{\link[=generate_statistics]{generate_statistics()}} that contains the
results of the statistical tests. It should be an S3 object of class \code{sTable}
for multiple group comparisons or \code{ssTable} for single group comparisons.}

\item{font_size}{An integer that specifies the font size for the flextables. Default is 12.}

\item{print_categorical}{Logical. Should the categorical results, if present,
be included in the output. Default is TRUE.}

\item{print_continuous}{Logical. Should the continuous results, if present,
be included in the output? Default is TRUE.}
}
\value{
A list with knitr::kable output and flextables for each variable in
the results. If the input results are of class 'sTable', it returns a list
of knitr::kable and flextables for the continuous and/or categorical variables.
If the input results are of class \code{ssTable}, it returns a knitr::kable list
with a continuous variables table, and a list with any categorical variable
tables that have been generated, as well as flextables for
the continuous variables and for each categorical variable.
}
\description{
This function generates tables using \code{knitr::kable} and \code{flextable} for
the results of statistical tests. It supports both single group and
multiple group comparisons. For single group comparisons, it generates descriptive summary statistics.
For multiple group comparisons, it generates summary tables with group
comparison statistics.
}
\examples{
results <- generate_statistics(data, group = "My_Grouping_Variable")
tables <- generate_results_tables(results)
}
