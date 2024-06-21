# sTabl3R: Quick Summary Statistics in R
Quickly generate general statistics and summary tables for demographic/phenotypic/clinical variables using a formatted data frame and a specified grouping variable. This project was motivated by a perceived need to accelerate data table production using some reasonable (albeit general) assumptions about the provided data.    

In its current form, the repository consists of a set of two major R functions to: (1) perform statistical analysis on a given data frame using a pre-specified grouping variable; and (2) generate tables using a combination of ``knitr::kable`` and ``flextable``. See package documentation for additional details.

To install:
``if (!require(devtools)) install.packages("devtools")
devtools::install_github("CodeDepotIV/sTabl3R")``

Trivia: While the name ``sTabl3R`` was chosen to signify its role as a statistics table utility in R, it was also done with an intentional nod to Hall of Fame NFL quarterback Ken Stabler.

