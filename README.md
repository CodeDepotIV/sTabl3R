# sTabl3R: Quick Summary Statistics in R
Quickly generate general statistics and summary tables for demographic/phenotypic/clinical variables using a formatted data frame and a specified grouping variable. This project was motivated by a perceived need to accelerate data table production using some reasonable (albeit general) assumptions about the provided data.    

In its current form, the package consists of a set of two major R functions to: (1) perform statistical analysis on a given data frame using a pre-specified grouping variable; and (2) generate tables using a combination of ``knitr::kable`` and ``flextable``. See package documentation for additional details.

To install:
The latest version can be found on [GitHub](https://github.com/CodeDepotIV/sTabl3R), and installed using `devtools`.

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("CodeDepotIV/sTabl3R")
```

Trivia: While the name ``sTabl3R`` was chosen to signify its role as a statistics table utility in R, it was also done with an intentional nod to Hall of Fame NFL quarterback Ken Stabler.

# Quickstart:
``` r
library(sTabl3R)

data(mtcars)

# Generate a grouping variable
mtcars$Group <- as.factor(rep("GroupA", nrow(mtcars))) # Single group case
res1 <- generate_statistics(mtcars, group = "Group")
generate_results_tables(res1)

data(mtcars)
res2 <- generate_statistics(mtcars, group = "cyl") # Multiple group levels
generate_results_tables(res2)
```
