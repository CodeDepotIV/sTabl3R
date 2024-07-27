# sTabl3R: Quick Statistical Testing and Summary Tables in R
Quickly generate general statistical hypothesis tests and summary tables for demographic/phenotypic/clinical variables using a formatted data frame and a specified grouping variable. This project was motivated by a perceived need to accelerate data table production using some reasonable (albeit general) assumptions about the provided data.    

In its current form, the package consists of a set of two major R functions to: (1) perform statistical analysis on a given data frame using a pre-specified grouping variable; and (2) generate tables using a combination of ``knitr::kable`` and ``flextable``. Some additional functions (≥ v0.5.0) are available to allow the user to access results more easily and quickly identify categorical variables that may need to be recoded. See package documentation for additional details.

To install:
The latest version can be found on [GitHub](https://github.com/CodeDepotIV/sTabl3R), and installed using `devtools`.

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("CodeDepotIV/sTabl3R")
```

Or for the development version:

```r
devtools::install_github("CodeDepotIV/sTabl3R", ref = "dev")
```
Trivia: While the name ``sTabl3R`` was chosen to signify its role as a statistics table utility in R, it was also done with an intentional nod to Hall of Fame NFL quarterback Ken Stabler.

# Quick start:
``` r
library(sTabl3R)

data(mtcars)

# Generate a grouping variable
mtcars$Group <- as.factor(rep("GroupA", nrow(mtcars))) # Single group case
res1 <- generate_statistics(mtcars, group = "Group")
summary(res1)
generate_results_tables(res1)

data(mtcars)
res2 <- generate_statistics(mtcars, group = "cyl") # Multiple group levels
summary(res2)
generate_results_tables(res2)
```

# Here's the deal:

The package is built around a central ``generate_statistics()`` function. This function function attempts to identify numeric (continuous and ordinal) and categorical variables within a user-supplied data frame.

For each identified numeric variable, Shapiro-Wilk testing is employed to check for normality. Provided that the user does not specify ``force_nonparametric = TRUE`` in the arguments, each numeric variable will be subjected either to parametric (t-test or one-way ANOVA) or non-parametric (Mann-Whitney U test or Kruskal-Wallis test) comparisons depending on the number of strata in the grouping variable. For instances where the grouping variable has a single level, only summary statistics will be generated.

For categorical variables, the function will apply either the Chi-squared test or the Fisher’s Exact test based on the expected frequencies in each contingency table, unless the grouping variable has a single level, in which case only summary tables will be generated.

**FORMATTING:** The data frame should be an R ``data.frame()`` object in which the first column is a list of unique identifiers. The **group** argument should identify a column in the data frame that will be used to distinguish the groups that are to be compared statistically. *Please note* that if unique identifiers are not present in the first column, unique identifiers will be appended to the dataframe.

The other functions include ``extract_stats()`` which allows the user to extract the statistical results for a variable of interest quickly, ``generate_results_tables()`` which generates a series of tables displaying all of the results generated following a call to ``generate_statistics()``, and ``summary()`` functions to show which variables in the user input were used in the analysis and how they were categorized. 

# New features as they arrive:
**v0.6.0.0** Added ``flag_high_cardinality()`` function to scan a dataset for categorical variables that may require recoding. 
