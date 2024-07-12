#' Check input for [generate_statistics()] function.
#'
#' Checks to determine if the input is a data frame, whether the first column
#' contains unique IDs, and whether the grouping variable is present. If the
#' first row of the data frame does not contain unique IDs, these will be
#' supplied. If the grouping variable isn’t already a factor, it will be be
#' coerced to one.
#'
#' Error checking is currently limited, and will be expanded as testing
#' continues.
#'
#' @param df A [data.frame()] object. See `FORMATTING` in [generate_statistics()].
#' @param group A grouping variable.
#'
#' @examples
#' data(mtcars)
#' check_input(mtcars) # Generates error messages
#'
#' # Make grouping variable
#' mtcars$Group <- rep("Group1", nrow(mtcars))
#' check_input(mtcars, group = "Group") # Warning message about identifiers
#' @return A data frame, modified as necessary, to be used as input for the
#' [generate_statistics()] function.
#'
#' @export
check_input <- function(df, group) {
  # Check to see if 'df' is a data frame
  if (!is.data.frame(df)) {
    stop("ERROR: The input is not a dataframe.")
  }
  # Check if the first column is a unique ID
  if(any(duplicated(df[,1]))){
    message("Non-unique row IDs in column 1 of data frame object.")
    message("WARNING: Deleting column 1 and replacing with unique IDs.")
    df <- data.frame(ID = paste0("Sample",seq_along(df[[1]])), df[,-1])
  }
  # Check if the group argument is a column in the dataframe
  if (!group %in% names(df)) {
    stop(paste("The group argument", group, "is not a column in the dataframe."))
  }
  # Coerce group to factor if it is not already a factor
  if (!is.factor(df[[group]])) {
    df[[group]] <- as.factor(df[[group]])
  }
  # Arrange the dataframe so the group variable is the second column
  df <- df[, c(1, which(names(df) == group),
               setdiff(2:ncol(df), which(names(df) == group)))]
  # Return the modified dataframe
  return(df)
}

dTable <- function(obj) {
  class(obj <- "sTable")
  return(obj)
} # Need accessor, maybe drop this idea for now

#' Generate statistical summary of a data frame.
#'
#' This function will perform a quick statistical analysis on a supplied
#' data frame (df) using a specified grouping variable (group). Overall, this
#' function is intended to automate the process of simple statistical analysis
#' for a data set, and may be of use in exploratory data analysis.
#'
#' Following limited error checking, the function attempts to identify
#' numeric (continuous and ordinal) and categorical variables within the
#' data frame.
#'
#' For each identified numeric variable, Shapiro-Wilk testing is employed to
#' check for normality. Then, each numeric variable is subjected either to
#' parametric (t-test or one-way ANOVA) or non-parametric (Mann-Whitney U test
#' or Kruskal-Wallis test) comparisons depending on the number of
#' strata in the grouping variable. For instances where the grouping variable
#' has a single level, only summary statistics will be generated.
#'
#' For categorical variables, the function will apply either the Chi-squared test
#' or the Fisher’s Exact test based on the expected frequencies in each contingency
#' table, unless the grouping variable has a single level, in which case only
#' summary tables will be generated.
#'
#' `FORMATTING`
#' The data frame should be an R [data.frame()] object in which the first
#' column is a list of unique identifiers. The `group` argument should identify
#' a column in the data frame that will be used to distinguish the groups that
#' are to be compared statistically. A single group can be generated as shown in
#' the examples below for the ``mtcars`` dataset.
#'
#' TODO: Consider writing accessors for the output to ease data extraction.
#'
#' @param df A [data.frame()] object.
#' @param group A grouping variable. Defaults to group = "Group".
#' @return
#' The function returns a list containing counts, continuous variable tables
#' with associated statistical results, and, for categorical variables,
#' contingency tables with associated statistical results. The formatting of
#' the output differs slightly depending on the number of levels in the grouping
#' variable.
#'
#' The output is an informal S3 list of class `sTable` or `ssTable` that may
#' either be examined directly or used as input for the [generate_results_tables()]
#' function.
#'
#' Structure of the `sTable` class:
#'  
#'  List of 3      :      
#'   $ Counts      :   data.frame of counts (samples per group)
#'   $ Continuous  :   if present, summary tables and statistics for each variable
#'   $ Categorical :   if present, contingency tables and statistics for each variable
#'
#' Structure of the `ssTable` class:
#'  
#'  List of 3      :
#'   $ Counts      :   data.frame of counts (number of samples)
#'   $ Continuous  :   if present, summary table of continuous variables
#'   $ Categorical :   if present, single group contingency tables for each variable
#'
#' @examples
#' data(mtcars)
#' # Generate a grouping variable
#' mtcars$Group <- as.factor(rep("GroupA", nrow(mtcars))) # Single group case
#' res1 <- generate_statistics(mtcars, group = "Group")
#' generate_results_tables(res1)
#'
#' data(mtcars)
#' res2 <- generate_statistics(mtcars, group = "cyl") # Multiple group levels
#' generate_results_tables(res2)
#' @export
#' 
generate_statistics <- function(df, group = "Group"){

  # Functions
  # Error checking
  df <- check_input(df, group)

  check_numeric_var <- function(num_var) {
    num_var <- num_var[!is.na(num_var)]
    # Check if the numeric variable has more than one unique value
    if (length(unique(num_var)) <= 1) {
      message("Bad variable. Check user entry:")
      message(num_var)
      stop("Numeric variable has identical values or is not a numeric variable.")
    }
    return(TRUE)
  }

  find_variable_types <- function(df) {
    sapply(df, function(x) {
      if (any(class(x) %in% c("integer", "numeric"))) {
        return("numeric")
      } else {
        return(class(x))
      }
    })
  }

  find.numeric <- function(df) {
    types <- find_variable_types(df)
    return(types[types == "numeric"] |> names())
  }

  find.categorical <- function(df) {
    types <- find_variable_types(df)
    return(types[types != "numeric"] |> names())
  }

  run_num_stats <- function(df, group_var, num_var) {
    # Check if the variables exist in the dataframe
    if (!(group_var %in% names(df)) || !(num_var %in% names(df))) {
      stop("Variables not found in the dataframe")
    }

    # Extract the grouping and numeric variables
    group <- df[[group_var]]
    num <- df[[num_var]]
    
    # Check if the numeric variable is valid for the Shapiro-Wilk test
    if (!check_numeric_var(num)) {
      return(NULL)
    }

    # Perform Shapiro-Wilk test
      # TODO: Consider using a different test
      # TODO: Sometimes you want just non-parametric, so an improved package would 
      # allow the use to override this. Coder needs to develop comfort with 
      # using additional function arguments.
    shapiro_test <- shapiro.test(num)

    # Determine if the data is parametric or non-parametric
    is_parametric <- shapiro_test$p.value > 0.05

    # Initialize an empty list to store test results
    test_results <- list()

    if (is_parametric) {
      
      # Parametric data
      levels_in_group <- length(unique(group))

      if (levels_in_group == 2) {
      
        # Two levels in group, perform t-test
        test_results$t_test <- t.test(num ~ group, data = df)
      
      } else if (levels_in_group > 2) {
        
        # More than two levels, perform ANOVA
        test_results$aov <- aov(num ~ group, data = df)
      
      }
      
    } else {
      
      # Non-parametric data
      levels_in_group <- length(unique(group))

      if (levels_in_group == 2) {
        
        # Two levels in group, perform Mann-Whitney U test
        if (anyDuplicated(num)) {
          
          message(paste0("Ties detected in the data: ", num_var))
          message("The p-value from the Wilcoxon Rank Sum test will be approximated.")
          
          test_results$mann_whitney <- 
            wilcox.test(num ~ group, data = df, exact = FALSE)
        
        } else {
          
          test_results$mann_whitney <- wilcox.test(num ~ group, data = df)
        
        }
        
      } else if (levels_in_group > 2) {
      
        # More than two levels, perform Kruskal-Wallis test
        test_results$kruskal_wallis <- kruskal.test(num ~ group, data = df)
      
      }
      
    }
    return(test_results)
  }

  parse_num_stats_res <- function(res_stats) {
    # Initialize an empty list to store the parsed results
    parsed_results <- list()

    # Determine the type of test and extract the relevant statistics
    if ("kruskal_wallis" %in% names(res_stats)) {
      
      parsed_results$test_name <- "Kruskal-Wallis"
      parsed_results$stat_name <- "χ²"
      parsed_results$test_statistic <- unname(res_stats$kruskal_wallis$statistic)
      parsed_results$deg_freedom <- unname(res_stats$kruskal_wallis$parameter)
      parsed_results$p_value <- res_stats$kruskal_wallis$p.value
      
    } else if ("aov" %in% names(res_stats)) {
      
      parsed_results$test_name <- "One-Way ANOVA"
      parsed_results$stat_name <- "F"
      parsed_results$test_statistic <- summary(res_stats$aov)[[1]]$'F value'[1]
      parsed_results$deg_freedom <- 
        paste0("(", summary(res_stats$aov)[[1]]$'Df'[1], ",",
             summary(res_stats$aov)[[1]]$'Df'[2], ")"  )
      parsed_results$p_value <- summary(res_stats$aov)[[1]]$'Pr(>F)'[1]
      
    } else if ("t_test" %in% names(res_stats)) {
      
      parsed_results$test_name <- "Student's t"
      parsed_results$stat_name <- "t"
      parsed_results$test_statistic <- unname(res_stats$t_test$statistic)
      parsed_results$deg_freedom <- 
        unname(round(res_stats$t_test$parameter,digits = 2))
      parsed_results$p_value <- res_stats$t_test$p.value
      
    } else if ("mann_whitney" %in% names(res_stats)) {
      
      parsed_results$test_name <- "Wilcoxon Rank Sum"
      parsed_results$stat_name <- "W"
      parsed_results$test_statistic <- unname(res_stats$mann_whitney$statistic)
      parsed_results$deg_freedom <- "Not provided"
      parsed_results$p_value <- res_stats$mann_whitney$p.value
      
    }
    return(parsed_results)
  }

  choose_cont_tab_test <- function(contingency_table) {
    # Function to decide between chi squared & Fisher exact
    # Calculate the expected frequencies
    expected <- chisq.test(contingency_table,
                           simulate.p.value = TRUE, B = 2000)$expected

    # Check if any expected frequencies are less than 5
    if (any(expected < 5)) {
      
      # If there are cells with expected frequency < 5, use Fisher's Exact Test
      test_name <- "Fisher's Exact"
      stat_name <- "none"
      test_statistic <- "none"
      
      # Simulated vs exact p values
      tp_value <- NA
      tryCatch({
        tp_value <- fisher.test(contingency_table)$p.value
      }, error = function(e) {
        message("Using simulation for Fisher's test due to workspace limitations.")
        message("NOTE: These simulated p-values CAN VARY.")
        tp_value <<- fisher.test(contingency_table, simulate.p.value = TRUE)$p.value
      })
      
      deg_freedom <- "Not provided"
      p_value <- tp_value
      
    } else {
      
      # If all expected frequencies are >= 5, use Chi-squared Test
      
      test_name <- "χ²"
      stat_name <- "χ²"
      test_statistic <- chisq.test(contingency_table)$statistic
      deg_freedom <- unname(chisq.test(contingency_table)$parameter)
      p_value <- chisq.test(contingency_table)$p.value
      
    }

    # Return a list containing the test name and result
    return(list(test_name = test_name,
                stat_name = stat_name,
                test_statistic = test_statistic,
                deg_freedom = deg_freedom,
                p_value = p_value))
  }

  make_prop_table <- function(x) {
    # Function to format the categorical variable tables
    freq <- table(x, useNA="ifany")
    m <- matrix(freq, nrow=dim(freq)[1])
    prop <- sweep(m, 2, colSums(m), FUN="/") * 100
    proprnd <- round(prop, 2)
    comb <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
    
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)){
        comb[i,j] <- paste0(m[i,j], " (", proprnd[i,j], ")")
      }
    }
    
    comb <- as.matrix(comb)
    colnames(comb) <- colnames(freq)
    rownames(comb) <- rownames(freq)
    
    return(comb)
  }

  generate_one_group_summary_stats <- function(df) {
    
    df[[group]] <- droplevels(df[[group]])

    # Continuous variables one group summary
    num_vars <- find.numeric(df)

    cts_df <- data.frame(
      Variable = character(),
      Summary = character(),
      Missing = numeric(),
      stringsAsFactors = FALSE
    )

    for(nvar in num_vars) {
      if(shapiro.test(df[[nvar]])$p.value < 0.05){
        median <- summary(df[[nvar]])[3]
        qrt1 <- summary(df[[nvar]])[2]
        qrt3 <- summary(df[[nvar]])[3]
        summary_entry <- paste0(median, " [", qrt1, "-", qrt3, "]")
        missing_entry <- sum(is.na(df[[nvar]]))
      } else {
        mean <- round(summary(df[[nvar]])[4], 2)
        sd <- round(sd(df[[nvar]], na.rm = T), 2)
        summary_entry <- paste0(mean, " ± ", sd)
        missing_entry <- sum(is.na(df[[nvar]]))
      }
      row <- list()
      row$Variable <- nvar
      row$Summary <- summary_entry
      row$Missing <- missing_entry
      cts_df <- rbind(cts_df, row)
    }

    # Categorical variable summary
    cat_vars <- find.categorical(df)
    catvars_sel <- cat_vars[-1] # Omit IDs
    catvars_sel <- catvars_sel[catvars_sel != group] # Omit grouping variable

    cat_tables <- list()
    for(cvar in catvars_sel) {
      freq <- table(df[[cvar]], df[[group]], useNA="ifany")
      m <- matrix(freq, nrow=dim(freq)[1])
      prop <- sweep(m, 2, colSums(m), FUN="/") * 100
      proprnd <- round(prop, 2)
      comb <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
      for (i in 1:nrow(m)){
        for (j in 1:ncol(m)){
          comb[i,j] <- paste0(m[i,j], " (", proprnd[i,j], ")")
        }
      }
      comb <- as.matrix(comb)
      colnames(comb) <- colnames(freq)
      rownames(comb) <- rownames(freq)
      cat_tables[[cvar]] <- comb
    }

    counts <- data.frame(n = summary(df[[group]])) |> t() |> as.data.frame()

    stable_data <- list(Counts = counts,
                        Continuous = cts_df,
                        Categorical = cat_tables)

    class(stable_data) <- "ssTable"
    return(stable_data)
  }

  # # Main code # # # # # # # # # # # # # # # # # # # # # # # # # # # # ----

  # Check that the group has more that one level
  # If one level, just output summary stats tables

  group_levels <- unique(df[[group]])
  
  if(length(group_levels) == 1) {
    
    message("Single group level detected.")
    message("Generating summary statistics for single group.")
    generate_one_group_summary_stats(df)
    
  } else {
    
    message("Two or more group levels detected.")
    message("Generating group comparisons.")
    
    # Initialize an empty data frame to store the results
    results_df <- data.frame(
      Variable = character(),
      Stat_test = character(),
      Stat_Name = character(),
      Test_Statistic = numeric(),
      Deg_Freedom = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )

    # Get the stats for the numeric variables
    num_vars <- find.numeric(df)
    
    for (var in num_vars) {
      # Run the statistics function
      res_stats <- run_num_stats(df, group, var)
      # Parse the results
      parsed_stats <- parse_num_stats_res(res_stats)

      # Append the results to the results data frame
      results_df <- rbind(results_df, data.frame(
        Variable = var,
        Stat_Test = parsed_stats$test_name,
        Stat_Name = parsed_stats$stat_name,
        Test_Statistic = parsed_stats$test_statistic,
        Deg_Freedom = parsed_stats$deg_freedom,
        P_Value = parsed_stats$p_value
      ))
    }

    # Get the stats for the categorical variables
    cat_vars <- find.categorical(df)[-1] # Assumes ID is first row
    cat_vars <- cat_vars[-which(cat_vars == group)] # Drops Grouping Variable
    
    for(cvar in cat_vars) {
      # Replace empty strings with NA
      df[[cvar]][df[[cvar]] == ""] <- NA
      contingency_table <- table(df[[group]], df[[cvar]])
      res_stats <- choose_cont_tab_test(contingency_table)
      results_df <- rbind(results_df, data.frame(
        Variable = cvar,
        Stat_Test = res_stats$test_name,
        Stat_Name = res_stats$stat_name,
        Test_Statistic = res_stats$test_statistic,
        Deg_Freedom = res_stats$deg_freedom,
        P_Value = res_stats$p_value
      ))
    }

    # Tables for categorical variables
    list_of_cat_tables <- list()
    for(cvar in cat_vars) {
      
      # Generate proportion table from contingency table
      tab <- df[,c(cvar, group)]
      prop_tab <- make_prop_table(tab)
      
      list_of_cat_tables[[cvar]]$Table <- prop_tab
      list_of_cat_tables[[cvar]]$Stat_Test <-
        results_df[results_df$Variable == cvar,]$Stat_Test
      list_of_cat_tables[[cvar]]$Stat_Name <-
        results_df[results_df$Variable == cvar,]$Stat_Name
      list_of_cat_tables[[cvar]]$Test_Statistic <-
        results_df[results_df$Variable == cvar,]$Test_Statistic
      list_of_cat_tables[[cvar]]$Deg_Freedom <-
        results_df[results_df$Variable == cvar,]$Deg_Freedom
      list_of_cat_tables[[cvar]]$P_Value <-
        results_df[results_df$Variable == cvar,]$P_Value
    }

    # Tables for continuous or ordinal variables
    list_of_num_tables <- list()

    for(nvar in num_vars){
      tab <- df[,c(nvar, group)]

      aggr_res <- # Aggregate the results
        aggregate(tab[[nvar]] ~ tab[[group]], data = , FUN = function(x) {
          # I was trying a different method here rather than using summary() 
          # just out of pur spite
          c(n = length(x),
            mean = mean(x, na.rm = T),
            sd = sd(x, na.rm = T),
            min = min(x, na.rm = T),
            max = max(x, na.rm = T),
            median = median(x, na.rm = T),
            pct_25 = quantile(x, probs = c(0.25), na.rm = T),
            pct_75 = quantile(x, probs = c(0.75), na.rm = T),
            IQR = IQR(x, na.rm = T)
            ) 
        })
      colnames(aggr_res) <- c("Group", "Num_Var")

      if (results_df[results_df$Variable == nvar,]$Stat_Test %in%
        c("One-Way ANOVA", "Student's t")) { 
        
        # Parametric
        # Mean and SD
        aggr_res$Num_Var |> as.data.frame() -> dframe
        mean <- round(dframe$mean, 2)
        std_dev <- round(dframe$sd, 2)
        entries <- paste0(mean, " ± ", std_dev)
        names(entries) <- aggr_res$Group
        entries <- as.data.frame(t(entries))
        
      } else if (results_df[results_df$Variable == nvar,]$Stat_Test %in%
        c("Kruskal-Wallis", "Wilcoxon Rank Sum")) { 
        
        # Non-parametric
        # Median [IQR]
        aggr_res$Num_Var |> as.data.frame() -> dframe
        median <- round(dframe$median, 2)
        x25 <- round(dframe$`pct_25.25%`, 2)
        x75 <- round(dframe$`pct_75.75%`, 2)
        entries <- paste0(median, " [", x25, "-", x75, "]")
        names(entries) <- aggr_res$Group
        entries <- as.data.frame(t(entries))
        
      }

      list_of_num_tables[[nvar]]$Table <- entries
      list_of_num_tables[[nvar]]$Stat_Test <-
        results_df[results_df$Variable == nvar,]$Stat_Test
      list_of_num_tables[[nvar]]$Stat_Name <-
        results_df[results_df$Variable == nvar,]$Stat_Name
      list_of_num_tables[[nvar]]$Test_Statistic <-
        results_df[results_df$Variable == nvar,]$Test_Statistic
      list_of_num_tables[[nvar]]$Deg_Freedom <-
        results_df[results_df$Variable == nvar,]$Deg_Freedom
      list_of_num_tables[[nvar]]$P_Value <-
        results_df[results_df$Variable == nvar,]$P_Value
      
    }

    counts <- data.frame(n = summary(df[[group]])) |> t() |> as.data.frame()

    table_data <- list(Counts = counts,
                       Continuous = list_of_num_tables,
                       Categorical = list_of_cat_tables)

    class(table_data) <- "sTable"
    return(table_data)
  }
}


#' Generate results tables from [generate_statistics()] output.
#'
#' This function generates tables using ``knitr::kable`` and ``flextable`` for
#' the results of statistical tests. It supports both single group and
#' multiple group comparisons. For single group comparisons, it generates descriptive summary statistics.
#' For multiple group comparisons, it generates summary tables with group
#' comparison statistics.
#' @param results A list from [generate_statistics()] that contains the
#' results of the statistical tests. It should be an S3 object of class `sTable`
#' for multiple group comparisons or `ssTable` for single group comparisons.
#' @return A list of flextables for each variable in the results. If the input
#' results are of class 'sTable', it returns a list of flextables for the
#' continuous and/or categorical variables. If the input results are of class
#' 'ssTable', it returns a flextable for the continuous variables and a list
#' of flextables for each categorical variable.
#' @examples
#' results <- generate_statistics(data, group = "My_Grouping_Variable")
#' tables <- generate_results_tables(results)
#' @import flextable
#' @importFrom tibble rownames_to_column
#' @importFrom knitr kable
#' @export
generate_results_tables <- function(results) {

  stopifnot(inherits(results, "sTable") || inherits(results, "ssTable"))

  rflexbind <- function(x, y, fill = ""){
    diffcolnames <- setdiff(union(colnames(x), colnames(y)),
                            intersect(colnames(x), colnames(y)))
    for(dcn in diffcolnames){
      x[[dcn]] <- ""
    }
    df <- rbind(x,y)
    return(df)
  }

  function_sTable <- function(results) {

    # Continuous results ----
    continuous_results <- results$Continuous

    if (!is.null(results$Continuous) && length(results$Continuous) > 0) {

      # Empty list for extracted tables
      extracted_cts_tables <- extracted_cts_stats <- list()
      # Extract tables and stats x
      for (i in seq_along(continuous_results)) {
        extracted_cts_tables[[names(continuous_results)[i]]] <-
          continuous_results[[i]]$Table
        extracted_cts_stats[[names(continuous_results)[i]]] <-
          continuous_results[[i]][2:6]
      }
      
      # Combine and put into a dataframe
      combined_cts_table <- do.call(rbind, extracted_cts_tables) |> as.data.frame()
      combined_cts_stats <- do.call(cbind, extracted_cts_stats) |> t() |> 
        as.data.frame()
      combined_cts_stats$Stat_Test <- unlist(combined_cts_stats$Stat_Test)
      combined_cts_stats$Stat_Name <- unlist(combined_cts_stats$Stat_Name)
      combined_cts_stats$Test_Statistic <- unlist(combined_cts_stats$Test_Statistic)
      combined_cts_stats$Deg_Freedom <- unlist(combined_cts_stats$Deg_Freedom)
      combined_cts_stats$P_Value <- unlist(combined_cts_stats$P_Value )
      combined_cts_stats$Test_Statistic <-
        round(as.numeric(combined_cts_stats$Test_Statistic), 2)
      combined_cts_stats$P_Value <-
        signif(as.numeric(combined_cts_stats$P_Value), digits = 2)

      penult_cts_table <- cbind(combined_cts_table, combined_cts_stats)
      final_cts_table <- rflexbind(results$Counts, penult_cts_table)

      generate_cts_flextable <- function(cts_table) {
        ft <- cts_table |> tibble::rownames_to_column("rowname") |>
          flextable::flextable()
        ft <- flextable::add_header_lines(ft, values = "Continuous Variables",
                                          top = TRUE)
        ft <- flextable::set_header_labels(ft,
                                           rowname = "",
                                           Stat_Test = "Test",
                                           Stat_Name = "Statisic",
                                           Test_Statistic = "Value",
                                           Deg_Freedom = "df",
                                           P_Value = "p-val")
        ft <- flextable::add_footer_lines(ft, "Mean ± SD or Median [IQR]")
        ft <- flextable::set_table_properties(ft, width = 0.8, layout = "autofit")
        ft <- flextable::theme_vanilla(ft)
        return(ft)
      }
    }

    # Categorical results ----
    if(!is.null(results$Categorical) && length(results$Categorical) > 0) {
      
      categorical_results <- results$Categorical
      # Empty list for extracted tables
      extracted_cats_tables <- extracted_cats_stats <- list()
      
      for (i in seq_along(categorical_results)) {
        extracted_cats_tables[[names(categorical_results)[i]]] <-
          categorical_results[[i]]$Table
        extracted_cats_stats[[names(categorical_results)[i]]] <-
          categorical_results[[i]][2:6]
      }

      combined_cats_stats <- do.call(cbind, extracted_cats_stats) |> t() |>
        as.data.frame()
      combined_cats_stats$Stat_Test <- unlist(combined_cats_stats$Stat_Test)
      combined_cats_stats$Stat_Name <- unlist(combined_cats_stats$Stat_Name)
      combined_cats_stats$Test_Statistic <- 
        unlist(combined_cats_stats$Test_Statistic)
      combined_cats_stats$Deg_Freedom <- 
        unlist(combined_cats_stats$Deg_Freedom)
      combined_cats_stats$P_Value <- unlist(combined_cats_stats$P_Value )
      
      numeric_test_stat <-
        suppressWarnings(as.numeric(combined_cats_stats$Test_Statistic))
      combined_cats_stats$Test_Statistic <- ifelse(
        !is.na(numeric_test_stat),
        round(as.numeric(numeric_test_stat), 2),
        "none"
      )
      
      combined_cats_stats$P_Value <-
        signif(as.numeric(combined_cats_stats$P_Value), digits = 2)

      add_custom_margins <- function(table, stats) {
        n_fillers <- nrow(table) - 1
        fillers <- rep("", n_fillers)
        # Combine stats and fillers
        Stat_Test <- c(stats$Stat_Test, fillers)
        Stat_Name <- c(stats$Stat_Name, fillers)
        Test_Statistic <- c(stats$Test_Statistic, fillers)
        Deg_Freedom <- c(stats$Deg_Freedom, fillers)
        P_Value <- c(stats$P_Value, fillers)
      
        # Add the custom margins to the table
        new_table <-
          suppressWarnings(suppressMessages(
            addmargins(table, margin = c(2,2,2,2,2),
                       FUN = list(function(x) Stat_Test,
                                  function(x) Stat_Name,
                                  function(x) Test_Statistic,
                                  function(x) Deg_Freedom,
                                  function(x) P_Value),
                       quiet = TRUE)
          ))
        
        # Rename the margins
        dimnames(new_table)[[2]][(ncol(table)+1):(ncol(table)+5)] <-
          c("Stat_Test", "Stat_Name", "Test_Statistic", 
            "Deg_Freedom", "P_Value")
        
        return(new_table)
      }

      for (index in names(extracted_cats_tables)) {
        # Extract the corresponding stats for the table
        stats <- combined_cats_stats[index, ]
        # Get the contingency table
        table <- extracted_cats_tables[[index]]
        # Add custom margins
        new_table <- add_custom_margins(table, stats)
        # Overwrite the output
        extracted_cats_tables[[index]] <- new_table
      }

      generate_cats_flextable <- function(table_name, tables_list) {
        catvar_df <- as.data.frame.matrix(tables_list[[table_name]])
        cft <- catvar_df |> tibble::rownames_to_column("rowname") |>
          flextable::flextable()
        cft <- flextable::add_header_lines(cft,
                                          values = paste0("Categorical Variable: ",
                                                          table_name),
                                          top = TRUE)
        cft <- flextable::set_header_labels(cft,
                                           rowname = table_name,
                                           Stat_Test = "Test",
                                           Stat_Name = "Statistic",
                                           Test_Statistic = "Value",
                                           Deg_Freedom = "df",
                                           P_Value = "p-val")
        cft <- flextable::add_footer_lines(cft, "n (%)")
        cft <- flextable::set_table_properties(cft, width = 0.8, layout = "autofit")
        cft <- flextable::theme_vanilla(cft)
        return(cft)
      }
    }

    # Generate flextables
    if (!is.null(results$Continuous) && length(results$Continuous) > 0) {
      cts_flextables_list <- generate_cts_flextable(final_cts_table)
    }
                           
    if(!is.null(results$Categorical) && length(results$Categorical) > 0) {
      cat_flextables_list <-
        lapply(names(extracted_cats_tables), generate_cats_flextable,
               tables_list = extracted_cats_tables)
    }

    # Collect flextables to final list                       
    flextables_list <- list()
    if (!is.null(results$Continuous) && length(results$Continuous) > 0) {
      flextables_list[[1]] <- cts_flextables_list
    }

    # Ugh... should it start at index 2 if index 1 is empty?
    if(!is.null(results$Categorical) && length(results$Categorical) > 0) {
      for(i in seq_along(cat_flextables_list)){
        if(length(flextables_list) < i || is.null(flextables_list[[i]])){
          # Start with 1 if missing
          flextables_list[[i]] <- cat_flextables_list[[i]]
        } else {
          # Start with the next available index spot if present
          flextables_list[[length(flextables_list)+1]] <- cat_flextables_list[[i]]
        }
      } 
    }

    print_cts_tables <- function(final_cts_table) {
      kable_output1 <- knitr::kable(final_cts_table, format = "simple",
                                    caption = "Continuous Variables")
      print(kable_output1)
    }

    print_cat_tables <- function(extracted_cats_tables) {
      catvar_names <- names(extracted_cats_tables)
      for(catvarname in catvar_names){
        catvar_df <- as.data.frame.matrix(extracted_cats_tables[[catvarname]])
        kable_output2 <- knitr::kable(catvar_df, format = "simple",
                                      caption = paste0("Categorical Variable: ",
                                                       catvarname))
        print(kable_output2)
      }
    }


    # knitr tables
    if (!is.null(results$Continuous) && length(results$Continuous) > 0) {
      print_cts_tables(final_cts_table)
    }

    if(!is.null(results$Categorical) && length(results$Categorical) > 0) {
      print_cat_tables(extracted_cats_tables)
    }
    
    #print(flextables_list)
    return(flextables_list)
  }

  function_ssTable <- function(results) {

    # Extract results
    continuous_df <- results$Continuous
    categorical_tables <- results$Categorical

    # flextable functions
    generate_ss_cts_flextable <- function(continuous_df) {
      row1 <- list()
      row1$Variable <- names(results$Counts)
      row1$Summary <- paste0(
        rownames(results$Counts), " = ", results$Counts
      )
      row1$Missing <- ""
      continuous_df_xtra <- rbind(row1, continuous_df)
      ft <- flextable::flextable(continuous_df_xtra)
      ft <- flextable::set_table_properties(ft, width = 0.8,
                                            layout = "autofit")
      ft <- flextable::add_header_lines(ft, values = "Continuous Variables",
                                        top = TRUE)
      ft <- flextable::add_footer_lines(ft, "Mean ± SD or Median [IQR]")
      ft <- flextable::set_table_properties(ft, width = 0.8, layout = "autofit")
      ft <- flextable::theme_vanilla(ft)
      return(ft)
    }

    generate_ss_cats_flextable <- function(table_name, catables_list) {
      catvar_df <- as.data.frame.matrix(catables_list[[table_name]])
      ft <- catvar_df |> tibble::rownames_to_column("rowname") |>
        flextable::flextable()
      ft <- flextable::add_header_lines(ft,
                                        values = paste0("Categorical Variable: ",
                                                        table_name),
                                        top = TRUE)
      ft <- flextable::set_header_labels(ft,
                                         rowname = table_name)
      ft <- flextable::add_footer_lines(ft, "n (%)")
      ft <- flextable::set_table_properties(ft, width = 0.8, layout = "autofit")
      ft <- flextable::theme_vanilla(ft)
      return(ft)
    }


    # Generate flextables
    catables_list <- categorical_tables
    cat_flextables_list <-
      lapply(names(categorical_tables), generate_ss_cats_flextable,
             catables_list = catables_list)

    flextables_list <- list()
    flextables_list[[1]] <- generate_ss_cts_flextable(continuous_df)
    for(i in seq_along(cat_flextables_list)){
      flextables_list[[i+1]] <- cat_flextables_list[[i]]
    }

    # knitr tables
    kable_output0 <-
      knitr::kable(results$Counts, format = "simple",
                   caption = "Summary of Data")
    print(kable_output0)

    kable_output1 <-
      knitr::kable(continuous_df, format = "simple",
                   caption = "Continuous Variables")
    print(kable_output1)

    catvar_names <- names(categorical_tables)
    for(catvarname in catvar_names) {
      catvar_df <- as.data.frame.matrix(categorical_tables[[catvarname]])
      kable_output2 <- knitr::kable(catvar_df, format = "simple",
                                    caption = paste0("Categorical Variable: ",
                                                     catvarname))
      print(kable_output2)
    }
    
    #print(flextables_list)
    return(flextables_list)
  }

  if (inherits(results, "ssTable")) {
    # Execute the function for 'ssTable'
    message("Single group. Collecting descriptibe summary statistics.")
    tbls <- function_ssTable(results)

  } else if (inherits(results, "sTable")) {
    # Execute the function for 'sTable'
    message("Two or more groups. Collecting group comparison statistics.")
    tbls <- function_sTable(results)
  } else {
      stop("Could not parse the 'results' object. Please check input.")
  }
  
  return(tbls)
}
