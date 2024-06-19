setwd("E:\\Ackerman05_PE_Twins_VT\\Draft_Tables")

table <- read.csv("For_Demographics_Table.csv")

#library(tableone)
library(tidyverse)

table %>% select(Case_ID1, Group, Mat_age, Gravidity, Parity,
                 Mother_Race, Mother_Hispanic_Ethnicity, 
                 Gestational_age_at_delivery_.wks.days.) %>%  
  rename(GAD.wks = Gestational_age_at_delivery_.wks.days.) %>% 
  mutate(Group = factor(Group)) -> df

check_input <- function(df, group) {
  # Check to see if 'df' is a data frame
  if (!is.data.frame(df)) {
    stop("ERROR: The input is not a dataframe.")
  }
  # Check if the first column is a unique ID
  if(any(duplicated(df[,1]))){
    message("Non-unique row IDs in column 1 of data frame object.")
    message("WARNING: Deleting columns 1 and replacing with unique IDs.")
    df <- data.frame(ID = seq_along(df[[1]]), df[,-1])
  }
  # Check if the group argument is a column in the dataframe
  if (!group %in% names(df)) {
    stop(paste("The group argument", group, "is not a column in the dataframe."))
  }
  # Return the modified dataframe
  return(df)
}

generate_statistics <- function(df, group = "Group"){
  
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
  
  # Just output summary stats table if group not found
  find_variable_types <- function(df) {
    sapply(df, function(x) {
      if (any(class(x) %in% c("integer", "numeric"))) {
        return("numeric")
      } else {
        return(class(x))
      }
    })
  }
  
  find.numeric <- function(df){
    types <- find_variable_types(df)
    return(types[types == "numeric"] |> names())
  }
  
  find.categorical <- function(df){
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
        test_results$mann_whitney <- wilcox.test(num ~ group, data = df)
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
      parsed_results$stat_name <- "chi-squared"
      parsed_results$test_statistic <- unname(res_stats$kruskal_wallis$statistic)
      parsed_results$p_value <- res_stats$kruskal_wallis$p.value
    } else if ("aov" %in% names(res_stats)) {
      parsed_results$test_name <- "One-Way ANOVA"
      parsed_results$stat_name <- "F"
      parsed_results$test_statistic <- summary(res_stats$aov)[[1]]$'F value'[1]
      parsed_results$p_value <- summary(res_stats$aov)[[1]]$'Pr(>F)'[1]
    } else if ("t_test" %in% names(res_stats)) {
      parsed_results$test_name <- "Student's t"
      parsed_results$stat_name <- "t"
      parsed_results$test_statistic <- unname(res_stats$t_test$statistic)
      parsed_results$p_value <- res_stats$t_test$p.value
    } else if ("mann_whitney" %in% names(res_stats)) {
      parsed_results$test_name <- "Wilcoxon Rank Sum"
      parsed_results$stat_name <- "W"
      parsed_results$test_statistic <- unname(res_stats$mann_whitney$statistic)
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
      tp_value <- NA
      tryCatch({
        tp_value <- fisher.test(contingency_table)$p.value
      }, error = function(e) {
        message("Using simulation for Fisher's test due to workspace limitations.")
        tp_value <<- fisher.test(contingency_table, simulate.p.value = TRUE)$p.value
      })
      p_value <- tp_value
    } else {
      # If all expected frequencies are >= 5, use Chi-squared Test
      test_name <- "Chi-Squared"
      stat_name <- "chi-squared"
      test_statistic <- chisq.test(contingency_table)$statistic
      p_value <- chisq.test(contingency_table)$p.value
    }
    
    # Return a list containing the test name and result
    return(list(test_name = test_name, 
                stat_name = stat_name, 
                test_statistic = test_statistic,
                p_value = p_value))
  }
  
  make_prop_table <- function(x){
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
  
  # Initialize an empty data frame to store the results
  results_df <- data.frame(
    Variable = character(),
    Stat_test = character(),
    Stat_Name = character(),
    Test_Statistic = numeric(),
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
      P_Value = parsed_stats$p_value
    ))
  }
  
  # Get the stats for the categorical variables
  cat_vars <- find.categorical(df)[-1] # Assumes ID is first row
  cat_vars <- cat_vars[-which(cat_vars == group)] # Drops Grouping Variable
  for(cvar in cat_vars){
    contingency_table <- table(df[[group]], df[[cvar]])
    res_stats <- choose_cont_tab_test(contingency_table)
    results_df <- rbind(results_df, data.frame(
      Variable = cvar,
      Stat_Test = res_stats$test_name,
      Stat_Name = res_stats$stat_name,
      Test_Statistic = res_stats$test_statistic,
      P_Value = res_stats$p_value
    ))
  }  
  
  # Tables for categorical variables
  list_of_cat_tables <- list()
  for(cvar in cat_vars){
    tab <- df[,c(cvar, group)]
    prop_tab <- make_prop_table(tab)
    list_of_cat_tables[[cvar]]$Table <- prop_tab
    list_of_cat_tables[[cvar]]$Stat_Test <- 
      results_df[results_df$Variable == cvar,]$Stat_Test
    list_of_cat_tables[[cvar]]$Stat_Name <- 
      results_df[results_df$Variable == cvar,]$Stat_Name
    list_of_cat_tables[[cvar]]$Test_Statistic <- 
      results_df[results_df$Variable == cvar,]$Test_Statistic
    list_of_cat_tables[[cvar]]$P_Value <- 
      results_df[results_df$Variable == cvar,]$P_Value
  }
  
  # Tables for numeric variables
  list_of_num_tables <- list()
  
  for(nvar in num_vars){
    tab <- df[,c(nvar, group)] 
    
    aggr_res <- # Aggregate the results
      aggregate(tab[[nvar]] ~ tab[[group]], data = , FUN = function(x) {
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
    
    if(results_df[results_df$Variable == nvar,]$Stat_Test %in%
      c("One-Way ANOVA", "Student's t")){ # Parametric
      # Mean and SD
      aggr_res$Num_Var |> as.data.frame() -> dframe
      mean <- round(dframe$mean, 2)
      std_dev <- round(dframe$sd, 2)
      entries <- paste0(mean, " ± ", std_dev)
      names(entries) <- aggr_res$Group
      entries <- as.data.frame(t(entries))
    } else if(results_df[results_df$Variable == nvar,]$Stat_Test %in%
      c("Kruskal-Wallis", "Wilcoxon Rank Sum")){ # Non-parametric
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
    list_of_num_tables[[nvar]]$P_Value <- 
      results_df[results_df$Variable == nvar,]$P_Value
  }
  
  counts <- data.frame(n = summary(df[[group]])) |> t() |> as.data.frame()
  
  table_data <- list(Counts = counts,
                     Continuous = list_of_num_tables,
                     Categorical = list_of_cat_tables)
  
  return(table_data)
}

generate_results_tables <- function(results){
  continuous_results <- results$Continuous
  # Empty list for extracted tables
  extracted_cts_tables <- extracted_cts_stats <- list()
  # Extract tables and stats x
  for (i in seq_along(continuous_results)) {
    extracted_cts_tables[[names(continuous_results)[i]]] <-
      continuous_results[[i]]$Table
    extracted_cts_stats[[names(continuous_results)[i]]] <-
      continuous_results[[i]][2:5]
  }
  # Combine and put into a dataframe
  combined_cts_table <- do.call(rbind, extracted_cts_tables) |> as.data.frame()
  combined_cts_stats <- do.call(cbind, extracted_cts_stats) |> t() |> as.data.frame()
  combined_cts_stats$Stat_Test <- unlist(combined_cts_stats$Stat_Test)
  combined_cts_stats$Stat_Name <- unlist(combined_cts_stats$Stat_Name)
  combined_cts_stats$Test_Statistic <- unlist(combined_cts_stats$Test_Statistic)
  combined_cts_stats$P_Value <- unlist(combined_cts_stats$P_Value )
  combined_cts_stats$Test_Statistic <-
    round(as.numeric(combined_cts_stats$Test_Statistic), 2)
  combined_cts_stats$P_Value <-
    signif(as.numeric(combined_cts_stats$P_Value), digits = 2)
  rflexbind <- function(x, y, fill = ""){
    diffcolnames <- setdiff(union(colnames(x), colnames(y)), 
                            intersect(colnames(x), colnames(y)))
    for(dcn in diffcolnames){
      x[[dcn]] <- ""
    }
    df <- rbind(x,y)
    return(df)
  }
  penult_cts_table <- cbind(combined_cts_table, combined_cts_stats)
  final_cts_table <- rflexbind(results$Counts, penult_cts_table)
  
  library(flextable)
  
  generate_cts_flextable <- function(cts_table) {
    ft <- cts_table %>% rownames_to_column("rowname") %>% flextable()
    ft <- add_header_lines(ft, values = "Continuous Variables", top = TRUE)
    ft <- set_header_labels(ft,
                            rowname = "",
                            Stat_Test = "Test",
                            Stat_Name = "Statisic",
                            Test_Statistic = "Value",
                            P_Value = "p-val")
    ft <- add_footer_lines(ft, "Mean ± SD or Median [IQR]")
    ft <- set_table_properties(ft, width = 0.8, layout = "autofit")
    ft <- theme_vanilla(ft)
    return(ft)
  }
  
  categorical_results <- results$Categorical
  # Empty list for extracted tables
  extracted_cats_tables <- extracted_cats_stats <- list()
  for (i in seq_along(categorical_results)) {
    extracted_cats_tables[[names(categorical_results)[i]]] <-
      categorical_results[[i]]$Table
    extracted_cats_stats[[names(categorical_results)[i]]] <-
      categorical_results[[i]][2:5]
  }
  #combined_cats_table <- do.call(rbind, extracted_cats_tables) 
  combined_cats_stats <- do.call(cbind, extracted_cats_stats) |> t() |> 
    as.data.frame()
  combined_cats_stats$Stat_Test <- unlist(combined_cats_stats$Stat_Test)
  combined_cats_stats$Stat_Name <- unlist(combined_cats_stats$Stat_Name)
  combined_cats_stats$Test_Statistic <- unlist(combined_cats_stats$Test_Statistic)
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
    P_Value <- c(stats$P_Value, fillers)
    # Add the custom margins to the table
    new_table <- 
      suppressWarnings(suppressMessages(
        addmargins(table, margin = c(2,2,2,2), 
                   FUN = list(function(x) Stat_Test,
                              function(x) Stat_Name,
                              function(x) Test_Statistic,
                              function(x) P_Value), 
                   quiet = TRUE)
      ))
    # Rename the margins
    dimnames(new_table)[[2]][(ncol(table)+1):(ncol(table)+4)] <-
      c("Stat_Test", "Stat_Name", "Test_Statistic", "P_Value")
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
    ft <- catvar_df %>% rownames_to_column("rowname") %>% flextable()
    ft <- add_header_lines(ft, 
                           values = paste0("Categorical Variable: ",
                                           table_name), 
                           top = TRUE)
    ft <- set_header_labels(ft,
                            rowname = table_name,
                            Stat_Test = "Test",
                            Stat_Name = "Statistic",
                            Test_Statistic = "Value",
                            P_Value = "p-val")
    ft <- add_footer_lines(ft, "n (%)")
    ft <- set_table_properties(ft, width = 0.8, layout = "autofit")
    ft <- theme_vanilla(ft)
    return(ft)
  }
  
  # Generate flextables
  cts_flextables_list <- generate_cts_flextable(final_cts_table)
  cat_flextables_list <- 
    lapply(names(extracted_cats_tables), generate_cats_flextable, 
           tables_list = extracted_cats_tables)
  
  flextables_list <- list()
  flextables_list[[1]] <- cts_flextables_list
  for(i in seq_along(cat_flextables_list)){
    flextables_list[[i+1]] <- cat_flextables_list[[i]]
  }
  
  # knitr tables
  print_cat_tables <- function(final_cts_table, extracted_cats_tables){
    kable_output1 <- knitr::kable(final_cts_table, format = "simple",
                                  caption = "Continuous Variables")
    print(kable_output1)
    
    catvar_names <- names(extracted_cats_tables)
    for(catvarname in catvar_names){
      catvar_df <- as.data.frame.matrix(extracted_cats_tables[[catvarname]])
      kable_output2 <- knitr::kable(catvar_df, format = "simple",
                                    caption = paste0("Categorical Variable: ", catvarname))
      print(kable_output2)
    }
  }
  
  print_cat_tables(final_cts_table, extracted_cats_tables)
  return(flextables_list)
}

df2 <- rbind(df, df[1,])

df$Q2 <- table$Q2  
df$Q2 <- NULL

group1 <- c(rep("0",2), rep("1",0), rep("2",0), rep("3+",0), rep("NP",8))
group2 <- c(rep("0",5), rep("1",0), rep("2",0), rep("3+",0), rep("NP",4))
group4 <- c(rep("0",0), rep("1",1), rep("2",3), rep("3+",2), rep("NP",1))
group6 <- c(rep("0",2), rep("1",0), rep("2",2), rep("3+",5), rep("NP",1))
group7 <- c(rep("0",1), rep("1",0), rep("2",8), rep("3+",5), rep("NP",1))
Group <- c(rep("Gr1", length(group1)),
           rep("Gr2", length(group2)),
           rep("Gr4", length(group4)),
           rep("Gr6", length(group6)),
           rep("Gr7", length(group7)) )
df$Dipcat <- c(group1, group2, group4, group6, group7)


# One group error checking
# Get the ns and NAs




results <- generate_statistics(df)
generate_results_tables(results)



