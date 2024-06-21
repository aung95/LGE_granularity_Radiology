### INCREMENTAL FUNCTIONS

# ### SUBFUNCTION AUC


# ### SUBFUNCTION CHI2
chi2_function <- function(df, event, time, model1, model2 = NULL) {
  # Null model
  formula_str <- sprintf("Surv(%s, %s) ~ 1", time, event)
  null_model <- coxph(formula = as.formula(formula_str), data = df)
  
  # Compare
  if (is.null(model2)) {
    # If there's no model2 provided, compare model1 to the null model
    comparison_result <- anova(null_model, model1)
  } else {
    # If model2 is provided, compare model1 to model2
    comparison_result <- anova(model1, model2)
  }
  
  Chi2 = comparison_result$Chisq[2]
  LR = comparison_result$`Pr(>|Chi|)`[2]
  
  # Constructing concordance info string
  results <- sprintf("Chi2 = %.2f, LR test %.4f", Chi2, LR)

  return(results)
}

### SUBFUNCTION 1 FIT COX MODEL
fitCoxModel <- function(df, event, time, variables, x_type = FALSE) {
  # Creating the formula string
  variables_str <- paste(variables, collapse = "+")
  formula_str <- sprintf("Surv(%s, %s) ~ %s", time, event, variables_str)
  
  # Converting the string into a formula
  formula <- as.formula(formula_str)
  
  # Fitting the Cox model using the 'survival' package
  model.cox <- coxph(formula, x = TRUE, data = df)
  
  # Return the fitted model
  return(model.cox)
}


### FUNCTION 1 : CALCULUS
incremental_function <- function(df, event, time, time_analyzed, ...) {
  require(survival)  # Assuming use of survival package
  require(compareC)
  require(pec)
  
  df <- droplevels(df)
  
  list_var <- list(...)
  
  results_master_list <- list(
    chi2_results = list(), 
    chi2_comparison = list(),
                              
    AUC_results = list(), 
    AUC_comparaison = list()
    )

  list_models <- list()
  
  # fit models
  for (variables in list_var) {
    # Increment model counter and generate model name
    model_name <- paste0("model_", length(list_models) + 1)
    # Storing the model in the list using the fitCoxModel function
    list_models[[model_name]] <- fitCoxModel(df = df, event = event, time = time, variables = variables, x_type = TRUE)
  }
  
  print("analysis CHI2")
  # Chi2 comparisons with null model
  # list_chi2_null <- list()
  for (i in seq_along(list_models)) {
    model_name <- names(list_models)[i]
    comparison_result <- chi2_function(df, event, time, list_models[[model_name]])
    results_master_list$chi2_results[[model_name]] <- comparison_result
  }
  

  print("analysis CHI2 comparaison")
  # Chi2 Compare consecutive models (example for storing results)
  # list_chi2_comparison_results <- list()
  for (i in 1:(length(list_models) - 1)) {
    comparison_results <- chi2_function(
      df = df, 
      time = time, 
      event = event, 
      model1 = list_models[[i]], 
      model2= list_models[[i + 1]])
    
    results_master_list$chi2_comparison[[paste("Comparison", i, "vs", i+1)]] <- comparison_results
  }
  
  # print(list_chi2_comparison_results)
  
  print("analysis AUC")
    for (i in seq_along(list_models)) {
    model <- list_models[[i]]
    
    
    concordance_result <- concordance(model, newdata = df)  # could use pec::cindex to fix a time of analysis ?
    
    C <- concordance_result[["concordance"]]
    se_C <- sqrt(concordance_result[["var"]])
    lower_CI <- C - 1.96 * se_C
    upper_CI <- C + 1.96 * se_C
    
    # Constructing concordance info string
    concordance_info <- sprintf("%.2f, 95%%CI(%.2f-%.2f)", C, lower_CI, upper_CI)
    
    results_master_list$concordance[[paste("Model", i)]] <- concordance_info
  }
  # 
  # print(list_concordance)
  
  
  print("analysis c index comparaison")
  # Initialize a list to store comparison results
  # comparison_results_list <- list()
  
  for (i in 1:(length(list_models) - 1)) {
    comparison_result <- CompareCindexList(
      df = df,
      time = time,
      event = event,
      # time_to_analyze = time_analyzed,  # Adjust as necessary
      model_1 = list_models[[i]],
      model_2 = list_models[[i + 1]]
    )
    
    # Constructing a descriptive name for each comparison
    name <- paste0("Comparison of Model ", i, " and Model ", i + 1)

    
    # Formatting the results for readability
    results <- paste0("C-index: ", comparison_result$est.c,
                      ", P-value: ", comparison_result$pval)
    
    # Storing the formatted results with the comparison name
    results_master_list$c_index_comparison[[name]] <- results
  }
  
  
  return(results_master_list)
  
}
# 
# Test
# D <- incremental_function(
#   df = df_all,
#   event = "outcome_death",
#   time = "outcome_FU_time_death",
#   time_analyzed = median(df_all$outcome_FU_time_death),
#   var_1 = "demo_age",
#   var_2 = c("demo_age", "CMR_LGE_ischemic_extent_categ"),
#   var_3 = c("demo_age", "CMR_LGE_ischemic_extent_categ", "CMR_LGE_midwall_presence")
#   )
# 
# D$nri_idi_basic
