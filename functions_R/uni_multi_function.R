# # Helper function to format results from a model
# Helper function to format results from a model
# Part 1 of the function - help to extract information from a model 
format_results_function <- function(model_evaluated, conf.level = 0.995) {
  
  coef_df <- coef(summary(model_evaluated))

  log_ci_df <- confint(model_evaluated, level = conf.level)
  
  formatted <- vector("character", nrow(coef_df))
  for (i in 1:nrow(coef_df)) {
    coef <- exp(coef_df[i, "coef"])
    lower_ci <- exp(log_ci_df[i, 1])
    upper_ci <- exp(log_ci_df[i, 2])
    p_value <- coef_df[i, "Pr(>|z|)"]
    
    formatted[i] <- sprintf("%.2f (%.2f-%.2f), p=%.3f", coef, lower_ci, upper_ci, p_value)
  }

  df <- data.frame(
    row.names = rownames(summary(model_evaluated)$coefficients),
    value = formatted)
  
  return(df)
}


# Part 2 of the function, univariate then multivariate analysis
uni_multi_function <- function(df, event, time, var_base, var_added, conf.level = 0.995) {
  
  df <- df %>% droplevels() # really important this part !

  # Univariate Analysis
  univ_results <- list()
  
  for (var in c(var_base, unlist(var_added))) {
    formula.cox = paste0("Surv(", time, ", ", event, ") ~ ", var)
    model <- coxph(as.formula(formula.cox), data = df)

    
    result <- format_results_function(model, conf.level = conf.level)

    detailed_name <- rownames(result)  # or another method to extract the detailed names
    
    value <- result
    
    # Store in univ_results
    univ_results[[var]] <- list("names" = detailed_name, "values" = value)
    
  }
  
  results_df <- tibble(
    Variable = unlist(lapply(univ_results, `[[`, "names")),
    Univariate_Analysis = unlist(lapply(univ_results, `[[`, "values"))
  )
  
  ####  Multivariable Analysis
  model_formulas <- list(var_base) # Initialize with base model
  
  # Initialize the cumulative variable set with the base variable
  variables_cumulative <- var_base
  
  # Loop through each variable in the added variables list
  for (new_var in var_added) {
    # Add the new variable to the cumulative list
    variables_cumulative <- c(variables_cumulative, new_var)
    
    # Store the current cumulative variable set
    model_formulas[[length(model_formulas) + 1]] <- variables_cumulative
  }
  
  # Create and format models from formulas
  models_list <- list()
  i = 0
  for (variables in model_formulas) {
    i = i+ 1
    formula.cox = paste("Surv(", time, ", ", event, ") ~ ", paste(variables, collapse = " + "))
    model.cox <- coxph(as.formula(formula.cox), data = df)
    models_list[[paste0("multi_", i)]] <- format_results_function(model.cox)
  }
  
  i = 1
  for(models in models_list) {
    # Convert model results to a dataframe for easier manipulation
    
    model_name = paste0("model_multi_",i)
    i = i+1
    model_df <- tibble(Variable = rownames(models), Value = unlist(models))
    
    # Merge the model results into `results_df`
    results_df <- left_join(results_df, model_df, by = "Variable") %>%
      rename_with(~ model_name, .cols = "Value")
    
  }
  
  return(results_df)
}



# TEST
# table <- uni_multi_function(
#   df = df_selected, 
#   event = "outcome_death",
#   time = "outcome_FU_time_death", 
#   var_base = c("demo_age"), 
#   var_added = c("CMR_LGE_midwall_extent_categ"))
