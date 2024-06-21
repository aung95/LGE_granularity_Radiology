# FUNCTION BIS

# The idea is to take a dataframe, a variables of interest and variables to stratify the group, 
# for each subgroups, assess the HR of the variable of interest 

library(survival)
library(dplyr)

forest_function <- function(df, time_var = "outcome_FU_time", event_var = "outcome_death", 
                            var_of_interest = "CMR_ischemic_LGE",  ... , ci_level = 0.95) {
  
  # Capture additional variables passed through '...'
  vars <- c(time_var, event_var, var_of_interest, ...)
  if (!all(vars %in% names(df))) {
    stop("One or more specified variables do not exist in the dataframe.")
  }
  
  # Initialize an empty dataframe for results
  final_results <- data.frame(name = character(), mean = numeric(), lower = numeric(),
                              upper = numeric(), ratio1 = character(), ratio2 = character(),
                              OR = character(), Pval = character(), PValInter = character(),
                              stringsAsFactors = FALSE)
  
  extra_vars <- list(...) 
  
  for (var in extra_vars) {
    
    # Constructing the formula for Cox model with interaction
    formula_str <- paste("Surv(", time_var, ",", event_var, ") ~", var_of_interest, "*", var)
    
    # Fitting Cox model for interaction
    cox_model <- try(coxph(as.formula(formula_str), data = df), silent = TRUE)
    
    # Check if Cox model fitting was successful for interaction
    if (!inherits(cox_model, "try-error")) {
      pval <- summary(cox_model)$coefficients[3, 5] # on prend bien la p-value d'interaction
      PVALINTER <- ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
      
      interaction_output <- data.frame(name = paste(var, "interactions", sep = "_"),
                                       mean = NA, lower = NA, upper = NA, ratio1 = NA,
                                       ratio2 = NA, OR = NA, Pval = NA,
                                       PValInter = PVALINTER, stringsAsFactors = FALSE)
      
      # # Combine the interaction output with the final results
      final_results <- rbind(final_results, interaction_output)
    }
    
    for (levels in levels(df[[var]])) {
      
      # Dynamic filtering based on variable levels
      df_filtered <- df %>% filter(.data[[var]] == levels) 
      
      # Constructing the formula for Cox model without interaction
      formula_str <- paste("Surv(", time_var, ",", event_var, ") ~", var_of_interest)
      
      # Fitting Cox model without interaction
      cox_model <- try(coxph(as.formula(formula_str), data = df_filtered), silent = TRUE)
      
      # Check if Cox model fitting was successful without interaction
      if (!inherits(cox_model, "try-error")) {
        
        # Extracting model results
        hr <- exp(coef(cox_model))
        ci <- exp(confint(cox_model, level = ci_level))
        pval <- summary(cox_model)$coefficients[1, 5]
        
        # Calculate event ratios for each level
        table_events <- table(df_filtered[[event_var]], df_filtered[[var_of_interest]])
        ratio1 <- paste(table_events[2, 1], sum(table_events[, 1]), sep="/")
        ratio2 <- paste(table_events[2, 2], sum(table_events[, 2]), sep="/")
        
        # Preparing output data frame
        subgroup_output <- data.frame(name = paste(var, levels, sep = "_"),
                                      mean = hr, lower = ci[1], upper = ci[2],
                                      ratio1 = ratio1, ratio2 = ratio2,
                                      OR = sprintf("%.2f (%.2f; %.2f)", hr, ci[1], ci[2]),
                                      Pval = ifelse(pval < 0.001, "<0.001", sprintf("%.4f", pval)),
                                      PValInter = NA, stringsAsFactors = FALSE)
        
        rownames(subgroup_output) <- NULL
        
        # Combine the subgroup output with the final results
        final_results <- rbind(final_results, subgroup_output)
      }
    }
  }
  
  return(final_results)
}



