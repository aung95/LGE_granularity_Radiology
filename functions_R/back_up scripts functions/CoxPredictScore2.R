# Required Libraries
library(survival)
library(pec)
library(intsurv)
library(MLmetrics)
library(dplyr)
library(pROC)

# Function to Process Data
process_data <- function(data, event.name, time.name, times){
  data$event.time = 
    ifelse(data[[event.name]] == 1 & data[[time.name]] < times, 1,
           ifelse(data[[time.name]] >= times, 0, NA))
  data = filter(data, !is.na(event.time))
  
  return(data)
}

# Bootstrap Function for AUC and PRAUC
bootstrap_metrics <- function(data, model, times) {
  indices <- sample(1:nrow(data), replace = TRUE)
  bootstrap_sample <- data[indices, ]
  predictions <- 1 - predictSurvProb(model, newdata = bootstrap_sample, times = times)
  auc <- MLmetrics::AUC(y_pred = as.numeric(predictions), y_true = bootstrap_sample$event.time)
  prauc <- MLmetrics::PRAUC(y_pred = as.numeric(predictions), y_true = bootstrap_sample$event.time)
  c(auc, prauc)
}

# Calculate value 
calculate_ci <- function(metric_values, CI = 0.995) {
    median_val <- round(median(metric_values), 2)
    probs.needed <- CI
    half_width <- (1 - probs.needed) / 2
    
    # Calculate lower and upper bounds of the confidence interval
    ci_lower <- round(quantile(metric_values, probs = half_width), 2)
    ci_upper <- round(quantile(metric_values, probs = 1 - half_width), 2)
    
    # Format as value [lower CI, upper CI]
    paste0(median_val, " [", ci_lower, "-", ci_upper, "]")
  }
  


# Main Function
CoxPredictScore2 <- function(derivation, validation, variables, time.name = "time", 
                            times = 10 * 12, event.name = "event", n_bootstrap = 5, 
                            ci_level = 0.95, seed = 123) {
  set.seed(seed)
  
  # Model Formula
  formula.cox <- as.formula(paste("Surv(", time.name, ", ", event.name, ") ~ ", paste(variables, collapse = " + ")))
  
  # Fit Cox Model on DERIVATION
  model.cox.derivation <- coxph(formula.cox, data = derivation, x = TRUE)
  
  # Fit Null Model (intercept only) on DERIVATION
  formula_null <- as.formula(paste("Surv(", time.name, ", ", event.name, ") ~ 1"))
  model.null.derivation <- coxph(formula_null, data = derivation)
  
  # Comparison using anova
  comparison_result <- anova(model.null.derivation, model.cox.derivation)
  Chi2 <- comparison_result$Chisq[2]  # Chi-squared value for the full model
  LR <- comparison_result$`Pr(>|Chi|)`[2]  # LR test p-value for the full model
  chi_LR <- sprintf("Chi2 = %.2f, LR test p-value = %.4f", Chi2, LR)
  
  # Process Data - transform variable to have a time roc AUC
  print(dim(derivation))
  derivation_processed <- process_data(derivation, event.name, time.name, times)
  print(dim(derivation_processed))
  print(dim(validation))
  validation_processed <- process_data(validation, event.name, time.name, times)
  print(dim(validation_processed))
  # # Calculate Metrics
  # metrics_derivation <- t(sapply(1:n_bootstrap, function(i) bootstrap_metrics(derivation_processed, model.cox.derivation, times)))
  # metrics_validation <- t(sapply(1:n_bootstrap, function(i) bootstrap_metrics(validation_processed, model.cox.derivation, times)))
  # # Calculate Metrics
  metrics_derivation <- sapply(1:n_bootstrap, function(i) bootstrap_metrics(derivation_processed, model.cox.derivation, times))

  metrics_validation <- sapply(1:n_bootstrap, function(i) bootstrap_metrics(validation_processed, model.cox.derivation, times))

  
  
  # Apply the calculate_ci function to each metric column
    list(
      auc = paste0("AUC: ", calculate_ci(metrics[1,])),
      prauc = paste0("PRAUC: ", calculate_ci(metrics[2,]))
    )
  
  
  # print(compile_results(metrics_derivation))
  
  # Graphical Output
  compute_roc <- function(data) {
    predictions <- 1 - predictSurvProb(model.cox.derivation, newdata = data, times = times)
    roc_obj <- pROC::roc(response = data$event.time, predictor = predictions, na.rm = TRUE, smooth = FALSE)
    coords <- pROC::coords(roc_obj, "all", ret = c("threshold", "precision", "recall"))
    list(roc_obj = roc_obj, coords = coords)
  }
  
  # LIST
  listresults <- list(
    derivation = compile_results(metrics_derivation, CI = ci_level),
    validation = compile_results(metrics_validation, CI = ci_level),
    roc_derivation = compute_roc(derivation_processed),
    roc_validation = compute_roc(validation_processed),
    model_stats = chi_LR 
  )
  return(listresults)
}

results.score.10 <- CoxPredictScore2(
  derivation = data, 
  validation = data, 
  time.name = "outcome_FU_time_death", 
  times = 10*12,
  event.name ="outcome_death", 
  variable = "score",
  n_bootstrap = 100, 
  ci_level = 0.995, 
  seed = 123)
