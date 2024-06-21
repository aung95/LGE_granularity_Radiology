# Required Libraries
library(survival)
library(pec)
library(intsurv)
library(MLmetrics)
library(dplyr)
library(pROC)
library(ggplot2)

# Function to Process Data
process_data <- function(data, event.name, time.name, times){
  data$event.time <- ifelse(data[[event.name]] == 1 & data[[time.name]] < times, 1,
                            ifelse(data[[time.name]] >= times, 0, NA))
  
  data <- filter(data, !is.na(event.time))
  return(data)
}

# Bootstrap Function for AUC and PRAUC
bootstrap_metrics <- function(data, model, times) {
  indices <- sample(1:nrow(data), replace = TRUE)
  bootstrap_sample <- data[indices, ]
  predictions <- 1 - predictSurvProb(model, newdata = bootstrap_sample, times = times)
  auc <- MLmetrics::AUC(y_pred = as.numeric(predictions), y_true = as.numeric(bootstrap_sample$event.time))

  prauc <- MLmetrics::PRAUC(y_pred = as.numeric(predictions), y_true = as.numeric(bootstrap_sample$event.time))

  
  return(c(auc, prauc))
}


# Calculate CI for metrics
calculate_ci <- function(metric_values, CI = 0.995) {
  median_val <- round(median(metric_values), 2)
  probs.needed <- CI
  half_width <- (1 - probs.needed) / 2
  ci_lower <- round(quantile(metric_values, probs = half_width), 2)
  ci_upper <- round(quantile(metric_values, probs = 1 - half_width), 2)
  paste0(median_val, " [", ci_lower, "-", ci_upper, "]")
}

# Compute ROC plots
compute_roc <- function(data, model.pred = model.cox.derivation, times = times) {
  predictions <- 1 - predictSurvProb(model.pred, newdata = data, times = times)
  roc_obj <- pROC::roc(response = data$event.time, predictor = predictions, na.rm = TRUE, smooth = FALSE)
  coords <- pROC::coords(roc_obj, "all", ret = c("threshold", "precision", "recall"))
  list(roc_obj = roc_obj, coords = coords)
}


# Main Function
CoxPredictScore <- function(derivation, validation, variables, time.name, times, event.name, n_bootstrap, ci_level, seed) {
  set.seed(seed)
  
  # Fit Models and Process Data
  formula.cox <- as.formula(paste("Surv(", time.name, ", ", event.name, ") ~ ", paste(variables, collapse = " + ")))
  model.cox.derivation <- coxph(formula.cox, data = derivation, x = TRUE)
  derivation_processed <- process_data(derivation, event.name, time.name, times)
  validation_processed <- process_data(validation, event.name, time.name, times)

  # Calculate Metrics
  metrics_derivation <- sapply(1:n_bootstrap, function(i) bootstrap_metrics(derivation_processed, model.cox.derivation, times))
  metrics_validation <- sapply(1:n_bootstrap, function(i) bootstrap_metrics(validation_processed, model.cox.derivation, times))
  

  # Compile and Return Results
  list(
    derivation_metrics = list(auc = calculate_ci(metrics_derivation[1,], ci_level), prauc = calculate_ci(metrics_derivation[2,], ci_level)),
    validation_metrics = list(auc = calculate_ci(metrics_validation[1,], ci_level), prauc = calculate_ci(metrics_validation[2,], ci_level)),
    roc_derivation = compute_roc(derivation_processed, model = model.cox.derivation, times = times),
    roc_validation = compute_roc(validation_processed, model = model.cox.derivation, times = times)
  )
}
# 
# 
# results.score.10 <- CoxPredictScore(
#   derivation = data, 
#   validation = data, 
#   time.name = "outcome_FU_time_death", 
#   times = 10*12,
#   event.name ="outcome_death", 
#   variable = "score",
#   n_bootstrap = 100, 
#   ci_level = 0.995, 
#   seed = 123)
