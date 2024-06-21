# CoxPredictNew

# CoxPredict function

CoxPredictScore <- function(
    derivation, 
    validation, 
    variables = c(),
    time.name = "time", 
    times = 10*12,
    event.name ="event",
    n_bootstrap = 5, 
    ci_level = 0.95, seed = 123){
  
  require(survival)
  require(pec)
  require(intsurv)

  set.seed(seed)
  
  #########################
  ##### MODEL CREAT #######
  ########################
  
  variables_str <- paste(variables, collapse = " + ")
  formula.cox <- paste("Surv(", time.name, ", ", event.name, ") ~ ", variables_str)
  
  
  print(paste0("Variable analysed are: ",formula.cox))
  formula = as.formula(formula.cox)
  model.cox.derivation <- coxph(formula, x = TRUE, data = derivation)
  print(model.cox.derivation
        )
  #########################
  ##### DERIVATION #######
  ########################
  data <- derivation
  
  data$event.time <- ifelse(
    data[[event.name]] == 1 & data[[time.name]] <= times, 1, ifelse(
      data[[event.name]]  == 0 & data[[time.name]]> times, 0, NA)
  )
  
  data <- filter(data,!is.na(event.time))
  
  ### 
  auc_values_derivation <- numeric(n_bootstrap)
  prauc_values_derivation <- numeric(n_bootstrap)
  
  for(i in 1:n_bootstrap){ # ajouter ici la Se Sp après bootstrap des cut-offs dérivés de la valdiation
    indices <- sample(1:nrow(data), replace = TRUE)
    bootstrap_sample <- data[indices, ]
    predictions = 1- predictSurvProb(model.cox.derivation, newdata = bootstrap_sample, times = times)
    bootstrap_sample$predictions <- as.numeric(predictions)
    
    # measurements
    auc_values_derivation[i] <- MLmetrics::AUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
    prauc_values_derivation[i] <- MLmetrics::PRAUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
  }
  
  # RESULTS
  results.auc.derivation <- paste0("AUC :",  round(median(auc_values_derivation),2), " [", round(quantile(auc_values_derivation, 0.25),2), "-", round(quantile(auc_values_derivation, 0.75),2), "]")
  results.prauc.derivation <- paste0("PRAUC :",  round(median(prauc_values_derivation),2), " [", round(quantile(prauc_values_derivation, 0.25),2), "-", round(quantile(prauc_values_derivation, 0.75),2), "]")
  results.death.derivation <- round(sum(data$event.time == 1)/nrow(data),2)
  
  # graphical
  cox.predict = 1- predictSurvProb(model.cox.derivation , newdata = data, times = times)
  rocobj.AUC.derivation <- pROC::roc(response = data$event.time, predictor = cox.predict, na.rm = TRUE, smooth = FALSE)
  rocobj.PRAUC.derivation <- pROC::coords(rocobj.AUC.derivation, "all", ret=c("threshold", "precision", "recall"))
  
  #########################
  ##### VALIDATION #######
  ########################
  data <- validation
  
  data$event.time <- ifelse(
    data[[event.name]] == 1 & data[[time.name]] <= times, 1, ifelse(
      data[[event.name]]  == 0 & data[[time.name]]> times, 0, NA)
  )
  
  data <- filter(data,!is.na(event.time))
  
  ### 
  auc_values_validation <- numeric(n_bootstrap)
  prauc_values_validation <- numeric(n_bootstrap)
  
  for(i in 1:n_bootstrap){ # ajouter ici la Se Sp après bootstrap des cut-offs dérivés de la valdiation
    indices <- sample(1:nrow(data), replace = TRUE)
    bootstrap_sample <- data[indices, ]
    predictions = 1- predictSurvProb(model.cox.derivation, newdata = bootstrap_sample, times = times)
    bootstrap_sample$predictions <- as.numeric(predictions)
    
    # measurements
    auc_values_validation[i] <- MLmetrics::AUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
    prauc_values_validation[i] <- MLmetrics::PRAUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
  }
  
  # RESULTS
  results.auc.validation <- paste0("AUC :",  round(median(auc_values_validation),2), " [", round(quantile(auc_values_validation, 0.25),2), "-", round(quantile(auc_values_validation, 0.75),2), "]")
  results.prauc.validation <- paste0("PRAUC :",  round(median(prauc_values_validation),2), " [", round(quantile(prauc_values_validation, 0.25),2), "-", round(quantile(prauc_values_validation, 0.75),2), "]")
  results.death.validation <- round(sum(data$event.time == 1)/nrow(data),2)
  
  # graphical
  cox.predict.validation = 1- predictSurvProb(model.cox.derivation , newdata = data, times = times)
  rocobj.AUC.validation <- pROC::roc(response = data$event.time, predictor = cox.predict.validation, na.rm = TRUE, smooth = FALSE)
  rocobj.PRAUC.validation <- pROC::coords(rocobj.AUC.validation, "all", ret=c("threshold", "precision", "recall"))
  
 
  list_res <- list(
    results.auc.derivation = results.auc.derivation,
    results.prauc.derivation = results.prauc.derivation,
    results.death.derivation = results.death.derivation,
    rocobj.AUC.derivation =rocobj.AUC.derivation,
    rocobj.PRAUC.derivation = rocobj.PRAUC.derivation,
    
    results.auc.validation = results.auc.validation,
    results.prauc.validation = results.prauc.validation,
    results.death.validation = results.death.validation,
    rocobj.AUC.validation =rocobj.AUC.validation,
    rocobj.PRAUC.validation = rocobj.PRAUC.validation
  )
  
  return(list_res)
}
