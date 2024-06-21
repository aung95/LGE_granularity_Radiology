# CoxPredictNew
CoxPredictAll <- function(data,
                       time.name = "time", 
                       times = 10*12,
                       event.name ="event", 
                       variables = c() , 
                       n_bootstrap = 100, 
                       ci_level = 0.95, 
                       seed = 123){
  
  require(survival)
  require(pec)
  require(intsurv)

  set.seed(seed)
  
  #########################
  ##### MODEL CREAT #######
  ########################
  
  formula.cox = paste0("Surv(", time.name, ", ", event.name, ") ~ ", variables[1])
  if (length(variables) > 1){for (i in variables[2:length(variables)]){
    formula.cox = paste0(formula.cox,"+", i)
  }}
  
  # print(paste0("Variable analysed are: ",formula.cox))
  formula = as.formula(formula.cox)
  model.cox <- coxph(formula, x = TRUE, data = data)
  
  
  # formula.cox = paste0("Surv(", time.name, ", ", event.name, ") ~ ", variables[1])
  formula_str <- sprintf("Surv(%s, %s) ~ 1", time.name, event.name)
  null_model <- coxph(formula = as.formula(formula_str), data = data)
  
  #########################
  ##### DERIVATION #######
  ########################
    
  data$event.time <- ifelse(
    data[[event.name]] == 1 & data[[time.name]] <= times, 1, ifelse(
      data[[event.name]]  == 0 & data[[time.name]]> times, 0, NA)
    )
    
  data <- filter(data,!is.na(event.time))
    
    ### 
    auc_values <- numeric(n_bootstrap)
    prauc_values <- numeric(n_bootstrap)
    
    for(i in 1:n_bootstrap){ # ajouter ici la Se Sp après bootstrap des cut-offs dérivés de la valdiation
      indices <- sample(1:nrow(data), replace = TRUE)
      bootstrap_sample <- data[indices, ]
      predictions = 1- predictSurvProb(model.cox, newdata = bootstrap_sample, times = times)
      bootstrap_sample$predictions <- as.numeric(predictions)
      
      # measurements
      auc_values[i] <- MLmetrics::AUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
      prauc_values[i] <- MLmetrics::PRAUC(y_pred = bootstrap_sample$predictions, y_true= bootstrap_sample$event.time)
    }
    
    # RESULTS
    results.auc <- paste0("AUC :",  round(median(auc_values),2), " [", round(quantile(auc_values, 0.25),2), "-", round(quantile(auc_values, 0.75),2), "]")
    results.prauc <- paste0("PRAUC :",  round(median(prauc_values),2), " [", round(quantile(prauc_values, 0.25),2), "-", round(quantile(prauc_values, 0.75),2), "]")
    results.death <- round(sum(data$event.time == 1)/nrow(data),2)
    
    # CHI2
    comparison_result <- anova(null_model, model.cox)
    
    Chi2 = comparison_result$Chisq[2]
    LR = comparison_result$`Pr(>|Chi|)`[2]
    
    chi_LR <- sprintf("Chi2 = %.2f, LR test %.4f", Chi2, LR)
    
    # Constructing concordance info string
    results <- sprintf("Chi2 = %.2f, LR test %.4f", Chi2, LR)
    
    # graphical
    cox.predict = 1- predictSurvProb(model.cox , newdata = data, times = times)
    rocobj.AUC <- pROC::roc(response = data$event.time, predictor = cox.predict, na.rm = TRUE, smooth = FALSE)
    rocobj.PRAUC <- pROC::coords(rocobj.AUC, "all", ret=c("threshold", "precision", "recall"))
    
  
    
    list_res <- list(
      chi_LR = chi_LR,
      results.auc = results.auc,
      results.prauc = results.prauc,
      results.death = results.death,
      rocobj.AUC =rocobj.AUC,
      rocobj.PRAUC = rocobj.PRAUC
    )
    
    return(list_res)
  }


