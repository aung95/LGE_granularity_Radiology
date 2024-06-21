# R function to create graphics

# Coments to me
# Add the fact that is saves directly the files in the document you need. 

# Save the plot in a folder 
createSurvivalPlot <- function(
    data, 
    compared_with,
    time = "outcome_FU_time_death",
    event = "outcome_death",
    confint_choosen = 0.995,
    legend_title = "Legend_title",
    mytitle = "KM_title",
    my_legends = NULL,  # Default legend labels
    my_colors = NULL,
    show.CI = TRUE,
    show.pval = TRUE,
    ylim_choosen = c(0, 1),
    xlim_choosen = c(0, 13),
    size_font = 12
) {
  
  require(survival)
  require(survminer)
  require(gtsummary)
  require(dplyr)
  require(ggplot2)
  # FIRST DROP
  data <- droplevels(data)
  
  # LEGENDS
  # Check if custom legends are provided, if not, use unique categories from compared_with column
  if(is.null(my_legends) || length(my_legends) == 0){
    if(is.factor(data[[compared_with]])) {
      my_legends = levels(data[[compared_with]])
    } else {
      my_legends = unique(as.character(data[[compared_with]]))
    }
  }

  # COLORS
  if(is.null(my_colors))
  {
    vector_color <- c("darkgreen","#FF002B", "orange", "darkblue", "#8B4513", "#2C3E50", "#000000")
    my_colors <- vector_color[1:length(my_legends)]
    } 
  else{
      my_colors = my_colors
    }
  
  # Cox univariate model
  formula <- as.formula(paste("Surv(", time, "/12 ,", event, ") ~", compared_with))
  
  # Debug: Print formulas to check correctness
  print(paste("Cox model formula:", deparse(formula)))
  
  model <- coxph(formula, data = data) 
  HR <- extract_model_stats(model, show.coef = FALSE, conf_level = confint_choosen)
  
  # survfit function
  fit <- surv_fit(formula, conf.int = confint_choosen, data = data) 
  
  # Calculate dynamic heights based on the number of groups
  num_groups <- length(my_legends)
  base_height <- 0.20
  increment <- 0.03
  tables_height <- (base_height + increment * (num_groups - 1)) / (1 + increment * (num_groups - 1))
  
  ggsurv <- ggsurvplot(
    fit, # Fit object from survival analysis
    
    # Aesthetics
    linetype = 1,
    palette = my_colors,
    conf.int = show.CI,
    
    # Titles and labels
    title = mytitle,
    legend.title = legend_title, 
    legend.labs = my_legends,
    xlab = "Years of follow-up",
    ylab = "Survival probability",
    
    # Font sizes
    
    pval.size = round(size_font / 2, digits = 0),
    
    # P-value
    pval = show.pval, 
    pval.coord = c(0.35, 0.80),
    
    # Axes limits
    xlim = xlim_choosen,
    ylim = ylim_choosen,
    
    # Risk table
    risk.table = TRUE,
    risk.table.col = "strata",
    risk.table.fontsize = size_font / 2, # Makes the risk table font size half the base size
    tables.theme = theme_cleantable(),
    y.text = F, 
    tables.y.text = F,
    
    # Custom theme
    ggtheme = theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", # Example position inside the plot
      legend.text = element_text(size = size_font),
      legend.title = element_blank(), # element_text(size = size_font),
      axis.text.x = element_text(size = size_font+3), # Adjusting x-axis font size
      axis.text.y = element_text(size = size_font+3),  # Adjusting y-axis font size
      axis.title.x = element_text(size = size_font * 1.2, face = "bold"), # Increasing X-axis title font size
      axis.title.y = element_text(size = size_font * 1.2, face = "bold"), #,  # Increasing Y-axis title font size
      strip.background = element_rect(fill = "white", colour = "black") # see if it has interest ?
    ),
    
    # Additional aesthetics adjustments
    tables.height = tables_height, # 0.20 when 2
    surv.plot.height = 0.75,
    break.x.by = 2,
    axes.offset = 0.01
  )
  
  return(list(HR = HR, ggsurv = ggsurv))
  
}

# # Example usage
# # Adjust 'compared_with' and 'my_legends' as per your actual column names and group labels
# X <- createSurvivalPlot(data = df_all, compared_with = "CMR_LGE_ischemic_presence", my_legends = NULL)
# X$ggsurv
# X <- createSurvivalPlot(data = df_all, compared_with = "CMR_LGE_ischemic_transmurality", my_legends = NULL)
# X$ggsurv
