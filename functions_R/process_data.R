process_data <- function(data, event.name, time.name, times){
  # Ensure the column names exist
  if (!(event.name %in% names(data)) || !(time.name %in% names(data))) {
    stop("Column names specified do not exist in the dataframe")
  }
  
  data <- data %>%
    mutate(event.time = case_when(
      .data[[event.name]] == 1 & .data[[time.name]] < times ~ 1,
      .data[[time.name]] >= times ~ 0,
      TRUE ~ NA_real_  # handle unexpected cases explicitly
    )) %>%
    filter(!is.na(event.time))  # filtering NA values
  
  return(data)
}
