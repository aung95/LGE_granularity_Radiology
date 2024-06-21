library(here)
library(here)

manage_file_export <- function(x, base_dir, file_prefix) {
  # Define the filename pattern
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  file_name <- paste0(file_prefix, today_str, ".docx")
  full_path <- file.path(base_dir, file_name)
  
  # List and delete previous versions
  files_to_delete <- list.files(base_dir, pattern = paste0("^", file_prefix, "\\d{4}-\\d{2}-\\d{2}\\.docx$"), full.names = TRUE)
  files_to_delete <- files_to_delete[files_to_delete != full_path]
  if (length(files_to_delete) > 0) {
    file.remove(files_to_delete)
  }
  
  # Export to Word with specified settings
  export2word(x = x, 
              file = full_path, 
              which.table = "descr", 
              nmax = TRUE, 
              header.labels = c(), 
              caption = NULL, 
              strip = FALSE, 
              first.strip = FALSE, 
              background = "#D2D2D2",
              size = NULL, 
              header.background = NULL, 
              header.color = NULL)
}

# Example usage
# base_directory <- here("outputs", "tables")
# file_prefix <- "Tab2-descr-all-center-"
# manage_file_export(x = Descr_table, base_dir = base_directory, file_prefix = file_prefix)
