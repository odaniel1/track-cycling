race_pdf_path <- function(lookup_df){
  
  lookup_df <- lookup_df %>% mutate(
    pdf_path = paste0(
      "./0_Raw-Data/Data/results-pdf/",
      event, "_",
      race, "_",
      round, ".pdf"
    ),
    
    pdf_path = str_replace_all(pdf_path, " ", "-"),
    pdf_path = str_replace_all(pdf_path, "[^[a-zA-Z0-9-_/.] ]", "")
  )

 return(lookup_df)
}

race_csv_path <- function(lookup_df){
  
  lookup_df <- lookup_df %>% mutate(
    csv_path = paste0(
      "./0_Raw-Data/Data/results-csv/",
      event, "_",
      race, "_",
      round, ".csv"
    ),
    
    csv_path = str_replace_all(csv_path, " ", "-"),
    csv_path = str_replace_all(csv_path, "[^[a-zA-Z0-9-_/.] ]", "")
  )
  
  return(lookup_df)
}