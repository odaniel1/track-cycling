# update-result-files.R

library(tidyverse)
library(httr)

source("./0_Raw-Data/Functions/generate_race_lookup.R")
source("./0_Raw-Data/Functions/pdf-parsers.R")

# Get file paths for all pdfs currently saved.
race_pdfs <- list.files("./0_Raw-Data/Data/results-pdf", full.names = TRUE)

# Read race lookup file, and filter to races without a saved pdf.
race_lookup <- generate_race_lookup() %>%
  filter( !(pdf_path %in% race_pdfs) )

# Download pdfs for races not currently on file.
pmap(race_lookup %>% select(id, url_path, pdf_path),
  function(id, url_path, pdf_path){
    
    # Get file type of url; it is expected that the url points to a
    # pdf file.
    url_get <- GET(url_path)
    url_type <- headers(url_get)['content-type']$`content-type`
    
    if(url_type != "application/pdf"){

      # If the file is not a pdf, return the race id, and a success flag
      # indicating a failure.
      return(tibble(id = id, success = FALSE))

    } else{
      
      # Otherwise download the file, and return a success flag indicating
      # success.
      download.file(url_path, pdf_path, mode = "wb")
      
      return(tibble(id = id, success = TRUE))
    }
  }
) %>% bind_rows() %>% filter(success == FALSE)

# Get file paths for all csvs currently saved.
race_csvs <- list.files("./0_Raw-Data/Data/results-csv", full.names = TRUE)

# Read race lookup file, and filter to races without a saved pdf.
race_lookup <- generate_race_lookup() %>%
  filter( !(csv_path %in% race_csvs) )

# Parse pdfs, and write to csv.
pmap(race_lookup %>% select(pdf_path, race, round,  csv_path),
     function(pdf_path, race, round,  csv_path){
       
       if(race == "Team Sprint"){
         results_df <- parse_team_sprint(pdf_path)
       }
       else if( (race == "Individual Sprint") && (round == "Qualifying") ){
         results_df <- parse_sprint_qualifying(pdf_path)
       }
       else if( (race == "Individual Sprint") && (round != "Qualifying")){
         results_df <- parse_sprint_matches(pdf_path)         
       }
       
       write_csv(results_df, csv_path)
       return(paste(race, round, pdf_path, sep = " - "))
     }) %>% unlist() %>% enframe()
