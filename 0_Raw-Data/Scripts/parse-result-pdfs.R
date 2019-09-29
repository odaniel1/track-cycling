# parse-result-pdfs.R

library(tidyverse)
source("./4_Utilities/create-race-file-paths.R")
source("./0_Raw-Data/Functions/pdf-parsers.R")

# Read race lookup file, and add file paths for pdfs and csvs.
race_lookup <- read_csv("./0_Raw-Data/Data/race_lookup.csv") %>%
  race_pdf_path() %>%
  race_csv_path()

# Get file paths for all pdfs currently saved.
race_csvs <- list.files("./0_Raw-Data/Data/results-csv", full.names = TRUE)

# Filter to those races which do not have a pdf on file.
race_lookup <- race_lookup %>%
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
