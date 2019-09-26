library(tidyverse)
source("./4_Utilities/create-race-file-paths.R")

# Read race lookup file, and add file paths for pdfs.
race_lookup <- read_csv("./0_Raw-Data/Data/race_lookup.csv") %>%
  race_pdf_path()

# Get file paths for all pdfs currently saved.
race_pdfs <- list.files("./0_Raw-Data/Data/results-pdf", full.names = TRUE)

# Filter to those races which do not have a pdf on file.
race_lookup <- race_lookup %>%
  filter(
    !(pdf_path %in% race_pdfs)
  )

# Download pdfs for races not currently on file.
pmap(race_lookup %>% select(pdf_url, pdf_path),
  function(pdf_url, pdf_path){
    download.file(pdf_url, pdf_path, mode = "wb")
  }
)




