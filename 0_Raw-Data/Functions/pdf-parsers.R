library(pdftools)

#----------------------------------------------------------------------
parse_team_sprint <- function(path){
  
  # Parse pdf to a data frame.
  df <- pdf_text(path) %>%
    paste(sep = " ", collapse = "") %>%
    readr::read_lines() %>% data_frame(raw = .)
  
  # Finals pdf files need to have medal results removed.
  df <- df %>%
    mutate(
      raw = gsub("GOLD|SILVER|BRONZE", "", raw) %>% str_squish()
    )
  
  # Reduce to rows which contain team race timings. These are identified as they start with a number
  # followed by a three letter team code and a hyphen.
  df <- df %>%
    filter(grepl("^([0-9] ){0,1}[0-9]{1,2} [A-Z]{3} - ", raw))
  
  # Clean the raw text
  df <- df %>% 
    mutate(
      # Remove team code and hypthen; raw now starts with the team name.
      raw = gsub(".*- ", "",raw),
      
      # Remove appearances of QB (Qualifier Bronze) or QG (Qualifier Gold)
      raw = gsub("QB|QG", "", raw),
      
      # Remove numbers in brackets (indicating position after lap)
      raw = gsub("\\([0-9]{1,2}\\)", "", raw) %>% str_squish()
    )
  
  # Separate raw into columns based on white space; columns now correspond
  # to the team name, timings for each lap, and the average speed.
  df <- df %>%
    separate(
      raw,
      into = c("team", "lap_1", "lap_2", "lap_3", "avg_speed_kph"),
      sep = "\\s(?=[^a-zA-Z]*$)"# "[a-zA-Z]\\s(?=[^a-zA-Z]*$)"
    ) 
  
  # Reduce data to the team name, distance, final time (lap 3), average
  # speed, and pdf file path.
  df <- df %>%
    select(team, time = lap_3, avg_speed_kph) %>%
    mutate(
      time = as.numeric(time),
      avg_speed_kph = as.numeric(avg_speed_kph),
      pdf_path = path
    )
  
  return(df)
}

parse_sprint_qualifying <- function(path){
  
  # Parse pdf to a string.
  df <- pdf_text(path) %>%
    paste(sep = " ", collapse = "")%>%
    readr::read_lines() %>% data_frame(raw = .)
  
  # Remove leading/trailing whitespace, and reduce multiple whitespaces to single spaces.
  df <- df %>% mutate(raw = str_squish(raw))
  
  # Reduce to rows which contain race timings. These are identified as they start with a
  # one or two digit number indicating qualifying position, followed by a two or three digit
  # rider number.
  df <- df %>%
    filter(grepl("^([0-9] ){0,1}[0-9]{1,2} [0-9]{2,3}", raw))

  # Clean the raw text
  df <- df %>% 
    mutate(
      raw = str_replace(raw,"[0-9]{1,2} [0-9]{1,3}", "")
    )
  
  # Separate raw into a name component, and result component.
  df <- df %>%
    mutate(
      raw_name = str_replace(raw, " [0-9].*", "") %>% trimws(),
      raw_result = str_replace(raw, raw_name, "") %>% trimws()
    )
  
  # Create team and name fields.
  df <- df %>%
    mutate(
      name = substr(raw_name, 1, nchar(raw_name) - 3) %>% trimws(),
      team = str_replace(raw_name, name, "") %>% trimws()
    )
  
  # Clean raw_result field, removing bracketed figures (lap position).
  df <- df %>%
    mutate(
      raw_result =  gsub("\\([0-9]{1,2}\\)", "", raw_result) %>% str_squish()
    )
  
  # Separate raw result into lap times, and average speed.
  df <- df %>%
    separate(
      raw_result,
      into = c("lap_1", "lap_2", "avg_speed_kph"),
      sep = " "
    )
  
  # Reduce data to the team name, final time (lap 3), average
  # speed, and pdf file path.
  df <- df %>%
    select(name, team, time = lap_2, avg_speed_kph) %>%
    mutate(
      time = as.numeric(time),
      avg_speed_kph = as.numeric(avg_speed_kph),
      pdf_path = path
    )
  
  return(df)
}

parse_sprint_matches <- function(path){
  
  # Parse pdf to a data frame.
  df <- pdf_text(path) %>%
    paste(sep = " ", collapse = "") %>%
    readr::read_lines() %>% data_frame(raw = .)

  # For final matches we need to clean the text to remove rows indicating the riders who
  # did not qualify. We do this by finding the first row in the data frame which matches
  # the string "Did not qualify", and removing all rows of df below this.
  dnq_pos <- match("TRUE", str_detect(df$raw, "Did not qualify.*"))

  if(is.na(dnq_pos) == FALSE){
    df <- df[1:(dnq_pos - 1),]    
  }

  # Finals pdf files need to have medal results removed.
  df <- df %>%
    mutate(
      raw = gsub("GOLD|SILVER|BRONZE", "", raw) %>% str_squish()
    )

  
  # Remove rows of the file that have the date/communique number (otherwise some date
  # strings are mistaken as results)
  df <- df %>%
    filter(!grepl(".*Communiqu", raw))
  
  # Reduce to rows which contain race timings. These are identified as they start with a
  # one or two digit number indicating qualifying position, followed by a two or three digit
  # rider number.
  df <- df %>%
    filter(grepl("^([0-9] ){0,1}[0-9 ]{0,1}[0-9]{2,3} ", raw))

    
  # Clean the raw text, removing race/rider id, separate raw into a name
  # and result component.
  df <- df %>% 
    mutate(
      raw = str_replace(raw,"^([0-9] ){0,1}[0-9 ]{0,1}[0-9]{2,3} ", ""),
      raw_name = str_remove(raw, "Winner.*|\\+.*|REL.*|DNF.*|DNS.*") %>% trimws(),
      raw_result = str_extract(raw, "Winner.*|\\+.*|REL.*|DNF.*|DNS.*") %>% trimws()
    )
  # Create team and name fields.
  df <- df %>%
    mutate(
      name = substr(raw_name, 1, nchar(raw_name) - 3) %>% trimws(),
      team = str_replace(raw_name, name, "") %>% trimws()
    )
  
  # Clean raw_result field, removing bracketed figures (lap position).
  df <- df %>%
    mutate(
      raw_result =  gsub("\\([0-9]{1,2}\\)", "", raw_result) %>% str_squish()
    )
  
  # Add row id number, and heat id.
  df <- df %>%
    mutate(
      row_id = 1:n(),
      heat_id = ceiling(row_id/2)
    )
  
  # Add win count column, and file path column
  df <- df %>%
    mutate(
      win_count = str_count(raw_result, "Winner"),
      pdf_path = path
    )
  
  # Reduce data to the team name, distance, final time (lap 3), average
  # speed, and pdf file path.
  df <- df %>%
    select(name, team, heat_id, win_count, pdf_path)
  
  return(df)
}
