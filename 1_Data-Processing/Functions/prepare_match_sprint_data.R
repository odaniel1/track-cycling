library(tidyverse)

prepare_match_sprint_data <- function(race_lookup){
  
  # Check that all races in race_lookup are a) individual sprints,
  # and b) not qualifying rounds.
  check_sprints <- race_lookup %>% filter(
    race != "Individual Sprint" |
    round == "Qualifying"
  )
  
  if(nrow(check_sprints) != 0){
    stop("Not all races are match sprints.")
  }
  
  # Check that all races are between the same gender.
  gender_check <- race_lookup$gender %>% unique()
  
  if(length(gender_check) != 1){
    stop("Not all races are between the same gender.")
  }
  
  # Read in individual csv files, and bind.
  matches_df <- map(race_lookup$csv_path, read_csv) %>% bind_rows
  
  # Bind on race identifiers from race_lookup.
  matches_df <- matches_df %>%
    left_join(race_lookup %>% select(pdf_path, event, location, gender, round))  
  
  
  matches_df <- matches_df %>%
    mutate(
      # Create unique match id by combining race details with the heat id.
      match_id = paste(heat_id, round, gender, location, event, sep = "_"),
      
      # Identify riders as rider 0 and 1.
      rider_no = rep(0:1, n()/2)
    )
    
  # Reduce to required columns
  matches_df <- matches_df %>%
    select(match_id, rider_no, name, win_count)
  
  
  # We now create a wide format data set for each match - i.e. there is a single
  # row for each match, with both rider names, and the number of sprints won by
  # each rider.
  
  rider_df <- matches_df %>% select(match_id, rider_no, name) %>% 
    spread(rider_no, name) %>%
    rename(rider_0 = `0`, rider_1 = `1`)
  
  result_df <- matches_df %>% select(match_id, rider_no, win_count) %>%
    spread(rider_no, win_count) %>%
    rename(wins_0 = `0`, wins_1 = `1`)
  
  matches_wide_df <- left_join(rider_df, result_df)
  
  # We now create a data frame with a row for each individual sprint (currently
  # a match may involve upto 3 sprints).
  
  
  sprints_df <- pmap(matches_wide_df, 
    function(match_id, rider_0, rider_1, wins_0, wins_1){
      
      # Data for sprints won by rider_0
      sprints_0 <- tibble(
        match_id = match_id,
        rider_0 = rider_0,
        rider_1 = rider_1,
        winner  = rep(0, wins_0)
      )
      
      # Data for sprints won by rider_1
      sprints_1 <- tibble(
        match_id = match_id,
        rider_0 = rider_0,
        rider_1 = rider_1,
        winner  = rep(1, wins_1)
      )
      
    bind_rows(sprints_0, sprints_1)
    }) %>% bind_rows()
  
  return(sprints_df)
}