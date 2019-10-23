library(tidyverse)
source("./1_Data-Processing/Functions/generate_rider_team_lookups.R")


format_match_sprint_data <- function(race_lookup){
  
  # Run checks to ensure data is valid.
  check_match_sprint_data(race_lookup)
  
  # Read in csv files, and bind.
  matches <- map(race_lookup$csv_path, read_csv) %>% bind_rows
  
  # Bind on race identifiers from race_lookup.
  matches <- matches %>%
    left_join(race_lookup %>% select(pdf_path, event, location, gender, round))  
  
  # Create an id for each match, and identify riders as rider 0 and rider 1.
  matches <- matches %>%
    mutate(
      match_id = paste(heat_id, round, gender, location, event, sep = "_"),
      rider_no = rep(0:1, n()/2)
    )
  
  # Convert round data to factor.
  matches <- matches %>%
    mutate(
      round = factor(round, levels = c("1-16 Finals", "1-8 Finals", "Quarterfinals", "Semifinals", "Finals"))
    )
  
  # # Create rider lookup data.
  # rider_lookup <- generate_rider_lookup(matches)
  
  sprints_wide <- generate_wide_match_sprints_data(matches)
  
  sprints_long <- generate_long_match_sprints_data(sprints_wide)
  
  # Add rider and team lookups.
  riders <- generate_rider_lookup(matches)
  teams  <- generate_team_lookup(matches)
  
  sprints_long <- sprints_long %>%
    left_join(riders %>% rename_all(~paste0(., "_0"))) %>%
    left_join(teams %>% rename_all(~paste0(., "_0"))) %>%
    
    left_join(riders %>% rename_all(~paste0(., "_1"))) %>%
    left_join(teams %>% rename_all(~paste0(., "_1")))
  
  sprints_long <- sprints_long %>% select(-winner, winner)
  
  return(list(sprints = sprints_long, riders = riders, teams = teams))
}

check_match_sprint_data <- function(race_lookup){
  
  # Check that all races in race_lookup are a) individual sprints (eg. match sprints),
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
  
  return(TRUE)
}

generate_wide_match_sprints_data <- function(matches){
  
  # We now create a wide format data set for each match - i.e. there is a single
  # row for each match, with both rider names, and the number of sprints won by
  # each rider.
  
  # Generate rider columns.
  rider_cols <- matches %>% select(match_id, round, rider_no, name) %>% 
    spread(rider_no, name) %>%
    rename(rider_0 = `0`, rider_1 = `1`)
  
  # Generate team columns.
  team_cols <- matches %>% select(match_id, rider_no, team) %>% 
    spread(rider_no, team) %>%
    rename(team_0 = `0`, team_1 = `1`)
  
  # Generate result columns
  result_cols <- matches %>% select(match_id, rider_no, win_count) %>%
    spread(rider_no, win_count) %>%
    rename(wins_0 = `0`, wins_1 = `1`)
  
  # Join rider and results data.
  sprints_wide <- left_join(rider_cols, team_cols) %>% left_join(result_cols)
  
  return(sprints_wide)
}

generate_long_match_sprints_data <- function(sprints_wide){
  # Generate a data frame with a row for each individual sprint;
  sprints_long  <- sprints_wide %>%
    rowwise() %>%
    do(
      merge(
        as_tibble(.),
        tibble( winner = c(rep(0,.$wins_0), rep(1,.$wins_1)) )
      )
    ) %>%
    ungroup() %>%
    select(-wins_0, -wins_1)
}

format_qualifying_sprint_data <- function(race_lookup){
  
  # Run checks to ensure data is valid.
  check_qualifying_sprint_data(race_lookup)
  
  # Read in csv files, and bind.
  sprints <- map(race_lookup$csv_path, read_csv) %>% bind_rows
  
  # Bind on race identifiers from race_lookup.
  sprints <- sprints %>%
    left_join(race_lookup %>% select(pdf_path, event, location, gender, round))  
  
  
  # Add rider and event lookups.
  riders <- sprints %>% select(name) %>% unique() %>%
    mutate(rider_id = 1:n() )
  
  events <- sprints %>% select(event, location) %>% unique() %>%
    mutate(event_id = 1:n())
  
  sprints <- sprints %>%
    left_join(riders) %>%
    left_join(events)
  
  return(list(sprints = sprints, riders = riders, events = events))
}

check_qualifying_sprint_data <- function(race_lookup){
  
  # Check that all races in race_lookup are a) individual sprints (eg. match sprints),
  # and b) not qualifying rounds.
  
  check_sprints <- race_lookup %>% filter(
    race != "Individual Sprint" |
      round != "Qualifying"
  )
  
  if(nrow(check_sprints) != 0){
    stop("Not all races are sprints.")
  }
  
  # Check that all races are between the same gender.
  gender_check <- race_lookup$gender %>% unique()
  
  if(length(gender_check) != 1){
    stop("Not all races are between the same gender.")
  }
  
  return(TRUE)
}