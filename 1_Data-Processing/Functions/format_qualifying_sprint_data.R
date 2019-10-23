library(tidyverse)
source("./1_Data-Processing/Functions/generate_rider_team_lookups.R")

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