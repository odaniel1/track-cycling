source("./1_Data-Processing/Functions/format_individual_sprint_data.R")

model_parameters <- function(race_lookup){
  
  sprints <- format_match_sprint_data(race_lookup)

  rider_teams <-bind_rows(
    sprints$sprints %>% select(rider = rider_0, team = team_0),
    sprints$sprints %>% select(rider = rider_1, team = team_1)
   ) %>% group_by(rider, team) %>% count() %>%
    ungroup() %>% group_by(rider) %>% slice(which.max(n)) %>%
    select(-n) %>%
    left_join(sprints$riders) %>%
    left_join(sprints$teams) %>%
    arrange(rider_id)
  
  
  parameters <- list(
    R = max(sprints$riders$rider_id),
    M = nrow(sprints$sprints),
    T = max(sprints$teams$team_id),
    rider_id_0 = sprints$sprints$rider_id_0,
    rider_id_1 = sprints$sprints$rider_id_1,
    rider_team = rider_teams$team_id,
    rider_round = rider_teams$round_id,
    outcome = sprints$sprints$winner,
    sd_prior = c(0.1, 0.1, 0.25) 
  )
  
  return(list(stan_parameters = parameters, riders = sprints$riders, teams = sprints$teams ))
}