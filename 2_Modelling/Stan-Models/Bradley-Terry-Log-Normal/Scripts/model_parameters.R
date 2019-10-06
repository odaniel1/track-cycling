source("./1_Data-Processing/Functions/format_individual_sprint_data.R")

model_parameters <- function(race_lookup){
  
  sprints <- format_individual_sprint_data(race_lookup)
  
  parameters <- list(
    R = nrow(sprints$riders),
    M = nrow(sprints$sprints),
    rider_id_0 = sprints$sprints$rider_id_0,
    rider_id_1 = sprints$sprints$rider_id_1,
    outcome = sprints$sprints$winner,
    alpha = rep(2, nrow(sprints$riders))
  )
  
  return(list(stan_parameters = parameters, riders = sprints$riders, teams = sprints$teams ))
}