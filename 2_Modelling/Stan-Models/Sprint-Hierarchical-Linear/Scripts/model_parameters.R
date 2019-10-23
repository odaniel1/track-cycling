source("./1_Data-Processing/Functions/format_individual_sprint_data.R")

model_parameters <- function(race_lookup){
  
  sprints <- format_qualifying_sprint_data(race_lookup)

  parameters <- list(
    R = max(sprints$riders$rider_id),
    S = nrow(sprints$sprints),
    E = max(sprints$events$event_id),
    rider_id = sprints$sprints$rider_id,
    event_id = sprints$sprints$event_id,
    time = sprints$sprints$time,
    event_hyperprior = c(9.75, 0.05, 0.025),
    rider_hyperprior = 0.1,
    sd_prior = 0.02
  )
  
  return(list(stan_parameters = parameters, riders = sprints$riders, events = sprints$events ))
}