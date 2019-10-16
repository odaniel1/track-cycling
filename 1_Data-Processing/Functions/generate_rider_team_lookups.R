generate_rider_lookup <- function(matches){

  riders <- matches %>%
    select(rider = name, round) %>%
    group_by(rider) %>%
    slice(which.max(round)) %>%
    ungroup() %>%
    mutate(
      round_id = as.numeric(round),
      rider_id = 1:n()
    )

  return(riders)
}

generate_team_lookup <- function(matches){
  
  teams <- matches %>%
    select(team) %>%
    unique() %>%
    mutate(team_id = 1:n())
  
  return(teams)
}
