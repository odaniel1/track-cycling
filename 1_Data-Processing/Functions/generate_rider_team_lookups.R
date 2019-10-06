generate_rider_lookup <- function(matches){
  
  riders <- matches %>%
    select(rider = name) %>%
    unique() %>%
    mutate(rider_id = 1:n())

  return(riders)
}

generate_team_lookup <- function(matches){
  
  teams <- matches %>%
    select(team) %>%
    unique() %>%
    mutate(team_id = 1:n())
  
  return(teams)
}
