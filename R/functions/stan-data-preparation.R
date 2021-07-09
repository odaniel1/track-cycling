
prepare_stan_data <- function(riders_df, matches_df, pairing_df){

  stan_data <- list(
    R = nrow(riders_df),  # No. Riders
    M = nrow(matches_df), # No Matches
    P = nrow(pairing_df), # No. Pairings, models >= 4.0
    winner_id = matches_df$winner_id, # ID of first rider
    loser_id = matches_df$loser_id, # ID of second rider
    pairings = as.matrix(pairing_df %>% select(min_rider_id, max_rider_id)), # No. Pairings, models >= 4.0
    sprints = matches_df$sprints # No. Sprints in match, models >= 2.0
  )
  
  return(stan_data)
}