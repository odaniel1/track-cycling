
prepare_stan_data <- function(riders_df, matches_df, pairing_df, days_df){

  date_diffs_rider_pos <- days_df %>%
    mutate(row_no = 1:n()) %>%
    group_by(rider_id) %>%
    summarise(pos = min(row_no)) %>%
    ungroup() %>%
    arrange(rider_id)
  
  stan_data <- list(
    R = nrow(riders_df),  # No. Riders
    M = nrow(matches_df), # No Matches
    P = nrow(pairing_df), # No. Pairings, models >= 4.0
    D = nrow(days_df),
    Z = nrow(days_df %>% filter(days_to_prev > 0)),
    winner_id = matches_df$winner_id, # ID of first rider
    loser_id = matches_df$loser_id, # ID of second rider
    pairings = as.matrix(pairing_df %>% select(min_rider_id, max_rider_id)), # No. Pairings, models >= 4.0
    sprints = matches_df$sprints, # No. Sprints in match, models >= 2.0
    winner_date_no = matches_df$winner_date_no,
    loser_date_no = matches_df$loser_date_no,
    winner_at_home = matches_df$winner_at_home,
    loser_at_home = matches_df$loser_at_home,
    date_diffs_rider_pos = date_diffs_rider_pos$pos,
    date_diffs = days_df$days_to_prev
  )
  
  return(stan_data)
}