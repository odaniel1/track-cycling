
prepare_stan_data <- function(riders_df, matches_df, pairing_df, days_df){

  # arrange so all training data preceeds evaluation data
  matches_df <- matches_df  %>%
    arrange(desc(split), round)
  
  split_round_index <- matches_df %>%
    mutate(row_no = 1:n()) %>%
    group_by(split, round) %>%
    summarise(pos = min(row_no)) %>%
    arrange(desc(split), round) %>%
    ungroup()
  
  rider_date_start <- days_df %>%
    mutate(row_no = 1:n()) %>%
    group_by(rider_id) %>%
    summarise(pos = min(row_no)) %>%
    ungroup() %>%
    arrange(rider_id)
  
  stan_data <- list(
    R = nrow(riders_df),  # No. Riders
    M = nrow(matches_df), # No Matches
    T = sum(matches_df$split == "training"),
    P = nrow(pairing_df), # No. Pairings, models >= 4.0
    D = nrow(days_df),
    winner_id = matches_df$winner_id, # ID of first rider
    loser_id = matches_df$loser_id, # ID of second rider
    pairings = as.matrix(pairing_df %>% select(min_rider_id, max_rider_id)),
    sprints = matches_df$sprints, # No. Sprints in match, models >= 2.0
    split_round_index = c(split_round_index$pos, nrow(matches_df)),
    winner_date_no = matches_df$winner_date_no,
    loser_date_no = matches_df$loser_date_no,
    winner_at_home = matches_df$winner_at_home,
    loser_at_home = matches_df$loser_at_home,
    date_index_R = c(rider_date_start$pos, nrow(days_df)),
    rider_dates = days_df$years_to_start,
    B = 15,
    qual_diff = matches_df$winner_qual_time_diff
  )
  
  return(stan_data)
}