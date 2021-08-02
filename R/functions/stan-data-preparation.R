
prepare_stan_data <- function(riders_df, matches_df, pairing_df, days_df, training = TRUE){

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
    training = 1 * training,
    R = nrow(riders_df),  # No. Riders
    M = nrow(matches_df), # No Matches
    T = sum(matches_df$split == "training"),
    D = nrow(days_df),
    E = max(matches_df$event_id),
    winner_id = matches_df$winner_id, # ID of first rider
    loser_id = matches_df$loser_id, # ID of second rider
    sprints = matches_df$sprints, # No. Sprints in match, models >= 2.0
    winner_date_no = matches_df$winner_date_no,
    loser_date_no = matches_df$loser_date_no,
    winner_at_home = matches_df$winner_at_home,
    loser_at_home = matches_df$loser_at_home,
    date_index_R = c(rider_date_start$pos, nrow(days_df)+1),
    rider_dates = days_df$years_to_start,
    B = 10,
    qual_diff = matches_df$winner_qual_time_diff,
    event = matches_df$event_id
  )
  
  return(stan_data)
}