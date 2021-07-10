
location_lookup <- function(){
  tribble(
    ~location_country, ~location_country_code,
    "Australia", "AUS",
    "Canada",    "CAN",
    "China",     "CHN",
    "France",    "FRA",
    "Germany",   "GER",
    "New Zealand","NZL",
    "Poland",    "POL",
    "United Kingdom", "GBR",
    "Russia",    "RUS"
  )
}

prepare_races <- function(path){
  
  races <- read_csv(path) %>%
    filter(
      csv_cached == TRUE,
      season %in% c("2018/19", "2019/20"),
      race   ==   'Individual Sprint',
      gender ==   'Women',
      round  !=   'Qualifying',
    ) %>%
    left_join(location_lookup())
  
  return(races)
}

prepare_results <- function(races_df){
  
  results <- vroom::vroom(paste0('../tissot-scraper/',races_df$csv_path)) %>% 
    bind_rows() %>%
    left_join(races_df %>% select(csv_path, location_country_code)) %>%
    select(event, date, location_country_code, gender, race, round, rider, team, heat_id, win_count)
  
  results <- results %>%
    group_by(date, race, round, heat_id) %>%
    mutate(seed = paste0('seed_',1:n())) %>%
    ungroup() 
  
  results <- results %>%
    mutate(
      round = factor(round, levels = c("1-16 Finals", "1-8 Finals", "Quarterfinals", "Semifinals", "Finals"))
    )

  return(results)
}

prepare_riders <- function(results_df){
  
  riders <- results_df %>%
    distinct(rider) %>%
    transmute(
      rider_name = rider,
      rider_id = 1:n()
    )
  
  return(riders)
}

prepare_rider_days <- function(results_df, riders_df){
  
  rider_days <- results_df %>%
    left_join(riders_df, by = c("rider" = "rider_name")) %>%
    distinct(rider_id, date) %>%
    arrange(rider_id, date) %>%
    group_by(rider_id) %>%
    mutate(
      rider_date_no = 1:n() - 1,
      days_to_prev = as.numeric(date - lag(date,1)),
      days_to_prev = if_else(is.na(days_to_prev),0,days_to_prev)
    ) %>%
    ungroup()
  
  return(rider_days)
  
}

prepare_matches <- function(results_df, riders_df, days_df){
  
  matches <- results_df %>%
    # reformat in wide: one row per match, and add rider names
    pivot_wider(names_from = c(seed), values_from = c(team, rider, win_count)) %>%
    left_join(riders_df %>% rename(rider_seed_1 = rider_name, rider_id_seed_1 = rider_id)) %>%
    left_join(riders_df %>% rename(rider_seed_2 = rider_name, rider_id_seed_2 = rider_id)) %>%
    rename_with(~str_remove(., "seed_"))
  
  # identify rider id of winning/losing rider and no. sprints in match
  matches <- matches %>%
    mutate(
      winner_id = if_else(win_count_1 > win_count_2, rider_id_1, rider_id_2),
      loser_id  = if_else(win_count_1 > win_count_2, rider_id_2, rider_id_1),
      sprints = win_count_1 + win_count_2
    )
  
  # add the winner/loser date index
  matches <- matches %>%
    left_join(days_df %>% select(rider_id, date, winner_date_no = rider_date_no), by = c("winner_id" = "rider_id", "date" = "date")) %>%
    left_join(days_df %>% select(rider_id, date, loser_date_no = rider_date_no), by = c("loser_id" = "rider_id", "date" = "date"))

  return(matches)
}

prepare_pairings <- function(matches_df){
  pairings <- matches_df %>%
    mutate(
      min_rider_id = pmin(rider_id_1, rider_id_2),
      max_rider_id = pmax(rider_id_1, rider_id_2)
    ) %>% 
    count(min_rider_id, max_rider_id, sort = TRUE) %>%
    filter(n > 1)
  
  return(pairings)
}
