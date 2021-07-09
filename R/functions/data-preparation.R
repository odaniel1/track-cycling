
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
      season %in% c("2019/20", "2018/19"),
      race   ==   'Individual Sprint',
      gender ==   'Women',
      round  !=   'Qualifying'
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
      date_sc = as.numeric(date - min(date))/365
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

prepare_matches <- function(results_df, riders_df){
  
  matches <- results_df %>%
    pivot_wider(names_from = c(seed), values_from = c(team, rider, win_count)) %>%
    left_join(riders_df %>% rename(rider_seed_1 = rider_name, rider_id_seed_1 = rider_id)) %>%
    left_join(riders_df %>% rename(rider_seed_2 = rider_name, rider_id_seed_2 = rider_id)) %>%
    rename_with(~str_remove(., "seed_")) %>%
    mutate(
      winner_id = if_else(win_count_1 > win_count_2, rider_id_1, rider_id_2),
      loser_id  = if_else(win_count_1 > win_count_2, rider_id_2, rider_id_1),
      sprints = win_count_1 + win_count_2
    )
  
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
