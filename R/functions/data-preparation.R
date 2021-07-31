
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
    "Russia",    "RUS",
    "Chile", "CHL",
    "Netherlands", "NED",
    "Indonesia", "IDN",
    "Switzerland", "CHE"
  )
}

prepare_races <- function(tissot_path, manual_path){
  
  races <- bind_rows(
    read_csv(tissot_path) %>% mutate(csv_path = paste0('../tissot-scraper/',csv_path)),
    read_csv(manual_path)) %>%
    filter(
      csv_cached == TRUE,
      race   ==   'Individual Sprint',
      gender == 'Women'
    ) %>%
    left_join(location_lookup())
  
  races <- races %>%
    mutate(
      split = if_else(date < ymd(20200101), 'training', 'evaluation')
    )
  
  return(races)
}

prepare_qualifying <- function(races_df){
  qualifying <- races_df %>%
    filter(round == 'Qualifying')
  
  qual_results <- vroom::vroom(qualifying$csv_path, id = "file_path") %>% 
    bind_rows() %>%
    left_join(races_df %>% select(csv_path, year, location_country_code, split), by = c("file_path" = "csv_path")) %>%
    select(split, event, year, date, location_country_code, gender, race, round, rider, team, time) %>%
    filter(time > 0) %>%
    arrange(desc(split), date)
  
  return(qual_results)
}

prepare_results <- function(races_df, qualifying_df){
  
  races <- races_df %>%
    filter(round  !=   'Qualifying')
  
  results <- vroom::vroom(races$csv_path,  id =  "file_path") %>% 
    bind_rows() %>%
    left_join(races_df %>% select(csv_path, year, location_country_code, split), by = c("file_path" = "csv_path")) %>%
    select(split, event, date, year, location_country_code, gender, race, round, rider, team, heat_id, win_count)
  
  # a bit of a bodge to create data from the qualifying rounds.
  qualifying_heat_1 <- qualifying_df %>%
    group_by(event, year, date, location_country_code, gender) %>%
    mutate(heat_id = sample(n(), n()))
  
  qualifying_heat_2 <- qualifying_df %>%
    group_by(event, year, date, location_country_code, gender) %>%
    mutate(heat_id = sample(n(), n()))
  
  qualifying_results <- bind_rows(qualifying_heat_1, qualifying_heat_2) %>%
    ungroup() %>%
    arrange(event, year, date, location_country_code, gender, heat_id, time) %>%
    group_by(event, year, date, location_country_code, gender, heat_id) %>%
    mutate(win_count = 2 - 1:n()) %>%
    select(-time)
  
  results <- bind_rows(results, qualifying_results) %>%
    group_by(date, race, round, gender, heat_id) %>%
    mutate(seed = paste0('seed_',1:n())) %>%
    ungroup()
  
  results <- results %>%
    mutate(
      round = factor(round, levels = c("Qualifying", "1-16 Finals", "1-8 Finals", "Quarterfinals", "Semifinals", "Finals"))
    )
  
  return(results)
}

prepare_events <- function(races_df){
  events <- races_df %>%
    distinct(event, year, gender, location_country_code) %>%
    mutate(event_id = 1:n())
  
  return(events)
}
  
prepare_teams <- function(path, results_df){
  
  team_lookup <- read_csv(path) %>%
    distinct(team_code, country_code)

  teams <- results_df %>%
    distinct(team) %>%
    left_join(team_lookup, by = c("team" = "team_code")) %>%
    mutate(country_code = coalesce(country_code, team))
  
  return(teams)
}

prepare_riders <- function(results_df){
  
  riders <- results_df %>%
    group_by(rider, gender) %>%
    slice(which.max(date)) %>%
    ungroup() %>%
    transmute(
      rider_name = rider,
      gender,
      rider_id = 1:n(),
      max_date = date
    )
  
  return(riders)
}

prepare_rider_days <- function(results_df, riders_df){
  
  rider_days <- results_df %>%
    left_join(riders_df, by = c("rider" = "rider_name")) %>%
    distinct(rider_id, date) 
  
  # add a date for the start of the track cycling at tokyo 2020
  rider_olympics <- rider_days %>% distinct(rider_id) %>%
    mutate(date = ymd(20210802))
  
  rider_days <- bind_rows(rider_days, rider_olympics)
  
  # add additional dates for athletes to be used as example data
  rider_extras <- riders_df %>%
    filter(rider_name %in% c("LEE WAI SZE", "VOINOVA ANASTASIIA", "HINZE EMMA")) %>%
    select(rider_id) %>%
    crossing(date = seq(min(rider_days$date), max(rider_days$date), length.out = 20))
  
  
  rider_days <- bind_rows(rider_days, rider_extras)
  
  rider_days <- rider_days %>%
    distinct() %>%
    arrange(rider_id, date) %>%
    group_by(rider_id) %>%
    mutate(
      rider_date_no = 1:n() - 1,
      days_to_prev = as.numeric(date - lag(date,1)),
      days_to_prev = if_else(is.na(days_to_prev),0,days_to_prev),
      days_to_start = as.numeric(date - min(date)),
      years_to_start = days_to_start/365.25
    ) %>%
    ungroup()
  
  return(rider_days)
  
}

prepare_matches <- function(results_df, riders_df, days_df, team_df, qual_df, events_df){
  
  matches <- results_df %>%
    # reformat in wide: one row per match, and add rider names
    pivot_wider(names_from = c(seed), values_from = c(team, rider, win_count)) %>%
    left_join(riders_df %>% select(rider_seed_1 = rider_name, rider_id_seed_1 = rider_id)) %>%
    left_join(riders_df %>% select(rider_seed_2 = rider_name, rider_id_seed_2 = rider_id)) %>%
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

  # add indicator of winner/loser home game
  matches <- matches %>%
    left_join(team_df %>% select(team_1 = team, country_code_1 = country_code)) %>%
    left_join(team_df %>% select(team_2 = team, country_code_2 = country_code)) %>%
    mutate(
      winner_at_home = if_else(win_count_1 > win_count_2, 1 * (location_country_code == country_code_1),1 * (location_country_code == country_code_2)),
      loser_at_home = if_else(win_count_2 > win_count_1, 1 * (location_country_code == country_code_2),1 * (location_country_code == country_code_2)),
    )
  
  # add winner/loser qualifying time 
  matches <- matches %>%
    left_join(qual_df %>% rename(rider_1 = rider, qual_time_1 = time) %>% select(-round,-date, -team)) %>%
    left_join(qual_df %>% rename(rider_2 = rider, qual_time_2 = time) %>% select(-round,-date, -team)) %>%
    mutate(
      winner_qual_time_diff =
        if_else(winner_id == rider_id_1, qual_time_1 - qual_time_2, qual_time_2 - qual_time_1)
    ) %>%
    filter(!is.na(winner_qual_time_diff))
  
  # add event id
  matches <- matches %>%
    left_join(events_df)

  matches <- matches %>%
    filter(!is.na(rider_1) & !is.na(rider_2)) %>%
    arrange(desc(split), date, round, heat_id)
  
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
