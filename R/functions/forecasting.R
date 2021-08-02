
prepare_forecast_races <- function(forecast_lookup, race_gender = 'Women'){
  
  races <- read_csv(forecast_lookup) %>%
    filter(
      csv_cached == TRUE,
      race   ==   'Individual Sprint',
      gender == race_gender
    ) %>%
    left_join(location_lookup())

  return(races)
}

prepare_forecast_qualifying <- function(races_df, riders_df){
  qualifying <- races_df %>%
    filter(round == 'Qualifying')
  
  qual_results <- vroom::vroom(qualifying$csv_path, id = "file_path") %>% 
    bind_rows() %>%
    left_join(races_df %>% select(csv_path, year, location_country_code), by = c("file_path" = "csv_path")) %>%
    select(event, year, date, location_country_code, gender, race, round, rider, team, time) %>%
    filter(time >= 0) %>%
    arrange(date)
  
  qual_results <- qual_results %>% left_join(riders_df %>% select(rider = rider_name, rider_id))
  
  return(qual_results)
}

prepare_event_strength_draws <- function(draws_df,  days_df, qual_df){
  
  days_df <- days_df %>% mutate(date_id = 1:n())
  
  rider_draws <- qual_df %>% crossing(.draw = 1:(max(draws_df$.draw)))
  
  draws_long <- draws_df %>% gather(variable, value, -starts_with("."))
  
  draws_strength <- draws_long %>%
    filter(str_detect(variable, 'alphaD')) %>%
    mutate(date_id = str_match(variable, '\\[(.*)\\]')[,2] %>% as.numeric()) %>%
    rename(strength = value) %>% select(-variable) %>%
    left_join(days_df) %>%
    filter(date == max(date)) %>%
    select(.draw, rider_id, strength)
  
  draws_sigma <- draws_long %>%
    filter(str_detect(variable, 'sigma')) %>%
    select(.draw, sigma = value)
  
  home_riders <- qual_df %>% filter(team == location_country_code) %>% select(rider_id)
  
  draws_home_effect <- draws_long %>%
    filter(str_detect(variable, 'eta_theta')) %>%
    mutate(rider_id = str_extract(variable, '\\d+') %>% as.numeric()) %>%
    rename(home_effect = value) %>% select(-variable) %>%
    left_join(home_riders, .) %>%
    select(.draw, rider_id, home_effect)
  
  draws_kappa <- draws_long %>%
    filter(str_detect(variable, '^kappa|^psi')) %>%
    spread(variable, value) %>%
    transmute(
      .draw,
      kappa = kappa + rnorm(n(), mean = 0, sd = psi)
    )
  
  rider_draws <- rider_draws %>%
    left_join(draws_sigma) %>%
    left_join(draws_kappa) %>%
    left_join(draws_home_effect) %>%
    left_join(draws_strength) %>%
    transmute(
      .draw,
      rider_id,
      rider,
      time,
      kappa,
      strength = coalesce(home_effect,0) + coalesce(strength, rnorm(n(), 0, sigma))
    )

  return(rider_draws)
}



rename_rider_fields <- function(standings_df, rider_no){
  standings_df %>%
    select(rider_code= round_code, rider, rider_id, time, strength = strength) %>%
    rename_with(~paste0(., "_", rider_no))
}

forecast_tournament_draws <- function(draws_df, round_index, .samples = 1, round_codes = NULL){
  
  # Set a "plan" for how the code should run.
  plan(multisession, workers = 4)
  
  tournament_draws <- draws_df %>%
    filter(.draw %% 10 == 0) %>%
    group_nest(.draw) %>%
    mutate(
      tournament = 
        # future_map(data, ~forecast_tournament(., round_index, round_codes))
        future_map(data, .f = function(dat){map_df(1:.samples, ~forecast_tournament(dat,round_index, round_codes))})
    )
  
  tournament_draws <- tournament_draws %>%
    select(.draw, tournament) %>%
    unnest()
  
  return(tournament_draws)
}

forecast_tournament <- function(qualifying, round_index, round_codes = NULL){
  
  results <- qualifying
  
  if(max(results$time) == 0){
    results <- results %>% arrange(desc(strength))
  } else {
    results <- results %>% arrange(time)
  }
  
  results <- results %>% mutate(
      round_no = 0,
      round_code = paste0('N',1:n())
    )
  
  r <- 1
  while(r <= 9){
    r <- r +1
    results <- forecast_event_round(results, round_index)
  }
  
  if(!is.null(round_codes)){
    results <- results %>% filter(round_code %in% round_codes)
  }
  
  return(results)
}

forecast_event_round <- function(results_df, round_index){
  
  curr_round_no <- max(results_df$round_no) + 1
  
  kappa <- results_df$kappa[1]
  
  round_index <- round_index %>% filter(round_no == curr_round_no)
  
  if(round_index$competitors[1] == 2){
    round_matches <- round_index %>%
      inner_join(rename_rider_fields(results_df, rider_no = 1), by = "rider_code_1") %>%
      inner_join(rename_rider_fields(results_df, rider_no = 2), by = "rider_code_2") %>%
      mutate(
        time_diff = time_1 - time_2,
        strength_diff = strength_1 - strength_2,
        winner = 2 - rbernoulli(n(), p = plogis(kappa * time_diff + strength_diff)),
        new_round_code_1 = if_else(winner == 1, winner_code, loser_code),
        new_round_code_2 = if_else(winner == 2, winner_code, loser_code),        
      )
    
  }
  else{
    round_matches <- round_index %>%
      inner_join(rename_rider_fields(results_df, rider_no = 1), by = "rider_code_1") %>%
      inner_join(rename_rider_fields(results_df, rider_no = 2), by = "rider_code_2") %>%
      inner_join(rename_rider_fields(results_df, rider_no = 3), by = "rider_code_3") %>%
      mutate(
        max_time = pmax(time_1, time_2, time_3),
        strength_1 = strength_1 -kappa*(time_1 - max_time),
        strength_2 = strength_2 -kappa*(time_2 - max_time),
        strength_3 = strength_3 -kappa*(time_3 - max_time),
        beta_1 = exp(strength_1),
        beta_2 = exp(strength_2),
        beta_3 = exp(strength_3),
        F_1 = beta_1/(beta_1+beta_2+beta_3),
        F_2 = (beta_1+beta_2)/(beta_1+beta_2+beta_3),
        u = runif(n()),
        winner = case_when(
          u < F_1 ~ 1,
          u < F_2 ~ 2,
          TRUE ~ 3
        ),
        new_round_code_1 = if_else(winner == 1, winner_code, NA_character_),
        new_round_code_2 = if_else(winner == 2, winner_code, NA_character_),
        new_round_code_3 = if_else(winner == 3, winner_code, NA_character_)
      )  
  }
  
  round_results <- bind_rows(
    round_matches %>% select(
      round_no,
      rider = rider_1,
      rider_id = rider_id_1, 
      time = time_1,
      strength = strength_1,
      round_code = new_round_code_1),
    round_matches  %>% select(
      round_no,
      rider = rider_2,
      rider_id = rider_id_2, 
      time = time_2,
      strength = strength_2,
      round_code = new_round_code_2)
  )
  
  if(round_index$competitors[1] == 3){
    round_results <- round_results %>%
      bind_rows(
        round_matches %>% select(
          round_no,
          rider = rider_3,
          rider_id = rider_id_3, 
          time = time_3,
          strength = strength_3,
          round_code = new_round_code_3)
      )
  }
  
  round_results <- round_results %>% mutate(kappa = kappa)
  
  results_combined <- bind_rows(results_df, round_results)
  
  return(results_combined)
}