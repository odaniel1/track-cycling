
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

forecast_tournament <- function(qualifying_strengths, round_index, samples = 1, gold_only = TRUE){
  
  sample_rounds <- round_index %>%
    crossing(.sample = 1:samples, .draw = unique(qualifying_strengths$.draw)) %>%
    mutate(unif_sample = runif(n())) %>%
    group_nest(round_no, keep = TRUE)
  
  init <- qualifying_strengths %>% group_by(.draw)
  
  if(max(init$time) == 0){
    init <- init %>% arrange(desc(strength))
  } else {
    init <- init %>% arrange(time)
  }
  
  init <- init %>%
    mutate(round_code = paste0('N',1:n())) %>%
    crossing(.sample = 1:samples) %>%
    ungroup()

    
  forecast <- reduce(sample_rounds$data, forecast_tournament_round, .init = init)
  
  if(gold_only == TRUE){ forecast <- forecast %>% filter(round_code == 'Gold')}
  
  return(forecast)
}

forecast_tournament_round <- function(standings, round_matches){
  
  round_matches <- round_matches %>%
    left_join(standings %>% select(.draw, .sample, kappa, rider_code_1 = round_code, rider_1 = rider, time_1 = time, strength_1 = strength),
              by = c("rider_code_1", ".sample", ".draw")) %>%
    left_join(standings %>% select(.draw, .sample, rider_code_2 = round_code, rider_2 = rider, time_2 = time, strength_2 = strength),
              by = c("rider_code_2", ".sample", ".draw"))
  
  if(round_matches$competitors[1] == 2){
    round_matches <- round_matches %>%
      mutate(
        prob_1 = plogis(kappa * (time_1 - time_2) + (strength_1 - strength_2)),
        winner = case_when(
          sprints == 1 ~ 1 + (unif_sample > prob_1),
          TRUE         ~ 1 + (unif_sample > (3*prob_1^2 - prob_1^3))
        ),
        next_round_code_1 = if_else(winner == 1, winner_code, loser_code),
        next_round_code_2 = if_else(winner == 2, winner_code, loser_code)
      )
  } else {
    round_matches <- round_matches %>%
      left_join(standings %>% select(.draw, .sample, rider_code_3 = round_code, rider_3 = rider, time_3 = time, strength_3 = strength),
                by = c("rider_code_3", ".sample", ".draw")) %>%
      mutate(
        max_time = pmax(time_1, time_2, time_3),
        beta_1 = exp(strength_1 - kappa*(time_1 - max_time)),
        beta_2 = exp(strength_2 - kappa*(time_2 - max_time)),
        beta_3 = exp(strength_3 - kappa*(time_3 - max_time)),
        beta_sum = beta_1 + beta_2 + beta_3,
        winner = case_when(
          unif_sample < beta_1/beta_sum ~ 1,
          unif_sample < (beta_1 + beta_2)/beta_sum ~2,
          TRUE ~ 3
        ),
        next_round_code_1 = if_else(winner == 1, winner_code, loser_code),
        next_round_code_2 = if_else(winner == 2, winner_code, loser_code),
        next_round_code_3 = if_else(winner == 3, winner_code, loser_code)
      )
  }
  
  round_results <- bind_rows(
    round_matches %>% select(
      .sample,
      .draw,
      round_no,
      kappa,
      rider = rider_1,
      time = time_1,
      strength = strength_1,
      round_code = next_round_code_1),
    round_matches  %>% select(
      .sample,
      .draw,
      round_no,
      kappa,
      rider = rider_2,
      time = time_2,
      strength = strength_2,
      round_code = next_round_code_2)
  )
  
  if(round_matches$competitors[1] == 3){
    round_results <- round_results %>%
      bind_rows(
        round_matches  %>% select(
          .sample,
          .draw,
          round_no,
          kappa,
          rider = rider_3,
          time = time_3,
          strength = strength_3,
          round_code = next_round_code_3)
      )
  }
  
  round_results <- round_results %>% filter(!is.na(round_code))
  
  if(str_detect(round_matches$round[1], 'Repechage')){
    
    round_results <- bind_rows(round_results, standings)
    
  }
  
  return(round_results)
}
