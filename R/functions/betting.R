


expected_log_wealth <- function(stake, probs, fractional_odds){
  
  probs <- probs[stake != 0]
  fractional_odds <- fractional_odds[stake !=0]
  stake <- stake[stake != 0]
  
  inv_stake <- 1 - sum(stake)
  
  odds_return <- 1 + fractional_odds
  
  log_return <- log(inv_stake + stake * odds_return)
  
  expected_log_wealth <- sum(probs * log_return)
  
  return(expected_log_wealth)
}

expected_negative_log_wealth <- function(stake, probs, fractional_odds){
  -expected_log_wealth(stake,probs,fractional_odds)
}
  
  
kelly_constraints <- function(stake,probs,fractional_odds){sum(stake) -1}

optimise_kelly_stakes <- function(probs, fractional_odds){
  
  stake0 <- rep(0.5 * 1/length(probs), length(probs))
  lb <- rep(0, length(probs))
  ub <- rep(1, length(probs))
  
  opt <- nloptr(
    x0 = stake0,
    eval_f = expected_negative_log_wealth,
    lb = lb, ub = ub,
    eval_g_ineq = kelly_constraints,
    opts = list("algorithm"="NLOPT_LN_COBYLA","xtol_rel"=1.0e-8),
    probs=probs,
    fractional_odds=fractional_odds
  )
  
  return(opt$solution)
}


prepare_odds_and_stakes <- function(odds_path, fcst_tournament, race_gender){
  
  odds <- read_csv(odds_path) %>% 
    filter(
      odds_source == "Unibet",
      gender == race_gender
    ) %>%
    filter(odds_date == max(odds_date)) %>%
    mutate(
      bookies_prob_dutch = 1/(1 + fractional_odds),
      bookies_prob = bookies_prob_dutch/sum(bookies_prob_dutch),
    )
  
  fcst_probs <- fcst_tournament %>%
    filter(round_code == 'Gold') %>%
    count(rider) %>%
    transmute(
      rider,
      forecast_prob = coalesce(n/sum(n),0)
    )
  
  odds_probs <- left_join(odds, fcst_probs) %>%
    mutate(
      forecast_prob =if_else(is.na(forecast_prob),0, forecast_prob),
      expected_unit_return = (1 + fractional_odds) * forecast_prob
    )
  
  kelly_stakes <- optimise_kelly_stakes(odds_probs$forecast_prob, odds_probs$fractional_odds)
  
  odds_probs$kelly_stake <- round(kelly_stakes,2)
  
  odds_and_returns <- odds_probs %>%
    mutate(expected_return = kelly_stake * expected_unit_return)
  
  total_bet <- sum(odds_and_returns$kelly_stake)
  expected_growth <- (1-total_bet) + sum(odds_and_returns$expected_return)
  probability_of_growth <- sum(odds_and_returns$forecast_prob[odds_and_returns$expected_return > total_bet])
  
  cat(sprintf('%s\n\tProportion of Wealth Bet: %f\n\tExpected Wealth Growth: %f\n\tProbability of Growth: %f\n', race_gender, total_bet, expected_growth,probability_of_growth))
  
  return(odds_and_returns)
}