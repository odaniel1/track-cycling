
prepare_odds <- function(odds_path, stake_date, race_gender){
  
  odds <- read_csv(odds_path) %>% 
    filter(
      odds_source == "Unibet",
      odds_date == stake_date,
      gender == race_gender
    ) %>%
    mutate(
      bookies_prob_dutch = 1/(1 + fractional_odds),
      bookies_prob = bookies_prob_dutch/sum(bookies_prob_dutch),
    )
  
  return(odds)
}


log_wealth <- function(stake, fractional_odds){

  inv_stake <- 1 - sum(stake)
  
  odds_return <- 1 + fractional_odds
  
  log_wealth <- log(inv_stake + stake * (1+fractional_odds))
  
  if(is.nan(sum(log_wealth))){
    print("Sum of log wealth is NaN")
  }
  
  return(log_wealth)  
}

expected_log_wealth_mat <- function(stake, fractional_odds, prob_mat){
  
  log_wealth_vec <- log_wealth(stake, fractional_odds)
  
  expected_log_wealth_vec <- prob_mat %*% log_wealth_vec
  
  return(expected_log_wealth_vec)
}


variance_log_wealth_mat <- function(stake, fractional_odds, prob_mat){
  
  log_wealth_vec <- log_wealth(stake, fractional_odds)
  log_wealth2_vec <- log_wealth(stake, fractional_odds)^2
  
  expected_log_wealth_vec <- prob_mat %*% log_wealth_vec
  expected_log_wealth2_vec <- prob_mat %*% log_wealth2_vec
  
  variance_vec <- expected_log_wealth2_vec - expected_log_wealth_vec^2
  
  return(variance_vec)
}

kelly_evaluation <- function(stake, fractional_odds, prob_mat){
  -sum(expected_log_wealth_mat(stake, fractional_odds, prob_mat))
} 

kelly_var_evaluation <- function(stake, fractional_odds, prob_mat, lambda){
  -sum(expected_log_wealth_mat(stake, fractional_odds, prob_mat) - lambda * variance_log_wealth_mat(stake, fractional_odds, prob_mat))
} 

sum_less_1_constraints <- function(stake,fractional_odds, prob_mat){sum(stake) -1}

optimise_kelly_stakes <- function(fractional_odds, prob_mat, strategy_func = kelly_evaluation){
  
  stake0 <- rep(0.5 * 1/ncol(prob_mat),ncol(prob_mat))

  lb <- rep(0, ncol(prob_mat))
  ub <- rep(0.99, ncol(prob_mat))
  
  opt <- nloptr(
    x0 = stake0,
    eval_f = strategy_func,
    lb = lb, ub = ub,
    eval_g_ineq = sum_less_1_constraints,
    opts = list("algorithm"="NLOPT_LN_COBYLA","xtol_rel"=1.0e-4, "xtol_rel"=1.0e-3,"maxeval"=1e5),
    fractional_odds=fractional_odds,
    prob_mat = prob_mat
  )
  
  return(opt)
}

optimise_kelly_bayes <- function(odds_df, probs_df, strategy_func = kelly_evaluation){
  
  odds_df <- (probs_df %>% filter(.draw == 1) %>% select(rider)) %>%
    left_join(odds_df,by= "rider") %>%
    arrange(rider) %>%
    replace_na(list(fractional_odds = 0.0001))
  
  probs_df <- probs_df %>% arrange(.draw,rider)
  
  probs_mat <- matrix(probs_df$gold_prob, ncol = nrow(odds_df),byrow = TRUE)
  
  kelly_opt <- optimise_kelly_stakes(odds_df$fractional_odds, probs_mat, strategy_func) 
  
  print(sprintf('NLopt solver status: %s (%s)', kelly_opt$status, kelly_opt$message))
  
  odds_df$kelly_stakes <- kelly_opt$solution
  
  return(odds_df)
}

posterior_kelly_stakes <- function(kelly_df, probs_df){
  
  probs_df %>%
    left_join(kelly_df) %>%
    mutate(expected_return = gold_prob * kelly_stakes * (1 + fractional_odds))
}


summarise_bet <- function(odds_stakes, kelly_post, wealth, cap, output_path){
  
  odds_stakes <- odds_stakes %>% 
    mutate(
      kelly_stakes = wealth * kelly_stakes,
      sum_kelly_stakes = sum(kelly_stakes) ,
      capped_kelly_stakes = if_else(sum_kelly_stakes > cap, kelly_stakes * (cap/sum(kelly_stakes)), kelly_stakes),
      capped_kelly_stakes = round(capped_kelly_stakes,1)
    ) 
  
  probs_mean <- kelly_post %>%
    group_by(rider) %>%
    summarise(gold_prob = mean(gold_prob))
  
  odds_stakes <- odds_stakes %>%
    left_join(probs_mean) %>%
    mutate(
      expected_return = gold_prob * capped_kelly_stakes * (1 + fractional_odds)
    ) %>%
    arrange(desc(gold_prob)) %>%
    ungroup() %>%
    mutate(wealth = wealth, cap = cap)
  
  odds_stakes <-odds_stakes %>%
    filter(!is.na(bookies_prob))
  
  write_csv(odds_stakes, output_path)
  return(output_path)
}