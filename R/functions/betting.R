
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
    print(stake)
    print(sum(stake))
    print(log_wealth)
  }
  
  return(log_wealth)  
}

expected_log_wealth_mat <- function(stake, fractional_odds, prob_mat){
  
  log_wealth_vec <- log_wealth(stake, fractional_odds)
  
  expected_log_wealth_vec <- prob_mat %*% log_wealth_vec
  
  return(expected_log_wealth_vec)
}

kelly_evaluation <- function(stake, fractional_odds, prob_mat){
  -sum(expected_log_wealth_mat(stake, fractional_odds, prob_mat))
} 

kelly_constraints <- function(stake,fractional_odds, prob_mat){sum(stake) -1}

optimise_kelly_stakes <- function(fractional_odds, prob_mat){
  
  stake0 <- rep(0.5 * 1/ncol(prob_mat),ncol(prob_mat))

  lb <- rep(0, ncol(prob_mat))
  ub <- rep(0.99, ncol(prob_mat))
  
  opt <- nloptr(
    x0 = stake0,
    eval_f = kelly_evaluation,
    lb = lb, ub = ub,
    eval_g_ineq = kelly_constraints,
    opts = list("algorithm"="NLOPT_LN_COBYLA","xtol_rel"=1.0e-4, "xtol_rel"=1.0e-3,"maxeval"=1e5),
    fractional_odds=fractional_odds,
    prob_mat = prob_mat
  )
  
  return(opt)
}

optimise_kelly_bayes <- function(odds_df, probs_df){
  
  odds_df <- (probs_df %>% filter(.draw == 1) %>% select(rider)) %>%
    left_join(odds_df,by= "rider") %>%
    arrange(rider) %>%
    replace_na(list(fractional_odds = 0.0001))
  
  probs_df <- probs_df %>% arrange(.draw,rider)
  probs_mat <- matrix(probs_df$gold_prob, ncol = nrow(odds_df),byrow = TRUE)
  
  kelly_opt <- optimise_kelly_stakes(odds_df$fractional_odds, probs_mat) 
  
  print(sprintf('NLopt solver status: %s (%s)', kelly_opt$status, kelly_opt$message))
  
  odds_df$kelly_stakes <- kelly_opt$solution
  
  return(odds_df)
}

posterior_kelly_stakes <- function(kelly_df, probs_df){
  
  probs_df %>%
    left_join(kelly_df) %>%
    mutate(expected_return = gold_prob * kelly_stakes * (1 + fractional_odds))
}


summarise_bet <- function(odds_stakes, kelly_post, wealth, cap){
  
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
}