define_match_pairings <- function(n_seeds, n_auto_qualify){
  
  # The number who need to race the repechage.
  n_fold <- (n_seeds - n_auto_qualify)/2
  
  # Test that folding will create valid pairings for those whodo not auto qualify.
  if( n_fold %% 1 != 0)
    stop("n_seeds - n_auto_qualify must be even to define matches")
  
  if( n_seeds < 0 | n_auto_qualify < 0 | n_seeds <n_auto_qualify )
    stop("n_seeds and n_auto_qualify must be positive; n_auto_qualify must be smaller than n_seeds")
  
  repechage_pairings <- tibble(
      seed_1 = 1:(n_fold),
      seed_2 = rev( (n_fold + 1):(2*n_fold))
    ) %>%
    
    mutate_all(~(. + n_auto_qualify))
  
  if(n_auto_qualify > 0){
    
    repechage_pairings <- repechage_pairings %>%
      bind_rows(
        tibble(seed_1 = 1:n_auto_qualify, seed_2 = NA), .
      )     
  }
    
  repechage_pairings  <- repechage_pairings %>% 
    mutate(
      winner_seed = 1:n()
    )
  
  return(repechage_pairings)
}
