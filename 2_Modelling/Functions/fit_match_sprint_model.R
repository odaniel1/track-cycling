source("./1_Data-Processing/Functions/prepare_match_sprint_data.R")

library(rstan)

rstan_options(auto_write = TRUE)

compile_match_sprint_model <- function(model_path){

  compiled_model <- stan_model(model_path)
  return(compiled_model)
}
 
fit_match_sprint_model <- function(match_pairs, compiled_model, prior_alpha){

  # Generate a rider data frame, with a row for each
  # unique rider.
  rider_df <- tibble(rider_name = c(match_pairs$rider_0, match_pairs$rider_1)) %>%
  unique() %>%
  mutate(
      rider_id = 1:n()
  )
  
  # Add rider id to sprints data.
  match_pairs <- match_pairs %>%
    left_join(rider_df, by = c("rider_0" = "rider_name")) %>% rename(rider_id_0 = rider_id) %>%
    left_join(rider_df, by = c("rider_1" = "rider_name")) %>% rename(rider_id_1 = rider_id)

  # Define model parameters.
  model_data <- list(
    R = nrow(rider_df),
    M = nrow(match_pairs),
    rider_id_0 = match_pairs$rider_id_0,
    rider_id_1 = match_pairs$rider_id_1,
    outcome = match_pairs$winner,
    alpha = rep(prior_alpha, nrow(rider_df))    
  )
  
  # Fit stan model.
  fitted_model <- sampling(
    object = compiled_model,
    data = model_data,
    iter = 2000,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 10)
  )
  
  model_pair <- list(stan_fit = fitted_model, rider_lookup = rider_df)
  
  return(model_pair)
}

# Extract posterior samples.
extract_ratings <- function(model_pair){

  posterior_samples <- extract(model_pair$stan_fit)$rating
  colnames(posterior_samples) <- 1:nrow(model_pair$rider_lookup)
  
  posterior_samples <- posterior_samples %>%
    as_tibble %>%
    mutate(sample_id = 1:n()) %>%
    
    # Convert to long format.
    gather("rider_id", "rating", -sample_id) %>%
    
    # Add rider name from rider_df
    mutate(rider_id = as.numeric(rider_id)) %>%
    left_join(model_pair$rider_lookup) %>%
    
    select(sample_id, rider_name, rating)
}
