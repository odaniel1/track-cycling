source("./1_Data-Processing/Functions/prepare_match_sprint_data.R")

fit_match_sprint_model <- function(race_lookup){
  
  # Create individual data frame.
  sprints_df <- prepare_match_sprint_data(race_lookup)
  
  # Generate a rider data frame, with a row for each
  # unique rider.
  rider_df <- tibble(rider_name = c(sprints_df$rider_0, sprints_df$rider_1)) %>%
  unique() %>%
  mutate(
      rider_id = 1:n()
  )
  
  # Add rider id to sprints data.
  sprints_df <- sprints_df %>%
    left_join(rider_df, by = c("rider_0" = "rider_name")) %>% rename(rider_id_0 = rider_id) %>%
    left_join(rider_df, by = c("rider_1" = "rider_name")) %>% rename(rider_id_1 = rider_id)
  
  # Compile stan model for match sprints.
  require(rstan)
  compiled_model <- stan_model("./2_Modelling/bradley-terry-dirichlet.stan")
  
  # Define model parameters.
  model_data <- list(
    R = nrow(rider_df),
    M = nrow(sprints_df),
    rider_id_0 = sprints_df$rider_id_0,
    rider_id_1 = sprints_df$rider_id_1,
    outcome = sprints_df$winner,
    alpha = rep(1/2, nrow(rider_df))    
  )
  
  # Fit stan model.
  fitted_model <- sampling(
    object = compiled_model,
    data = model_data,
    iter = 2000,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.95, max_treedepth = 10)
  )
  
  # Extract posterior samples.
  posterior_samples <- extract(fitted_model)$ratings
  colnames(posterior_samples) <- 1:nrow(rider_df)
  
  
  posterior_samples <- posterior_samples %>%
    as_tibble %>%
    mutate(sample_id = 1:n()) %>%
    
    # Convert to long format.
    gather("rider_id", "ranking", -sample_id) %>%
    
    # Add rider name from rider_df
    mutate(rider_id = as.numeric(rider_id)) %>%
    left_join(rider_df) %>%
    
    select(sample_id, rider_name, ranking)

  
  return(posterior_samples)
}
