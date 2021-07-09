# the code is maintained via the targets package (replacement to drake); for
# detailed documentation see: https://books.ropensci.org/targets/


# Load R files that contain all required packages/functions 
source("R/packages.R")
purrr::map(dir_ls("R/functions", recurse = TRUE, glob = "*.R"), source)


list(
  
  ## ---- LOCAL PATHS ---------------------------------------------------------
  
  tar_target(race_path, "../tissot-scraper/data/DONT-EDIT-race-lookup.csv", format = 'file'),

  
  ## ---- DATA PREPARATION ----------------------------------------------------
  
  tar_target(races, prepare_races(race_path)),
  
  tar_target(results, prepare_results(races)),

  tar_target(riders, prepare_riders(results)),

  tar_target(matches, prepare_matches(results, riders)),
  
  tar_target(pairings, prepare_pairings(matches)),
  
  
  ## ---- STAN MODELS ---------------------------------------------------------
  
  tar_target(stan_data, prepare_stan_data(riders, matches, pairings)),
  
  tar_stan_mcmc(
    name = bt1,
    stan_files = "stan/bt1.stan",
    data = stan_data,
    variables = c("sigma", "alpha", "avg_log_loss"),
    iter_warmup = 1000, iter_sampling = 1000,
    parallel_chains = 4
  ),
  
  tar_stan_mcmc(
    name = bt1.1,
    stan_files = "stan/bt1.1.stan",
    data = stan_data,
    variables = c("sigma", "alpha", "avg_log_loss", "avg_match_log_loss"),
    iter_warmup = 1000, iter_sampling = 1000,
    parallel_chains = 4
  ),
  
  tar_stan_mcmc(
    name = bt2,
    stan_files = "stan/bt2.stan",
    data = stan_data,
    variables = c("sigma", "alpha", "avg_log_loss", "avg_match_log_loss"),
    iter_warmup = 1000, iter_sampling = 1000,
    parallel_chains = 4
  )
)


