# the code is maintained via the targets package (replacement to drake); for
# detailed documentation see: https://books.ropensci.org/targets/


# Load R files that contain all required packages/functions 
source("R/packages.R")
purrr::map(dir_ls("R/functions", recurse = TRUE, glob = "*.R"), source)

models <- tribble(
  ~model_name, ~path, ~description,
  "bt1", "stan/bt1.stan", "Basic Bradley-Terry",
  "bt2", "stan/bt2.stan", "Informative (Gamma) Prior",
  "bt3", "stan/bt3.stan", "Match Likelihood",
  "bt1.1", "stan/bt1.1.stan", "Basic Bradley-Terry",
  "bt2.1", "stan/bt2.1.stan", "Informative (Gamma) Prior",
  "bt4", "stan/bt4.stan", "Home advantage",
  "bt5", "stan/bt5.stan", "Time Dependent Strengths"
  # "bt6", "stan/bt6.stan", "bt5, with qualifying time diff predictor",
  # "bt6.1", "stan/bt6.1.stan", "bt5, with qualifying time diff predictor",
  # "bt6.2", "stan/bt6.2.stan", "bt5, with qualifying time diff predictor"
)

## ---- DATA PREPARATION ----------------------------------------------------
list(
  
  tar_target(model_list, models),
  
  tar_target(race_path, "../tissot-scraper/data/DONT-EDIT-race-lookup.csv", format = 'file'),
  
  tar_target(team_path, "../tissot-scraper/data/team-lookup.csv", format = 'file'),

  tar_target(races, prepare_races(race_path)),
  
  tar_target(results, prepare_results(races)),
  
  tar_target(qualifying, prepare_qualifying(races)),

  tar_target(teams, prepare_teams(team_path, results)),
  
  tar_target(riders, prepare_riders(results)),
  
  tar_target(rider_days, prepare_rider_days(results, riders)),

  tar_target(matches, prepare_matches(results, riders, rider_days, teams, qualifying)),
  
  tar_target(pairings, prepare_pairings(matches)),
  
  tar_target(stan_data, prepare_stan_data(riders, matches, pairings, rider_days)),
  
## ---- STAN MODELS --------------------------------------------------------- 
tar_stan_mcmc(
    name = bt,
    stan_files = models$path,
    data = stan_data,
    iter_warmup = 1999, iter_sampling = 2000,
    parallel_chains = 4,
    seed = 1414214,
    refresh = 500
  )
)
