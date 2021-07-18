# the code is maintained via the targets package (replacement to drake); for
# detailed documentation see: https://books.ropensci.org/targets/


# Load R files that contain all required packages/functions 
source("R/packages.R")
purrr::map(dir_ls("R/functions", recurse = TRUE, glob = "*.R"), source)

models <- tribble(
  ~model_name, ~path, ~description,
  "bt1", "stan/bt1.stan", "Basic Bradley Terry model.",
  "bt1.1", "stan/bt1.1.stan", "bt1, with match log loss",
  "bt2", "stan/bt2.stan", "BT with match log density",
  "bt3", "stan/bt3.stan", "bt2, with home advantage effect",
  # "bt3.2", "stan/bt3.2.stan", "bt2, with Gaussian Process for time changing strengths"
)

## ---- DATA PREPARATION ----------------------------------------------------
list(
  
  tar_target(race_path, "../tissot-scraper/data/DONT-EDIT-race-lookup.csv", format = 'file'),
  
  tar_target(team_path, "../tissot-scraper/data/team-lookup.csv", format = 'file'),

  tar_target(races, prepare_races(race_path)),
  
  tar_target(results, prepare_results(races)),

  tar_target(teams, prepare_teams(team_path, results)),
  
  tar_target(riders, prepare_riders(results)),
  
  tar_target(rider_days, prepare_rider_days(results, riders)),

  tar_target(matches, prepare_matches(results, riders, rider_days, teams)),
  
  tar_target(pairings, prepare_pairings(matches)),
  
  tar_target(stan_data, prepare_stan_data(riders, matches, pairings, rider_days)),
  
## ---- STAN MODELS --------------------------------------------------------- 
tar_stan_mcmc(
    name = bt,
    stan_files = models$path,
    data = stan_data,
    iter_warmup = 999, iter_sampling = 1000,
    parallel_chains = 4,
    seed = 1414214,
    refresh = 500
  )
)

