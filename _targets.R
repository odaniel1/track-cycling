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
  "bt5", "stan/bt5.stan", "Time Dependent Strengths",
  "bt6", "stan/bt6.stan", "Qualifying Times",
  "bt_final", "stan/bt_final.stan", "Fit to all data",
  "gp_prior", "stan/gp_prior.stan", "GP Prior only"
)

## ---- DATA PREPARATION ----------------------------------------------------
list(
  
  tar_target(model_list, models),
  
  tar_target(tissot_race_path, "../tissot-scraper/data/DONT-EDIT-race-lookup.csv", format = 'file'),
  
  tar_target(manual_race_path, "data/MANUAL-race-lookup.csv", format = 'file'),
  
  tar_target(team_path, "../tissot-scraper/data/team-lookup.csv", format = 'file'),

  tar_target(races, prepare_races(tissot_race_path, manual_race_path)),
  
  tar_target(qualifying, prepare_qualifying(races)),
  
  tar_target(results, prepare_results(races, qualifying)),
  
  tar_target(events, prepare_events(races)),

  tar_target(teams, prepare_teams(team_path, results)),
  
  tar_target(riders, prepare_riders(results)),
  
  tar_target(rider_days, prepare_rider_days(results, riders)),

  tar_target(matches, prepare_matches(results, riders, rider_days, teams, qualifying, events)),
  
  tar_target(stan_data_without_qual, prepare_stan_data(riders, matches %>% filter(round != "Qualifying"), pairings, rider_days)),
  
  tar_target(stan_data_with_qual, prepare_stan_data(riders, matches, pairings, rider_days)),
  
## ---- STAN MODELS --------------------------------------------------------- 
tar_stan_mcmc(
    name = bt,
    stan_files = models$path,
    data = stan_data_without_qual,
    iter_warmup = 1999, iter_sampling = 2000,
    parallel_chains = 4,
    seed = 1414214,
    refresh = 500
  ),

tar_stan_mcmc(
  name = bt_qual,
  stan_files = models$path[models$model_name %in% c("bt6", "bt_final")],
  data = stan_data_with_qual,
  iter_warmup = 1999, iter_sampling = 2000,
  parallel_chains = 4,
  seed = 1414214,
  refresh = 500
),

## ---- FORECASTING ---------------------------------------------------------

tar_target(fcst_lookup, 'data/MANUAL-forecast-lookup.csv'),

tar_target(fcst_rounds, read_csv('data/olympic-rounds.csv')),

tar_target(fcst_races, prepare_forecast_races(fcst_lookup)),

tar_target(fcst_qualifying, prepare_forecast_qualifying(fcst_races, riders)),

tar_target(fcst_strength_draws,
           prepare_event_strength_draws(bt_qual_draws_bt_final,rider_days, fcst_qualifying)),

tar_target(fcst_tournament_draws,
           forecast_tournament_draws(fcst_strength_draws, fcst_rounds, podium_only = TRUE))

)
