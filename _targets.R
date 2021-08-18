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

genders <- tibble(gender = c("Men", "Women"))

fcst_inputs <- tribble(
  ~model_gender, ~round_no, ~round_path, ~odds_date, ~wealth, ~cap, ~output_path,
  "Men", 8, "data/csv/202021_2020-TOKYO-OLYMPICS_Individual-Sprint-Men_Quarterfinals.csv", '2021-08-05', 20 - 3.33 - 3.4, 6 ,"data/bets/2020-08-05-Men-PostQuarterfinals.csv",
  "Women", 8, "data/csv/202021_2020-TOKYO-OLYMPICS_Individual-Sprint-Women_Quarterfinals.csv", '2021-08-07', 11, 11,"data/bets/2020-08-07-Women-PostQuarterfinals.csv"
)

## ---- DATA PREPARATION ----------------------------------------------------
tar_map(values = genders,
  
  tar_target(model_list, models),
  
  tar_target(tissot_race_path, "../tissot-scraper/data/DONT-EDIT-race-lookup.csv", format = 'file'),
  
  tar_target(manual_race_path, "data/MANUAL-race-lookup.csv", format = 'file'),
  
  tar_target(team_path, "../tissot-scraper/data/team-lookup.csv", format = 'file'),

  tar_target(races, prepare_races(tissot_race_path, manual_race_path, gender)),
  
  tar_target(qualifying, prepare_qualifying(races)),

  tar_target(results, prepare_results(races, qualifying)),

  tar_target(events, prepare_events(races)),

  tar_target(teams, prepare_teams(team_path, results)),

  tar_target(riders, prepare_riders(results)),

  tar_target(rider_days, prepare_rider_days(results, riders)),

  tar_target(matches, prepare_matches(results, riders, rider_days, teams, qualifying, events)),

  tar_target(stan_data_without_qual, prepare_stan_data(riders, matches %>% filter(round != "Qualifying"), pairings, rider_days, training = TRUE)),

  tar_target(stan_data_with_qual, prepare_stan_data(riders, matches, pairings, rider_days, training = TRUE)),
  
## ---- STAN MODELS --------------------------------------------------------- 
# tar_stan_mcmc(
#     name = bt,
#     stan_files = models$path,
#     data = stan_data_without_qual,
#     iter_warmup = 1999, iter_sampling = 2000,
#     parallel_chains = 4,
#     seed = 1414214,
#     refresh = 500
#   ),

tar_target(stan_data_with_qual_full, prepare_stan_data(riders, matches, pairings, rider_days, training = FALSE)),

tar_stan_mcmc(
  name = bt_qual,
  stan_files = models$path[models$model_name %in% c("bt6")],
  data = stan_data_with_qual,
  variables = c("sigma","eta_theta","kappa", "psi", "alphaD"),
  iter_warmup = 2000, iter_sampling = 2000,
  thin = 8,
  parallel_chains = 4,
  seed = 1414214,
  refresh = 500
),

## ---- FORECASTING ---------------------------------------------------------

tar_target(fcst_lookup, 'data/MANUAL-forecast-lookup.csv'),

tar_target(fcst_rounds, read_csv('data/olympic-rounds.csv')),

tar_target(fcst_races, prepare_forecast_races(fcst_lookup,gender)),

tar_target(fcst_qualifying, prepare_forecast_qualifying(fcst_races, riders)),

tar_target(fcst_strength_draws,
           prepare_event_strength_draws(bt_qual_draws_bt6,rider_days, fcst_qualifying)),

tar_target(fcst_tournament_draws, forecast_tournament(fcst_strength_draws, fcst_rounds, samples = 200,
                                                      accumulate = FALSE, gold_only = TRUE,
                                                      init_round = fcst_inputs$round_no[fcst_inputs$model_gender == gender],
                                                      init_path = fcst_inputs$round_path[fcst_inputs$model_gender == gender])),

tar_target(fcst_gold_probs, forecast_gold_probs(fcst_tournament_draws, fcst_strength_draws)),

## ---- BETTING --------------------------------------------------------------

tar_target(odds, prepare_odds('data/202021_2020-TOKYO-OLYMPICS_Odds.csv', fcst_inputs$odds_date[fcst_inputs$model_gender == gender], gender)),

tar_target(kelly_strategy, optimise_kelly_bayes(odds, fcst_gold_probs)),

tar_target(kelly_posterior, posterior_kelly_stakes(kelly_strategy, fcst_gold_probs)),

tar_target(bet_summary,
           summarise_bet(
             kelly_strategy, kelly_posterior,
             wealth = fcst_inputs$wealth[fcst_inputs$model_gender == gender],
             cap =fcst_inputs$cap[fcst_inputs$model_gender == gender],
             output_path = fcst_inputs$output_path[fcst_inputs$model_gender == gender]
            ),
           format = "file"
  )
)