library(rstan)
library(tidyverse)
rstan_options(auto_write = TRUE)

compile_model <- function(model_path){
  
  stan_path <- list.files(paste0(model_path,"/Scripts"), pattern = ".stan", full.names = TRUE)
  
  if(length(stan_path) != 1){
    stop("Error compiling stan model: either no or multiple stan models defined in Scripts directory.")
  }
  
  compiled_model <- stan_model(stan_path)
  return(compiled_model)
}

fit_model <- function(race_lookup, compiled_model, model_path){
  source(paste0(model_path, "/Scripts/model_parameters.R"))
  model_parameters <- model_parameters(race_lookup)

  fitted_model <- sampling(
    object = compiled_model,
    data = model_parameters$stan_parameters,
    iter = 3000,
    cores = 6,
    chains = 6,
    control = list(adapt_delta = 0.997)
  )

  return(model = list(stan_fit = fitted_model, parameters = model_parameters))
}
