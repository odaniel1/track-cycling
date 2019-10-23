data {
  int<lower=0> R; // Riders
  int<lower=0> S; // Sprints
  int<lower=0> E; // Events

  // Each rider is referred to by an integer that acts as an index for the ratings vector. 
  int rider_id[S]; // ID for rider.
  int event_id[S]; // ID for event.
  real time[S]; // Rider sprint time.
  vector[3] event_hyperprior;
  real<lower=0> rider_hyperprior;
  real<lower=0> sd_prior;
}

parameters {
  real hyp_mu_event;
  real<lower = 0> hyp_sigma_event;
  vector[E] mu_event;

  real<lower=0> hyp_sigma_rider;
  vector[R] mu_rider;
  real<lower=0> sigma_rider;

  real<lower=0> tau;
}

transformed parameters{
  // Mean for normal distribution centred at the round/team parameter.
  vector[S] nu;

  for(s in 1:S){
    nu[s] = mu_rider[rider_id[s]] + mu_event[event_id[s]];
  }
}

model {
  hyp_mu_event ~ normal(event_hyperprior[1], event_hyperprior[2]);
  hyp_sigma_event ~ normal(0, event_hyperprior[3]);
  mu_event ~ normal(hyp_mu_event, hyp_sigma_event);
  
  hyp_sigma_rider ~ normal(0,rider_hyperprior);
  mu_rider ~ normal(0, hyp_sigma_rider);
  
  tau ~ normal(0, sd_prior);
  
  time ~ normal(nu, tau);
}
