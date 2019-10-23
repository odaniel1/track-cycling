data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0> T; // Teams

  // Each rider is referred to by an integer that acts as an index for the ratings vector. 
  int rider_id_0[M]; // ID for rider 0.
  int rider_id_1[M]; // ID for rider 1.
  int rider_team[R]; // Team of a given rider.
  int rider_round[R]; // Max round rider has reached historically.
  int outcome[M]; // Match outcome: 0 if rider 0 wins, 1 if rider 1 wins.
  
  real sd_prior[3]; // Parameter for normal prior.
}

parameters {
  // Vector of ratings for each team.
  real<lower=0> sigma_team;
  real<lower=0> sigma_round;
  real<lower=0> tau;
  vector[T] mu_team;
  ordered[5] mu_round;
  vector[R] log_rating;
}

transformed parameters{
  // Mean for ormal distribution centred at the round/team parameter.
  vector[R] nu;
  for(r in 1:R){
    nu[r] = mu_team[rider_team[r]] + mu_round[rider_round[r]];
  }
}

model {
  sigma_team ~ normal(0, sd_prior[1]);
  mu_team ~ normal(0, sigma_team);
  
  sigma_round ~ normal(0, sd_prior[2]);
  mu_round ~ normal(0, sigma_round);

  tau ~ normal(0, sd_prior[3]);
  
  log_rating ~ normal(nu, tau);
  // for(r in 1:R){
  // log_rating[r] ~ normal(mu_team[rider_team[r]], tau);   
  // }

  // The Bradley-Terry model
  outcome ~ bernoulli_logit(log_rating[rider_id_1] - log_rating[rider_id_0]);
}

generated quantities {
  vector[R] rating;
  rating = exp(log_rating);
}
