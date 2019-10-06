data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches

  // Each rider is referred to by an integer that acts as an index for the ratings vector. 
  int rider_id_0[M]; // ID for rider 0.
  int rider_id_1[M]; // ID for rider 1.
  int outcome[M]; // Match outcome: 0 if rider 0 wins, 1 if rider 1 wins.
  
  real alpha[R]; // Parameter for normal prior.
}

parameters {
  // Vector of ratings for each team.
  vector[R] log_rating;
}

model {
  log_rating ~ normal(0, alpha); // Dirichlet prior.
  
  // The Bradley-Terry model
  outcome ~ bernoulli_logit(log_rating[rider_id_1] - log_rating[rider_id_0]);
}

generated quantities {
  vector[R] rating;
  rating = exp(log_rating);
}
