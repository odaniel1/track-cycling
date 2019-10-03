// Adapted from https://opisthokonta.net/?p=1589

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches

  // Each rider is referred to by an integer that acts as an index for the ratings vector. 
  int rider_id_0[M]; // ID for rider 0.
  int rider_id_1[M]; // ID for rider 1.
  int outcome[M]; // Match outcome: 0 if rider 0 wins, 1 if rider 1 wins.
  
  vector[R] alpha; // Parameters for Dirichlet prior.
}

parameters {
  // Vector of ratings for each team. For us of a Dirichlet prior, ratings are constrained to 
  // sum to 1.
  simplex[R] ratings;
}

transformed parameters {
   // The probability that rider 1 wins the given match, as defined by the Bradley-Terry model.
  vector[M] prob_1_wins;
  
  for(m in 1:M){
    prob_1_wins[m] = ratings[rider_id_1[m]] / (ratings[rider_id_0[m]] + ratings[rider_id_1[m]]);    
  }
}

model {
  ratings ~ dirichlet(alpha); // Dirichlet prior.
  
  // The Bradley-Terry model specifies that the probability that rider 1 wins is Bernoulli distributed
  // with success probabiliy determined by the ratings (prob_1_wins, defined above).
  
  for (m in 1:M){
    outcome[m] ~ bernoulli(prob_1_wins[m]);
  }
}

generated quantities {
  // We calculate the (in-sample) log loss as an indicator of model fit.
  real log_loss;
  log_loss = 0.0;
  
  for(m in 1:M){
    log_loss += outcome[m] * log(prob_1_wins[m]) +(1-outcome[m])*log(1 - prob_1_wins[m]);
  }
}
