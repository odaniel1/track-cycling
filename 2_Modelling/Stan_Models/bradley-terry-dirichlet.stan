// Adapted from https://opisthokonta.net/?p=1656

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches

  // Each team is referred to by an integer that acts as an index for the ratings vector. 
  int rider_id_0[M]; // ID for rider 0.
  int rider_id_1[M]; // ID for rider 1.
  int outcome[M]; // Match outcome: 0 if rider 0 wins, 1 if rider 1 wins.
  
  vector[R] alpha; // Parameters for Dirichlet prior.
}

parameters {
  // Vector of ratings for each team.
  // The simplex constrains the ratings to sum to 1 
  simplex[R] ratings;
}

model {
  real prob_1_wins[M]; // Win probabilities for player 1
  ratings ~ dirichlet(alpha); // Dirichlet prior.
  
  for (m in 1:M){
    prob_1_wins[m] = ratings[rider_id_1[m]] / (ratings[rider_id_0[m]] + ratings[rider_id_1[m]]);
    outcome[m] ~ bernoulli(prob_1_wins[m]);
  }
}
