// Adapted from Bob Carpenter's example model at
// https://github.com/stan-dev/example-models/blob/master/knitr/bradley-terry/individual.stan

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
}

parameters {
  // rating vector
  vector[R] alpha;
}

model {
  alpha ~ normal(0,1); 
  1 ~ bernoulli_logit(alpha[winner_id] - alpha[loser_id]);
}

generated quantities {
  real avg_log_loss = -inv(M) * bernoulli_logit_lpmf(1 | alpha[winner_id] - alpha[loser_id]);
}
