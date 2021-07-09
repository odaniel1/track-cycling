// Adapted from Bob Carpenter's example model at
// https://github.com/stan-dev/example-models/blob/master/knitr/bradley-terry/individual.stan

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
}

parameters {
  // rider ratings
  real<lower=0> sigma;
  vector[R] alpha;
}

transformed parameters {
  // difference of winner and loser rating
  vector[M] delta = alpha[winner_id] - alpha[loser_id];
}

model {
  sigma ~ student_t(3,0,1);
  alpha ~ normal(0,sigma); 
  1 ~ bernoulli_logit(delta);
}

generated quantities {
  real avg_log_loss = -inv(M) * bernoulli_logit_lpmf(1 | alpha[winner_id] - alpha[loser_id]);
}
