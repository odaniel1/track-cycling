// Adapted from Bob Carpenter's example model at
// https://github.com/stan-dev/example-models/blob/master/knitr/bradley-terry/individual.stan

functions {
  real match_logit_lpmf(int[] s, vector alpha0){
    for(n in 1:num_elements(s)){
      if (s[n] < 3)
        return binomial_logit_lpmf(s[n] | s[n], alpha0);
      else
        return binomial_logit_lpmf(2 | 3, alpha0) - log(3) + log(2);
    }
  }
    real match_log_loss(int[] s, vector alpha0){
    for(n in 1:num_elements(s)){
      if (s[n] == 1)
        return -binomial_logit_lpmf(1 | 1, alpha0);
      else
        return -log_sum_exp(binomial_logit_lpmf(2 | 2, alpha0),  binomial_logit_lpmf(2 | 3, alpha0) - log(3) + log(2));
    }
  }
}

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
}

parameters {
  // rider ratings
  real<lower=0> sigma;
  vector[R] alpha0;
}

transformed parameters {
  // difference of winner and loser rating
  vector[M] delta = alpha0[winner_id] - alpha0[loser_id];
}

model {
  sigma ~ student_t(3,0,1);
  alpha0 ~ normal(0,sigma); 
  sprints ~ match_logit(delta);
}

generated quantities {
  real avg_log_loss = -inv(M) * bernoulli_logit_lpmf(1 | delta);
  real avg_match_log_loss = inv(M) * match_log_loss(sprints,  delta);
}