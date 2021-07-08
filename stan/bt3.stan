// Adapted from Bob Carpenter's example model at
// https://github.com/stan-dev/example-models/blob/master/knitr/bradley-terry/individual.stan

functions {
  real match_logit_lpmf(int[] s, vector alpha){
    for(n in 1:num_elements(s)){
      if (s[n] < 3)
        return binomial_logit_lpmf(s[n] | s[n], alpha);
      else
        return binomial_logit_lpmf(2 | 3, alpha) - log(3) + log(2);
    }
  }
}
}

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0> P; // Pairings
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=R> pairings[P,2];
  int<lower=1,upper=3> sprints[M];
}

parameters {
  // rating vector
  vector[R] alpha;
  vector[P] gamma;
}

transformed parameters {
  matrix[R,R] gamma_mat = rep_matrix(0, R, R);
  vector[M] mu;
  
  for(p in 1:P){
    gamma_mat[pairings[p,1], pairings[p,2]] =  gamma[p];
    gamma_mat[pairings[p,2], pairings[p,1]] = -gamma[p];
  }
  
  for(m in 1:M){
    mu[m] =  alpha[winner_id[m]] - alpha[loser_id[m]] + gamma_mat[winner_id[m], loser_id[m]];
  }
}

model {
  alpha ~ normal(0,1); 
  gamma ~ normal(0, 0.1);
  sprints ~ match_logit(mu);
}