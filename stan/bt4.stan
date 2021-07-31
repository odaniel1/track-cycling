
functions {
  real match_logit_lpmf(int[] s, vector delta){
    real lpmf = 0;
    vector[num_elements(delta)] p = inv_logit(delta);
    
    for(n in 1:num_elements(s)){
      if (s[n] == 1) lpmf += log(p[n]);
      else if (s[n] == 2) lpmf += 2*log(p[n]);
      else lpmf += log(2) + 2*log(p[n]) + log(1-p[n]);
    }
    return lpmf;
  }
  
  real accuracy(vector delta, int start, int end){
    vector[end - start + 1] delta_seg = segment(delta, start, end - start + 1);
    real acc = 0;
    
    for(n in 1:num_elements(delta_seg)) acc += (delta_seg[n] > 0);
    
    return inv(num_elements(delta_seg)) * acc;
  }
  
  real log_loss(vector delta, int start, int end){
    vector[end - start + 1] delta_seg = segment(delta, start, end - start + 1); 
    real ll = 0;
    
    return inv(num_elements(delta_seg)) * bernoulli_logit_lpmf(1 | delta_seg);
  }
  
  real match_log_loss(int[] s, vector delta, int start, int end){
    int s_seg[end - start + 1]  = segment(s, start, end - start + 1);
    vector[end - start + 1] delta_seg = segment(delta, start, end - start + 1); 
    real mll = 0;
    
    return inv(num_elements(s_seg)) * match_logit_lpmf(s_seg | delta_seg);
  }
}

data {
  // Dimensions
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches

  // Match results
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
  
  // Home advantage effects
  real<lower=0,upper=1> winner_at_home[M];
  real<lower=0,upper=1> loser_at_home[M];
}

parameters {
  // rider ratings
  real<lower=0> sigma;
  vector[R] alpha0;
  
  // home advantage effect
  real<lower=0>eta;
  real<lower=0>upsilon;
  real theta[R];
}

transformed parameters {
  // difference of winner and loser rating
  vector[M] delta;
  real eta_theta[R];
  
  for(r in 1:R) eta_theta[r] = eta + theta[r];
  
  for(m in 1:M){
    
    delta[m] = 
      alpha0[winner_id[m]] - alpha0[loser_id[m]] +
      (winner_at_home[m] * eta_theta[winner_id[m]]) - (loser_at_home[m] * eta_theta[loser_id[m]]);
  }
}

model {
  // (hierarchical) priors - strength
  sigma ~ gamma(80,60);
  alpha0 ~ normal(0,sigma); 
  
  // (hierarchical) priors - home effect
  eta ~ gamma(2,4);
  upsilon ~ normal(0,0.2);
  theta ~ normal(0, upsilon);
  
  // likelihood
  head(sprints, T) ~ match_logit(head(delta, T));
}

generated quantities {
  // maximum theoretical strength difference between two riders
  real<lower=0> delta_max = max(alpha0) - min(alpha0);
  
  // Calculate log-loss, match log-loss and accuracy. A separate value is returned
  // for training/evaluation data, and within each of these the metrics are further
  // broken down by round (5 rounds in total) and the total (6th entry in vectors).
  real training_accuracy;
  real evaluation_accuracy;
  real training_match_log_loss;
  real evaluation_match_log_loss;

  training_accuracy = accuracy(delta,1, T);
  training_match_log_loss = match_log_loss(sprints, delta, 1,T);
  evaluation_accuracy = accuracy(delta, T+1, M);
  evaluation_match_log_loss = match_log_loss(sprints, delta, T+1, M);
}