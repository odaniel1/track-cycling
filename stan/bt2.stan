
functions {
  
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
}

data {
  // Dimensions
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches

  // Match results
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
}

parameters {
  // rider ratings
  real<lower=0> sigma;
  vector[R] alpha0;
}

transformed parameters {
  // difference of winner and loser rating
  vector[M] delta =alpha0[winner_id] - alpha0[loser_id];
}

model {
  // (hierarchical) priors - strength
  sigma ~ gamma(80,60);
  alpha0 ~ normal(0,sigma);
  
  // likelihood
  1 ~ bernoulli_logit(head(delta, T));
}
generated quantities {
  // maximum theoretical strength difference between two riders
  real<lower=0> delta_max = max(alpha0) - min(alpha0);
  
  // Calculate log-loss, match log-loss and accuracy. A separate value is returned
  // for training/evaluation data, and within each of these the metrics are further
  // broken down by round (5 rounds in total) and the total (6th entry in vectors).
  real training_accuracy;
  real evaluation_accuracy;
  real training_log_loss;
  real evaluation_log_loss;
  
  training_accuracy = accuracy(delta,1, T);
  training_log_loss = log_loss(delta, 1,T);
  evaluation_accuracy = accuracy(delta, T+1, M);
  evaluation_log_loss = log_loss(delta, T+1, M);
}