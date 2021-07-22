
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
      
      return -inv(num_elements(delta_seg)) * bernoulli_logit_lpmf(1 | delta_seg);
  }
  
  real match_log_loss(int[] s, vector delta, int start, int end){
      int s_seg[end - start + 1]  = segment(s, start, end - start + 1);
      vector[end - start + 1] delta_seg = segment(delta, start, end - start + 1); 
      real mll = 0;
      
      return -inv(num_elements(s_seg)) * match_logit_lpmf(s_seg | delta_seg);
  }
}

data {
  // Dimensions
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  
  // Index for where train/test and round splits start 
  int<lower=1,upper=M> split_round_index[11];

  // Match results
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
  vector[M] delta =alpha0[winner_id] - alpha0[loser_id];
}

model {
  // (hierarchical) priors - strength
  sigma ~ gamma(80,60);
  alpha0 ~ normal(0,sigma);
  
  // likelihood
  head(sprints, T) ~ match_logit(head(delta, T));
}

generated quantities {
  // maximum theoretical strength difference between two riders
  real<lower=0> delta_max = max(alpha0) - min(alpha0);
  
  // Calculate log-loss, match log-loss and accuracy. A separate value is returned
  // for training/evaluation data, and within each of these the metrics are further
  // broken down by round (5 rounds in total) and the total (6th entry in vectors).
  vector[6] training_accuracy = rep_vector(0,6);
  vector[6] evaluation_accuracy = rep_vector(0,6);
  vector[6] training_log_loss = rep_vector(0,6);
  vector[6] evaluation_log_loss = rep_vector(0,6);
  vector[6] training_match_log_loss = rep_vector(0,6);
  vector[6] evaluation_match_log_loss = rep_vector(0,6);

  // Training data 
  for(r in 1:5){
    training_accuracy[r] = accuracy(delta, split_round_index[r], split_round_index[r+1]-1);
    training_log_loss[r] = log_loss(delta, split_round_index[r], split_round_index[r+1]-1);
    training_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r], split_round_index[r+1]-1);
    evaluation_accuracy[r] = accuracy(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_log_loss[r] = log_loss(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r+5], split_round_index[r+6]-1);
  }
  
    training_accuracy[6] = accuracy(delta,1, T);
    training_log_loss[6] = log_loss(delta, 1,T);
    training_match_log_loss[6] = match_log_loss(sprints, delta, 1,T);
    evaluation_accuracy[6] = accuracy(delta, T+1, M);
    evaluation_log_loss[6] = log_loss(delta, T+1, M);
    evaluation_match_log_loss[6] = match_log_loss(sprints, delta, T+1, M);
}