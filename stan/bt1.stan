
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
      
      ll += -bernoulli_logit_lpmf(1 | delta_seg);
      return inv(num_elements(delta_seg)) * ll;
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
  sigma ~ gamma(15,40);
  alpha0 ~ normal(0,sigma);
  
  // likelihood
  1 ~ bernoulli_logit(head(delta, T));
}

generated quantities {
  // Calculate log-loss, match log-loss and accuracy. This is broken down into individual
  // rounds and split data (eg. log loss for Finals Evaluation data)
  vector[5] training_accuracy = rep_vector(0,5);
  vector[5] evaluation_accuracy = rep_vector(0,5);
  vector[5] training_log_loss = rep_vector(0,5);
  vector[5] evaluation_log_loss = rep_vector(0,5);
  vector[5] training_match_log_loss = rep_vector(0,5);
  vector[5] evaluation_match_log_loss = rep_vector(0,5);

  // Training data 
  for(r in 1:5){
    training_accuracy[r] = accuracy(delta, split_round_index[r], split_round_index[r+1]-1);
    training_log_loss[r] = log_loss(delta, split_round_index[r], split_round_index[r+1]-1);
    evaluation_accuracy[r] = accuracy(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_log_loss[r] = log_loss(delta, split_round_index[r+5], split_round_index[r+6]-1);
  }
} 
