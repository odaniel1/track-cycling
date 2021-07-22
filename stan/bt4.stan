
functions {
  real match_logit_lpmf(int[] s, vector delta){
    for(n in 1:num_elements(s)){
      if (s[n] < 3)
        return binomial_logit_lpmf(s[n] | s[n], delta);
      else
        return binomial_logit_lpmf(2 | 3, delta) - log(3) + log(2);
    }
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
      
      ll += -bernoulli_logit_lpmf(1 | delta_seg);
      return inv(num_elements(delta_seg)) * ll;
  }
  
  real match_log_loss(int[] s, vector delta, int start, int end){
      int s_seg[end - start + 1]  = segment(s, start, end - start + 1);
      vector[end - start + 1] delta_seg = segment(delta, start, end - start + 1); 
      real mll = 0;
      
      for(n in 1:num_elements(s_seg)){
        if (s_seg[n] == 1) mll += -binomial_logit_lpmf(1 | 1, delta_seg[n]);
        else mll += -log_sum_exp(binomial_logit_lpmf(2 | 2, delta_seg[n]),  binomial_logit_lpmf(2 | 3, delta_seg[n]) - log(3) + log(2));
      }
      
      mll = inv(num_elements(s_seg)) * mll;
      return mll;
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
  
  for(m in 1:M){
    delta[m] = 
      alpha0[winner_id[m]] - alpha0[loser_id[m]] +
      winner_at_home[m] * (eta + theta[winner_id[m]]) - loser_at_home[m] * (eta + theta[winner_id[m]]);
  }
}

model {
  // (hierarchical) priors - strength
  sigma ~ gamma(15,40);
  alpha0 ~ normal(0,sigma); 
  
  // (hierarchical) priors - home effect
  eta ~ normal(0,0.5);
  upsilon ~ normal(0,0.1);
  theta ~ normal(0, upsilon);
  
  // likelihood
  head(sprints, T) ~ match_logit(head(delta, T));
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
    training_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r], split_round_index[r+1]-1);
    evaluation_accuracy[r] = accuracy(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_log_loss[r] = log_loss(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r+5], split_round_index[r+6]-1);
  }
}