
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
  int<lower=0> M; // Matches
  int<lower=0> T; 
    
  // Index for where train/test and round splits start 
  int<lower=1,upper=M> split_round_index[11];
  int<lower=1,upper=3> sprints[M];

  // qualifying time difference;
  real qual_diff[M];
}

parameters {
	// qualifying time difference;
	real<lower=0> psi;
	real kappa;
}

transformed parameters{
  vector[M] delta;   // match differences

	for(m in 1:M){
	  delta[m] = kappa * qual_diff[m];
	}
}

model{
	// qualifying time difference;
	psi ~ normal(0,0.5);
	kappa ~ normal(0,psi);

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