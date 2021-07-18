// Adapted from Bob Carpenter's example model at
// https://github.com/stan-dev/example-models/blob/master/knitr/bradley-terry/individual.stan

functions {
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
  int<lower=0,upper = M> T; // Training matches
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
  int<lower=1,upper=M> split_round_index[10];
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
  sigma ~ gamma(15,40);
  alpha0 ~ normal(0,sigma); 
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
  {
   // Training data 
   for(r in 1:5){
    for(m in split_round_index[r]:(split_round_index[r+1]-1)){training_accuracy[r] += (delta[m] > 0);}
    training_accuracy[r] = inv(split_round_index[r+1] - split_round_index[r]) * training_accuracy[r];
    
    training_log_loss[r] = - inv(split_round_index[r+1] - split_round_index[r]) *
      bernoulli_logit_lpmf(1 | segment(delta, split_round_index[r], split_round_index[r+1] - split_round_index[r]));
      
    training_match_log_loss[r] = inv(split_round_index[r+1] - split_round_index[r]) *
      match_log_loss(segment(sprints, split_round_index[r], split_round_index[r+1] - split_round_index[r]),
                     segment(delta, split_round_index[r], split_round_index[r+1] - split_round_index[r]));
  }
  
  // Evaluation data
  for(r in 6:9){
    for(m in split_round_index[r]:(split_round_index[r+1]-1)){evaluation_accuracy[r-5] += (delta[m] > 0);}
    evaluation_accuracy[r-5] = inv(split_round_index[r+1] - split_round_index[r]) * evaluation_accuracy[r-5];
    
    evaluation_log_loss[r-5] = - inv(split_round_index[r+1] - split_round_index[r]) *
      bernoulli_logit_lpmf(1 | segment(delta, split_round_index[r], split_round_index[r+1] - split_round_index[r]));
      
    evaluation_match_log_loss[r-5] = inv(split_round_index[r+1] - split_round_index[r]) *
      match_log_loss(segment(sprints, split_round_index[r], split_round_index[r+1] - split_round_index[r]),
                     segment(delta, split_round_index[r], split_round_index[r+1] - split_round_index[r]));
  }
  
  // Evaluation data (final row, not indexed)
    for(m in split_round_index[10]:M){evaluation_accuracy[5] += (delta[m] > 0);}
    evaluation_accuracy[5] = inv(M - split_round_index[10]) * evaluation_accuracy[5];
    
    evaluation_log_loss[5] = - inv(M - split_round_index[10]) *
      bernoulli_logit_lpmf(1 | segment(delta, split_round_index[10], M - split_round_index[10]));
      
    evaluation_match_log_loss[5] = inv(M - split_round_index[10]) *
      match_log_loss(segment(sprints, split_round_index[10],  M - split_round_index[10]),
                     segment(delta, split_round_index[10],  M - split_round_index[10]));
  }  
}
