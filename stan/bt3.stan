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
    real match_log_loss(int[] s, vector alpha){
    for(n in 1:num_elements(s)){
      if (s[n] == 1)
        return -binomial_logit_lpmf(1 | 1, alpha);
      else
        return -log_sum_exp(binomial_logit_lpmf(2 | 2, alpha),  binomial_logit_lpmf(2 | 3, alpha) - log(3) + log(2));
    }
  }
}

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  int<lower=0> D; // Dates
  int<lower=0> Z; // No. Riders with multiple dates
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
  int<lower=0> winner_date_no[M];
  int<lower=0> loser_date_no[M];
  int<lower=0> date_diffs[D];
  int<lower=0> date_diffs_rider_pos[R];
}

parameters {
  // rider ratings
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[R] alpha0;
  vector[Z] zeta;
}

transformed parameters {
  vector[D] alpha;
  vector[M] delta;
  {
  int z = 1;
  
  for(r in 1:R-1){
    for(s in date_diffs_rider_pos[r]:date_diffs_rider_pos[r+1]-1){
      if(s == date_diffs_rider_pos[r]){
        alpha[s] = alpha0[r];
    }
      else{
        alpha[s] = alpha[s-1] + sqrt(date_diffs[s]* inv(365)) * zeta[z];
        z+=1;
      }
    }
  }
  
    for(s in date_diffs_rider_pos[R]:D){
      if(s ==date_diffs_rider_pos[R])
        alpha[s] = alpha0[R];
      else
        alpha[s] = alpha[s-1] + sqrt(date_diffs[s]/365) * zeta[z];
    }
    
  for(m in 1:M){
    delta[m] = alpha[date_diffs_rider_pos[winner_id[m]] + winner_date_no[m]] - 
    alpha[date_diffs_rider_pos[loser_id[m]] + loser_date_no[m]];
  }

  }
}


model {
  sigma ~ gamma(15,40);
  tau ~ student_t(3,0,1);
  alpha0 ~ normal(0,sigma);
  zeta ~ normal(0,tau);
  head(sprints, T) ~ match_logit(head(delta, T));
}

generated quantities {
  real max_delta = max(alpha0) - min(alpha0);

  real training_avg_log_loss = -inv(T) * bernoulli_logit_lpmf(1 | head(delta, T));
  real evaluation_avg_log_loss = -inv(M-T) * bernoulli_logit_lpmf(1 | tail(delta, M - T));
  
  real training_avg_match_log_loss = inv(T) * match_log_loss(head(sprints, T), head(delta, T));
  real evaluation_avg_match_log_loss = inv(M-T) * match_log_loss( tail(sprints, M - T), tail(delta, M - T));
  
  real training_accuracy = 0;
  real evaluation_accuracy = 0;
  
  for(m in 1:T){
    training_accuracy += (delta[m] > 0);
  }
    for(m in (T+1):M){
    evaluation_accuracy += (delta[m] > 0);
  }
  
  training_accuracy = training_accuracy * inv(T);
  evaluation_accuracy = evaluation_accuracy * inv(M-T);
}