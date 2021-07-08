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

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0> P; // Pairings
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=R> pairings[P,2];
  int<lower=1,upper=3> sprints[M];
}

transformed data {
  real<lower=0> scale_icept =1; // prior std for the intercept
  real<lower=0> scale_global=1 ; // scale for the half -t prior for tau
  real<lower=1> nu_global =1; // degrees of freedom for the half -t priors for tau
  real<lower=1> nu_local =1; // degrees of freedom for the half - t priors for lambdas
  real<lower=0> slab_scale = 0.5; // slab scale for the regularized horseshoe
  real<lower=0> slab_df = 25; // slab degrees of freedom for the regularized horseshoe
}

parameters {
  // rating vector
  vector[R] alpha;
  // parameters for horsehoe pair effect
  vector[P] z;
  real<lower=0> aux1_global ;
  real<lower=0> aux2_global ;
  vector<lower=0>[P] aux1_local ;
  vector<lower=0>[P] aux2_local ;
  real<lower=0> caux ;
}

transformed parameters {
  vector[P] gamma;
  matrix[R,R] gamma_mat = rep_matrix(0, R, R);
  vector[M] mu;
  real<lower=0> tau;
  vector<lower=0>[P] lambda ; // local shrinkage parameter
  vector<lower=0>[P] lambda_tilde ; // ’ truncated ’ local shrinkage parameter
  real<lower=0>c; // slab scale
  lambda = aux1_local .* sqrt ( aux2_local );
  tau = aux1_global * sqrt ( aux2_global ) * scale_global;
  c = slab_scale * sqrt ( caux );
  lambda_tilde = sqrt ( c ^2 * square ( lambda ) ./ (c ^2 + tau ^2* square ( lambda )) );
  gamma = z .* lambda_tilde * tau ;

  
  for(p in 1:P){
    gamma_mat[pairings[p,1], pairings[p,2]] =  gamma[p];
    gamma_mat[pairings[p,2], pairings[p,1]] = -gamma[p];
  }
  
  for(m in 1:M){
    mu[m] =  alpha[winner_id[m]] - alpha[loser_id[m]] + gamma_mat[winner_id[m], loser_id[m]];
  }
}

model {
  z ~ normal (0 , 1);
  aux1_local ~ normal (0 , 1);
  aux2_local ~ inv_gamma (0.5* nu_local , 0.5* nu_local );
  aux1_global ~ normal (0 , 1);
  aux2_global ~ inv_gamma (0.5* nu_global , 0.5* nu_global );
  caux ~ inv_gamma (0.5* slab_df , 0.5* slab_df );
  
  alpha ~ normal(0,1);
  sprints ~ match_logit(mu);
}