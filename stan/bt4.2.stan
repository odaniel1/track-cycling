
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
  
  // basis function approx. to GPs, developed by Riutort-Mayol et al. arxiv.org/2004.11408
  // implementation adapted from github.com/gabriuma/basis_functions_approach_to_GP

  // square root of m-th eigenvalue (of Laplacian with Dirichlet bnd. conds.)
  real sqrt_lambda(real L, int m){ return m * pi()/(2 * L); }
  
  // m-th eigenvalue
  real lambda(real L, int m){ return sqrt_lambda(L, m)^2;}
  
  // m-th eigenfunction
  vector phi(real L, int m, vector x){
    return (1/sqrt(L)) * sin( sqrt_lambda(L,m) * (L + x));
  }  
  
  // spectral density (spd) of exponentiated quadratic (exp_quad) kernel
  real spd_exp_quad(real tau, real rho, real w){
    return tau^2 * sqrt(2 * pi()) * rho * exp(-0.5 * (rho * w)^2);
  }
  
  // diagonal matrix for eigen-decomposition of exponentiated quadratic kernel
  vector diag_exp_quad(real L, int B, real tau, real rho){
    vector[B] diag_mat;
    for(b in 1:B) diag_mat[b] = spd_exp_quad(tau, rho, sqrt_lambda(L,b));
    return diag_mat;
  }
}

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  int<lower=0> D; // Dates
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=M> split_round_index[10];
  int<lower=1,upper=3> sprints[M];
  int<lower=0> winner_date_no[M];
  int<lower=0> loser_date_no[M];
  int<lower=0> date_index_R[R];
  vector[D] rider_dates;
  
  // basis function approx.
  int<lower=1> B; 
}

transformed data {
  vector[D] d_sc;
  real<lower=0> L;
  matrix[D,B] PHI;
  
  {
    real mu_d = mean(rider_dates);
    real sd_d = sd(rider_dates);
    d_sc = (rider_dates-mu_d)/sd_d;
    L = (max(d_sc) - min(d_sc)) * 5.0/2.0;

    // rider GP approximation      
    for(b in 1:B) PHI[,b] = phi(L,b,d_sc);
  }
}

parameters {
  // Rider average strength
  real alpha0[R];
  real<lower=0>sigma;
  
  // GP parameters and hyperparameters
	real<lower=0> rho_pr;
	real<lower=0> tau_pr;
	vector<lower=0>[R] rho;
	vector<lower=0>[R] tau;
	
	// latent variables for GP approx.
	vector[R*B] zeta;
}

transformed parameters{
	vector[D] f;
	vector[D] alphaD;
  vector[M] delta;
	  {
	    
    for(r in 1:R-1){
        f[date_index_R[r]:(date_index_R[r+1]-1)] =
          PHI[date_index_R[r]:(date_index_R[r+1]-1),] *  (sqrt(diag_exp_quad(L, B, tau[r], rho[r])) .* segment(zeta, 1 + (r-1)*B, B));
        
        alphaD[date_index_R[r]:(date_index_R[r+1]-1)] =  alpha0[r] + f[date_index_R[r]:(date_index_R[r+1]-1)];
    }
  
      f[date_index_R[R]:D] =
          PHI[date_index_R[R]:D,] *  (sqrt(diag_exp_quad(L, B, tau[R], rho[R])) .* segment(zeta, 1 + (R-1)*B, B));
      
      alphaD[date_index_R[R]:D] = alpha0[R] + f[date_index_R[R]:D];
	  }
	  
	  for(m in 1:M){
	    delta[m] = alphaD[date_index_R[winner_id[m]] + winner_date_no[m]] - alphaD[date_index_R[loser_id[m]] + loser_date_no[m]];
	  }
}


model{
  // rider average strength
  sigma ~ normal(0,2); //gamma(15,40);
	alpha0 ~ normal(0,sigma);

  // Gaussian process lengthscale/magnitude
  rho_pr ~ inv_gamma(6,3);
  tau_pr ~ inv_gamma(10,2);
	rho ~ normal(0,rho_pr);		// lengthscale GP
	tau ~ normal(0,tau_pr);		// magnitud GP

	// latent variables for approx. Gaussian Process 
	zeta ~ normal(0,1);

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