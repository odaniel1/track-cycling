
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
  // Dimensions
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  int<lower=0> D; // Dates
    
  // Index for where train/test and round splits start 
  int<lower=1,upper=M> split_round_index[11];
  
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
    
  // Home advantage effects
  real<lower=0,upper=1> winner_at_home[M];
  real<lower=0,upper=1> loser_at_home[M];
  
  // Time series effects
  int<lower=0> winner_date_no[M];
  int<lower=0> loser_date_no[M];
  int<lower=0> date_index_R[R+1];
  vector[D] rider_dates;
  
  // basis function approx.
  int<lower=1> B; 
}

transformed data {
  vector[D] d_sc;  // rider dates centred/scaled
  real<lower=0> L; // GP basis function window length
  matrix[D,B] PHI; // GP basis function matrix
  
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
    
  // home advantage effect
  real<lower=0>eta;
  real<lower=0>upsilon;
  real theta[R];
  
  // GP parameters and hyperparameters
	real<lower=0> rho_pr;   // lengthscale hyperparameter
	real<lower=0> tau_pr;   // magnitude hyperparameter
	vector<lower=0>[R] rho; // lengthscale
	vector<lower=0>[R] tau; // magnitude
	vector[R*B] zeta; 	    // latent variables
}

transformed parameters{
	vector[D] f;       // approximate GP
	vector[D] alphaD;  // approx. GP with centered on average strength
  vector[M] delta;   // match differences
 
  for(r in 1:R){
    // basis function approx. to Gaussian Process
    f[date_index_R[r]:(date_index_R[r+1]-1)] =
    PHI[date_index_R[r]:(date_index_R[r+1]-1),] *  (sqrt(diag_exp_quad(L, B, tau[r], rho[r])) .* segment(zeta, 1 + (r-1)*B, B));
    
    // time dependent rider strength
    alphaD[date_index_R[r]:(date_index_R[r+1]-1)] =  alpha0[r] + f[date_index_R[r]:(date_index_R[r+1]-1)];
  }
	  
	for(m in 1:M){
	  delta[m] = alphaD[date_index_R[winner_id[m]] + winner_date_no[m]] - alphaD[date_index_R[loser_id[m]] + loser_date_no[m]] +
      winner_at_home[m] * (eta + theta[winner_id[m]]) - loser_at_home[m] * (eta + theta[winner_id[m]]);
	}
}

model{
  // (hierarchical) priors - strength
  sigma ~ gamma(80,60);
  alpha0 ~ normal(0,sigma); 
  
  // (hierarchical) priors - home effect
  eta ~ gamma(2,4);
  upsilon ~ normal(0,0.2);
  theta ~ normal(0, upsilon);

  // Gaussian process lengthscale/magnitude
  rho_pr ~ inv_gamma(6,3);
  tau_pr ~ inv_gamma(10,2);
	rho ~ normal(0,rho_pr);		// lengthscale GP
	tau ~ normal(0,tau_pr);		// magnitud GP
	zeta ~ normal(0,1);   	  // latent variables for approx. Gaussian Process

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
  vector[6] training_match_log_loss = rep_vector(0,6);
  vector[6] evaluation_match_log_loss = rep_vector(0,6);

  // Training data 
  for(r in 1:5){
    training_accuracy[r] = accuracy(delta, split_round_index[r], split_round_index[r+1]-1);
    training_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r], split_round_index[r+1]-1);
    evaluation_accuracy[r] = accuracy(delta, split_round_index[r+5], split_round_index[r+6]-1);
    evaluation_match_log_loss[r] = match_log_loss(sprints, delta, split_round_index[r+5], split_round_index[r+6]-1);
  }
  
    training_accuracy[6] = accuracy(delta,1, T);
    training_match_log_loss[6] = match_log_loss(sprints, delta, 1,T);
    evaluation_accuracy[6] = accuracy(delta, T+1, M);
    evaluation_match_log_loss[6] = match_log_loss(sprints, delta, T+1, M);
}