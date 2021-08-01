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
  // implementation from
  // https://github.com/avehtari/casestudies/blob/master/Motorcycle/gpbasisfun_functions.stan
  vector diagSPD_exp_quad(real alpha, real rho, real L, int B) {
  return sqrt((alpha^1) * sqrt(2*pi()) * rho * exp(-0.5*(rho*pi()/2/L)^2 * linspaced_vector(B, 1, B)^2));
  }
  
  matrix PHI(int N, int B, real L, vector x) {
  return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), B), linspaced_vector(B, 1, B)))/sqrt(L);
  }
}

data {
  // Dimensions
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  int<lower=0> D; // Dates
  int<lower=0> E; // Events
  
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
  
  // qualifying time difference;
  real qual_diff[M];
  int event[M];
}

transformed data {
  vector[D] d_sc;  // rider dates centred/scaled
  real<lower=0> L; // GP basis function window length
  matrix[D,B] PHI_f; // GP basis function matrix
  
  {
    real mu_d = mean(rider_dates);
    real sd_d = sd(rider_dates);
    d_sc = (rider_dates-mu_d)/sd_d;
    L = 1.5 * max(d_sc); // set based on https://avehtari.github.io/casestudies/Motorcycle/motorcycle.html
  }
  
 PHI_f = PHI(D, B, L, d_sc);
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
	
	// qualifying time differences;
	real kappa;
	real<lower=0> psi;
	real phi[E];
}

transformed parameters{
	vector[D] f;       // approximate GP
	vector[D] alphaD;  // approx. GP with centered on average strength
  vector[M] delta;   // match differences
  real eta_theta[R];
  
  for(r in 1:R){
    
    eta_theta[r] = eta + theta[r];
    
    vector[B] diagSPD_f_r = diagSPD_exp_quad(tau[r], rho[r], L, B);
    
    // basis function approx. to Gaussian Process
    f[date_index_R[r]:(date_index_R[r+1]-1)] =
    PHI_f[date_index_R[r]:(date_index_R[r+1]-1),] * (diagSPD_f_r .* segment(zeta, 1 + (r-1)*B, B));

    // time dependent rider strength
    alphaD[date_index_R[r]:(date_index_R[r+1]-1)] =  alpha0[r] + f[date_index_R[r]:(date_index_R[r+1]-1)];
  }
	  
	for(m in 1:M){
	  delta[m] = alphaD[date_index_R[winner_id[m]] + winner_date_no[m]] - alphaD[date_index_R[loser_id[m]] + loser_date_no[m]] +
      (winner_at_home[m] * eta_theta[winner_id[m]]) - (loser_at_home[m] * eta_theta[loser_id[m]]) -
      (kappa + phi[event[m]]) * qual_diff[m];
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
  tau_pr ~ inv_gamma(11,1);
	rho ~ normal(rho_pr, 0.1);		// lengthscale GP
	tau ~ normal(tau_pr, 0.05);		// magnitude GP
	zeta ~ normal(0,1);   	  // latent variables for approx. Gaussian Process

	// qualifying time difference;
	kappa ~ normal(4,4);
	psi ~ normal(0,0.5);
	phi ~ normal(0, psi);

  sprints ~ match_logit(delta);
}