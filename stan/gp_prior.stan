functions {
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
  int<lower=0> D; // Dates
  int<lower=0> date_index_R[R+1];
  vector[D] rider_dates;
  
  // basis function approx.
  int<lower=1> B;
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
  // GP parameters and hyperparameters
	real<lower=0> rho_pr;   // lengthscale hyperparameter
	real<lower=0> tau_pr;   // magnitude hyperparameter
	vector<lower=0>[R] rho; // lengthscale
	vector<lower=0>[R] tau; // magnitude
	vector[R*B] zeta; 	    // latent variables
}

transformed parameters{
	vector[D] f;       // approximate GP
  for(r in 1:R){
    vector[B] diagSPD_f_r = diagSPD_exp_quad(tau[r], rho[r], L, B);
    
    // basis function approx. to Gaussian Process
    f[date_index_R[r]:(date_index_R[r+1]-1)] =
    PHI_f[date_index_R[r]:(date_index_R[r+1]-1),] * (diagSPD_f_r .* segment(zeta, 1 + (r-1)*B, B));
  }
}

model{
  // Gaussian process lengthscale/magnitude
  rho_pr ~ inv_gamma(6,3);
  tau_pr ~ inv_gamma(11,1);
	rho ~ normal(rho_pr, 0.1);		// lengthscale GP
	tau ~ normal(tau_pr, 0.05);		// magnitude GP
	zeta ~ normal(0,1);   	  // latent variables for approx. Gaussian Process
}